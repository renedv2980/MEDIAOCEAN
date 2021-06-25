*          DATA SET NEWRIDRBB  AT LEVEL 063 AS OF 05/01/02                      
*PHASE T00A4DX,+0                                                               
*INCLUDE NETACC                                                                 
*INCLUDE NETCOM                                                                 
*INCLUDE NETNET                                                                 
*INCLUDE GETPEST                                                                
*INCLUDE NETSPB                                                                 
*INCLUDE MSPACKS                                                                
*INCLUDE MSUNPKS                                                                
*INCLUDE NEPACC                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'T00A4D - DRIVER FOR NETWORK WRITER'                             
***********************************************************                     
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
WRIDRIVE CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEDV**                                                       
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING WRIDRIVE,RB,R8,R7,R6                                             
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         SPACE 1                                                                
*                                  TEST HOOK VALUE TO SEE WHERE WE ARE          
         CLI   GLHOOK,GLRESOLV                                                  
         BNE   ND2                                                              
         BAS   RE,SYSINIT                                                       
         B     XIT                                                              
         SPACE 1                                                                
ND2      CLI   GLHOOK,GLROUT                                                    
         BNE   ND6                                                              
         CLI   GLMODE,GLINPUT                                                   
         BNE   ND4                                                              
         BAS   RE,SYSINP                                                        
         B     XIT                                                              
         SPACE 1                                                                
ND4      CLI   GLMODE,GLOUTPUT                                                  
         BNE   XIT                                                              
         BAS   RE,SYSOUTP                                                       
         B     XIT                                                              
         SPACE 1                                                                
ND6      CLI   GLHOOK,GLINCOMP                                                  
         BNE   XIT                                                              
         BAS   RE,SYSCOMP                                                       
         B     XIT                                                              
         EJECT                                                                  
*              SYSTEM INITIALIZATION                                            
         SPACE 3                                                                
SYSINIT  NTR1                                                                   
         L     R2,=A(GETPESTB)     INITIALIZE GETPEST                           
         USING GETPESTD,R2                                                      
         XC    GTPBAMC,GTPBAMC                                                  
         DROP  R2                                                               
         L     R1,=A(ROUTLIST)                                                  
         LA    R2,GLLABEL                                                       
         SPACE 1                                                                
SYSINIT2 CLC   0(8,R1),0(R2)                                                    
         BE    SYSINIT4                                                         
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         B     SYSINIT2                                                         
         SPACE 1                                                                
SYSINIT4 MVC   GLAROUT,8(R1)                                                    
*                                                                               
                                                                                
         L     RF,=A(TCDOLDB)      INIT BUCKETS                                 
         ZAP   0(L'TCDOLDB,RF),=P'0'                                            
         L     RF,=A(TCDOLCR)      INIT BUCKETS                                 
         ZAP   0(L'TCDOLCR,RF),=P'0'                                            
         L     RF,=A(TBDOLDB)      INIT BUCKETS                                 
         ZAP   0(L'TBDOLDB,RF),=P'0'                                            
         L     RF,=A(TBDOLCR)      INIT BUCKETS                                 
         ZAP   0(L'TBDOLCR,RF),=P'0'                                            
*                                                                               
*        INIT ALL TOTALLING BUCKETS                                             
*                                                                               
         L     RF,=A(TOTBKS)       START OF BUCKETS                             
         LA    R0,TOTNMBKQ         TOTAL NUMBER OF BUCKETS                      
*                                                                               
         ZAP   0(L'TOTBKS,RF),=P'0' INIT BUCKET                                 
         LA    RF,L'TOTBKS(RF)     BUMP BUCKET                                  
         BCT   R0,*-10                                                          
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              CONTROL OF INPUT/OUTPUT                                          
         SPACE 3                                                                
SYSINP   NTR1                      INPUT ROUTINES                               
         L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         B     DRIVEGO                                                          
         SPACE 1                                                                
SYSOUTP  NTR1                      OUTPUT ROUTINES                              
         L     R2,GLAIFLD                                                       
         L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         L     R4,GLADTENT                                                      
         USING DROD,R4                                                          
         CLI   0(R4),X'30'         ..IF NOT OUTPUT ELEMENT                      
         BNE   SYSOUTP2            ..GO ON(HEAD INPUT HAVE NO OUTPUT)           
*                                  ..(IN W1-N COL REQUEST FOR EXAMPLE)          
         CLI   DROLTYP,C'N'        NO PRINT - NOT INTERESTED                    
         BE    XIT                                                              
         MVC   MYPOSO,DROPOS                                                    
         MVC   MYOLEN,DROLEN                                                    
         DROP  R4                                                               
         L     R1,=A(MYOLEN2)                                                   
         MVC   0(1,R1),MYOLEN                                                   
         MVC   1(1,R1),MYLTYP                                                   
         SPACE 1                                                                
*                                  DEDUCE TYPE OF RECORD WE HAVE                
SYSOUTP2 CLI   GLRECNO,1           IF THIS IS A RECAP                           
         BNE   SYSOUTT                USE TOTALS                                
         TM    GLINDS,GLTOTLIN     IF NOT A TOTAL                               
         BNO   SYSOUTD                USE DETAILS                               
         LA    R1,NDDETPTL         IF ITS A PERIOD SUBTOTAL                     
         CLI   GLRECNO,1                                                        
         BE    *+8                                                              
         LA    R1,NDRECPTL                                                      
         CLI   0(R1),0                                                          
         BE    SYSOUTT                                                          
         CLC   GLLEVEL,0(R1)                                                    
         BNL   SYSOUTS                USE SUB-TOTALS                            
         B     SYSOUTT             ELSE USE TOTALS                              
         SPACE 1                                                                
SYSOUTD  MVI   MYRECTYP,C'D'       SET FROM DETAILS                             
         MVC   WORK(8),NDDETDEF                                                 
         B     SYSOUTX                                                          
*                                                                               
         SPACE 1                                                                
SYSOUTS  MVI   MYRECTYP,C'S'       SET FROM SUB-TOTALS                          
         MVC   WORK(8),NDSUBDEF                                                 
         B     SYSOUTX                                                          
         SPACE 1                                                                
SYSOUTT  MVI   MYRECTYP,C'T'       SET FROM TOTALS                              
         MVC   WORK(8),NDTOTDEF                                                 
         SPACE 1                                                                
SYSOUTX  L     R1,=A(MYDEFLST)                                                  
         MVC   0(8,R1),WORK                                                     
         B     DRIVEGO                                                          
         EJECT                                                                  
*              CONTROL OF INTERNAL COMPUTES                                     
         SPACE 3                                                                
SYSCOMP  NTR1                      INPUT ROUTINES                               
         LA    R2,COMPROUT                                                      
         SPACE 1                                                                
SYSCOMP2 CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         CLC   GLAROUT+1(3),1(R2)                                               
         BE    SYSCOMP4                                                         
         LA    R2,8(R2)                                                         
         B     SYSCOMP2                                                         
         SPACE 1                                                                
SYSCOMP4 L     RF,4(R2)                                                         
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         MVI   GLHOOK,GLIDID                                                    
         B     DRIVEGO                                                          
         SPACE 1                                                                
COMPROUT DS    0F                                                               
         DC    A(NOCPM),A(NOCPM)                                                
         DC    A(NOCPP),A(NOCPP)                                                
         DC    A(NOCPU),A(NOCPU)                                                
         DC    A(NORPU),A(NORPU)                                                
         DC    A(NORTG),A(NORTG)                                                
         DC    A(NOHUT),A(NOHUT)                                                
         DC    A(NOSHR),A(NOSHR)                                                
         DC    A(NOVPH),A(NOVPH)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
DRIVEGO  DS    0H                   BRANCH TO I/O ROUTINE                       
         L     RE,=A(DRIVE3)       IS THE ROUTINE IN DRIVE3?                    
         SLL   RE,8                                                             
         SRL   RE,8                                                             
         SLL   RF,8                                                             
         SRL   RF,8                                                             
         CR    RF,RE                                                            
         BNLR  RE                     YES, GO THERE                             
*                                                                               
         L     RE,=A(DRIVE2)       IS THE ROUTINE IN MAIN SECTION               
         SLL   RE,8                                                             
         SRL   RE,8                                                             
         CR    RF,RE                                                            
         BLR   RF                     YES SO GO THERE                           
         BR    RE                     NO  GO TO DRIVE2                          
         EJECT                                                                  
*                                                                               
NIUNQID  DS    0H                                                               
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'         IS IT UNIT                                   
         BNE   XIT                                                              
         MVI   ELCODE,X'05'        UNIQUE ELEM                                  
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUIDD,R4                                                         
         MVC   0(4,R3),NUID                                                     
         B     XIT                                                              
         DROP  R4                                                               
*              CLIENT AND OFFICE ROUTINES                                       
         SPACE 3                                                                
*              ARGUMENT 1          C=CODE N=NAME B=BOTH                         
         SPACE 1                                                                
NICLI    MVC   0(3,R3),NBACTAM     PASS AM/2-BYTE CLIENT                        
         CLI   GLARGS,C'X'         ..IF CROSS-AGY READ                          
         BNE   NICLI1                                                           
         MVC   0(3,R3),NBCLICOD    ..PASS ONLY 3 BYTE CLIENT                    
         B     XIT                                                              
NICLI1   MVC   3(3,R3),NBCLICOD         3 BYTE CLIENT                           
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         LR    R2,R3                                                            
         BAS   RE,GETCLI                                                        
         CLI   GLARGS+1,C'C'       CODE AND NAME                                
         BNE   NICLI10                                                          
         MVC   0(3,R3),3(R3)       MOVE UP CODE                                 
         MVI   3(R3),X'40'                                                      
         MVC   4(20,R3),NDCLINAM                                                
         B     *+10                                                             
NICLI10  MVC   0(20,R3),NDCLINAM        OR NAME                                 
         B     XIT                                                              
         SPACE 1                                                                
NOCLI    MVC   LABLAREA(6),=C'CLIENT'                                           
         CLI   GLARGS,C'X'         ..IF CROSS AGY READ                          
         BNE   NOCLI1                                                           
         MVC   CODEAREA(3),0(R2)   ..CODE CARRIED WITHOUT A/M                   
         B     GENOUT                                                           
NOCLI1   CLI   GLARGS,C'N'                                                      
         BNE   NOCLI2                                                           
         MVC   NAMEAREA(20),0(R2)                                               
         CLI   GLARGS+1,C'C'       CODE AND NAME                                
         BNE   GENOUT                                                           
         MVC   NAMEAREA(24),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
NOCLI2   MVC   CODEAREA(3),3(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         BAS   RE,GETCLI                                                        
         MVC   NAMEAREA(20),NDCLINAM                                            
         B     GENOUT                                                           
         SPACE 1                                                                
NOCLIPAG CLI   MYLTYP,C'H'          HEADLINES ONLY                              
         BNE   XIT                                                              
         MVC   0(10,R3),=C'CLIENT   '                                           
         MVC   11(3,R3),3(R2)      CLIENT CODE                                  
         BAS   RE,GETCLI                                                        
         MVC   15(20,R3),NDCLINAM  AND CLIENT NAME                              
         B     XIT                                                              
         SPACE 1                                                                
NIOFFICE MVC   0(1,R3),NBEFFOFF    OFFICE CODE                                  
         B     XIT                                                              
         SPACE 1                                                                
NOOFFICE MVC   LABLAREA(6),=C'OFFICE'                                           
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
         EJECT                                                                  
*              PRODUCT GROUP ROUTINES                                           
         SPACE 3                                                                
*              ARGUMENT 2          LEVEL NUMBER (1/2)                           
         SPACE 1                                                                
*              NDPRGBUF CONTAINS 2 BYTE POSITIONAL PRD GRP CODES                
*              NBSELPGR HAS THE 1 BYTE SCHEME CODE (V,W,X)                      
*              NBSPLPRN HAS POSITIONAL PRD NUMBER                               
         SPACE 1                                                                
NIPRDGRP MVC   0(3,R3),NBACTAM     PASS AM/CLIENT                               
         MVC   3(1,R3),NBSELPGR    PASS SCHEME CODE                             
         LR    R2,R3                                                            
         BAS   RE,GETSCM           MAKE SURE SCHEME DETAIL AROUND               
         LA    R2,NDPRGBUF         POSITIONAL 2X220 PRD GRP CODES               
         ZIC   R1,NBSPLPRN         GET POSITIONAL PRD NUMBER                    
         CLI   NDSPLOPT,0          IS SPLIT BILLING ON?                         
         BE    NIPGRP1                                                          
         L     R1,NDASPLBL                                                      
         USING SPLTBLKD,R1                                                      
         MVC   WORK(3),SPLIPRD                                                  
         BAS   RE,LUPCODE          LOOK UP PRESENT PRODUCT NUMBER               
         ZIC   R1,BYTE                                                          
         SPACE 1                                                                
         DROP  R1                                                               
         SPACE 1                                                                
NIPGRP1  LTR   R1,R1                                                            
         BNZ   NIPGRP2                                                          
         MVC   4(4,R3),=C'9999'    PRE-FILL WITH 9999 FOR UNALLOCATED           
         CLI   NBPRD,X'FE'                                                      
         CLI   NBPRD,0                                                          
         BE    NIPGRP4                                                          
         IC    R1,NBPRD                                                         
         SPACE 1                                                                
NIPGRP2  BCTR  R1,0                                                             
         SLA   R1,1                DOUBLE NUMBER                                
         AR    R2,R1               POINT TO REQUIRED PRD GRP CODE               
         UNPK  WORK(5),0(3,R2)     CONVERT FROM PWOS                            
         ZIC   R1,NDPRGLL1         PICK UP LENGTH OF LEVEL 1                    
         ZIC   R0,NDPRGLL2                                                      
         CLI   GLARGS+1,2                                                       
         BNE   *+6                                                              
         AR    R1,R0               (OPTIONALLY - LEVEL 2)                       
         BCTR  R1,0                                                             
         MVC   4(4,R3),NDSPACES    PRE-FILL WITH SPACES                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),WORK                                                     
         SPACE 1                                                                
NIPGRP4  CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         LR    R2,R3                                                            
         BAS   RE,GETPRG                                                        
         MVC   0(24,R3),NDPRGNM1                                                
         CLI   GLARGS+1,1                                                       
         BE    XIT                                                              
         MVC   0(24,R3),NDPRGNM2                                                
         B     XIT                                                              
         SPACE 1                                                                
NHPRDGRP MVC   0(12,R3),NDPRGBK1                                                
         CLI   GLARGS,2                                                         
         BNE   *+10                                                             
         MVC   0(12,R3),NDPRGBK2                                                
         B     XIT                                                              
         SPACE 1                                                                
NOPRDGRP MVC   LABLAREA(12),NDPRGBK1                                            
         CLI   GLARGS+1,2                                                       
         BNE   *+10                                                             
         MVC   LABLAREA(12),NDPRGBK2                                            
         CLI   GLARGS,C'N'                                                      
         BNE   NOPGRP2                                                          
         MVC   NAMEAREA(24),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
NOPGRP2  MVC   CODEAREA(5),3(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         BAS   RE,GETPRG                                                        
         MVC   NAMEAREA(24),NDPRGNM1                                            
         CLI   GLARGS+1,2                                                       
         BNE   *+10                                                             
         MVC   NAMEAREA(24),NDPRGNM2                                            
         B     GENOUT                                                           
         EJECT                                                                  
*              CLIENT GROUP ROUTINES                                            
         SPACE 3                                                                
*                                                                               
NICLTGRP DS    0H                                                               
         MVC   0(3,R3),NBACTAM     PASS AM/CLI CODE                             
         MVC   3(1,R3),NBSELCGR    PASS SCHEME CODE                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D06'     GET CLTGRP DEFINITION RECORD                 
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(1),NBSELCGR                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         NETGO NVSETSPT,DMCB                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RESUNT                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   RESUNT                                                           
         USING CLGDESD,R4                                                       
         MVC   8(12,R3),CLGBK1     PASS BREAK  TITLE                            
         CLI   GLARGS+1,2                                                       
         BNE   *+10                                                             
         MVC   8(12,R3),CLGBK2                                                  
         MVC   CGRPTSV,8(R3)       PASS BREAK  TITLE FOR HEAD                   
*                                                                               
         UNPK  WORK(5),NBACTCGR(3)    ACTUAL CLIENT GROUP                       
         ZIC   R1,CLGBK1LN         PICK UP LENGTH OF LEVEL1                     
         ZIC   R0,CLGBK2LN                                                      
         CLI   GLARGS+1,2                                                       
         BNE   *+6                                                              
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         MVC   4(4,R3),NDSPACES    PREFILL WITH SPACES                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),WORK                                                     
*                                                                               
*-GET CLIENT GROUP RECORD                                                       
         MVC   KEY+4(2),NBACTCGR   SET CLIENT GROUP CODE                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RESUNT                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   RESUNT                                                           
         USING CLGMEMD,R4                                                       
         MVC   20(24,R3),CLGNAM1                                                
         CLI   GLARGS+1,2                                                       
         BNE   *+10                                                             
         MVC   20(24,R3),CLGNAM2                                                
         B     RESUNT              RESTS TO READ UNIT FILE                      
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
NHCLTGRP MVC   0(12,R3),CGRPTSV        CLT GRP TITLE SAVE                       
         B     XIT                                                              
         SPACE 1                                                                
NHTIME2  MVC   1(19,R3),=C'EASTERN     CENTRAL'                                 
         B     XIT                                                              
         SPACE 1                                                                
NOCLTGRP DS    0H                                                               
         MVC   LABLAREA(12),8(R2)                                               
         CLI   GLARGS,C'N'                                                      
         BNE   *+14                                                             
         MVC   NAMEAREA(24),20(R2)                                              
         B     GENOUT                                                           
         MVC   CODEAREA(4),3(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(24),20(R2)                                              
         B     GENOUT                                                           
         SPACE 1                                                                
         EJECT                                                                  
*              PRODUCT ROUTINE FOR CROSS AGENCY READ                            
         SPACE 3                                                                
*                                  INPUT                                        
NIPRODC  MVC   0(3,R3),NBACTAM     PASS AM/CLIENT/PRODUCT                       
         TM    NBSPLOPT,X'80'                                                   
         BNO   NIPRDC2                                                          
         MVC   BYTE,NBSPLPRN       SPLIT PROD NUMBER                            
         BAS   RE,LUPPROD                                                       
         MVC   0(3,R3),WORK                                                     
         B     XIT                                                              
NIPRDC2  DS    0H                                                               
         MVC   BYTE,NBPRD          BOTH PRODUCTS                                
         BAS   RE,LUPPROD                                                       
         MVC   0(3,R3),WORK                                                     
         CLC   WORK(3),=C'999'                                                  
         BE    XIT                                                              
         MVC   BYTE,NBPRD2                                                      
         CLI   BYTE,0                                                           
         BE    NIPROD3                                                          
         BAS   RE,LUPPROD                                                       
         MVC   3(3,R3),WORK                                                     
         B     XIT                                                              
         SPACE 3                                                                
NOPRODC  DS    0H                                                               
         MVC   LABLAREA(7),=C'PRODUCT'                                          
         MVC   CODEAREA(3),0(R2)                                                
         CLC   CODEAREA(3),=C'999'                                              
         BNE   *+10                                                             
         MVC   CODEAREA(3),=C'UNA'                                              
         CLI   3(R2),X'41'         IS THERE A SECOND PRODUCT?                   
         BL    GENOUT                                                           
         MVI   CODEAREA+3,C'/'                                                  
         MVC   CODEAREA+4(3),3(R2) YES SO SHOW IT AS AAA/BBB                    
         B     GENOUT                                                           
         EJECT                                                                  
*              ARGUMENT 1          C=CODE N=NAME B=BOTH I=INTERFACE             
         SPACE 1                                                                
*                                  INPUT                                        
NIPROD   MVC   0(3,R3),NBACTAM     PASS AM/CLIENT/PRODUCT                       
         TM    NBSPLOPT,X'80'                                                   
         BNO   NIPROD2                                                          
         MVC   BYTE,NBSPLPRN       SPLIT PROD NUMBER                            
         BAS   RE,LUPPROD                                                       
         MVC   3(3,R3),WORK                                                     
NIPROD0  CLI   GLARGS,C'I'         NIBHPRD ENTERS HERE                          
         BE    NIPRODI                                                          
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         LR    R2,R3                                                            
         BAS   RE,GETPRD                                                        
         CLI   GLARGS+1,C'C'       CODE AND NAME                                
         BNE   NIPRD10                                                          
         MVC   0(3,R3),3(R3)       MOVE UP CODE                                 
         MVI   3(R3),X'40'                                                      
         MVC   4(20,R3),NDPRDNAM                                                
         B     XIT                                                              
NIPRD10  MVC   0(20,R3),NDPRDNAM                                                
         B     XIT                                                              
         SPACE 1                                                                
NIPRODI  LR    R2,R3               INTERFACE CODE                               
         BAS   RE,GETPRD                                                        
         MVC   0(4,R3),NDPRDINT                                                 
         B     XIT                                                              
         SPACE 1                                                                
NIPROD2  MVC   BYTE,NBPRD          BOTH PRODUCTS                                
         BAS   RE,LUPPROD                                                       
         MVC   3(3,R3),WORK                                                     
         CLC   WORK(3),=C'999'                                                  
         BE    NIPROD3                                                          
         MVC   BYTE,NBPRD2                                                      
         CLI   BYTE,0                                                           
         BE    NIPROD3                                                          
         BAS   RE,LUPPROD                                                       
         MVC   6(3,R3),WORK                                                     
         SPACE 1                                                                
NIPROD3  CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         MVC   WORK,NDSPACES                                                    
         MVC   DUB(6),0(R3)                                                     
         LA    R2,DUB                                                           
         BAS   RE,GETPRD                                                        
         MVC   WORK(20),NDPRDNAM                                                
         CLI   6(R3),0                                                          
         BE    NIPROD4                                                          
         MVI   WORK+21,C'/'                                                     
         MVC   DUB(3),0(R3)                                                     
         MVC   DUB+3(3),6(R3)      SECOND PRODUCT NAME                          
         LA    R2,DUB                                                           
         BAS   RE,GETPRD                                                        
         MVC   WORK+23(20),NDPRDNAM                                             
         SPACE 1                                                                
NIPROD4  GOTO1 SQUASHER,DMCB,WORK,43                                            
         MVC   0(43,R3),WORK                                                    
         B     XIT                                                              
         SPACE 1                                                                
NOPROD   MVC   LABLAREA(7),=C'PRODUCT'                                          
         CLI   GLARGS,C'N'                                                      
         BNE   NOPROD2                                                          
         MVC   NAMEAREA,0(R2)                                                   
         CLI   GLARGS+1,C'C'       CODE AND NAME                                
         BNE   GENOUT                                                           
         MVC   0(24,R3),0(R2)                                                   
         B     XIT                                                              
         SPACE 1                                                                
NOPROD2  MVC   CODEAREA(3),3(R2)                                                
         CLC   CODEAREA(3),=C'999'                                              
         BNE   *+10                                                             
         MVC   CODEAREA(3),=C'UNA'                                              
         CLI   6(R2),X'41'         IS THERE A SECOND PRODUCT?                   
         BL    NOPROD4                                                          
         MVI   CODEAREA+3,C'/'                                                  
         MVC   CODEAREA+4(3),6(R2) YES SO SHOW IT AS AAA/BBB                    
         SPACE 1                                                                
NOPROD4  CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         BAS   RE,GETPRD                                                        
         MVC   NAMEAREA(20),NDPRDNAM                                            
         CLI   6(R2),X'41'         IS THERE A SECOND PRODUCT?                   
         BL    GENOUT                                                           
         MVC   WORK,NDSPACES                                                    
         MVC   WORK(20),NAMEAREA                                                
         MVI   WORK+21,C'/'                                                     
         MVC   DUB(3),0(R2)                                                     
         MVC   DUB+3(3),6(R2)                                                   
         LA    R2,DUB                                                           
         BAS   RE,GETPRD                                                        
         MVC   WORK+23(20),NDPRDNAM                                             
         GOTO1 SQUASHER,DMCB,WORK,43                                            
         MVC   NAMEAREA,WORK                                                    
         B     GENOUT                                                           
         SPACE 1                                                                
NISBPCT  L     R4,NDASPLBL         SPLIT BILLING PERCENT                        
         USING SPLTBLKD,R4                                                      
         MVC   SPLAMT,=F'10000'    DUMMY AMOUNT - NEED PERCENT                  
         MVI   SPLPRDO,0                                                        
         LA    R1,NISBPCT2                                                      
         ST    R1,SPLAHOOK                                                      
         GOTO1 =V(NETSPB),DMCB,(R4)                                             
         B     XIT                                                              
         SPACE 1                                                                
NISBPCT2 NTR1                                                                   
         MVC   0(4,R3),SPLEPCT     JUST NEED PERCENT                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
NIPRDPCT DS    0H                  GET 1ST PROD %                               
         TM    NBSPLOPT,X'80'      IF SPLITTING                                 
         BNO   *+14                                                             
         CLC   NBSPLPRN,NBPRD      MAKE SURE ITS FIRST PRODUCT                  
         BNE   XIT                                                              
         MVC   0(2,R3),NBP1SHR                                                  
         OC    NBP1SHR,NBP1SHR                                                  
         BNZ   XIT                                                              
         MVC   0(2,R3),=X'2710'    MAKE IT 100% (2DEC)                          
         TM    NBUNST2,X'04'       1ST PROD = 0%                                
         BNO   XIT                                                              
         XC    0(2,R3),0(R3)                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ESTIMATE ROUTINES                                                
         SPACE 3                                                                
*              ARGUMENT 1          C=CODE N=NAME B=BOTH                         
         SPACE 1                                                                
*                                  INPUT                                        
NIEST    MVC   0(1,R3),NBACTEST                                                 
         CLI   GLARGS,C'C'         IF WE ARE ONLY INTERESTED IN EST #           
         BE    XIT                    JUST PASS THAT                            
         MVC   1(3,R3),NBACTAM     PASS AM/CLIENT/PRODUCT/EST                   
         MVC   BYTE,NBSPLPRN       ASSUME SPLIT PROD NUMBER                     
         TM    NBSPLOPT,X'80'                                                   
         BO    *+10                                                             
         MVC   BYTE,NBPRD          UNLESS SPLIT NOT ON                          
         BAS   RE,LUPPROD                                                       
         MVC   4(3,R3),WORK                                                     
NIEST0   CLI   GLARGS,C'P'         IF ARG1=C'P'                                 
         BNE   *+10                                                             
         MVC   4(3,R3),=C'POL'        PASS POOL                                 
         CLI   GLARGS,C'N'         IF ONLY NAME IS NEEDED                       
         BNE   NIEST2                                                           
         LR    R2,R3                                                            
         BAS   RE,GETEST                                                        
         MVC   0(20,R3),NDESTNAM   PASS THIS THROUGH                            
         B     XIT                                                              
NIEST2   CLI   GLARGS,C'F'         IF ONLY FILTER IS NEEDED                     
         BNE   XIT                                                              
         LR    R2,R3                                                            
         BAS   RE,GETEST                                                        
         MVC   0(3,R3),NDESTFLT    PASS THIS THROUGH                            
         B     XIT                                                              
         SPACE 1                                                                
NOEST    MVC   LABLAREA(8),=C'ESTIMATE'                                         
         CLI   GLARGS,C'F'                                                      
         BE    NOEST3                                                           
         CLI   GLARGS,C'N'                                                      
         BNE   NOEST2                                                           
         MVC   NAMEAREA(20),0(R2)  NAME ONLY                                    
         B     GENOUT                                                           
         SPACE 1                                                                
NOEST2   EDIT  (1,0(R2)),(3,CODEAREA),ALIGN=LEFT                                
         CLI   GLARGS,C'C'         NAME AND NUMBER                              
         BE    GENOUT                                                           
         BAS   RE,GETEST                                                        
         MVC   NAMEAREA(20),NDESTNAM                                            
         B     GENOUT                                                           
         SPACE 1                                                                
NOEST3   MVC   NAMEAREA(3),0(R2)   FILTER ONLY                                  
         B     GENOUT                                                           
         EJECT                                                                  
*              DAYPART  ROUTINES                                                
         SPACE 3                                                                
NIDPT    MVC   0(1,R3),NBACTDP     INPUT                                        
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
NISD     MVC   0(1,R3),NBACTDP     DAYPART                                      
         MVC   1(1,R3),NBSUBDPT    DAYPART SUB CODE                             
         B     XIT                                                              
*                                                                               
NISDNAM  MVC   0(1,R3),NBACTDP     SUB-DAYPART NAME                             
         MVC   1(3,R3),NBSUBDPT                                                 
         LA    R1,SDNAMT           ADD TO TABLE                                 
NISDN5   CLI   0(R1),X'FF'                                                      
         BE    NISDNX                                                           
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   0(4,R1),0(R3)                                                    
         CLC   0(4,R1),0(R3)                                                    
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     NISDN5                                                           
NISDNX   B     XIT                                                              
*                                                                               
NODPT    EQU   *                   OUTPUT                                       
         MVC   LABLAREA(7),=C'DAYPART'                                          
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(1),0(R2)   CODE                                         
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         EXPDP WORK,0(R2)          EXPAND CODE INTO WORK                        
         MVC   NAMEAREA(8),WORK                                                 
         B     GENOUT                                                           
         SPACE                                                                  
*                                                                               
NOSD     MVC   LABLAREA(11),=C'SUB-DAYPART'                                     
         EXPDP WORK,0(R2)          EXPAND CODE INTO WORK                        
         MVC   NAMEAREA(8),WORK                                                 
         TM    GLINDS,X'40'        IS IT TOTALS                                 
         BNO   NODPTX                                                           
         CLI   SDNAMT,0            DO WE HAVE SUBDAYPARTS                       
         BE    NODPTX                                                           
         LA    R1,SDNAMT         IF THEY MATCH PULL SUBCODE FROM TABL           
         LA    RE,NAMEAREA+8                                                    
         MVI   BYTE,0                                                           
NODPT5   CLI   0(R1),X'FF'                                                      
         BE    NODPTX                                                           
         CLC   0(2,R1),0(R2)                                                    
         BNE   NODPT10                                                          
         CLI   BYTE,1                                                           
         BNE   *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         MVC   0(2,RE),2(R1)                                                    
         LA    RE,2(RE)                                                         
         MVI   BYTE,1                                                           
NODPT10  LA    R1,4(R1)                                                         
         B     NODPT5                                                           
NODPTX   B     GENOUT                                                           
*                                                                               
NOSDNM   DS    0H                  EXTRACT SUB DAYPART NAME                     
         MVC   NAMEAREA(2),2(R2)                                                
         B     GENOUT                                                           
*                                                                               
SDNAMT   DS    CL40                STORE SUB-DAYPART CODES                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              STATUS AND SCHEDULE                                              
         SPACE 3                                                                
NISTATUS TM    NBUNITST,X'40'     IF UNIT IS PREEMPTED                          
         BNO   NISTAT4                                                          
         MVC   0(5,R3),=C'PREMT'                                                
         B     XIT                                                              
NISTAT4  TM    NBUNITST,X'02'     IF UNIT IS MISSED                             
         BNO   NISTAT5                                                          
         MVC   0(5,R3),=C'MISSD'                                                
         B     XIT                                                              
NISTAT5  TM    NBUNST3,X'02'         IF IT'S ADU                                
         BNO   NISTAT6                                                          
         MVC   0(3,R3),=C'ADU'                                                  
         B     XIT                                                              
NISTAT6  TM    NBUNITST,X'01'     IF UNIT IS MAKE-GOOD                          
         BNO   NISTAT8                                                          
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'         IS IT UNIT                                   
         BNE   XIT                                                              
         MVI   ELCODE,X'06'        GET MISSED DETAILS                           
         BAS   RE,GETEL                                                         
         BNE   NISTAT7                                                          
         USING NUMGEL,R4                                                        
         TM    NUMGSTAT,X'04'     AND IF MISSED UNIT IS PFB                     
         BNO   NISTAT6B                                                         
         MVC   0(6,R3),=C'BON/MG'                                               
         B     XIT                                                              
NISTAT6B TM    NUMGST3,X'02'      AND IF MISSED UNIT ID ADU                     
         BNO   NISTAT7                                                          
         MVC   0(6,R3),=C'BON/AU'                                               
         B     XIT                                                              
         DROP  R4                                                               
NISTAT7  MVC   0(5,R3),=C'MG   '     MAKE-GOOD ONLY                             
         B     XIT                                                              
NISTAT8  TM    NBUNITST,X'04'        IF UNIT IS PFB                             
         BNO   NISTAT9                                                          
         MVC   0(5,R3),=C'BONUS'     IT IS A BONUS                              
         B     XIT                                                              
*                                                                               
NISTAT9  B     XIT                                                              
         EJECT                                                                  
*              NETWORK  ROUTINES                                                
         SPACE 3                                                                
NINETST  DS    0H                  NETWORK + DOLLAR STAT FOR JWNY               
         BAS   RE,INGOAL                                                        
         MVC   0(4,R3),NBACTNET                                                 
         MVI   4(R3),C'$'                                                       
         OC    NBACTUAL,NBACTUAL                                                
         BZ    *+8                                                              
         MVI   4(R3),0                                                          
         B     XIT                                                              
*                                                                               
NINET    DS    0H                  NETWORK                                      
         MVC   0(4,R3),NBACTNET                                                 
         CLI   NBMODE,NBPROCGL                                                  
         BNE   NINET4                                                           
*                                                                               
         CLI   NBN2B16,C'B'        PACKAGE AND NETWORK LEVEL GOALS              
         BE    XIT                                                              
         CLI   NBN2B16,C'N'        NETWORK LEVEL GOALS                          
         BE    XIT                                                              
         BAS   RE,INGOAL                                                        
*                                                                               
NINET4   MVC   4(1,R3),NBPOSTYP    NBPOSTYP FOR PKG AND UNIT                    
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   NINET5                                                           
         MVI   ELCODE,X'02'        IF UNIT                                      
         BAS   RE,GETEL            GET MEDIA TYPE                               
         BNE   XIT                                                              
         USING NUSDRD,R4                                                        
         MVC   4(1,R3),NUPOSTYP                                                 
NINET5   DS    0H                                                               
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         LA    R2,NBACTNET                                                      
         BAS   RE,GETNET                                                        
         MVC   0(24,R3),NDNETNAM                                                
         MVC   24(1,R3),NUPOSTYP                                                
         B     XIT                                                              
         SPACE 1                                                                
NODLSTA  MVC   0(5,R3),0(R2)                                                    
         MVI   4(R3),C'T'                                                       
         B     XIT                                                              
         SPACE 1                                                                
NIMEDIA  BAS   RE,INGOAL           MEDIA                                        
*        L     R4,NBAIO                                                         
*        MVI   ELCODE,X'02'                                                     
*        BAS   RE,GETEL                                                         
*        BNE   XIT                                                              
*        USING NUSDRD,R4                                                        
         CLI   GLARGS,C'S'                                                      
         BE    NIMEDIA2                                                         
         MVC   5(1,R3),NBSTATYP                                                 
*        MVC   5(1,R3),NBPOSTYP                                                 
         MVC   0(5,R3),=C'NET  '                                                
         CLI   NBSTATYP,C'N'                                                    
         BE    XIT                                                              
         MVC   0(5,R3),=C'CABLE'                                                
         CLI   NBSTATYP,C'C'                                                    
         BE    XIT                                                              
         MVC   0(5,R3),=C'SYND.'                                                
         CLI   NBSTATYP,C'S'                                                    
         BE    XIT                                                              
         MVC   0(5,R3),=C'RADIO'                                                
         CLI   NBSTATYP,C'D'                                                    
         BE    XIT                                                              
         MVC   0(5,R3),=C'OTHER'                                                
         B     XIT                                                              
         SPACE 1                                                                
NIMEDIA2 MVC   0(3,R3),=C'NAT'                                                  
         CLI   NBSDSBMD,C'N'                                                    
         BE    XIT                                                              
         MVC   0(5,R3),=C'REG'                                                  
         B     XIT                                                              
*****    LA    R2,NBACTNET                                                      
*****    BAS   RE,GETNET           LOOK UP IN NETWORK LIST                      
*****    MVC   0(5,R3),NDNETMAB                                                 
*****    B     XIT                                                              
         SPACE 1                                                                
NONET    BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(7),=C'NETWORK'                                          
         CLI   GLARGS,C'N'                                                      
         BNE   NONET2                                                           
         MVC   NAMEAREA(24),0(R2)                                               
         MVC   NDMEDCOD,24(R2)                                                  
         B     GENOUT                                                           
         SPACE 1                                                                
NONET2   MVC   CODEAREA(4),0(R2)                                                
         MVC   NDMEDCOD,4(R2)                                                   
         CLI   GLARGS,C'B'                                                      
         BNE   GENOUT                                                           
         BAS   RE,GETNET                                                        
         MVC   NAMEAREA(24),NDNETNAM                                            
         B     GENOUT                                                           
         SPACE 1                                                                
NOMEDIA  BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(5),=C'MEDIA'                                            
         CLI   GLARGS,C'W'         WLMED ENTRY                                  
         BNE   NOMED5                                                           
         MVI   CODEAREA,C'N'                                                    
         MVC   CODEAREA+1(1),5(R2)                                              
         B     GENOUT                                                           
NOMED5   MVC   CODEAREA(5),0(R2)                                                
         MVC   NDMEDCOD,5(R2)                                                   
         B     GENOUT                                                           
         EJECT                                                                  
*              CONTROL OF PHONY GOAL FIELDS                                     
         SPACE 3                                                                
INGOAL   CLI   NBMODE,NBPROCGL     OUTPUT SPECIAL CHARACTER                     
         BNER  RE                                                               
         MVI   0(R3),X'FE'                                                      
         B     XIT                                                              
         SPACE 1                                                                
OUTGOAL  CLI    0(R2),X'FE'        CHECK PHONY DATA FROM GOALS                  
         BNER  RE                                                               
         OI    NDLININD,X'80'      SUPPRESS DETAIL LINES                        
         B     XIT                 AND GET OUT                                  
         EJECT                                                                  
*                                                                               
*              INVOICE ROUTINE                                                  
NIINV    DS    0H                                                               
         GOTO1 =A(XTRARTN),DMCB,(10,(RA))                                       
         B     XIT                                                              
*                                                                               
NOINV    DS    0H               OUTPUT INVOICE DATA                             
         CLI   0(R2),X'FF'      DONT PRINT FLAG                                 
         BNE   *+12                                                             
         OI    NDLININD,X'80'                                                   
         B     XIT                                                              
         CLI   0(R2),1             MULTIPLE WRITER CALL FLAG                    
         BE    XIT                                                              
         LA    R5,10                                                            
NOIV10   MVC   0(1,R3),0(R2)       TYPE                                         
         MVC   2(7,R3),1(R2)       BILL NUMBER                                  
         MVC   10(6,R3),8(R2)      BILL INVOICE DATE                            
         EDIT  (B4,14(R2)),(10,17(R3)),2,MINUS=YES                              
         EDIT  (B4,18(R2)),(10,28(R3)),2,MINUS=YES                              
         LA    R2,23(R2)                                                        
         LA    R3,198(R3)                                                       
         CLC   0(2,R2),=X'4040'                                                 
         BNH   XIT                                                              
         BCT   R5,NOIV10                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PACKAGE ROUTINES                                                 
         SPACE 3                                                                
*                                  INPUT                                        
NIPACK   DS    0H                                                               
         MVC   0(1,R3),NBPACK      ONLY PASS THIS                               
         CLI   GLARGS,C'C'         IF WE ARE ONLY INTERESTED IN CODE            
         BE    XIT                                                              
         SPACE 1                                                                
NIPACK2  BAS   RE,INGOAL           IF GOAL REC                                  
         BAS   RE,SETPAK           PASS AM/CLIENT/NETWORK/EST/PACKAGE           
         LR    R2,R3                                                            
         BAS   RE,GETPAK                                                        
         CLI   GLARGS,C'N'                                                      
         BE    NIPACK5                                                          
         B     XIT                                                              
NIPACK5  MVC   0(4,R3),NDPAKNAM                                                 
         CLI   GLARGS+1,4                                                       
         BE    XIT                                                              
         MVC   0(16,R3),NDPAKNAM                                                
         B     XIT                                                              
         SPACE 1                                                                
NIPFILT  CLI   GLARGS,7            PACKAGE FILTER ARG1=7=ALL                    
         BNE   NIPF10                                                           
         MVC   0(6,R3),NBPKFILT                                                 
         B     XIT                                                              
NIPF10   ZIC   R1,GLARGS                                                        
         BCTR  R1,0                ARG1=1-6 FOR FILTER NUMBER                   
         LA    R1,NBPKFILT(R1)                                                  
         MVC   0(3,R3),NDSPACES                                                 
         MVC   1(1,R3),0(R1)                                                    
         B     XIT                                                              
         SPACE 1                                                                
NIPKGN2  DS    0H                  2ND PACKAGE NAME                             
         BAS   RE,SETPAK                                                        
*        MVC   0(3,R3),NBACTAM     PASS AM/CLIENT/NETWORK/EST/PACKAGE           
*        MVC   3(4,R3),NBACTNET                                                 
*        MVC   7(1,R3),NBACTEST                                                 
*        MVC   8(1,R3),NBPACK                                                   
         LR    R2,R3                                                            
         BAS   RE,GETPAK                                                        
         XC    0(16,R3),0(R3)                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NPK2EL,R4                                                        
         MVC   0(16,R3),NPK2NME2                                                
         CLI   GLARGS,C'B'           BOTH NAME AND NUMBER                       
         BNE   XIT                                                              
         EDIT  (B1,NBPACK),(3,18(R3))                                           
         B     XIT                                                              
*                                                                               
SETPAK   DS    0H                                                               
         MVC   0(3,R3),NBACTAM     PASS AM/CLIENT/NETWORK/EST/PACKAGE           
         MVC   3(4,R3),NBACTNET                                                 
         MVC   7(1,R3),NBACTEST                                                 
         MVC   8(1,R3),NBPACK                                                   
SETPAKX  BR    RE                                                               
*                                                                               
NIPKG    DS    0H                                                               
         L     R4,NBAIO                                                         
         CLI   0(R4),X'02'         IS IT PKG REC                                
         BNE   NIPKGX                                                           
*                                                                               
         CLI   GLARGS,C'3'         PKG COUNT                                    
         BNE   *+12                                                             
         MVI   3(R3),1                                                          
         B     NIPKGX                                                           
*                                                                               
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   NIPKGX                                                           
         USING NPAKEL,R4                                                        
         CLI   GLARGS,C'1'         PKG COST                                     
         BNE   NIPKG10                                                          
         MVC   0(4,R3),NPAKCOST                                                 
         B     NIPKGX                                                           
NIPKG10  CLI   GLARGS,C'2'         PKG CPM                                      
         BNE   NIPKGX                                                           
         MVC   0(4,R3),NPAKGCPM                                                 
NIPKGX   B     XIT                                                              
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* - SAVE PKG COST/CPM AND PRINT AT TOTAL                                        
NOPCOST  DS    0H                                                               
*        TM    GLINDS,GLTOTLIN                                                  
*        BO    NOPC10                                                           
*        LA    R1,PCOSTSV                                                       
*        CLI   GLARGS,C'1'                                                      
*        BE    *+8                                                              
*        LA    R1,PCPMSV                                                        
*        MVC   0(4,R1),0(R2)                                                    
         B     XIT                                                              
NOPC10   CLI   GLLEVEL,0           IF GRAND TOTAL                               
*        BE    XIT                     SKIP                                     
*        LA    R1,PCOSTSV                                                       
*        CLI   GLARGS,C'1'                                                      
*        BE    *+8                                                              
*        LA    R1,PCPMSV                                                        
*        EDIT  (B4,0(R1)),(12,0(R3))                                            
*        XC    PCOSTSV(8),PCOSTSV                                               
         B     XIT                                                              
*                                                                               
NOPACK   BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(7),=C'PACKAGE'                                          
         CLI   GLARGS,C'C'                                                      
         BNE   NOPACK2                                                          
         BAS   RE,OUTGOAL                                                       
         EDIT  (1,0(R2)),(3,CODEAREA),ALIGN=LEFT                                
         B     GENOUT                                                           
         SPACE 1                                                                
NOPACK2  CLI   GLARGS,C'N'                                                      
         BNE   NOPACK4                                                          
         MVC   NAMEAREA(4),0(R2)                                                
         CLI   GLARGS+1,4                                                       
         BE    GENOUT                                                           
         MVC   NAMEAREA(16),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
NOPACK4  LA    R2,8(R2)                                                         
         BAS   RE,OUTGOAL                                                       
         SH    R2,=H'8'                                                         
         BAS   RE,GETPAK                                                        
         MVC   NAMEAREA(16),NDPAKNAM                                            
         EDIT  (1,8(R2)),(3,CODEAREA),ALIGN=LEFT                                
         CLI   GLARGS,C'B'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(36),NDPAKNAM                                            
         GOTO1 SQUASHER,DMCB,NAMEAREA,36                                        
         B     GENOUT                                                           
         EJECT                                                                  
*              UNIT ROUTINES                                                    
         SPACE 3                                                                
NIFEEDMG MVC   0(3,R3),NBFEEDMG    FEED MARKET GROUP                            
         B     XIT                                                              
         SPACE 1                                                                
NIHUTAVE MVC   0(1,R3),NBHUTAVE    HUT AVERAGING USED                           
         B     XIT                                                              
         SPACE 1                                                                
NIRESULT MVC   0(1,R3),NBRESULT    RESULT CODE FOR DEMOS                        
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'DE'        IF DEMO OVERRIDE                             
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVI   1(R3),C'M'                                                       
         B     XIT                                                              
         SPACE 1                                                                
NIDLEVEL MVC   0(1,R3),NBDEMLEV    LEVEL AND RESULT CODE                        
         MVC   1(1,R3),NBRESULT                                                 
         B     XIT                                                              
         SPACE 1                                                                
NINTI    MVC   0(2,R3),NBNTI       NTI NUMBER                                   
         B     XIT                                                              
NONTI    MVC   HALF,0(R2)          NTI CODES ARE UNSIGNED NUMBS                 
         ZICM  R1,HALF,2           SO OVER A CERTAIN VALUE 'EDIT'               
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         CVD   R1,DUB              TREATS THEM AS NEGATIVE                      
         UNPK  0(5,R3),DUB                                                      
         OI    4(R3),X'F0'                                                      
*        EDIT  (B2,0(R2)),(5,0(R3)),ZERO=BLANK                                  
         B     XIT                                                              
         SPACE 1                                                                
NIAFFDAT CLI   GLARGS,2            IF 2 USE AFFID FOR CABLE                     
         BNE   NIAFF10                                                          
         CLI   NBSTATYP,C'C'                                                    
         BNE   NIAFF20                                                          
* - GET AFFID DATE                                                              
NIAFF10  L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   NIAFF15                                                          
         USING NUSDRD,R4                                                        
         MVC   0(2,R3),NUSDAFDT                                                 
         OC    NUSDAFDT,NUSDAFDT                                                
         BNZ   XIT                                                              
NIAFF15  CLI   GLARGS,1                                                         
         BE    NIAFF20                                                          
         CLI   GLARGS,2                                                         
         BNE   XIT                                                              
NIAFF20  MVC   0(2,R3),NBACTDAT                                                 
         B     XIT                                                              
*                                  ROTATION DAYS                                
NIROTATE BAS   RE,INGOAL                                                        
         L     R1,=A(DAYCONVT)         DEFAULT IS UNIT DAY                      
         SPACE 1                                                                
NIROT2   CLC   NBDAY,0(R1)                                                      
         BE    NIROT4                                                           
         LA    R1,5(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   NIROT2                                                           
         SPACE 1                                                                
NIROT4   MVC   0(3,R3),2(R1)       3 CHARACTER DAY                              
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'02'        NOW LOOK FOR ROTATION                        
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUSDRD,R4                                                        
         OC    NUSDROT,NUSDROT                                                  
         BZ    XIT                                                              
         GOTO1 UNDAY,DMCB,NUSDROT,(R3)                                          
         B     XIT                                                              
         SPACE 1                                                                
*                                  ROTATION DAYS XXXXXXX                        
NIROTATN BAS   RE,INGOAL                                                        
         CLI   GLARGS,2            IF FROM DLSTDAY                              
         BE    *+10                                                             
         MVC   0(7,R3),NDSPACES                                                 
         L     R1,=A(DAYCONVT)         DEFAULT IS UNIT DAY                      
         SPACE 1                                                                
NIROT2N  CLC   NBDAY,0(R1)                                                      
         BE    NIROT4N                                                          
         LA    R1,5(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   NIROT2N                                                          
         B     NIROT6N             DON'T ACCEPT OTHER                           
         SPACE 1                                                                
NIROT4N  CLI   0(R1),X'FE'           PHONY FOR GOALS                            
         BE    NIROT6N                                                          
         BAS   RE,XFIELD                                                        
NIROT6N  L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'02'        NOW LOOK FOR ROTATION                        
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUSDRD,R4                                                        
         OC    NUSDROT,NUSDROT                                                  
         BZ    XIT                                                              
         LA    R1,NUSDROT                                                       
         BAS   RE,XFIELD                                                        
         B     XIT                                                              
*                                                                               
XFIELD   NTR1                                                                   
         CLI   GLARGS,2                                                         
         BNE   XFL10                                                            
         TM    0(R1),X'40'         MON                                          
         BNO   *+12                                                             
         MVI   0(R3),C'1'                                                       
         B     XIT                                                              
         TM    0(R1),X'20'         TUE                                          
         BNO   *+12                                                             
         MVI   0(R3),C'2'                                                       
         B     XIT                                                              
         TM    0(R1),X'10'         WED                                          
         BNO   *+12                                                             
         MVI   0(R3),C'3'                                                       
         B     XIT                                                              
         TM    0(R1),X'08'         THUR                                         
         BNO   *+12                                                             
         MVI   0(R3),C'4'                                                       
         B     XIT                                                              
         TM    0(R1),X'04'         FRI                                          
         BNO   *+12                                                             
         MVI   0(R3),C'5'                                                       
         B     XIT                                                              
         TM    0(R1),X'02'         SAT                                          
         BNO   *+12                                                             
         MVI   0(R3),C'6'                                                       
         B     XIT                                                              
         TM    0(R1),X'01'         SUN                                          
         BNO   *+8                                                              
         MVI   6(R3),C'7'                                                       
         B     XIT                                                              
XFL10    TM    0(R1),X'40'         MON      SET UP XXXXXXX                      
         BNO   *+8                                                              
         MVI   0(R3),C'X'                                                       
         TM    0(R1),X'20'         TUE                                          
         BNO   *+8                                                              
         MVI   1(R3),C'X'                                                       
         TM    0(R1),X'10'         WED                                          
         BNO   *+8                                                              
         MVI   2(R3),C'X'                                                       
         TM    0(R1),X'08'         THUR                                         
         BNO   *+8                                                              
         MVI   3(R3),C'X'                                                       
         TM    0(R1),X'04'         FRI                                          
         BNO   *+8                                                              
         MVI   4(R3),C'X'                                                       
         TM    0(R1),X'02'         SAT                                          
         BNO   *+8                                                              
         MVI   5(R3),C'X'                                                       
         TM    0(R1),X'01'         SUN                                          
         BNO   *+8                                                              
         MVI   6(R3),C'X'                                                       
         XIT1                                                                   
         SPACE 1                                                                
NIRATYPE L     R4,NBAIO            SPECIAL RATE CODES                           
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUSDRD,R4                                                        
         MVC   0(1,R3),NUSDSRT                                                  
         MVC   1(1,R3),NUSDRTCV                                                 
         B     XIT                                                              
         EJECT                                                                  
*              PROGRAM ROUTINES                                                 
         SPACE 3                                                                
*              ARGUMENT 1          C=CODE N=NAME B=BOTH                         
         SPACE 1                                                                
NIPPROG  CLI   NBMODE,NBPROCPP                                                  
         BNE   XIT                                                              
         B     *+8                                                              
NIPROG   BAS   RE,INGOAL                                                        
         MVC   0(6,R3),NBACTPRG    PROGRAM CODE                                 
         CLI   GLARGS,C'C'                                                      
         BE    XIT                                                              
         CLI   GLARGS,C'B'                                                      
         BNE   *+8                                                              
         LA    R3,6(R3)                                                         
         MVC   0(16,R3),NBPROGNM   AND/OR NAME                                  
         OC    0(16,R3),NDSPACES                                                
         B     XIT                                                              
         SPACE 1                                                                
NOPROG   BAS   RE,OUTGOAL                                                       
         CLI   GLARGS,C'N'                                                      
         BNE   NOPROG2                                                          
         TM    NDCOLIND,X'10'                                                   
         BNZ   NOPROG1                                                          
         CLC   0(6,R2),=CL6'* PLAN'                                             
         BNE   NOPROG1                                                          
         OI    NDLININD,X'80'                                                   
         B     XIT                                                              
NOPROG1  MVC   LABLAREA(7),=C'PROGRAM'                                          
         MVC   NAMEAREA(16),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
NOPROG2  TM    NDCOLIND,X'10'                                                   
         BNZ   NOPROG3                                                          
         CLC   0(6,R2),=CL6'******'                                             
         BNE   NOPROG3                                                          
         OI    NDLININD,X'80'                                                   
         B     XIT                                                              
NOPROG3  MVC   CODEAREA(6),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(16),6(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
NIPRFILT MVC   0(3,R3),NBPRFILT    PROGRAM FILTER                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
NIPROGN  DS    0H                  NIELSEN PROGRAM NAME                         
         GOTO1 =A(XTRARTN),DMCB,(1,(RA))                                        
         XIT1                                                                   
         EJECT                                                                  
*- PUP PLAN                        INPUT                                        
*              PLAN ROUTINES                                                    
         SPACE 3                                                                
*              ARGUMENT 1          C=CODE N=NAME B=BOTH                         
         SPACE 1                                                                
NIPPLAN  CLI   NBMODE,NBPROCPP                                                  
         BNE   XIT                                                              
         MVC   0(4,R3),NBMGFPCD    PLAN CODE                                    
         CLI   GLARGS,C'C'                                                      
         BE    XIT                                                              
         CLI   GLARGS,C'B'                                                      
         BNE   *+8                                                              
         LA    R3,6(R3)                                                         
         MVC   0(16,R3),NBMGFPNM   AND/OR NAME                                  
         OC    0(16,R3),NDSPACES                                                
         B     XIT                                                              
         SPACE 1                                                                
NOPPLAN  BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(4),=C'PLAN'                                             
         CLI   GLARGS,C'N'                                                      
         BNE   NOPPLAN2                                                         
         MVC   NAMEAREA(16),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
NOPPLAN2 MVC   CODEAREA(4),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(16),6(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
*              PLAN COMMENT                                                     
NIPCOM   CLI   NBMODE,NBPROCPP                                                  
         BNE   XIT                                                              
         L     R4,NBAPRD                                                        
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),3(R4)                                                    
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
*              PLAN FILTER                                                      
NIPLFLT  MVC   0(3,R3),NBPKFILT                                                 
         B     XIT                                                              
         SPACE 1                                                                
*              DATES (INPUT)                                                    
         SPACE 3                                                                
NIDATE   BAS   RE,INGOAL           DATE                                         
         MVC   0(2,R3),NBACTDAT                                                 
         B     XIT                                                              
         SPACE 1                                                                
         USING DATELSTD,R1                                                      
NIWEEK   L     R1,NDADATES         WEEK                                         
         LA    R5,WEEKLIST                                                      
         B     NIPER                                                            
         SPACE 1                                                                
NIQUART  L     R1,NDADATES         QUARTER                                      
         LA    R5,QURTLIST                                                      
         B     NIPER                                                            
         SPACE 1                                                                
NIMONTHA OC    NDPRIOR,NDPRIOR                                                  
         BZ    NIMNTH05                                                         
         CLC   NBACTDAT,NDPRIOR                                                 
         BNL   NIMNTH05                                                         
         MVC   0(4,R3),=XL4'11111111'                                           
         B     NIPEREX                                                          
*                                                                               
NIMNTH05 OC    NDAFTER,NDAFTER                                                  
         BZ    NIMONTH                                                          
         CLC   NBACTDAT,NDAFTER                                                 
         BNH   NIMONTH                                                          
         MVC   0(4,R3),=XL4'FFFFFFFF'                                           
         B     NIPEREX                                                          
*                                                                               
NIMONTH  L     R1,NDADATES         MONTH                                        
         LA    R5,MNTHLIST                                                      
         B     NIPER                                                            
         DROP  R1                                                               
         SPACE 1                                                                
NIPER    CLC   NBACTDAT,0(R5)      LOOK UP DATE IN LIST                         
         BNL   NIPE4                                                            
*                                  (SHOULDN'T REALLY HAPPEN)                    
         B     NIPER8              (USE THIS ENTRY)                             
         SPACE 1                                                                
NIPE4    OC    0(4,R5),0(R5)       CHECK FOR EOL                                
         BNZ   NIPE6                                                            
         SH    R5,=H'4'            (SHOULDN'T REALLY HAPPEN)                    
         B     NIPER8              (USE LAST ENTRY)                             
         SPACE 1                                                                
NIPE6    CLC   NBACTDAT,2(R5)                                                   
         BNH   NIPER8                                                           
         LA    R5,4(R5)             NEXT IN LIST                                
         B     NIPER                                                            
         SPACE 1                                                                
NIPER8   MVC   0(4,R3),0(R5)        RETURN START-END DATES                      
NIPEREX  B     XIT                                                              
         SPACE 1                                                                
NIDATNUM MVC   0(2,R3),NBACTDAT   DATE                                          
         MVC   2(1,R3),NBACTSUB   LINE NUMBER                                   
***      A     R3,=F'-100'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DATE OUTPUT                                                      
         SPACE 3                                                                
*                                  MONTH AND QUARTER NO YEAR                    
NIMQNYR  CLI   NBMODE,NBPROCPP     PUP DATE                                     
         BNE   XIT                                                              
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,WORK)                                
         CLI   GLARGS,C'M'                                                      
         BNE   NIMQ10                                                           
*  MONTH IMPUT                                                                  
         MVC   0(3,R3),WORK                                                     
         B     XIT                                                              
*  QUARTER INPUT                                                                
NIMQ10   LA    RE,12                                                            
         L     RF,=A(TRANTAB)                                                   
*                                                                               
NIMQ20   MVC   0(2,R3),3(RF)       MOVE QUARTER                                 
         MVI   2(R3),X'40'                                                      
         CLC   0(3,RF),WORK                                                     
         BE    XIT                                                              
         LA    RF,5(RF)                                                         
         BCT   RE,NIMQ20                                                        
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*              DATE OUTPUT                                                      
         SPACE 3                                                                
*                                  DATE AND WEEK                                
NOWEEK   MVC   LABLAREA(4),=C'WEEK'                                             
         B     NODATE2                                                          
         SPACE 1                                                                
NODATN   GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)                                   
         CLI   GLARGS,1                                                         
         BNE   NODATN5                                                          
         GOTO1 GETDAY,DMCB,WORK,0(R3)                                           
         B     XIT                                                              
NODATN5  MVC   0(6,R3),WORK                                                     
         TM    NDLOCAL,NDCCYY     CCYYMMDD OPTION                               
         BZ    XIT                                                              
         MVC   0(2,R3),=C'19'                                                   
         MVC   2(6,R3),WORK                                                     
         B     XIT                                                              
*                                                                               
NODATE   BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(4),=C'DATE'                                             
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)                                   
         CLI   GLARGS,3                                                         
         BNE   NODATE1                                                          
         MVC   NAMEAREA(6),WORK                                                 
         TM    NDLOCAL,NDCCYY      CCYYMMDD OPTION                              
         BZ    GENOUT                                                           
         MVC   NAMEAREA(2),=C'19'                                               
         MVC   NAMEAREA+2(6),WORK                                               
         B     GENOUT                                                           
NODATE1  MVC   NAMEAREA(2),WORK+2  ARG1 1 = MM/DD                               
         MVI   NAMEAREA+2,C'/'                                                  
         MVC   NAMEAREA+3(2),WORK+4                                             
         CLI   GLARGS,1                                                         
         BE    GENOUT              ARG1 2 = MM/DD/YY                            
         MVI   NAMEAREA+5,C'/'                                                  
         MVC   NAMEAREA+6(2),WORK                                               
         CLI   GLARGS,2                                                         
         BE    GENOUT                                                           
         MVC   NAMEAREA,NDSPACES   ELSE ITS GOOD OLD MMMDD                      
         SPACE 1                                                                
NODATE2  GOTO1 DATCON,DMCB,(2,0(R2)),(4,NAMEAREA)                               
         B     DATEGEN                                                          
         SPACE 1                                                                
NOMONTH  MVC   LABLAREA(5),=C'MONTH'                                            
         LR    R0,R3                                                            
         LA    R3,NAMEAREA                                                      
         BAS   RE,EDMON            MONTH                                        
         LR    R3,R0                                                            
         B     DATEGEN                                                          
         SPACE 1                                                                
*                                  SPECIAL PERIODS GET MMMDD-MMMDD              
NOSPECP  MVC   LABLAREA(5),=C'MONTH'                                            
         CLI   GLARGS,C'Q'                                                      
         BNE   *+10                                                             
         MVC   LABLAREA(7),=C'QUARTER'                                          
         CLI   GLARGS,C'P'                                                      
         BNE   *+10                                                             
         MVC   LABLAREA(6),=C'PERIOD'                                           
         LR    R0,R3                                                            
         LA    R3,NAMEAREA                                                      
         GOTO1 DATCON,DMCB,(2,0(R2)),(4,0(R3))                                  
         MVI   5(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R2)),(4,6(R3))                                  
         LR    R3,R0                                                            
         B     DATEGEN                                                          
         SPACE 1                                                                
*                                  SPECIAL LONG PERIODS                         
*                                  PRIOR AND AFTER CHECK MMMDD-MMMDD/YY         
NOSPLONG MVC   LABLAREA(5),=C'MONTH'                                            
         LR    R0,R3                                                            
         LA    R3,NAMEAREA                                                      
         CLC   0(4,R2),=XL4'11111111'                                           
         BNE   NOSPL20                                                          
         MVC   0(5,R3),=CL5'PRIOR'                                              
         LR    R3,R0                                                            
         B     DATEGEN                                                          
*                                                                               
NOSPL20  CLC   0(4,R2),=XL4'FFFFFFFF'                                           
         BNE   NOSPL40                                                          
         MVC   0(5,R3),=CL5'AFTER'                                              
         LR    R3,R0                                                            
         B     DATEGEN                                                          
*                                                                               
NOSPL40  GOTO1 DATCON,DMCB,(2,0(R2)),(4,0(R3))                                  
         MVI   5(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R2)),(5,6(R3))                                  
         LR    R3,R0                                                            
         B     DATEGEN                                                          
         SPACE 1                                                                
NOQUART  MVC   LABLAREA(7),=C'QUARTER'                                          
         LR    R0,R3                                                            
         LA    R3,NAMEAREA                                                      
         BAS   RE,EDMON            QUARTER                                      
         LA    R2,2(R2)                                                         
         MVI   3(R3),C'-'                                                       
         LA    R3,4(R3)                                                         
         BAS   RE,EDMON                                                         
         MVC   3(2,R3),NDSPACES                                                 
         LR    R3,R0                                                            
         B     DATEGEN                                                          
         SPACE 1                                                                
NOYEAR  MVC    LABLAREA(4),=C'YEAR'                                             
        GOTO1  DATCON,DMCB,(2,0(R2)),(0,NAMEAREA)                               
        XC     NAMEAREA+2(4),NAMEAREA+2                                         
        MVC    NAMEAREA+2(2),NAMEAREA                                           
        MVC    NAMEAREA(2),=C'19'        HARDCODE FOR 1900'S FOR NOW            
        B      DATEGEN                                                          
         SPACE 1                                                                
DATEGEN  CLI   HEADDATE,C'Y'                                                    
         BNE   GENOUT                                                           
         MVI   HEADDATE,0                                                       
         MVC   0(12,R3),NAMEAREA                                                
         MVC   OUTAREA,NDSPACES                                                 
         B     XIT                                                              
         SPACE 1                                                                
NODATNUM GOTO1 DATCON,DMCB,(2,0(R2)),(4,(R3))                                   
         CLI   GLARGS,1                    ,,IS IT MMDDYY-NUM REQUEST           
         BNE   NODAT10                                                          
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK+20)  ,,YES                         
         MVC   0(4,R3),WORK+22                                                  
         MVC   4(2,R3),WORK+20                                                  
         LA    R3,1(R3)            BUMP TO FUDGE                                
NODAT10  CLI   2(R2),0                                                          
         BE    XIT                                                              
         CLI   2(R2),1                                                          
         BE    XIT                                                              
*                                                                               
NODAT12  ZIC   R0,2(R2)                                                         
         MVI   5(R3),C'-'                                                       
         EDIT  (R0),(3,6(R3)),ALIGN=LEFT                                        
         B     XIT                                                              
         SPACE 1                                                                
NOSL     MVC   LABLAREA(6),=C'LENGTH'                                           
         EDIT  (1,0(R2)),(3,CODEAREA)                                           
         B     GENOUT                                                           
         SPACE 1                                                                
NOSLN    DS    0H                                                               
         EDIT  (1,0(R2)),(3,0(R3)),FILL=0                                       
         B     XIT                                                              
         SPACE 1                                                                
NOPCPMG  MVC   LABLAREA(9),=C'GUARANTEE'                                        
         EDIT  (4,0(R2)),(10,CODEAREA),2,ALIGN=LEFT                             
         B     GENOUT                                                           
         SPACE 1                                                                
NOGEN    ZIC   R1,MYOLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),0(R2)                                                    
         EJECT                                                                  
*              SPECIAL FOR DATE/TOTALLING ROUTINES                              
         SPACE 3                                                                
NODTOT   LA    R1,GLARGS           TOTALS USE FIRST ARGUMENT                    
         TM    GLINDS,X'40'                                                     
         BNO   NODTOT2                                                          
         SPACE 1                                                                
NODTOT1  MVC   LABLAREA(4),=C'WEEK'       TO FORMAT LITERAL                     
         CLI   0(R1),C'W'                                                       
         BE    GENOUT                                                           
         MVC   LABLAREA(5),=C'MONTH'                                            
         CLI   0(R1),C'M'                                                       
         BE    GENOUT                                                           
         MVC   LABLAREA(7),=C'QUARTER'                                          
         CLI   0(R1),C'Q'                                                       
         BE    GENOUT                                                           
         MVC   0(7,R3),=C'PERIOD '                                              
         B     GENOUT                                                           
         SPACE 1                                                                
NODTOT2  OC    4(2,R2),4(R2)                                                    
         BZ    NODTOT1                                                          
         LA    R1,GLARGS+1         DETAILS USE SECOND ARGUMENT AND              
         LA    R2,4(R2)            ADDRESS NEXT FIELD IN RECORD                 
         CLI   0(R1),C'D'          NOW SELECT APPROPRIATE ROUTINE               
         BE    NODATE                                                           
         CLI   0(R1),C'W'                                                       
         BE    NOWEEK                                                           
         CLI   0(R1),C'M'                                                       
         BE    NOMONTH                                                          
         CLI   0(R1),C'Q'                                                       
         BE    NOQUART                                                          
         B     NOSPECP                                                          
         EJECT                                                                  
*              ROUTINE TO EDIT MONTH                                            
         SPACE 3                                                                
*              INPUT               R2=A(COMPRESSED DATE)                        
*              OUTPUT              R3=A(AREA FOR MMMYY)                         
         SPACE 1                                                                
EDMON    NTR1                                                                   
         CLI   GLARGS,C'D'         RETURN YYMMDD                                
         BNE   EDMON10                                                          
         GOTO1 DATCON,DMCB,(2,(R2)),(0,0(R3))                                   
         B     XIT                                                              
EDMON10  GOTO1 NDGETBM             RETURNS MONTH IN WORK+4                      
         ZIC   R1,WORK+4                                                        
         MH    R1,=H'3'                                                         
         LA    R1,MYMONTHS-3(R1)                                                
         MVC   0(3,R3),0(R1)                                                    
         MVC   3(2,R3),WORK                                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR ACTIVITY                                            
         SPACE 3                                                                
NIACTDAT BAS   RE,INGOAL                                                        
         L     R4,NBAIO            LAST ACTIVITY DATE                           
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUACTD,R4                                                        
         MVC   0(3,R3),NUACTCDT                                                 
         CLI   0(R3),0                                                          
         BNE   XIT                                                              
         MVC   0(3,R3),NUACTADT                                                 
         B     XIT                                                              
         SPACE 1                                                                
NIACTIVE BAS   RE,INGOAL                                                        
         MVC   0(1,R3),NBACTWHY                                                 
         MVC   1(1,R3),NBACT2WY    (NOW HAVE 2 BYTES FOR THIS)                  
         MVI   2(R3),1                                                          
         B     XIT                                                              
         SPACE 1                                                                
NIACTLOG BAS   RE,INGOAL                                                        
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'68'                                                     
         BAS   RE,GETEL                                                         
         BNE   NIACTLEX                                                         
         USING NUATV,R4                                                         
         LA    RE,NUATVINF                                                      
         LA    RF,10                                                            
NIACTL40 CLI   0(RE),0             LAST ENTRY                                   
         BE    NIACTLEX            YES EXIT                                     
         MVC   0(4,R3),0(RE)                                                    
         LA    R3,4(R3)                                                         
         LA    RE,6(RE)                                                         
         BCT   RF,NIACTL40                                                      
NIACTLEX B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
NOACTDAT BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(9),=C'ACTIVE ON'                                        
         CLI   0(R2),0                                                          
         BE    GENOUT                                                           
         GOTO1 DATCON,DMCB,(3,(R2)),(8,NAMEAREA)                                
         CLI   GLARGS,2                                                         
         BNE   GENOUT                                                           
         EDIT  (1,1(R2)),(2,NAMEAREA)                                           
         OI    NAMEAREA,X'F0'                                                   
         MVI   NAMEAREA+2,C'/'                                                  
         B     GENOUT                                                           
         SPACE 1                                                                
NOACTIVE BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(8),=C'ACTIVITY'                                         
         OC    0(2,R2),0(R2)                                                    
         BNZ   NOACTIV2                                                         
         MVC   NAMEAREA(12),=C'OTHER REASON'                                    
         B     GENOUT                                                           
         SPACE 1                                                                
NOACTIV2 LA    R4,BLOCK                                                         
         MVI   0(R4),C' '                                                       
         MVC   1(249,R4),0(R4)                                                  
         L     R1,=A(ACTIVTAB)                                                  
         BAS   RE,ACTVTEST         TEST FIRST BYTE                              
         L     R1,=A(ACTIVTB2)                                                  
         LA    R2,1(R2)                                                         
         BAS   RE,ACTVTEST         THEN SECOND                                  
         GOTO1 SQUASHER,DMCB,BLOCK,36                                           
         MVC   NAMEAREA(36),BLOCK                                               
         B     GENOUT                                                           
         SPACE 1                                                                
ACTVTEST LA    R0,8                                                             
         SPACE 1                                                                
ACTVTST2 MVC   DUB(1),0(R2)        TEST EACH BIT FOR ACTIVITY                   
         NC    DUB(1),0(R1)                                                     
         CLI   DUB,0                                                            
         BE    ACTVTST4                                                         
         MVC   0(10,R4),1(R1)                                                   
         LA    R4,11(R4)                                                        
         SPACE 1                                                                
ACTVTST4 LA    R1,11(R1)                                                        
         BCT   R0,ACTVTST2                                                      
         BR    RE                                                               
         SPACE 1                                                                
NOACTLOG BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(8),=C'ACTIVITY'                                         
         LR    R5,R3                                                            
         SPACE 1                                                                
NOACTLG2 CLI   0(R2),0                                                          
         BE    NOACTLGX                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,(R2)),(8,(R5))                                    
         LA    R5,9(R5)                                                         
         LA    R2,2(R2)                                                         
*                                                                               
         OC    0(2,R2),0(R2)                                                    
         BNZ   NOACTLG6                                                         
         MVC   0(12,R5),=C'OTHER REASON'                                        
         LA    R2,2(R2)                                                         
         B     NOACTLG8                                                         
*                                                                               
NOACTLG6 LA    R4,BLOCK                                                         
         MVI   0(R4),C' '                                                       
         MVC   1(249,R4),0(R4)                                                  
         L     R1,=A(ACTIVTAB)                                                  
         BAS   RE,ACTVTEST         TEST FIRST BYTE                              
         L     R1,=A(ACTIVTB2)                                                  
         LA    R2,1(R2)                                                         
         BAS   RE,ACTVTEST         THEN SECOND                                  
         LA    R2,1(R2)                                                         
         GOTO1 SQUASHER,DMCB,BLOCK,50                                           
         MVC   0(41,R5),BLOCK                                                   
NOACTLG8 LA    R5,189(R5)                                                       
         B     NOACTLG2                                                         
*                                                                               
NOACTLGX B     XIT                                                              
         EJECT                                                                  
*              DAYS AND TIMES                                                   
         SPACE 3                                                                
*                                  AFFID OR START-END TIME                      
*                                  NIOTHER WILL SUPPLY TIME ZONE                
         SPACE 1                                                                
NIACTTIM MVC   1(2,R3),NBAFFTIM    USE FIRST 2 BYTES, LEAVE REST 0              
         OC    NBAFFTIM,NBAFFTIM   IF AFFID TIME                                
         BNZ   NITIMADJ                                                         
         MVC   1(4,R3),NBTIME      ELSE USE 4-BYTE START-END TIME               
         B     NITIMADJ                                                         
         SPACE 1                                                                
NIDLTIME MVC   1(4,R3),NBTIME      START-END TIME                               
         SR    R1,R1              ADJUST START TIME 6AM=START OF DAY            
         ICM   R1,3,1(R3)                                                       
         C     R1,=F'600'          IF START HOUR LESS THAN 0600                 
         BNL   NIDLT10                                                          
         A     R1,=F'2400'          ADD 2400                                    
         STCM  R1,3,1(R3)                                                       
NIDLT10  SR    R1,R1                                                            
         ICM   R1,3,3(R3)          ADJUST END TIME                              
         C     R1,=F'600'                                                       
         BNL   NIOTHER                                                          
         A     R1,=F'2400'                                                      
         STCM  R1,3,3(R3)                                                       
         B     NIOTHER                                                          
         SPACE 1                                                                
NICALTIM MVI   BYTE,0              START-END TIME                               
NITI0    MVC   1(4,R3),NBTIME                                                   
         CLI   GLARGS,X'FF'        C/M/P TIME ZONES                             
         BNE   *+8                                                              
         BAS   RE,OHZONE                                                        
         B     NITIMADJ                                                         
         SPACE 1                                                                
OHZONE   LA    R1,100                                                           
         CLI   GLARGS+1,C'C'       CENTRAL                                      
         BE    OHO10                                                            
         LA    R1,200                                                           
         CLI   GLARGS+1,C'M'       MOUNTAIN                                     
         BE    OHO10                                                            
         LA    R1,300                                                           
         CLI   GLARGS+1,C'P'       PACIFIC                                      
         BNER  RE                                                               
OHO10    SR    R0,R0                                                            
         ICM   R0,3,1(R3)          GET START TIME                               
         CR    R0,R1               IS START TIME LESS THAN ADJUSTMENT           
         BL    OHO15            NO/                                             
         SR    R0,R1                                                            
         LTR   R0,R0                      IF ZERO                               
         BNZ   OHO20                                                            
         LA    R0,2400                    MAKE IT MIDNIGHT                      
         B     OHO20                                                            
OHO15    SR    R0,R1            YES/SUBTRACT ADJ FROM START TIME                
         LA    RF,2400              GIVES US MINUS NUMBER WHICH                 
         AR    R0,RF                WE ADD TO 2400 FOR ADJ START TIME           
OHO20    STCM  R0,3,1(R3)                                                       
         SR    R0,R0               NOW DEAL WITH END TIME                       
         ICM   R0,3,3(R3)                                                       
         LTR   R0,R0                                                            
         BZ    OHO40                                                            
         CR    R0,R1                                                            
         BL    OHO30                                                            
         SR    R0,R1                                                            
         LTR   R0,R0                                                            
         BNZ   OHO40                                                            
         LA    R0,2400                                                          
         B     OHO40                                                            
OHO30    SR    R0,R1                                                            
         LA    RF,2400                                                          
         AR    R0,RF                                                            
OHO40    STCM  R0,3,3(R3)                                                       
         BR    RE                                                               
         SPACE 1                                                                
NIAFFTIM MVC   1(2,R3),NBAFFTIM    AFFIDAVIT TIME                               
         OC    NBAFFTIM,NBAFFTIM                                                
         BNZ   NITIMADJ                                                         
         MVC   1(2,R3),=X'FFFF'    IF NO AFF/SET TO FF'S                        
         SPACE 1                                                                
NITIMADJ SR    R1,R1              ADJUST START TIME 6AM=START OF DAY            
         ICM   R1,3,1(R3)                                                       
         C     R1,=F'600'          IF START HOUR LESS THAN 0600                 
         BNL   NITI10                                                           
         A     R1,=F'2400'          ADD 2400                                    
         STCM  R1,3,1(R3)                                                       
NITI10   CLI   GLARGS+2,C'B'       ARE WE DOING TWO TIMES                       
         BNE   NIOTHER                                                          
         CLI   BYTE,C'Y'           HAVE WE ALREADY DONE 2ND TIME                
         BE    XIT                                                              
         LA    R3,5(R3)                                                         
         MVI   BYTE,C'Y'                                                        
         MVI   GLARGS+1,C'C'       AND 2ND TIME IS CENTRAL                      
         B     NITI0                                                            
         SPACE 1                                                                
NOTIMEN  CLC   1(2,R2),=X'FFFF'    NOAFFID TIME                                 
         BE    XIT                                                              
         SR    R4,R4                                                            
         ICM   R4,3,1(R2)                                                       
NOTIMN5  BAS   R1,CHKTIME                                                       
         EDIT  (R4),(4,0(R3)),FILL=0                                            
         CLI   GLARGS,1                                                         
         BNE   XIT                                                              
         MVI   GLARGS,0                                                         
         ICM   R4,3,3(R2)                                                       
         LA    R3,4(R3)                                                         
         B     NOTIMN5                                                          
*        BAS   R1,CHKTIME                                                       
*        EDIT  (R4),(4,4(R3)),FILL=0                                            
*        B     XIT                                                              
* - MILITARY TIME MUST BE UNDER 2400 HOURS                                      
CHKTIME  DS    0H                                                               
         CH    R4,=H'2400'                                                      
         BNHR  R1                                                               
         SH    R4,=H'2400'                                                      
         BR    R1                                                               
*                                                                               
         SPACE 1                                                                
NOTIME   MVI   BYTE,0              OUTPUT TIME                                  
         BAS   RE,OUTGOAL                                                       
*                                                                               
NOTIM0   CLC   1(2,R2),=X'FFFF'    NO AFFID TIME                                
         BE    XIT                 SO EXIT                                      
         SR    R1,R1                 RESET ADJUSTED START TIME                  
         ICM   R1,3,1(R2)                                                       
         C     R1,=F'2400'                                                      
         BNH   NOTIM5                                                           
         S     R1,=F'2400'                                                      
         STCM  R1,3,1(R2)                                                       
*                                                                               
NOTIM5   LA    R1,1(R2)                                                         
         ST    R1,DMCB             START-END BYTES 2-5                          
         MVC   DMCB(1),0(R2)       TIME ZONE IS FIRST BYTE                      
         GOTO1 UNTIME,DMCB,,0(R3)                                               
         CLI   GLARGS,C'B'         ARE WE DOING DOUBLE TIMES                    
         BNE   XIT                                                              
         CLI   BYTE,C'Y'           HAVE WE ALREADY DONE 2ND TIME                
         BE    XIT                                                              
         LA    R2,5(R2)                                                         
         LA    R3,12(R3)                                                        
         MVI   BYTE,C'Y'                                                        
         B     NOTIM0                                                           
         SPACE 1                                                                
*-- PUP DAY                                                                     
NIPDAY   CLI   NBMODE,NBPROCPP                                                  
         BNE   XIT                                                              
         MVC   0(1,R3),NBDAY       DAY CODE                                     
         MVC   1(3,R3),NBDAYNAM    DAY NAME                                     
         B     XIT                                                              
         SPACE 1                                                                
* - DAY AND TIME2  CONDENSED INTO ONE KEYWORD                                   
NIDT2    BAS   RE,GTDAY                                                         
         LA    R3,4(R3)                                                         
         BAS   RE,GTTM2                                                         
         B     XIT                                                              
GTDAY    NTR1                                                                   
         MVI   GLARGS,0                                                         
         B     NIDAY                                                            
         DC    H'0'                SHOULD NEVER GET HERE                        
GTTM2    NTR1                                                                   
         MVC   GLARGS(4),=X'FF40C200'      NICALTIM ARGS                        
         B     NICALTIM                                                         
         DC    H'0'                                                             
NODT2    CLC   DAYSAVE,1(R2)                                                    
*        BE    NODT2B              MAKE IT SOLID ALWAYS                         
*        MVC   DAYSAVE,1(R2)                                                    
         MVC   0(3,R3),1(R2)                                                    
NODT2B   LA    R3,4(R3)                                                         
         LA    R2,4(R2)                                                         
         BAS   RE,GTOTM2           CALL NOTIME                                  
         B     XIT                                                              
DAYSAVE  DS    CL3                                                              
*                                                                               
GTOTM2   NTR1                                                                   
         MVI   GLARGS,C'B'         SET ARG FOR NOTIME2                          
         B     NOTIME                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
*                                  CONVERT DAY MASK INTO DAY NUMBER             
NIDAY    BAS   RE,INGOAL                                                        
         L     R1,=A(DAYCONVT)         DAY MASK TABLE                           
         SPACE 1                                                                
NIDAY2   CLC   NBDAY,0(R1)                                                      
         BE    NIDAY4                                                           
         LA    R1,5(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   NIDAY2                                                           
         SPACE 1                                                                
NIDAY4   MVC   0(4,R3),1(R1)       SET UP DAY NUMBER                            
         B     XIT                                                              
         SPACE 1                                                                
NODAY    BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(3),=C'DAY'                                              
         MVC   CODEAREA(3),1(R2)                                                
         B     GENOUT                                                           
*                                                                               
NODAYN   BAS   RE,OUTGOAL                                                       
         MVC   LABLAREA(3),=C'DAY'                                              
         MVC   CODEAREA(3),1(R2)                                                
         MVI   0(R3),C'1'                                                       
         CLC   =C'MO',CODEAREA                                                  
         BE    XIT                                                              
         MVI   0(R3),C'2'                                                       
         CLC   =C'TU',CODEAREA                                                  
         BE    XIT                                                              
         MVI   0(R3),C'3'                                                       
         CLC   =C'WE',CODEAREA                                                  
         BE    XIT                                                              
         MVI   0(R3),C'4'                                                       
         CLC   =C'TH',CODEAREA                                                  
         BE    XIT                                                              
         MVI   0(R3),C'5'                                                       
         CLC   =C'FR',CODEAREA                                                  
         BE    XIT                                                              
         MVI   0(R3),C'6'                                                       
         CLC   =C'SA',CODEAREA                                                  
         BE    XIT                                                              
         MVI   0(R3),C'7'                                                       
         CLC   =C'SU',CODEAREA                                                  
         BE    XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR COMMENTS                                            
         SPACE 3                                                                
*              ARGUMENT 1          C'C'=CLIENT COMMENTS ONLY                    
*                                  C'I'=INTERNAL COMMENTS ONLY                  
         SPACE 1                                                                
NICOMMNT BAS   RE,INGOAL                                                        
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'04'        LOOK FOR X'04' ELEMENTS                      
         BAS   RE,GETEL                                                         
         B     NICMNT4                                                          
         SPACE 1                                                                
NICMNT2  BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
NICMNT4  BNE   XIT                                                              
         USING NUCOMD,R4                                                        
         CLI   GLARGS,0                                                         
         BE    NICMNT6                                                          
         CLC   GLARGS(1),NUCOMTYP                                               
         BNE   NICMNT2             OPTIONAL FILTER ON TYPE                      
         SPACE 1                                                                
NICMNT6  ZIC   R1,NUCOMLEN                                                      
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NUCOMMNT                                                 
         CLI   GLARGS+1,1                                                       
         BE    XIT                                                              
         LA    R3,2(R1,R3)                                                      
         B     NICMNT2                                                          
         EJECT                                                                  
*              TRAFFIC FEED ROUTINES                                            
         SPACE 3                                                                
NITFEED  BAS   RE,INGOAL                                                        
         MVC   0(52,R3),NDSPACES                                                
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   NITFEED2                                                         
         USING NUFEDEL,R4                                                       
         MVC   0(4,R3),NUFEEDCD    PICK OFF FEED CODE                           
         SPACE 1                                                                
NITFEED2 L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'21'                                                     
         MVI   WORK,0                                                           
         BAS   RE,GETEL                                                         
         BNE   NITFEED4                                                         
         USING NUCMLEL,R4                                                       
         MVC   WORK(1),NUCMLFLG    SAVE FLAG CODE                               
         SPACE 1                                                                
NITFEED4 L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         LA    R0,4                                                             
         CLI   GLARGS,C'T'         FEED + TITLE?                                
         BE    NITFEET8            YES                                          
         B     NITFEED8                                                         
                                                                                
NITFEED6 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING NUFDCEL,R4                                                       
NITFEED8 TM    NUFDCFL2,X'80'      SKIP DELETED FEED                            
         BO    NITFEED6                                                         
         TM    NBSPLOPT,X'80'      ,,IF SPLITING PRODUCTS                       
         BZ    NITFEED9                                                         
***************************************************************                 
         CLI   NUFDPPOS,0          PROD POSITION IN X'14' ELEM?                 
         BE    NOPOSITN            NO                                           
         CLI   NBSPLCNT,0          YES/MULTI PRODS ACTIVE?                      
         BE    NOPOSITN                SHOULD BE - BUT DON'T DIE                
         CLC   NBSPLCNT,NUFDPPOS   PRODUCT POSITIONS MATCH??                    
         BE    NITFEED9                                                         
         B     NITFEED6                                                         
*************************************************************                   
NOPOSITN TM    NBUNST3,X'40'       ,,IF COPY SPLIT                              
         BZ    NITFEED9                                                         
         CLC   NBSPLPRN,NUFDCPRD   ,,MUST MATCH                                 
         BNE   NITFEED6                                                         
NITFEED9 MVC   0(4,R3),NUFDCFED                                                 
         CLI   GLARGS,C'O'         FEED CODE ONLY?                              
         BE    NITFED10                                                         
         MVC   5(8,R3),NUFDCML1                                                 
         TM    WORK,X'E0'          TEST FOR PROD,LEN,OR DATE CHANGE             
         BZ    *+10                                                             
         MVC   5(8,R3),=C'REASSIGN'                                             
NITFED10 LA    R3,13(R3)                                                        
         BCT   R0,NITFEED6                                                      
         B     XIT                                                              
         SPACE 1                                                                
NOTFEED  BAS   RE,OUTGOAL                                                       
         MVC   000(13,R3),00(R2)                                                
         MVC   198(13,R3),13(R2)                                                
         MVC   396(13,R3),26(R2)                                                
         MVC   594(13,R3),39(R2)                                                
         B     XIT                                                              
         EJECT                                                                  
*              TRAFFIC FEED ROUTINES (AS IN NITFEED + COM TITLE)                
         SPACE 3                                                                
NITFEET  DS     0H                                                              
*NITFEET  BAS   RE,INGOAL                                                       
*        MVC   0(116,R3),NDSPACES                                               
*        L     R4,NBAIO                                                         
*        CLI   0(R4),X'04'                                                      
*        BNE   XIT                                                              
*        MVI   ELCODE,X'22'                                                     
*        BAS   RE,GETEL                                                         
*        BNE   NITFEET2                                                         
*        USING NUFEDEL,R4                                                       
*        MVC   0(4,R3),NUFEEDCD    PICK OFF FEED CODE                           
*        SPACE 1                                                                
*NITFEET2 L     R4,NBAIO                                                        
*        MVI   ELCODE,X'21'                                                     
*        MVI   WORK,0                                                           
*        BAS   RE,GETEL                                                         
*        BNE   NITFEET4                                                         
*        USING NUCMLEL,R4                                                       
*        MVC   WORK(1),NUCMLFLG    SAVE FLAG CODE                               
*        SPACE 1                                                                
*NITFEET4 L     R4,NBAIO                                                        
*        MVI   ELCODE,X'23'                                                     
*        BAS   RE,GETEL                                                         
*        LA    R0,4                                                             
*        B     NITFEET8                                                         
         SPACE 1                                                                
NITFEET6 BAS   RE,NEXTEL                                                        
         BNE   NITFEETX                                                         
         SPACE 1                                                                
*NITFEET8 BNE   NITFEETX                                                        
NITFEET8 DS    0H                                                               
         USING NUFDCEL,R4                                                       
***************************************************************                 
         CLI   NUFDPPOS,0          PROD POSITION IN X'14' ELEM?                 
         BE    NITFEET9            NO                                           
         CLI   NBSPLCNT,0          YES/MULTI PRODS ACTIVE?                      
         BE    NITFEET9                SHOULD BE - BUT DON'T DIE                
         CLC   NBSPLCNT,NUFDPPOS   PRODUCT POSITIONS MATCH??                    
         BNE   NITFEET6                                                         
*************************************************************                   
NITFEET9 MVC   0(4,R3),NUFDCFED                                                 
         MVC   5(8,R3),NUFDCML1                                                 
         TM    WORK,X'E0'          TEST FOR PROD,LEN,OR DATE CHANGE             
         BZ    *+14                                                             
         MVC   5(8,R3),=CL8'REASSIGN'                                           
         B     NITFBUMP                                                         
*                                                                               
         NETGO NVSETSPT,DMCB           SET UP TO READ SPOT FILE                 
         MVC   FILENAME,=C'TRFDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),NBACTCLI                                                
         MVC   KEY+5(8),5(R3)                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BNE   NITFBUMP                                                         
         MVC   FILENAME,=C'TRFFILE '                                            
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         USING CMLRECD,R5                                                       
         MVC   14(15,R3),CMLTITLE   GET OTHER STUFF                             
         B     NITFBUMP                                                         
         SPACE 1                                                                
NITFBUMP LA    R3,29(R3)                                                        
         BCT   R0,NITFEET6                                                      
NITFEETX XC    FILENAME,FILENAME   RESET TO READ UNTFIL                         
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              TRAFFIC FEED ROUTINES (AS IN NITFEED + FEED DESCRIP)             
         SPACE 3                                                                
NITFEEDN BAS   RE,INGOAL                                                        
         MVC   0(116,R3),NDSPACES                                               
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   NITFEEN2                                                         
         USING NUFEDEL,R4                                                       
         MVC   0(4,R3),NUFEEDCD    PICK OFF FEED CODE                           
         SPACE 1                                                                
NITFEEN2 L     R4,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         MVI   WORK,0                                                           
         BAS   RE,GETEL                                                         
         BNE   NITFEEN4                                                         
         USING NUCMLEL,R4                                                       
         MVC   WORK(1),NUCMLFLG    SAVE FLAG CODE                               
         SPACE 1                                                                
NITFEEN4 L     R4,NBAIO                                                         
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         LA    R0,4                                                             
         B     NITFEEN8                                                         
         SPACE 1                                                                
NITFEEN6 BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
NITFEEN8 BNE   NITFEENX                                                         
         USING NUFDCEL,R4                                                       
         LR    R5,R4               SAVE R4                                      
         MVC   0(4,R3),NUFDCFED                                                 
         MVC   5(8,R3),NUFDCML1                                                 
         TM    WORK,X'E0'          TEST FOR PROD,LEN,OR DATE CHANGE             
         BZ    *+14                                                             
         MVC   5(8,R3),=CL8'REASSIGN'                                           
         B     NITNBUMP                                                         
*                                                                               
         NETGO NVSETSPT,DMCB           SET UP TO READ SPOT FILE                 
         MVC   FILENAME,=C'TRFDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2B'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(4),NBACTNET                                                
         MVC   KEY+7(2),NBACTCLI   TRY  CLIENT SPECIFIC FIRST                   
         MVC   KEY+9(4),0(R3)      FEED CODE                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    NITFEN10                                                         
         XC    KEY,KEY             TRY FOR NETWORK FEED                         
         MVC   KEY(2),=X'0A2B'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(4),NBACTNET                                                
         MVC   KEY+9(4),0(R3)      FEED CODE                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BNE   NITNBUMP                                                         
NITFEN10 MVC   FILENAME,=C'TRFFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING FEEDELEM,R4                                                      
         MVC   4(25,R3),FEEDELDS   FEED DESCRIPTION                             
         SPACE 1                                                                
NITNBUMP LA    R3,29(R3)                                                        
         LR    R4,R5               RESET R4                                     
         MVI   ELCODE,X'23'        RESET ELCODE                                 
         BCT   R0,NITFEEN6                                                      
NITFEENX XC    FILENAME,FILENAME   RESET TO READ UNTFIL                         
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
NOTFEET  BAS   RE,OUTGOAL                                                       
         MVC   000(29,R3),00(R2)                                                
         MVC   198(29,R3),29(R2)                                                
         MVC   396(29,R3),58(R2)                                                
         MVC   594(29,R3),87(R2)                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
NIPOST   DS    0H                  POSTING TYPE                                 
         BAS   RE,INGOAL                                                        
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUSDREL,R4                                                       
         MVC   0(1,R3),NUPOSTYP                                                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              COMMERCIAL SCHEDULED ROUTINES                                    
         SPACE 3                                                                
*              ARGUMENT 1          1=COMMERCIAL NUMBER                          
*                                  2=COMMERCIAL NAME                            
*                                  3=POSITION CODE                              
*                                  4=BILLBOARD LENGTH                           
*                                  5=BILLBOARD SLIDE NUMBER                     
*                                  6=BILLBOARD POSITION                         
         SPACE 1                                                                
NICOMMLS BAS   RE,INGOAL                                                        
         MVC   0(8,R3),NDSPACES                                                 
         L     R4,NBAIO            COMMERCIAL SCHEDULED                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUCMLEL,R4                                                       
         MVC   WORK(1),NUCMLFLG    SAVE FLAG CODE                               
         CLI   GLARGS,3                                                         
         BL    NICMS1                                                           
         BE    NICMS3                                                           
         CLI   GLARGS,5                                                         
         BL    NICMS4                                                           
         BE    NICMS5                                                           
         B     NICMS6                                                           
         SPACE 1                                                                
NICMS1   DS    0H                                                               
         CLI   NBPRDLST,0          ..IF MULT PRODUCTS                           
         BNE   NICMS1D             ..COPY SPLITS                                
         LA    RE,NUCML1                                                        
         CLI   NBSPLPRN,0                                                       
         BE    NICMS2                                                           
         CLC   NBPRD,NBPRD2     *  IF THESE ARE SAME PIGGY PRODS                
         BNE   NICMS1B          *                                               
         CLI   NBSPLTYP,C'S'    *  IF WE ARE ON SECOND PROD                     
         BE    NICMS1D          *                                               
NICMS1B  CLC   NBSPLPRN,NBPRD                                                   
         BE    NICMS2                                                           
NICMS1D  LA    RE,NUCML2                                                        
         TM    NBUNST3,X'40'       IF WE ARE HANDLING COPY SPLIT                
         BNO   NICMS2                                                           
         L     R4,NBAIO               GET COMMERCIAL FROM FEED ELEMENT          
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUFDCEL,R4                                                       
         LA    RE,NUFDCML1                                                      
         SPACE 1                                                                
NICMS2   MVC   0(8,R3),0(RE)                                                    
         TM    WORK,X'E0'          CHK PROD,LEN.DATE CHANGE                     
         BZ    *+10                                                             
         MVC   0(8,R3),=C'REASSIGN'                                             
         CLI   GLARGS,1            1=COMMERCIAL NUMBER                          
         BE    XIT                                                              
         LR    R2,R3               2=COMMERCIAL NAME                            
         BAS   RE,GETCOMM                                                       
         CLI   GLARGS+1,C'B'      (ARG 2 - NAME AS WELL)                        
         BNE   *+8                                                              
         LA    R3,8(R3)                                                         
         MVC   0(48,R3),COMMNAME                                                
         TM    WORK,X'E0'          CHK PROD,LEN.DATE CHANGE                     
         BZ    NICM2X                                                           
         XC    0(48,R3),0(R3)                                                   
         MVC   0(8,R3),=C'REASSIGN'                                             
NICM2X   B     XIT                                                              
         SPACE 1                                                                
         USING NUCMLEL,R4                                                       
NICMS3   MVC   0(4,R3),NUCMLPOS    3=COMERCIAL POSITION CODE                    
         B     XIT                                                              
         SPACE 1                                                                
NICMS4   DS    0H                  4=BILLBOARD LENGTH                           
         EDIT  (1,NUCMLBSL),(3,0(R3))                                           
         B     XIT                                                              
         SPACE 1                                                                
NICMS5   MVC   0(8,R3),NUCMLBSN    5=BILLBOARD SLIDE NUMBER                     
         TM    WORK,X'E0'          CHK PROD ETC CHANGE                          
         BZ    *+10                                                             
         MVC   0(8,R3),=C'REASSIGN'                                             
         OC    NUCMLBCN(8),NUCMLBCN                                             
         BZ    XIT                                                              
         MVI   8(R3),C'/'                                                       
         MVC   9(4,R3),NUCMLBCN+4                                               
         B     XIT                                                              
         SPACE 1                                                                
NICMS6   MVC   0(4,R3),NUCMLBPS    6=BILLBOARD POSITION CODE                    
         B     XIT                                                              
         SPACE 1                                                                
NOCOMM   BAS   RE,OUTGOAL                                                       
         MVC   0(8,R3),0(R2)       COMMERCIAL CODE                              
         CLC   1(7,R3),NDSPACES                                                 
         BH    NOCOMM0                                                          
         MVC   0(8,R3),NDSPACES                                                 
         B     NOCOMM1                                                          
NOCOMM0  CLI   0(R3),C'*'                                                       
         BH    *+10                                                             
         MVC   0(9,R3),0(R2)       * OR BLANK + CODE                            
NOCOMM1  CLI   GLARGS,C'C'                                                      
         BE    XIT                                                              
         CLI   GLARGS,C'B'         (BOTH)                                       
         BNE   NOCOMM2                                                          
         CLI   0(R2),C'*'          MAY BE BLANK OR *                            
         BH    *+12                                                             
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         SPACE 1                                                                
NOCOMM2  MVC   0(24,R3),0(R2)      COMMERCIAL NAME(S) OUTPUT                    
***      CLC   24(24,R2),NDSPACES                                               
***      BE    NOCOMMX                                                          
         OC    24(24,R2),24(R2)                                                 
         BZ    NOCOMMX                                                          
         MVC   198(24,R3),24(R2)                                                
         B     XIT                                                              
NOCOMMX  MVC   198(24,R3),NDSPACES   NEED THIS TO CLEAR MIDLINES                
         B     XIT                                                              
         SPACE 1                                                                
NIBB     BAS   RE,INGOAL                                                        
         L     R4,NBAIO            BILLBOARD FROM COMMERCIAL                    
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUCMLEL,R4                                                       
         TM    NUCMLFLG,X'04'                                                   
         BNO   NIBB2                                                            
         MVC   0(2,R3),=C'BB'                                                   
         SPACE 1                                                                
NIBB2    CLI   NUCMLBSL,0                                                       
         BE    XIT                                                              
         MVC   0(2,R3),=C'BC'                                                   
         B     XIT                                                              
         SPACE 1                                                                
NIBBC    BAS   RE,INGOAL           BILLBOARD COUNT                              
         L     R4,NBAIO            BILLBOARD FROM COMMERCIAL                    
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUCMLEL,R4                                                       
         TM    NUCMLFLG,X'04'                                                   
         BNO   NIBBC2                                                           
         MVI   3(R3),1                                                          
         B     XIT                                                              
         SPACE 1                                                                
NIBBC2   CLI   NUCMLBSL,0                                                       
         BE    XIT                                                              
         MVI   3(R3),1                                                          
         B     XIT                                                              
         EJECT                                                                  
*              COMMERCIAL AIRED ROUTINES                                        
         SPACE 3                                                                
*              ARGUMENT 1          1=COMMERCIAL NUMBER                          
*                                  2=COMMERCIAL NAME                            
         SPACE 1                                                                
NICOMMLA BAS   RE,INGOAL           COMMERCIAL THAT AIRED                        
         TM    NBSPLOPT,X'80'                                                   
         BNO   CA020                                                            
         L     R4,NBAIO            COMMERCIAL THAT AIRED                        
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUPCELD,R4                                                       
         CLC   NBSPLPRN,NUPCPRD                                                 
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         MVC   1(8,R3),NUPCCML                                                  
*                                                                               
CA020    L     R4,NBAIO            COMMERCIAL SCHEDULED                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   CA100                                                            
         USING NUCMLEL,R4                                                       
         LA    RE,NUCML1                                                        
         CLI   NBSPLPRN,0                                                       
         BE    CA050                                                            
         CLC   NBSPLPRN,NBPRD                                                   
         BE    *+8                                                              
         LA    RE,NUCML2                                                        
         SPACE 1                                                                
CA050    CLC   1(8,R3),0(RE)                                                    
         BE    *+8                                                              
         SPACE 1                                                                
CA100    MVI   0(R3),C'*'                                                       
         CLI   GLARGS,2            2=COMMERCIAL NAME                            
         BL    XIT                                                              
         LA    R2,1(R3)            PASS R2 TO GETCOMM ROUTINE                   
         CLI   GLARGS+1,C'B'      (ARG 2 - NAME AS WELL)                        
         BNE   *+8                                                              
         LA    R3,9(R3)                                                         
         BAS   RE,GETCOMM                                                       
         MVC   0(48,R3),COMMNAME                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
NICCLASS DS    0H                  COMMERCIAL CLASS                             
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'21'        LOOK FOR X'21' ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   NICCLX                                                           
         USING NUCMLEID,R4                                                      
         TM    NUCMLFLG,X'C0'                                                   
         BO    NICCLX                                                           
         MVC   TARGID,NUCML1                                                    
         BAS   RE,GETCLASS                                                      
         MVC   TARGID,NUCML2                                                    
         BAS   RE,GETCLASS                                                      
*                                                                               
         MVI   ELCODE,X'23'        THEN TRY X'23' ELEMENT                       
         USING NUFDCEL,R4                                                       
NICC20   BAS   RE,NEXTEL                                                        
         BNE   NICCLX                                                           
         MVC   TARGID,NUFDCML1                                                  
         BAS   RE,GETCLASS                                                      
         MVC   TARGID,NUFDCML2                                                  
         BAS   RE,GETCLASS                                                      
         B     NICC20              ANY MORE 23'S                                
NICCLX   B     XIT                                                              
*                                                                               
GETCLASS NTR1                                                                   
         CLC   TARGID,NDSPACES                                                  
         BNH   GCX                                                              
         L     RE,NDCIDTBL           COMMERCIAL ID TABLE                        
         CLC   NBACTCLI,CURRCLI    ..IS IT NEW CLIENT                           
         BE    *+16                                                             
         MVC   CURRCLI,NBACTCLI    ..YES/SET CLIENT                             
         XC    0(240,RE),0(RE)     ..CLEAR TABLE  *** HARD CODED ***            
*                                     (TABLE RESIDES IN NEWRI20)                
         LA    RF,20               MAX 20 ENTRIES                               
GC20     CLI   0(RE),0                                                          
         BE    GC30                                                             
         CLC   TARGID,0(RE)        IS ID/CLASS ALREADY IN TABLE                 
         BNE   GC25                                                             
         MVC   TARGCLS,8(RE)       YES/PASS CLASS                               
         B     GC50                                                             
GC25     LA    RE,12(RE)                                                        
         BCT   RF,GC20                                                          
         L     RE,NDCIDTBL                                                      
         XC    0(240,RE),0(RE)   OVER 20 ENTRIES/START FROM SCRATCH             
*                                                                               
GC30     DS    0H                  NO/READ COMMERCIAL RECORD                    
         MVC   0(8,RE),TARGID                                                   
         LR    R5,RE               SAVE COMM ID TABLE POINTER                   
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),NBACTAM   (AGENCY/MEDIA/CLIENT)                        
         MVC   CMLKCML,TARGID                                                   
***      CLC   CMLKCML,=C'REASSIGN'    WHAT'S THISSSS                           
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'TRFDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GCX                                                              
         MVC   FILENAME,=C'TRFFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING CMLDTAEL,R4                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   GCX                                                              
         MVC   8(4,R5),CMLCLASS    SET IN TABLE                                 
         MVC   TARGCLS,CMLCLASS                                                 
GC50     MVC   0(4,R3),TARGCLS     SET IN OUT AREA                              
         LA    R3,198(R3)          FOR MULTIPLE COMM/IDS                        
GCX      DS    0H                                                               
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE LOOKS FIRST FOR COMMERCIAL NAME IN OTHER                 
*              IF NOT THERE IT GOEST TO TRAFFIC                                 
         SPACE 3                                                                
*                                                                               
NICMUT   BAS   RE,INGOAL                                                        
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'60'        LOOK FOR X'60' ELEMENTS                      
         BAS   RE,GETEL                                                         
         B     NICMU4                                                           
NICMU2   BAS   RE,NEXTEL                                                        
         SPACE                                                                  
NICMU4   BNE   NICMU10                                                          
         USING NUOTH,R4                                                         
         CLI   NUOTTYP,C'C'        COMMERCIAL                                   
         BNE   NICMU2                                                           
         ZIC   R1,NUOTLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),NUOTHER                                                  
         SPACE 1                                                                
NICMU10  DS    0H                  IS THERE TRAFFIC COMMERCIAL                  
         B     NICOMMLS                                                         
         EJECT                                                                  
*              ROUTINES FOR OTHERS                                              
         SPACE 3                                                                
*              ARGUMENT 1          TYPE FIELD TO MATCH ON                       
         SPACE 1                                                                
NIOTHER  BAS   RE,INGOAL                                                        
         CLI   GLARGS,C'D'         ALTERNATE DAYPART                            
         BNE   *+10                                                             
         MVC   0(1,R3),NBACTDP        DEFAULTS TO REGULAR DAYPART               
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'60'        LOOK FOR X'60' ELEMENTS                      
         BAS   RE,GETEL                                                         
         B     NIOTH4                                                           
         SPACE 1                                                                
NIOTH2   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
NIOTH4   BNE   NIOTH8                                                           
         USING NUOTH,R4                                                         
         CLC   GLARGS(1),NUOTTYP                                                
         BNE   NIOTH2              FILTER ON TYPE                               
         ZIC   R1,NUOTLEN                                                       
         SH    R1,=H'4'                                                         
         CLI   GLARGS,C'E'         E COST NEEDS TO BE NUMERIC                   
         BE    NIOTH6                                                           
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),NUOTHER                                                  
         SPACE 1                                                                
NIOTH6   EX    R1,*+8              SPECIAL FOR ECOST                            
         B     XIT                                                              
         PACK  0(8,R3),NUOTHER(0)                                               
         SPACE 1                                                                
NIOTH8   CLI   GLARGS,C'E'         IF MISSING GET COST ANYWAY                   
         BE    NIOTH20                                                          
***      CLI   GLARGS,C'S'         IF MISSING AND KEYWORD=CMER                  
***      BNE   XIT                                                              
         B     XIT                                                              
         LA    RF,NICOMMLS                                                      
         MVI   GLARGS,2                                                         
         BR    RF                                                               
NIOTH20  L     RF,=A(OTHCOST)      (NEED TO GET THIS FROM SECTION 2)            
         L     RE,=A(DRIVE2)                                                    
         BR    RE                                                               
         EJECT                                                                  
*              HIGH LEVEL COMMENTS                                              
         SPACE 3                                                                
NOHICOMM ZIC   R0,GLARGS                                                        
         SPACE 1                                                                
NOHICOM2 MVC   0(36,R3),0(R2)                                                   
         LA    R2,36(R2)                                                        
         LA    R3,198(R3)                                                       
         BCT   R0,NOHICOM2                                                      
         B     XIT                                                              
         SPACE 1                                                                
NIHICOMM GOTO1 =A(XTRARTN),DMCB,(2,(RA))                                        
         B     XIT                                                              
         EJECT                                                                  
*              INTERNAL ROUTINES                                                
         SPACE 3                                                                
NIAGY    MVC   0(2,R3),NBALPHA     AGENCY ALPHA                                 
         B     XIT                                                              
         SPACE 1                                                                
NICOMP   ZAP   0(8,R3),=P'0'       DUMMY COMPUTE                                
         B     XIT                                                              
         SPACE 1                                                                
NISUNIQ  L     R1,NRWUNIQ          RETURN A UNIQUE NUMBER SO EACH UNIT          
         LA    R1,1(R1)               PRINTS SEPERATELY                         
         ST    R1,NRWUNIQ                                                       
         ST    R1,0(R3)                                                         
         B     XIT                                                              
         SPACE 1                                                                
NOREPORT MVC   LABLAREA(6),=C'REPORT'                                           
*        B     TOTOUT                                                           
         GOTO1 =A(XGENOUT),DMCB,(RA),C'T'                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT PERCENT TO HEADINGS                            
         SPACE 3                                                                
*              INPUT               NDPERCNT='COST' OR %                         
         SPACE 1                                                                
NHPERCNT L     R2,NDPERCNT                                                      
         CLC   NDPERCNT(4),=C'COST'                                             
         BNE   NHPCT2                                                           
         MVC   0(5,R3),=C'COST%'                                                
         B     XIT                                                              
         SPACE 1                                                                
NHPCT2   EDIT  (4,NDPERCNT),(6,0(R3)),2                                         
         MVI   6(R3),C'%'                                                       
         CLI   5(R3),C'0'          GET RID OF TRAILING ZEROS                    
         BNE   XIT                                                              
         MVC   5(2,R3),=C'% '                                                   
         CLI   4(R3),C'0'                                                       
         BNE   XIT                                                              
         MVC   3(3,R3),=C'%  '                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT DATES TO HEADINGS                              
         SPACE 3                                                                
*              ARGUMENTS           SEE FILTUNIT                                 
         SPACE 1                                                                
NHFILTER CLI   GLARGS+15,0         ONLY HANDLING DATES FOR NOW                  
         BE    XIT                                                              
         MVI   HEADDATE,C'Y'       NOT THAT WE WERE COMING FROM HEADS           
         ZIC   R1,MYOLEN                                                        
         LTR   R1,R1                                                            
         BZ    NHFILT2                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),NDSPACES    IF ANYTHING IS THERE ALREADY                 
         BE    NHFILT2                                                          
         LA    R3,198(R3)             BUMP DOWN A LINE                          
         SPACE 1                                                                
NHFILT2  DS    0H                                                               
         ZIC   R2,GLARGS+15        (USE PERIOD NUMBER)                          
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         A     R2,NDADATES         (TO DISPLACE INTO DATE TABLE)                
         CLI   GLARGS+15,106       FOR 1-105,                                   
         BL    NOWEEK              USE REGULAR WEEK ROUTINES                    
         CLI   GLARGS+15,131       FOR 106-130                                  
         BL    NOMONTH             USE REGULAR MONTH ROUTINES                   
         CLI   GLARGS+15,139       FOR 131-138                                  
         BL    NOQUART             USE QUARTERS                                 
         CLI   GLARGS+15,153       FOR 139-152                                  
         BL    NODATE              USE DATE                                     
         B     NOYEAR              ELSE USE YEAR                                
         EJECT                                                                  
*              6-BYTE DEMO EXPRESSION                                           
         SPACE 3                                                                
*              INPUTS (GLARGS)     1 (R2) RELATIVE DEMO NUMBER                  
*                                  2 1=IRH/IR1 2=V1 ETC                         
         SPACE 1                                                                
NHDEMOUT DS    0H                                                               
         GOTO1 =A(XTRARTN),DMCB,(6,(RA))                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN CLIENT DETAILS                                
         SPACE 3                                                                
*              INPUT               R2 = A(4-BYTE AM/CLI CODE)                   
         SPACE 1                                                                
GETCLI   NTR1                                                                   
         CLC   NDCLILST,0(R2)      HAVE WE DONE THIS ONE BEFORE?                
         BE    XIT                    MAKE SURE WE READ CLIENT                  
         MVC   NDCLILST,0(R2)                                                   
         SPACE 1                                                                
         USING CLTHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   CKEYAM(3),NDCLILST  READ CLIENT RECORD                           
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   NDCLINAM,CNAME      GET OTHER STUFF                              
         MVC   NDCLIOFF,COFFICE                                                 
         L     R1,NDUDEFD          GET USER DEF DESCRIPTIONS                    
         USING SBLOCK,R1                                                        
         MVC   SBUP1DES,CPU1       PRODUCT DESCRIPTION                          
         MVC   SBUP1TYP,CPU1TYPE   LENGTH                                       
         MVC   SBUP1LEN,CPU1LEN    TYPE                                         
         MVC   SBUP2DES,CPU2       DESCRIPTION                                  
         MVC   SBUP2TYP,CPU2TYPE   LENGTH                                       
         MVC   SBUP2LEN,CPU2LEN    TYPE                                         
         MVC   SBUE1DES,CEU1       ESTIMATE DESCRIPTION                         
         MVC   SBUE1TYP,CEU1TYPE   LENGTH                                       
         MVC   SBUE1LEN,CEU1LEN    TYPE                                         
         MVC   SBUE2DES,CEU2       DESCRIPTION                                  
         MVC   SBUE2TYP,CEU2TYPE   LENGTH                                       
         MVC   SBUE2LEN,CEU2LEN    TYPE                                         
         DROP  R1                                                               
         SPACE 1                                                                
RESUNT   XC    FILENAME,FILENAME   RESET TO READ UNTFIL                         
         NETGO NVSETUNT,DMCB                                                    
         L     R5,NDAGBLOK                                                      
         USING NETGOALD,R5                                                      
         CLI   NGOALIOS,1          IF NETGOAL IS ACTIVE                         
         BNE   *+8                                                              
         MVI   NGOALIOS,2             TELL NETGOAL TO REESTABLISH SEQ           
         B     XIT                                                              
         DROP  R4                                                               
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO GET THE PRODUCT GROUP SCHEME                          
         SPACE 3                                                                
*                                  R2=A(AM/CLI/SCHEME)                          
         SPACE 1                                                                
GETSCM   NTR1                                                                   
         CLC   NDPRGLST(4),0(R2)   HAVE WE DONE THIS ONE BEFORE?                
         BE    XIT                                                              
         MVC   NDPRGLST(4),0(R2)                                                
         XC    KEY,KEY             GET PRODUCT GROUP DEFINITION RECORD          
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD(4),0(R2)   AM/CLI/ID                                    
         SPACE 1                                                                
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RESUNT                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   RESUNT                                                           
         USING PRGEL01,R4                                                       
         MVI   NDPRGNLV,1                                                       
         MVC   NDPRGBK1,PRGBK1    SAVE BREAK NAME                               
         MVC   NDPRGLL1,PRGBK1LN       AND LENGTH OF LEVEL 1                    
         CLI   PRGBK2,0                                                         
         BE    RESUNT                                                           
         MVI   NDPRGNLV,2                                                       
         MVC   NDPRGBK2,PRGBK2    SAVE BREAK NAME                               
         MVC   NDPRGLL2,PRGBK2LN       AND LENGTH OF LEVEL 2                    
         B     RESUNT                                                           
         EJECT                                                                  
*              ROUTINE TO GET PRODUCT GROUP NAMES                               
         SPACE 3                                                                
*              INPUT               R2 = A(8 BYTE AM/CLI/PRG CODE)               
         SPACE 1                                                                
GETPRG   NTR1                                                                   
         CLC   NDPRGLST,0(R2)      HAVE WE DONE THIS ONE BEFORE?                
         BE    XIT                                                              
         MVC   NDPRGLST,0(R2)                                                   
         CLC   4(4,R2),=C'9999'                                                 
         BNE   GETPRG2                                                          
         MVC   NDPRGNM1,NDSPACES                                                
         MVC   NDPRGNM1(11),=C'UNALLOCATED'                                     
         MVC   NDPRGNM2,NDSPACES                                                
         MVC   NDPRGNM2(11),=C'UNALLOCATED'                                     
         B     XIT                                                              
         SPACE 1                                                                
GETPRG2  MVC   WORK+10(4),4(R2)    CONVERT ID NO TO PWOS                        
         OC    WORK+10(4),=C'0000'                                              
         PACK  WORK(3),WORK+10(5)                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET GROUP RECORD                             
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD(4),NDPRGLST   (AM/CLI/ID)                               
         MVC   PRGKID+1(2),WORK                                                 
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      MATCH ON AM/CLI/ID?                          
         BNE   RESUNT                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   RESUNT                                                           
         USING PRGEL10,R4                                                       
         MVC   NDPRGNM1,PRGNAM1                                                 
         OC    NDPRGNM1,NDSPACES                                                
         MVC   NDPRGNM2,PRGNAM2                                                 
         OC    NDPRGNM2,NDSPACES                                                
         B     RESUNT                                                           
         EJECT                                                                  
*              ROUTINE TO LOOK UP PRODUCT CODE                                  
         SPACE 3                                                                
*              INPUT               BYTE HAS PRODUCT NUMBER                      
*              OUTPUT              RETURN 3 BYTE CODE IN WORK                   
         SPACE 1                                                                
LUPPROD  NTR1                                                                   
         MVC   WORK(3),=C'999'     MAKE SURE UNALLOCATED COMES LAST             
         CLI   BYTE,0                                                           
         BE    XIT                                                              
         CLI   BYTE,X'FF'                                                       
         BE    XIT                                                              
         CLI   NDSPLOPT,0          IS SPLIT BILLING ON?                         
         BE    LUP1                                                             
         L     R1,NDASPLBL                                                      
         USING SPLTBLKD,R1                                                      
         MVC   WORK(3),SPLIPRD                                                  
         B     XIT                                                              
         SPACE 1                                                                
LUP1     L     R4,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R4                                                        
         LA    R2,CLIST                                                         
         LA    R5,220                                                           
         SPACE 1                                                                
LUP2     CLC   BYTE,3(R2)                                                       
         BE    LUP4                                                             
         LA    R2,4(R2)                                                         
         BCT   R5,LUP2                                                          
         B     XIT                                                              
         SPACE 1                                                                
LUP4     MVC   WORK(3),0(R2)       3-BYTE PRODUCT                               
         B     XIT                                                              
         SPACE 1                                                                
LUPCODE  NTR1                      3 BYTE TO 1                                  
         L     R4,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R4                                                        
         LA    R2,CLIST                                                         
         LA    R5,220                                                           
         SPACE 1                                                                
LUPCODE2 CLC   WORK(3),0(R2)                                                    
         BE    LUPCODE4                                                         
         LA    R2,4(R2)                                                         
         BCT   R5,LUPCODE2                                                      
         B     XIT                                                              
         SPACE 1                                                                
LUPCODE4 MVC   BYTE,3(R2)          3-BYTE PRODUCT                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILL IN PRODUCT DETAILS                               
         SPACE 3                                                                
*              INPUT               R2 = A(6-BYTE AM/CLI/PRD CODE)               
         SPACE 1                                                                
GETPRD   NTR1                                                                   
         CLC   3(3,R2),=C'UNA'     SPECIAL FOR UNALLOCATED                      
         BE    GETPRD1                                                          
         CLC   3(3,R2),=C'999'     SPECIAL FOR UNALLOCATED                      
         BNE   GETPRD2                                                          
         SPACE 1                                                                
GETPRD1  MVC   NDPRDKEY,=C'UNA'                                                 
         MVC   NDPRDNAM,=CL20'UNALLOCATED'                                      
         MVI   NDPRDCOD,255                                                     
         MVC   NDPRDLST,0(R2)                                                   
         B     XIT                                                              
         SPACE 1                                                                
GETPRD2  CLC   NDPRDLST,0(R2)      HAVE WE DONE THIS ONE BEFORE?                
         BE    XIT                                                              
         MVC   NDPRDLST,0(R2)                                                   
         USING PRDHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   PKEYAM(6),NDPRDLST  (AM/CLI/PRODUCT)                             
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   NDPRDNAM,NDSPACES                                                
         MVI   NDPRDCOD,255                                                     
         CLC   KEYSAVE(13),KEY                                                  
         BNE   RESUNT                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   NDPRDNAM,PNAME      GET OTHER STUFF                              
         OC    NDPRDNAM,NDSPACES                                                
         MVC   NDPRDCOD,PCODE+1                                                 
         MVC   NDPRDINT,PACCT                                                   
         L     R1,NDUDEFD          GET USER DEFINITION                          
         USING SBLOCK,R1                                                        
         MVC   SBUP1FLD,PUSER1                                                  
         MVC   SBUP2FLD,PUSER2                                                  
         B     RESUNT                                                           
         DROP  R4,R1                                                            
         EJECT                                                                  
*              ROUTINE TO FILL IN ESTIMATE DETAILS                              
         SPACE 3                                                                
*              INPUT               R2 = A(EST/AM/CLI/PROD)                      
         SPACE 1                                                                
GETEST   NTR1                                                                   
         CLC   NDESTLST,0(R2)      HAVE WE DONE THIS ONE BEFORE?                
         BE    XIT                                                              
         MVC   NDESTLST,0(R2)                                                   
         USING ESTHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   EKEYAM(6),NDESTLST+1   (AM/CLI/PROD)                             
         MVC   EKEYEST,NDESTLST       (EST)                                     
         OC    EKEYPRD,EKEYPRD                                                  
         BNZ   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         CLC   EKEYPRD,=C'UNA'                                                  
         BNZ   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         SPACE 1                                                                
GTEST2   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   NDESTNAM,NDSPACES                                                
         CLC   KEYSAVE(13),KEY                                                  
         BE    GTEST4                                                           
*                                  IF NO MATCH/ TRY FOR POL                     
         CLC   KEYSAVE+4(3),=C'POL'                                             
         BE    RESUNT                                                           
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(3),=C'POL'                                                 
         B     GTEST2                                                           
         SPACE 1                                                                
GTEST4   MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   NDESTFLT,EPROF      RETURN FILTER NAME                           
         MVC   NDESTNAM,EDESC      RETURN ESTIMATE NAME                         
         OC    NDESTNAM,NDSPACES                                                
         L     R1,NDUDEFD          GET USER FIELDS                              
         USING SBLOCK,R1                                                        
         MVC   SBUE1FLD,EUSER1                                                  
         MVC   SBUE2FLD,EUSER2                                                  
         B     RESUNT                                                           
         DROP  R4,R1                                                            
         EJECT                                                                  
*              ROUTINE TO FILL IN NETWORK DETAILS                               
         SPACE 3                                                                
*              INPUT               R2 = A(4-BYTE NETWORK CODE)                  
         SPACE 1                                                                
GETNET   NTR1                                                                   
         CLC   NDNETKEY,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDNETKEY,0(R2)      SAVE CODE                                    
         XC    NDNETNAM,NDNETNAM                                                
         MVI   NDNETMED,0                                                       
         XC    NDNETMAB,NDNETMAB                                                
         L     R2,NBANBUFF         HAS A BUFFER BEEN PROVIDED                   
         LTR   R2,R2                                                            
         BZ    XIT                                                              
         LA    R2,5(R2)            FIRST ENTRY IS CLIENT                        
         USING NBUFFD,R2                                                        
         L     R3,=A(NETNAMES)                                                  
         LA    R0,199                                                           
         BAS   RE,FILNAMES         MAKE SURE WE HAVE NETWORK NAMES              
         SPACE 1                                                                
GETNET2  CLI   NBUFFNET,0          LOOK UP NETWORK BUFFER FOR MEDIA             
         BE    XIT                                                              
         CLC   NBUFFNET,NDNETKEY                                                
         BE    GETNET4                                                          
         LA    R2,L'NBUFFREC(R2)                                                
         LA    R3,24(R3)                                                        
         BCT   R0,GETNET2                                                       
         B     XIT                                                              
         SPACE 1                                                                
GETNET4  MVC   NDNETNAM,0(R3)      FOUND - PASS BACK NETWORK NAME               
         MVC   NDNETMED,NBUFFMED           PASS BACK MEDIA                      
         MVC   NDNETMAB,=C'NET  '          EXPAND - DEFAULT 'NET'               
         CLI   NDNETMED,C'O'                                                    
         BNE   *+10                                                             
         MVC   NDNETMAB,=C'OTHER'                                               
         CLI   NDNETMED,C'C'                                                    
         BNE   *+10                                                             
         MVC   NDNETMAB,=C'CABLE'                                               
         CLI   NDNETMED,C'S'                                                    
         BNE   *+10                                                             
         MVC   NDNETMAB,=C'SYND.'                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN NETWORK NAME BUFFER                           
         SPACE 3                                                                
*              INPUT               R2=A(CALL LETTER BUFFER)                     
*                                  R3=A('MARKET' NAME BUFFER)                   
*                                  R0=N'ENTRIES                                 
         SPACE 1                                                                
FILNAMES NTR1                                                                   
         CLI   0(R3),0             ONLY NEEDED FIRST TIME FOR AGENCY            
         BNE   XIT                                                              
         NETGO NVSETSTA            SET UP FOR STATION FILE                      
         MVC   FILENAME,=C'STATION '                                            
         MVI   USEIO,C'Y'                                                       
         SPACE 1                                                                
FILNN2   CLI   0(R2),0                                                          
         BE    FILNLAST                                                         
         XC    KEY,KEY             FIRST GET THE 'STATION' RECORD               
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL,0(R2)                                                   
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,NBSELAGY                                                 
         MVC   STAKCLT,=C'000'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   FILNNEXT                                                         
         MVC   0(4,R3),SMKT        PICK OUT MARKET NUMBER                       
         SPACE 1                                                                
         USING MKTREC,R4                                                        
         MVI   MKTKTYPE,C'M'       NOW GET 'MARKET'                             
         MVC   MKTKMKT,0(R3)                                                    
         MVC   MKTKAGY,NBSELAGY                                                 
         XC    MKTKFILL,MKTKFILL                                                
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   FILNNEXT                                                         
         MVC   0(24,R3),MKTNAME                                                 
         OC    0(24,R3),NDSPACES                                                
         DROP  R4                                                               
         SPACE 1                                                                
FILNNEXT LA    R2,5(R2)                                                         
         LA    R3,24(R3)                                                        
         BCT   R0,FILNN2                                                        
         SPACE 1                                                                
FILNLAST MVI   USEIO,0                                                          
         B     RESUNT                                                           
         EJECT                                                                  
*              ROUTINE TO FILL IN DAYPART DETAILS                               
         SPACE 3                                                                
*              INPUT               R2 = A(1-BYTE DAYPART CODE)                  
         SPACE 1                                                                
GETDPT   NTR1                                                                   
         CLC   NDDPTKEY,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDDPTKEY,0(R2)      SAVE CODE                                    
         EXPDP NDDPTNAM,0(R2)                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN PACKAGE DETAILS                               
         SPACE 3                                                                
*              INPUT               R2 = A(AM/CLI/NET/EST/PACKAGE)               
         SPACE 1                                                                
GETPAK   NTR1                                                                   
         CLC   NDPAKLST,0(R2)      HAVE WE DONE THIS ONE BEFORE?                
         BE    XIT                                                              
         MVC   NDPAKLST,0(R2)                                                   
         USING NPRECD,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   NPKTYPE,X'02'       READ PACKAGE RECORD                          
         MVC   NPKAM(9),NDPAKLST   (AM/CLI/NET/EST/PACKAGE)                     
         MVC   FILENAME,=C'UNTDIR  '     READ UNIT FILE                         
         GOTO1 HIGH                                                             
         MVC   NDPAKNAM,NDSPACES                                                
         CLC   KEYSAVE(20),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   NDPAKNAM(16),NPAKNAME     GET OTHER STUFF                        
         EXPDP NDPAKDET,NPAKDP           LOOK UP DAYPART                        
         OC    NDPAKNAM,NDSPACES                                                
         OC    NPAKCOST,NPAKCOST                                                
         BZ    GETPAKX                                                          
         LA    R5,NDPAKDET+9                                                    
         MVI   0(R5),C'$'                                                       
         EDIT  (4,NPAKCOST),(8,1(R5)),ALIGN=LEFT                                
         GOTO1 SQUASHER,DMCB,NDPAKDET,20                                        
         SPACE 1                                                                
GETPAKX  CLI   GLMODE,GLINPUT      IF WE ARE IN THE INPUT STAGE                 
         BNE   XIT                                                              
         MVC   FILENAME,=C'UNTDIR  '                                            
         MVC   KEY,NBKEY           REESTABLISH SEQUENCE                         
         GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO GET COMMERCIAL NAMES                                  
         SPACE 3                                                                
*              INPUT               R2=A(8 CHAR COMMERCIAL NUMBER)               
         SPACE 1                                                                
GETCOMM  NTR1                                                                   
         CLC   LASTCOKY,0(R2)                                                   
         BE    XIT                                                              
         MVC   LASTCOKY,0(R2)                                                   
         MVC   COMMNAME,NDSPACES                                                
         OC    0(8,R2),0(R2)                                                    
         BZ    XIT                                                              
         SPACE 1                                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY             COMMERCIAL NAME                              
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),NBACTAM   (AGENCY/MEDIA/CLIENT)                        
         MVC   CMLKCML,LASTCOKY                                                 
         CLC   CMLKCML,=C'REASSIGN'                                             
         BE    COMERR                                                           
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'TRFDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   CMLKCML,LASTCOKY                                                 
         BNE   COMERR                                                           
         MVC   FILENAME,=C'TRFFILE '                                            
         GOTO1 GETREC                                                           
         LA    R3,COMMNAME                                                      
         L     R4,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   RESUNT                                                           
         USING CMLDSCEL,R4                                                      
         MVC   0(24,R3),CMLDSC                                                  
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   24(24,R3),CMLDSC                                                 
         OC    0(48,R3),NDSPACES                                                
         B     RESUNT                                                           
         SPACE 1                                                                
COMERR   MVC   COMMNAME(8),=C'REASSIGN'                                         
         B     RESUNT                                                           
         EJECT                                                                  
*              TARGETS                                                          
         SPACE 3                                                                
NITARGET DS    0H                  TARGET DEMO                                  
         GOTO1 =A(XTRARTN),DMCB,(5,(RA))                                        
         B     XIT                                                              
*                                                                               
         SPACE 1                                                                
NOTARGET LA    R1,SAVETAR1                                                      
         MVC   LABLAREA(6),=C'TARGET'                                           
         CLI   GLARGS,1                                                         
         BE    NOTARG2                                                          
         LA    R1,SAVETAR2                                                      
         MVC   LABLAREA(6),=C'TARG.2'                                           
         SPACE 1                                                                
NOTARG2  MVC   0(6,R1),0(R2)       SAVE TARGET NAME                             
         CLI   0(R2),X'FF'         IS IT NAD                                    
         BNE   *+10                                                             
         MVC   0(10,R1),1(R2)      YES                                          
         CLC   MYPOSO,=X'C80101'   (NOTE PHONY HEADLINE)                        
         BE    XIT                                                              
         MVC   CODEAREA(6),0(R2)                                                
         CLI   0(R2),X'FF'         IS IT NAD                                    
         BNE   GENOUT                                                           
         MVC   CODEAREA(10),1(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
SAVETAR1 DC    C'TARGET    '                                                    
SAVETAR2 DC    C'TARG.2    '                                                    
         EJECT                                                                  
*              SHARED OUTPUT ROUTINE                                            
         SPACE 3                                                                
*              AT THIS STAGE...    LABLAREA HAS PREFIX                          
*                                  CODEAREA HAS CODE                            
*                                  NAMEAREA HAS NAME                            
         SPACE 1                                                                
GENOUT   DS    0H                                                               
         GOTO1 =A(XGENOUT),DMCB,(RA),0                                          
         B     XIT                                                              
         EJECT                                                                  
* - NIBH - BILLING HEADER ROUTINES                                              
*                                                                               
NIBHCLT  DS    0H                  CLIENT                                       
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         LA    R5,NBACTCLI                                                      
         CLC   BHSELCLI,=X'FFFF'   ARE WE READING UNITS                         
         BE    NIBHCLTX                                                         
         LA    R5,BHACTCLT                                                      
         GOTO1 NBCLUNPK,DMCB,0(R5),NBCLICOD  .SET UP FOR UNIT FUDGE             
         MVC   NBACTAM,BHACTKEY+1                                               
NIBHCLTX CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     NICLI                        .AND DO NORMAL PROCESS              
         DROP  R4                                                               
*                                                                               
NIBHPRD  DS    0H                  PRODUCT                                      
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         CLC   BHSELCLI,=X'FFFF'   ...ARE WE READING UNITS                      
         BE    NIPROD              ...YES/ SO FINISHED                          
         MVC   0(3,R3),BHACTKEY+1     FUDGE FOR UNIT READING                    
         MVC   3(3,R3),BHACTPRD                                                 
NIBHPX   DS    0H                                                               
         B     NIPROD0                                                          
         DROP  R4                                                               
*                                                                               
NIBHEST  DS    0H                  ESTIMATE                                     
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         CLC   BHSELCLI,=X'FFFF'   IF DOING UNITS                               
         BE    NIEST               GO THERE                                     
         MVC   0(1,R3),BHACTEST                                                 
         CLI   GLARGS,C'C'         IF ONLY CODE                                 
         BE    XIT                 XIT                                          
         MVC   1(3,R3),BHACTKEY+1  AM/CLI                                       
         MVC   4(3,R3),BHACTPRD    PRODUCT                                      
         B     NIEST0                                                           
         DROP  R4                                                               
*                                                                               
NIBHINV  DS    0H                  INVOICE NUMBER                               
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         CLI   GLARGS,C'M'         MEDIA  TOO                                   
         BNE   NIBHI5                                                           
         MVC   0(1,R3),BHMED                                                    
         LA    R3,1(R3)                                                         
NIBHI5   MVC   0(6,R3),BHINVNO                                                  
         OC    0(7,R3),NDSPACES                                                 
         B     XIT                                                              
         DROP  R4                                                               
NOBHINV  DS    0H                                                               
         MVC   0(7,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
NIBHCMO  DS    0H                  COMMISSION ONLY                              
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         TM    BHBSTAT,BSTCMONQ                                                 
         BNO   *+8                                                              
         MVI   0(R3),C'Y'                                                       
         B     XIT                                                              
         DROP  R4                                                               
NOBHCMO  DS    0H                                                               
         MVC   1(1,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
NIBHSTAT DS    0H                  STATUS                                       
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         TM    BHBSTAT,BSTTAORQ                                                 
         BNO   *+10                                                             
         MVC   0(3,R3),=C'AOR'                                                  
         TM    BHBSTAT,BSTCAORQ                                                 
         BNO   *+10                                                             
         MVC   0(4,R3),=C'CAOR'                                                 
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NIBHTYPE DS    0H                  BILLING TYPE                                 
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(4,R3),BHBTYPE                                                  
         B     XIT                                                              
         DROP  R4                                                               
NOBHTYPE DS    0H                                                               
         MVC   0(4,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
NIBHINSM DS    0H                  INSERTION MONTH (MONTH OF SERVICE)           
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
*        LA    R1,MNTHTBL                                                       
         LA    R1,MYMONTHS                                                      
         ZIC   R0,BHACTMSR                                                      
         BCTR  R0,0                                                             
         MH    R0,=H'3'                                                         
         AR    R1,R0                                                            
         MVC   0(3,R3),0(R1)                                                    
         B     XIT                                                              
         DROP  R4                                                               
*MNTHTBL  DC    CL36'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                      
*                                                                               
NIBHDUED DS    0H                  DUE DATE                                     
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         GOTO1 DATCON,DMCB,(3,BHDUEDT),WORK                                     
         MVC   0(4,R3),WORK                                                     
         CLI   GLARGS,C'M'         MONTH/YEAR                                   
         BE    XIT                                                              
         MVC   0(6,R3),WORK                                                     
         CLI   GLARGS,C'S'        SWITCH DATE                                   
         BE    SWITDAT                                                          
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NOBHDUED DS    0H                                                               
         MVC   0(6,R3),0(R2)                                                    
         TM    NDLOCAL,NDCCYY      CCYYMMDD OPTION                              
         BZ    XIT                                                              
         MVC   0(2,R3),=C'19'                                                   
         MVC   2(6,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
NIBHRDAT DS    0H                  RUN DATE                                     
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(6,R3),BHBDATE                                                  
         CLI   GLARGS,C'S'                                                      
         BE    SWITDAT                                                          
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NOBHRDAT DS    0H                                                               
         MVC   0(6,R3),0(R2)                                                    
         TM    NDLOCAL,NDCCYY                                                   
         BZ    XIT                                                              
         MVC   0(2,R3),=C'19'                                                   
         MVC   2(6,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
NIBHIDAT DS    0H                  INVOICE DATE                                 
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(4,R3),BHQDATE     REQUESTED PRINT DATE                         
         CLI   GLARGS,C'M'         MONTH/YEAR                                   
         BE    NIBHIDX                                                          
         MVC   0(6,R3),BHQDATE                                                  
         CLI   GLARGS,C'S'                                                      
         BE    SWITDAT                                                          
NIBHIDX  OC    0(6,R3),NDSPACES                                                 
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NOBHIDAT DS    0H                                                               
         MVC   0(6,R3),0(R2)                                                    
         TM    NDLOCAL,NDCCYY                                                   
         BZ    XIT                                                              
         MVC   0(2,R3),=C'19'                                                   
         MVC   2(6,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
NIBHPDAT DS    0H                  POSTING DATE                                 
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         GOTO1 DATCON,DMCB,(2,BHBPOST),(0,0(R3))                                
         CLI   GLARGS,C'S'                                                      
         BE    SWITDAT                                                          
         B     XIT                                                              
*                                                                               
SWITDAT  MVC   WORK(2),0(R3)       SWITCH  YYMMDD > MMDDYY                      
         MVC   0(6,R3),2(R3)                                                    
         MVC   4(2,R3),WORK                                                     
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NIBHSDAT DS    0H                  MONTH OF SERVICE MMYY                        
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   2(2,R3),BHMNSERV    BHMNSERV=YYMM                                
         MVC   0(2,R3),BHMNSERV+2                                               
         TM    NDLOCAL,NDCCYY           CCYYMMDD OPTION                         
         BZ    XIT                                                              
         MVC   0(2,R3),=C'19'           SET UP CCYYMM                           
         MVC   2(4,R3),BHMNSERV                                                 
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NIBHDPT  DS    0H                  DAYPART                                      
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(1,R3),BHDPT                                                    
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NIBHGRPK DS    0H                  PACKAGE GROUP NAME                           
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(5,R3),BHGRPKNM                                                 
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NIBHMED  DS    0H                  MEDIA                                        
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         CLC   =X'FFFF',BHSELCLI   UNIT READ                                    
         BE    NIMEDIA                                                          
         CLI   BHMED,X'40'                                                      
         BH    *+8                                                              
         MVI   BHMED,C'N'          DEFAULT IS NETWORK                           
         MVC   NBPOSTYP,BHMED      FUDGE LIKE UNIT                              
         MVC   NBSTATYP,BHMED                                                   
         B     NIMEDIA             AND PROCESS                                  
         DROP  R4                                                               
         EJECT                                                                  
* - NOT BH KEYWORD BUT USES BHBLOCKD (HOMEDEPOT BILLING REPORT)                 
NIVNDR   DS    0H                  VENDOR NAME                                  
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(24,R3),BHCLSDES                                                
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NOVNDR   DS    0H                  VENDOR NAME                                  
         MVC   0(24,R3),0(R2)                                                   
         B     XIT                                                              
* - NOT BH KEYWORD BUT USES BHBLOCKD (HOMEDEPOT BILLING REPORT)                 
NIVNDRCD DS    0H                  VENDOR CODE (CMML CLASS)                     
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(4,R3),BHVENDCD                                                 
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NOVNDRCD DS    0H                  VENDOR CODE (CMML CLASS)                     
         MVC   0(4,R3),0(R2)                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
NIBHGRS  DS    0H                  GROSS BILLING AMOUNT                         
         BAS   RE,FILTBH                                                        
         BNE   XIT                                                              
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(8,R3),BHBGRS                                                   
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NIBHNET  DS    0H                  NET BILLING AMOUNT                           
         BAS   RE,FILTBH                                                        
         BNE   XIT                                                              
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(8,R3),BHBNET                                                   
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NIBHACT  DS    0H                  ACTUAL BILLING AMOUNT                        
         BAS   RE,FILTBH                                                        
         BNE   XIT                                                              
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(8,R3),BHBACT                                                   
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NIBHAGY  DS    0H                  AGENCY COMMISSION (BHBACT-BHBNET)            
         BAS   RE,FILTBH                                                        
         BNE   XIT                                                              
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         CLI   BHAGYMD,0           IF NO BILLING/BLOCK IS 0                     
         BE    XIT                                                              
         MVC   DUB,BHBACT                                                       
         SP    DUB,BHBNET                                                       
         MVC   0(8,R3),DUB                                                      
         B     XIT                                                              
*                                                                               
FILTBH   NTR1                                                                   
         GOTO1 =A(XTRARTN),DMCB,(0,(RA))                                        
         B     XIT                                                              
*                                                                               
NIMIR    DS    0H                  MIRROR ON UNITS                              
         USING NUSDRD,R4                                                        
         MVI   ELCODE,X'02'                                                     
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLI   NUMIRTYP,0                                                       
         BE    XIT                                                              
         MVI   0(R3),C'Y'                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
NISIZE   DS    0H                                                               
         XC    KEY,KEY             FIRST GET THE 'STATION' RECORD               
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL,NBACTNET                                                
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,NBSELAGY                                                 
         MVC   STAKCLT,=C'000'                                                  
         L     R4,AIO                                                           
         CLC   KEY(9),0(R4)        DO I ALREADY HAVE IT                         
         BE    NIZ10                                                            
         NETGO NVSETSTA            SET UP FOR STATION FILE                      
         MVC   FILENAME,=C'STATION '                                            
         MVI   USEIO,C'Y'                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   RESUNT                                                           
NIZ10    MVC   0(1,R3),SSIZE       PICK OUT MARKET NUMBER                       
         B     RESUNT                                                           
*                                                                               
NOSIZE   DS    0H                                                               
         MVC   1(1,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
NIUNIQUE DS    0H                 UNIQUE ID CODE                                
**       L     R4,NBAIO                                                         
**       CLI   0(R4),X'04'                                                      
**       BNE   XIT                                                              
**       USING NUIDD,R4                                                         
**       MVI   ELCODE,5                                                         
**       BAS   RE,GETEL                                                         
**       BNE   XIT                                                              
**       EDIT  (B4,NUID),(8,0(R3))                                              
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
* - FOR CASH FLOW REPORTING/ USES BILLING OR PAYING DATA                        
NITYPE   DS    0H                                                               
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(1,R3),BHUBTYPE    USE BILL ELEM TYPE                           
         CLI   0(R3),0                                                          
         BNE   XIT                                                              
         MVC   0(1,R3),CHQTYPE     OR PAY ELEM TYPE                             
         B     XIT                                                              
         DROP  R4                                                               
* - FOR CASH FLOW REPORTING/ USES BILL INVOICE OR CHECK DATE                    
*   ACTVDAT KEYWORD USES GLARGS+11 FOR DUE DATE/RUN DATE OPTION                 
NIACTVDT DS    0H                                                               
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
* - IF BILL DATE NOT HERE                                                       
         CLI   BHQDATE,0                                                        
         BE    NIACT10                                                          
         MVC   1(6,R3),BHQDATE                                                  
         CLI   GLARGS+11,1        DUE DATE COLUMN OVERRRIDE                     
         BNE   NIACT05                                                          
         GOTO1 DATCON,DMCB,(3,BHDUEDT),(0,1(R3))                                
         B     NIACTX                                                           
NIACT05  CLI   GLARGS+11,2        RUN DATE COLUMN OVERRIDE                      
         BNE   NIACTX                                                           
         MVC   1(6,R3),BHBDATE                                                  
         B     NIACTX                                                           
* - ASSUME IT'S CHECKS READ                                                     
NIACT10  CLI   GLARGS,1            ..UNLESS ACTDATH                             
         BE    *+8                                                              
         MVI   0(R3),1             ..SORT CHECK AFTER BILLS                     
         GOTO1 DATCON,DMCB,(2,CHQDATE),1(R3)    ELSE USE CHECK DATE             
NIACTX   B     XIT                                                              
                                                                                
NOACTVDT DS    0H                                                               
         MVC   1(6,R3),1(R2)       DON'T PASS 1ST BYTE SORT CODE                
         B     XIT                                                              
                                                                                
*  - DAYS TO DISBURSEMENT                                                       
NIDISBD  L     R4,NDCIDTBL                                                      
         ICM   R1,15,CHQDAYD       GET NUMBER OF DAYS                           
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         CVD   R1,DUB                                                           
         MVC   0(8,R3),DUB                                                      
         CLI   GLARGS,C'N'         IS IS NET DOLLARS                            
         BE    NIDISBD5                                                         
         MVC   8(8,R3),CHQDALI     GROSS DAILY BALANCE                          
         ICM   R1,15,CHQGRS                                                     
         CVD   R1,DUB                                                           
         MVC   16(8,R3),DUB        GROSS CLEARED                                
         B     NIDISBDX                                                         
*                                                                               
NIDISBD5 MVC   8(8,R3),CHQDALIN    NET DAILY BALANCE                            
         ICM   R1,15,CHQNET                                                     
         CVD   R1,DUB                                                           
         MVC   16(8,R3),DUB        NET CLEARED                                  
NIDISBDX DS    0H                                                               
         B     XIT                                                              
*                                                                               
NODISBD  TM    GLINDS,GLTOTLIN     IF TOTAL                                     
         BNO   NODISB10                                                         
         CLI   GLLEVEL,0           IF GRAND TOTAL                               
         BE    XIT                     SKIP                                     
         CP    16(8,R2),=PL8'0'    IF 0 CLEARED/SKIP                            
         BE    XIT                                                              
         ZAP   WORK(16),8(8,R2)    GET DAILY BALANCE                            
         DP    WORK(16),16(8,R2)   DIVIDE BY CLEARED                            
         MVC   WORK+25(8),WORK                                                  
         LA    R2,WORK+25                                                       
*        EDIT  (P8,0(R2)),(8,0(R3)),MINUS=YES      AVG NO OF DAYS               
         MVC   0(3,R3),=C'ADD'                     AVERAGE DAYS OF D            
*        B     XIT                                                              
NODISB10 EDIT  (P8,0(R2)),(8,0(R3)),MINUS=YES                                   
         B     XIT                                                              
*                                                                               
NIDAILYB L     R4,NDCIDTBL                                                      
         MVC   0(8,R3),CHQDALI     GROSS BALANCE                                
         CLI   GLARGS,C'N'                                                      
         BNE   *+10                                                             
         MVC   0(8,R3),CHQDALIN    NET BALANCE                                  
         B     XIT                                                              
*                                                                               
NODAILYB TM    GLINDS,GLTOTLIN                                                  
         BNO   *+12                                                             
         CLI   GLLEVEL,0           SKIP AT FINAL TOTAL                          
         BE    XIT                                                              
         CP    0(8,R2),=PL8'0'                                                  
         BE    XIT                                                              
         EDIT  (P8,0(R2)),(15,0(R3)),2,MINUS=YES,ZERO=BLANK                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* - UNIT PAY REP                                                                
NIPREP   DS    0H                                                               
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   NIPREPX                                                          
         USING NUPAYD,R4                                                        
         MVC   0(3,R3),NUPAYREP                                                 
         CLI   GLARGS,C'C'         REP CODE ONLY                                
         BE    NIPREPX             YES                                          
         GOTO1 =A(GETPREPN)        NO GET NAME                                  
         CLI   GLARGS,C'N'                                                      
         BNE   *+14                                                             
         MVC   0(22,R3),WORK       NAME ONLY                                    
         B     NIPREPX                                                          
         MVC   4(22,R3),WORK       NAME AND CODE                                
NIPREPX  B     XIT                                                              
                                                                                
* - UNIT PAY REP                                                                
NOPREP   DS    0H                                                               
         MVC   0(3,R3),0(R2)                                                    
         CLI   GLARGS,C'C'         CODE ONLY                                    
         BE    NOPREPX                                                          
         CLI   GLARGS,C'N'                                                      
         BNE   *+14                                                             
         MVC   0(22,R3),0(R2)      NAME ONLY                                    
         B     NOPREPX                                                          
         MVC   0(26,R3),0(R2)      NAME AND CODE                                
NOPREPX  B     XIT                                                              
         EJECT                                                                  
NIHOURS  DS    0H                                                               
         MVC   HALF,NBAFFTIM                                                    
         OC    NBAFFTIM,NBAFFTIM                                                
         BNZ   *+10                                                             
         MVC   HALF,NBTIME                                                      
         L     R4,=A(HOROTBL)                                                   
NIR10    CLC   HALF,0(R4)                                                       
         BL    NIR20                                                            
         LA    R4,11(R4)                                                        
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BNE   NIR10                                                            
         DC    H'0'                SHOULD NEVER HAPPEN                          
NIR20    MVC   0(2,R3),2(R4)                                                    
         B     XIT                                                              
*                                                                               
NOHOURS  DS    0H                                                               
         L     R4,=A(HOROTBL)                                                   
NOR10    CLC   0(2,R2),2(R4)                                                    
         BE    NOR20                                                            
         LA    R4,11(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BNE   NOR10                                                            
         DC    H'0'                                                             
NOR20    MVC   0(7,R3),4(R4)                                                    
         B     XIT                                                              
         EJECT                                                                  
NIUDEF   DS    0H                                                               
         GOTO1 =A(XTRARTN),DMCB,(7,(RA))                                        
         B     XIT                                                              
NOUDEF   DS    0H                                                               
         GOTO1 =A(XTRARTN),DMCB,(8,(RA))                                        
         B     XIT                                                              
HUDEF    DS    0H                                                               
         GOTO1 =A(XTRARTN),DMCB,(9,(RA))                                        
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS & STORAGE                                               
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
MYMONTHS DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         SPACE 1                                                                
FAKEDEMO DS    CL3                 HOLDS A 3-BYTE DEMO CODE                     
DCMLADJ  DS    CL1                                                              
HEADDATE DC    X'00'                                                            
         SPACE 1                                                                
OUTAREA  DS    0CL59               OUTPUT AREAS                                 
LABLAREA DS    CL15                LABEL AREA                                   
         DS    CL1                                                              
CODENNAM DS    0CL43                                                            
CODEAREA DS    CL10                CODE                                         
*CODEAREA DS    CL6                 CODE                                        
         DS    CL1                                                              
NAMEAREA DS    CL32                NAME                                         
*NAMEAREA DS    CL36                NAME                                        
CGRPTSV  DS    CL12                CLIENT GROUP TITLE FOR HEADER                
         SPACE 1                                                                
MYPOSO   DS    0CL3                                                             
MYLTYP   DS    CL1                                                              
MYLINE   DS    XL1                                                              
MYCOL    DS    XL1                                                              
MYOLEN   DS    XL1                                                              
LASTCOKY DC    CL8' '                                                           
COMMNAME DC    CL48' '                                                          
ANYTOTST DS    CL1                                                              
MYRECTYP DS    CL1                 D=DETAIL S=SUB T=TOTAL                       
         SPACE 1                                                                
NDSPACES DC    CL198' '                                                         
         SPACE 1                                                                
CNKEY    DS    0CL20                COUNT KEY                                   
CNKCLT   DS    CL2                                                              
CNKPRD   DS    CL1                                                              
CNKEST   DS    CL1                                                              
CNKNET   DS    CL4                                                              
CNKDPT   DS    CL1                                                              
CNKPKG   DS    CL1                                                              
         DS    CL10                                                             
CNKEYSV  DS    CL20                COUNT KEY SAVE                               
TARGID   DS    CL8                 FOR COMMERCIAL CLASS ROUTINE                 
TARGCLS  DS    CL4                 FOR COMMERCIAL CLASS ROUTINE                 
CURRCLI  DS    CL2                 FOR COMMERCIAL CLASS ROUTINE                 
*                                                                               
         EJECT                                                                  
*              THIS IS THE END OF DRIVE(1)                                      
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*  TRANSLATE MONTH TO QUARTER                                                   
TRANTAB  DC    CL5'JANQ1'                                                       
         DC    CL5'FEBQ1'                                                       
         DC    CL5'MARQ1'                                                       
         DC    CL5'APRQ2'                                                       
         DC    CL5'MAYQ2'                                                       
         DC    CL5'JUNQ2'                                                       
         DC    CL5'JULQ3'                                                       
         DC    CL5'AUGQ3'                                                       
         DC    CL5'SEPQ3'                                                       
         DC    CL5'OCTQ4'                                                       
         DC    CL5'NOVQ4'                                                       
         DC    CL5'DECQ4'                                                       
         EJECT                                                                  
* - CONVERSION TABLE OF MILITARY TIME TO HOURS STARTING WITH 6AM                
HOROTBL  DC    AL2(0100),AL2(2500),C'12M-1AM'                                   
         DC    AL2(0200),AL2(2600),C'1-2AM  '                                   
         DC    AL2(0300),AL2(2700),C'2-3AM  '                                   
         DC    AL2(0400),AL2(2800),C'3-4AM  '                                   
         DC    AL2(0500),AL2(2900),C'4-5AM  '                                   
         DC    AL2(0600),AL2(3000),C'5-6AM  '                                   
         DC    AL2(0700),AL2(0000),C'6-7AM  '                                   
         DC    AL2(0800),AL2(0100),C'7-8AM  '                                   
         DC    AL2(0900),AL2(0200),C'8-9AM  '                                   
         DC    AL2(1000),AL2(0300),C'9-10AM '                                   
         DC    AL2(1100),AL2(0400),C'10-11AM'                                   
         DC    AL2(1200),AL2(0500),C'11-12N '                                   
         DC    AL2(1300),AL2(0600),C'12N-1PM'                                   
         DC    AL2(1400),AL2(0700),C'1-2PM  '                                   
         DC    AL2(1500),AL2(0800),C'2-3PM  '                                   
         DC    AL2(1600),AL2(0900),C'3-4PM  '                                   
         DC    AL2(1700),AL2(1000),C'4-5PM  '                                   
         DC    AL2(1800),AL2(1100),C'5-6PM  '                                   
         DC    AL2(1900),AL2(1200),C'6-7PM  '                                   
         DC    AL2(2000),AL2(1300),C'7-8PM  '                                   
         DC    AL2(2100),AL2(1400),C'8-9PM  '                                   
         DC    AL2(2200),AL2(1500),C'9-10PM '                                   
         DC    AL2(2300),AL2(1600),C'10-11PM'                                   
         DC    AL2(2400),AL2(1700),C'11-12M '                                   
         DC    X'FF'                                                            
         EJECT                                                                  
         SPACE 1                                                                
ACTIVTAB DS    0H                                                               
         DC    X'80',CL10'NEW BUY'                                              
         DC    X'40',CL10'ALLOCATION'                                           
         DC    X'20',CL10'ASS. COST'                                            
         DC    X'10',CL10'COMMENT'                                              
         DC    X'08',CL10'DEMO.'                                                
         DC    X'04',CL10'MAKEGOOD'                                             
         DC    X'02',CL10'CONVERTED'                                            
         DC    X'01',CL10'COPIED     '                                          
         SPACE 1                                                                
ACTIVTB2 DS    0H                                                               
         DC    X'80',CL10'DAY'                                                  
         DC    X'40',CL10'DATE'                                                 
         DC    X'20',CL10'TIME'                                                 
         DC    X'10',CL10'ACT. COST'                                            
         DC    X'08',CL10'PRE-EMPT'                                             
         DC    X'04',CL10'MISSED'                                               
         DC    X'02',CL10'   '                                                  
         DC    X'01',CL10'   '                                                  
*                                  DAY CONVERSSION TABLE                        
DAYCONVT EQU   *                                                                
         DC    X'7C',X'0',C'M-F'   M-F                                          
         DC    X'40',X'1',C'MON'   MON                                          
         DC    X'20',X'2',C'TUE'   TUE                                          
         DC    X'10',X'3',C'WED'   WED                                          
         DC    X'08',X'4',C'THU'   THU                                          
         DC    X'04',X'5',C'FRI'   FRI                                          
         DC    X'02',X'6',C'SAT'   SAT                                          
         DC    X'01',X'7',C'SUN'   SUN                                          
         DC    X'7F',X'8',C'M-S'   M-S                                          
         DC    X'FE',X'FE',C'   '  PHONY FOR GOALS                              
         DC    X'00',X'9',C'   '   OTHER                                        
         EJECT                                                                  
         PRINT GEN                                                              
       ++INCLUDE DENADCATS                                                      
         PRINT NOGEN                                                            
         EJECT                                                                  
ROUTLIST DS    0F                                                               
*                                  INPUT ROUTINES                               
         SPACE 1                                                                
         DC    C'NIDISBD ',A(NIDISBD)                                           
         DC    C'NODISBD ',A(NODISBD)                                           
         DC    C'NIDAILYB',A(NIDAILYB)                                          
         DC    C'NODAILYB',A(NODAILYB)                                          
         DC    C'NODLSTA ',A(NODLSTA)                                           
         DC    C'NIACCGEN',A(NIACCGEN)                                          
         DC    C'NIACTDAT',A(NIACTDAT)                                          
         DC    C'NIACTVDT',A(NIACTVDT)                                          
         DC    C'NOACTVDT',A(NOACTVDT)                                          
         DC    C'NIACTIVE',A(NIACTIVE)                                          
         DC    C'NIACTLOG',A(NIACTLOG)                                          
         DC    C'NIACTTIM',A(NIACTTIM)                                          
         DC    C'NIACTWHY',A(NIACTWHY)                                          
         DC    C'NIAG    ',A(NIAG)                                              
         DC    C'NOAG    ',A(NOAG)                                              
         DC    C'NIAGY   ',A(NIAGY)                                             
         DC    C'NIAFFTIM',A(NIAFFTIM)                                          
         DC    C'NIAFFDAT',A(NIAFFDAT)                                          
         DC    C'NIAST   ',A(NIAST)                                             
         DC    C'NIASTDTA',A(NISTDATA)                                          
         DC    C'NIBB    ',A(NIBB)                                              
         DC    C'NIBBC   ',A(NIBBC)                                             
         DC    C'NIBLEN  ',A(NIBLEN)                                            
         DC    C'NIBHIDAT',A(NIBHIDAT)                                          
         DC    C'NOBHIDAT',A(NOBHIDAT)                                          
         DC    C'NIBHRDAT',A(NIBHRDAT)                                          
         DC    C'NOBHRDAT',A(NOBHRDAT)                                          
         DC    C'NIBHDUED',A(NIBHDUED)                                          
         DC    C'NOBHDUED',A(NOBHDUED)                                          
         DC    C'NIBHPDAT',A(NIBHPDAT)                                          
         DC    C'NIBHGRS ',A(NIBHGRS)                                           
         DC    C'NIBHNET ',A(NIBHNET)                                           
         DC    C'NIBHACT ',A(NIBHACT)                                           
         DC    C'NIBHAGY ',A(NIBHAGY)                                           
         DC    C'NIBHEST ',A(NIBHEST)                                           
         DC    C'NIBHCLT ',A(NIBHCLT)                                           
         DC    C'NIBHDPT ',A(NIBHDPT)                                           
         DC    C'NIBHCTP ',A(NIBHCTP)                                           
         DC    C'NIBHMED ',A(NIBHMED)                                           
         DC    C'NIBHPRD ',A(NIBHPRD)                                           
         DC    C'NIBHPK  ',A(NIBHPK)                                            
         DC    C'NIBFRML ',A(NIBFRML)                                           
         DC    C'NOBFRML ',A(NOBFRML)                                           
         DC    C'NIBHPK  ',A(NIBHPK)                                            
         DC    C'NIBHPK  ',A(NIBHPK)                                            
         DC    C'NICHQ   ',A(NICHQ)                                             
         DC    C'NIVNDR  ',A(NIVNDR)                                            
         DC    C'NOVNDR  ',A(NOVNDR)                                            
         DC    C'NIVNDRCD',A(NIVNDRCD)                                          
         DC    C'NOVNDRCD',A(NOVNDRCD)                                          
         DC    C'NOCHQ   ',A(NOCHQ)                                             
         DC    C'NOBHPK  ',A(NOBHPK)                                            
         DC    C'NIBHPKNM',A(NIBHPKNM)                                          
         DC    C'NIBHSDAT',A(NIBHSDAT)                                          
         DC    C'NIBHINV ',A(NIBHINV)                                           
         DC    C'NIBHCMO ',A(NIBHCMO)                                           
         DC    C'NIBHTYPE',A(NIBHTYPE)                                          
         DC    C'NIBHSTAT',A(NIBHSTAT)                                          
         DC    C'NIBHINSM',A(NIBHINSM)                                          
         DC    C'NIBRP   ',A(NIBRP)                                             
         DC    C'NIBHGRPK',A(NIBHGRPK)                                          
         DC    C'NOBHCMO ',A(NOBHCMO)                                           
         DC    C'NOBHINV ',A(NOBHINV)                                           
         DC    C'NOBHTYPE',A(NOBHTYPE)                                          
         DC    C'NICALTIM',A(NICALTIM)                                          
         DC    C'NICCLASS',A(NICCLASS)                                          
         DC    C'NICLI   ',A(NICLI)                                             
         DC    C'NIUNQID ',A(NIUNQID)                                           
         DC    C'NICLIINT',A(NICLIINT)                                          
         DC    C'NOCLIINT',A(NOCLIINT)                                          
         DC    C'NICMUT  ',A(NICMUT)                                            
         DC    C'NICLTGRP',A(NICLTGRP)                                          
         DC    C'NICOMMNT',A(NICOMMNT)                                          
         DC    C'NICOMMLA',A(NICOMMLA)                                          
         DC    C'NICOMMLS',A(NICOMMLS)                                          
         DC    C'NICOMP  ',A(NICOMP)                                            
         DC    C'NICOST  ',A(NICOST)                                            
         DC    C'NICPCT  ',A(NICPCT)                                            
         DC    C'NICPM   ',A(NICPM)                                             
         DC    C'NICPP   ',A(NICPP)                                             
         DC    C'NIGCPP  ',A(NIGCPP)                                            
         DC    C'NICPU   ',A(NICPU)                                             
         DC    C'NICSH   ',A(NICSH)                                             
         DC    C'NOCSH   ',A(NOCSH)                                             
         DC    C'NICSHSQN',A(NICSHSQN)                                          
         DC    C'NIDATE  ',A(NIDATE)                                            
         DC    C'NIDLEVEL',A(NIDLEVEL)                                          
         DC    C'NIDLGAP ',A(NIDLGAP)                                           
         DC    C'NIDLTIME',A(NIDLTIME)                                          
         DC    C'NIDLTYPO',A(NIDLTYPO)                                          
         DC    C'NIDLTYPA',A(NIDLTYPA)                                          
         DC    C'NIDATNUM',A(NIDATNUM)                                          
         DC    C'NIDAY   ',A(NIDAY)                                             
         DC    C'NIDPT   ',A(NIDPT)                                             
         DC    C'NIDT2   ',A(NIDT2)                                             
         DC    C'NIHOURS ',A(NIHOURS)                                           
         DC    C'NOHOURS ',A(NOHOURS)                                           
         DC    C'NIPCOM  ',A(NIPCOM)                                            
         DC    C'NISD    ',A(NISD)                                              
         DC    C'NISDNAM ',A(NISDNAM)                                           
         DC    C'NITYPE  ',A(NITYPE)                                            
         DC    C'NIEA    ',A(NIEA)                                              
         DC    C'NIEST   ',A(NIEST)                                             
         DC    C'NIFEED  ',A(NIFEED)                                            
         DC    C'NIFEEDMG',A(NIFEEDMG)                                          
         DC    C'NIGCOST ',A(NIGCOST)                                           
         DC    C'NIGEA   ',A(NIGEA)                                             
         DC    C'NIGEADOL',A(NIGEADOL)                                          
         DC    C'NIGGRP  ',A(NIGGRP)                                            
         DC    C'NIGRP   ',A(NIGRP)                                             
         DC    C'NIHICOMM',A(NIHICOMM)                                          
         DC    C'NIHUT   ',A(NIHUT)                                             
         DC    C'NIHUTAVE',A(NIHUTAVE)                                          
         DC    C'NIHUTPCT',A(NIHUTPCT)                                          
         DC    C'NIIMP   ',A(NIIMP)                                             
         DC    C'NIINV   ',A(NIINV)                                             
         DC    C'NIIRH   ',A(NIIRH)                                             
         DC    C'NIIX    ',A(NIIX)                                              
         DC    C'NIIMPACT',A(NIIMPACT)                                          
         DC    C'NIKID   ',A(NIKID)                                             
         DC    C'NILEN   ',A(NILEN)                                             
         DC    C'NIMARKET',A(NIMARKET)                                          
         DC    C'NIMEDIA ',A(NIMEDIA)                                           
         DC    C'NIMIR   ',A(NIMIR)                                             
         DC    C'NIMONTHA',A(NIMONTHA)                                          
         DC    C'NIMONTH ',A(NIMONTH)                                           
         DC    C'NIMPROD ',A(NIMPROD)                                           
         DC    C'NOMPROD ',A(NOMPROD)                                           
         DC    C'NIMQNYR ',A(NIMQNYR)                                           
         DC    C'NINET   ',A(NINET)                                             
         DC    C'NINETST ',A(NINETST)                                           
         DC    C'NINTI   ',A(NINTI)                                             
         DC    C'NONTI   ',A(NONTI)                                             
         DC    C'NINSI   ',A(NINSI)                                             
         DC    C'NIOFFICE',A(NIOFFICE)                                          
         DC    C'NIOTHER ',A(NIOTHER)                                           
         DC    C'NIPACK  ',A(NIPACK)                                            
         DC    C'NIPKGN2 ',A(NIPKGN2)                                           
         DC    C'NIPKG   ',A(NIPKG)                                             
         DC    C'NIPACKST',A(NIPACKST)                                          
         DC    C'NIPBUDG ',A(NIPBUDG)                                           
         DC    C'NIPCPM  ',A(NIPCPM)                                            
         DC    C'NIPCPMG ',A(NIPCPMG)                                           
         DC    C'NIPCPP  ',A(NIPCPP)                                            
         DC    C'NIPCPU  ',A(NIPCPU)                                            
         DC    C'NIPDAY  ',A(NIPDAY)                                            
         DC    C'NIPDEMA ',A(NIPDEMA)                                           
         DC    C'NIPFILT ',A(NIPFILT)                                           
         DC    C'NIPGRP  ',A(NIPGRP)                                            
         DC    C'NIPHUT  ',A(NIPHUT)                                            
         DC    C'NIPIMP  ',A(NIPIMP)                                            
         DC    C'NIPINT  ',A(NIPINT)                                            
         DC    C'NIPIPU  ',A(NIPIPU)                                            
         DC    C'NIPLFLT ',A(NIPLFLT)                                           
         DC    C'NIPOST  ',A(NIPOST)                                            
         DC    C'NIPPKGA ',A(NIPPKGA)                                           
         DC    C'NIPPLAN ',A(NIPPLAN)                                           
         DC    C'NIPPROG ',A(NIPPROG)                                           
         DC    C'NIPROD  ',A(NIPROD)                                            
         DC    C'NIPRODC ',A(NIPRODC)                                           
         DC    C'NOPRODC ',A(NOPRODC)                                           
         DC    C'NIPRDGRP',A(NIPRDGRP)                                          
         DC    C'NIPRDPCT',A(NIPRDPCT)                                          
         DC    C'NIPREP  ',A(NIPREP)                                            
         DC    C'NIPRFILT',A(NIPRFILT)                                          
         DC    C'NIPROG  ',A(NIPROG)                                            
         DC    C'NIPRPU  ',A(NIPRPU)                                            
         DC    C'NIPRTG  ',A(NIPRTG)                                            
         DC    C'NIPSHR  ',A(NIPSHR)                                            
         DC    C'NIPUNIT ',A(NIPUNIT)                                           
         DC    C'NIPVPH  ',A(NIPVPH)                                            
         DC    C'NIRATYPE',A(NIRATYPE)                                          
         DC    C'NIRESULT',A(NIRESULT)                                          
         DC    C'NIROTATE',A(NIROTATE)                                          
         DC    C'NIROTATN',A(NIROTATN)                                          
         DC    C'NIRPU   ',A(NIRPU)                                             
         DC    C'NIRTG   ',A(NIRTG)                                             
         DC    C'NIRX    ',A(NIRX)                                              
         DC    C'NIQUART ',A(NIQUART)                                           
         DC    C'NISCHED ',A(NISCHED)                                           
         DC    C'NISHR   ',A(NISHR)                                             
         DC    C'NISIZE  ',A(NISIZE)                                            
         DC    C'NISL    ',A(NISL)                                              
         DC    C'NISBPCT ',A(NISBPCT)                                           
         DC    C'NISREP  ',A(NISREP)                                            
         DC    C'NISTACK ',A(NISTACK)                                           
         DC    C'NISTATUS',A(NISTATUS)                                          
         DC    C'NISTDATA',A(NISTDATA)                                          
         DC    C'NISTDOL ',A(NISTDOL)                                           
         DC    C'NISTI   ',A(NISTI)                                             
         DC    C'NISTR   ',A(NISTR)                                             
         DC    C'NISUNIQ ',A(NISUNIQ)                                           
         DC    C'NITFEED ',A(NITFEED)                                           
         DC    C'NITFEEDN',A(NITFEEDN)                                          
         DC    C'NITFEET ',A(NITFEET)                                           
         DC    C'NITARGET',A(NITARGET)                                          
         DC    C'NITOTSTK',A(NISTACK)                                           
         DC    C'NIUDEF  ',A(NIUDEF)                                            
         DC    C'NIUNCODE',A(NIUNCODE)                                          
         DC    C'NIUNIT  ',A(NIUNIT)                                            
         DC    C'NIUCNT  ',A(NIUCNT)                                            
         DC    C'NOUCNT  ',A(NOUCNT)                                            
         DC    C'NIUNITST',A(NIUNITST)                                          
         DC    C'NIUNIQUE',A(NIUNIQUE)                                          
         DC    C'NICSTAT ',A(NICSTAT)                                           
         DC    C'NIUNIV  ',A(NIUNIV)                                            
         DC    C'NIV     ',A(NIV)                                               
         DC    C'NIVPH   ',A(NIVPH)                                             
         DC    C'NIWEEK  ',A(NIWEEK)                                            
         DC    C'NIPROGN ',A(NIPROGN)                                           
         SPACE 1                                                                
*                                  HEADER ROUTINES                              
         SPACE 1                                                                
         DC    C'NHDEMOUT',A(NHDEMOUT)                                          
         DC    C'NHFILTER',A(NHFILTER)                                          
         DC    C'NHPERCNT',A(NHPERCNT)                                          
         DC    C'NHPRDGRP',A(NHPRDGRP)                                          
         DC    C'NHCLTGRP',A(NHCLTGRP)                                          
         DC    C'NHUNIT  ',A(NHUNIT)                                            
         DC    C'NHTIME2 ',A(NHTIME2)                                           
         DC    C'HUDEF   ',A(HUDEF)                                             
         SPACE 1                                                                
*                                  OUTPUT ROUTINES                              
         SPACE 1                                                                
         DC    C'NOACTDAT',A(NOACTDAT)                                          
         DC    C'NOACTIVE',A(NOACTIVE)                                          
         DC    C'NOACTLOG',A(NOACTLOG)                                          
         DC    C'NOAST   ',A(NOAST)                                             
         DC    C'NOASTDTA',A(NOASTDTA)                                          
         DC    C'NOCLI   ',A(NOCLI)                                             
         DC    C'NOCLIPAG',A(NOCLIPAG)                                          
         DC    C'NOBRP   ',A(NOBRP)                                             
         DC    C'NOCLTGRP',A(NOCLTGRP)                                          
         DC    C'NOCOMM  ',A(NOCOMM)                                            
         DC    C'NOCOMMNT',A(NOCOMMNT)                                          
         DC    C'NOCOST  ',A(NOCOST)                                            
         DC    C'NODLCST ',A(NODLCST)                                           
         DC    C'NOCPM   ',A(NOCPM)                                             
         DC    C'NOCPP   ',A(NOCPP)                                             
         DC    C'NOGCPP  ',A(NOGCPP)                                            
         DC    C'NOCPU   ',A(NOCPU)                                             
         DC    C'NODATE  ',A(NODATE)                                            
         DC    C'NODATOUT',A(NODATOUT)                                          
         DC    C'NODATN  ',A(NODATN)                                            
         DC    C'NODAY   ',A(NODAY)                                             
         DC    C'NODAYN  ',A(NODAYN)                                            
         DC    C'NODLGAP ',A(NODLGAP)                                           
         DC    C'NODPT   ',A(NODPT)                                             
         DC    C'NODT2   ',A(NODT2)                                             
         DC    C'NODATNUM',A(NODATNUM)                                          
         DC    C'NODTOT  ',A(NODTOT)                                            
         DC    C'NOEA    ',A(NOEA)                                              
         DC    C'NOEST   ',A(NOEST)                                             
         DC    C'NOGEA   ',A(NOGEA)                                             
         DC    C'NOGEN   ',A(NOGEN)                                             
         DC    C'NOGEADOL',A(NOGEADOL)                                          
         DC    C'NOGRP   ',A(NOGRP)                                             
         DC    C'NOHICOMM',A(NOHICOMM)                                          
         DC    C'NOHUT   ',A(NOHUT)                                             
         DC    C'NOIMP   ',A(NOIMP)                                             
         DC    C'NOINV   ',A(NOINV)                                             
         DC    C'NOIPU   ',A(NOIPU)                                             
         DC    C'NOIRH   ',A(NOIRH)                                             
         DC    C'NOIX    ',A(NOIX)                                              
         DC    C'NOMEDIA ',A(NOMEDIA)                                           
         DC    C'NOMONTH ',A(NOMONTH)                                           
         DC    C'NONET   ',A(NONET)                                             
         DC    C'NOOFFICE',A(NOOFFICE)                                          
         DC    C'NOPACK  ',A(NOPACK)                                            
         DC    C'NOPCOST ',A(NOPCOST)                                           
         DC    C'NOPCPMG ',A(NOPCPMG)                                           
         DC    C'NOPPLAN ',A(NOPPLAN)                                           
         DC    C'NOPRDGRP',A(NOPRDGRP)                                          
         DC    C'NOPREP  ',A(NOPREP)                                            
         DC    C'NOPROD  ',A(NOPROD)                                            
         DC    C'NOPROG  ',A(NOPROG)                                            
         DC    C'NOREPORT',A(NOREPORT)                                          
         DC    C'NOQUART ',A(NOQUART)                                           
         DC    C'NORPU   ',A(NORPU)                                             
         DC    C'NORTG   ',A(NORTG)                                             
         DC    C'NORX    ',A(NORX)                                              
         DC    C'NOSCHED ',A(NOSCHED)                                           
         DC    C'NOSD    ',A(NOSD)                                              
         DC    C'NOSDNM  ',A(NOSDNM)                                            
         DC    C'NOSHR   ',A(NOSHR)                                             
         DC    C'NOSIZE  ',A(NOSIZE)                                            
         DC    C'NOSL    ',A(NOSL)                                              
         DC    C'NOSLN   ',A(NOSLN)                                             
         DC    C'NOSPECP ',A(NOSPECP)                                           
         DC    C'NOSPLONG',A(NOSPLONG)                                          
         DC    C'NOSTACK ',A(NOSTACK)                                           
         DC    C'NOSTDATA',A(NOSTDATA)                                          
         DC    C'NOSTDOL ',A(NOSTDOL)                                           
         DC    C'NOSTI   ',A(NOSTI)                                             
         DC    C'NOSTR   ',A(NOSTR)                                             
         DC    C'NOTARGET',A(NOTARGET)                                          
         DC    C'NOTIME  ',A(NOTIME)                                            
         DC    C'NOTIMEN ',A(NOTIMEN)                                           
         DC    C'NOTFEED ',A(NOTFEED)                                           
         DC    C'NOTFEET ',A(NOTFEET)                                           
         DC    C'NOTOTSTK',A(NOTOTSTK)                                          
         DC    C'NOUDEF  ',A(NOUDEF)                                            
         DC    C'NOUNIT  ',A(NOUNIT)                                            
         DC    C'NOV     ',A(NOV)                                               
         DC    C'NOVPH   ',A(NOVPH)                                             
         DC    C'NOWEEK  ',A(NOWEEK)                                            
         SPACE 1                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
* - UNIT PAY REP                                                                
GETPREPN NTR1  BASE=*,LABEL=*                                                   
         NETGO NVSETSTA            SET UP FOR STATION FILE                      
         MVC   FILENAME,=C'STATION '                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY             FIRST GET THE 'STATION' RECORD               
         LA    R4,KEY                                                           
         USING REPREC,R4                                                        
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,0(R3)                                                    
         MVC   REPKAGY,NBSELAGY                                                 
         MVC   REPKFILL,=8C'0'                                                  
         GOTO1 HIGH                                                             
         L     R4,AIO                                                           
         CLC   0(15,R4),KEYSAVE                                                 
         BNE   NOPREPNX                                                         
         MVC   WORK(22),RNAME       GET REP NAME                                
         NETGO NVSETUNT                                                         
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         MVI   NBFUNCT,NBFRDHI                                                  
NOPREPNX XIT1                                                                   
         DROP  R4                                                               
         DROP  RB,R8,R7,R6                                                      
*                                                                               
         EJECT                                                                  
* - COMMON OUTPUT ROUTINE FOR HEADLINES/LABELS                                  
*                                                                               
XGENOUT  NMOD1 0,**NEXG**,R8                                                    
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,=A(OUTAREA)      ADDRESS OF HEADS ETC WORKAREA                
         USING OUTAREA,R7                                                       
                                                                                
         OC    OUTAREA,NDSPACES    ENSURE PRINTABLE                             
         TM    NDDOWNL,GLDLHEAD    ,,IF DOWNLOADING HEAD LINES                  
         BNO   *+10                                                             
         MVC   LABLAREA,NDSPACES   ,,CLEAR LABLAREA                             
         CLI   4(R1),C'T'          TOTOUT REQUEST                               
         BE    TOTOUT                                                           
         TM    GLINDS,X'40'        TOTALS ARE DEALT WITH BELOW                  
         BO    TOTOUT                                                           
         CLI   MYLTYP,C'H'         FOR HEADLINES,                               
         BNE   GENOUT2                                                          
         CLI   MYCOL,90                                                         
         BL    GENOUT1                                                          
         GOTO1 SQUASHER,DMCB,OUTAREA,59                                         
         MVC   0(33,R3),OUTAREA           COMPRESSED IF ON THE RIGHT            
         B     XGENX                                                            
         SPACE 1                                                                
GENOUT1  MVC   0(L'OUTAREA,R3),OUTAREA    MOVE THE LOT, IF LEFT                 
         B     XGENX                                                            
         SPACE 1                                                                
GENOUT2  GOTO1 SQUASHER,DMCB,CODENNAM,43                                        
         CLI   MYLTYP,C'M'         FOR MIDLINES, SQUASH FIRST                   
         BNE   GENOUT4                                                          
         MVC   0(L'CODENNAM,R3),CODENNAM                                        
         B     XGENX                                                            
         SPACE 1                                                                
GENOUT4  LA    R1,CODENNAM         SET UP TO CHOP THE NAME & CODE               
         ST    R1,DMCB             A(INPUT)                                     
         MVI   DMCB,43             L'INPUT                                      
         ST    R3,DMCB+4           A(OUTPUT)                                    
         ZIC   R1,MYOLEN                                                        
         STC   R1,DMCB+4           L'OUTPUT                                     
         LA    R1,4                MAX N'LINES                                  
         TM    GLDOWNLD,X'80'                                                   
         BNO   *+8                                                              
         LA    R1,1                ONLY 1 FOR DOWNLOADING                       
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198          PRINT LINES ARE 198 APART                    
         GOTO1 CHOPPER,DMCB                                                     
XGENX    DS    0H                                                               
         MVC   OUTAREA,NDSPACES                                                 
         XIT1                                                                   
         EJECT                                                                  
*              SHARED TOTAL ROUTINE                                             
         SPACE 3                                                                
*              AT THIS STAGE...    LABLAREA HAS PREFIX                          
*                                  CODEAREA HAS CODE                            
*                                  NAMEAREA HAS NAME                            
*              INPUT               NDROW1WD NDROWWID                            
         SPACE 1                                                                
TOTOUT   LA    R2,NDWDR1           R2=L'ROW1 AND L'ALLROWS                      
         CLI   GLRECNO,1                                                        
         BE    *+8                                                              
         LA    R2,NDWRR1                                                        
         ZIC   R1,GLRECNO          PICK UP PRESENT RECORD NUMBER                
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,GLAINTD(R1)      GET TO DETAILS FOR THIS REPORT               
         L     R1,0(R1)                                                         
         USING GLINTD,R1                                                        
         LH    R4,GLPDISP          NOW PICK UP PRINT DISPLACEMENT               
         A     R4,GLAINTP1         AND GET ACTUAL PRINT ADDRESS                 
         LA    R4,1(R4)                                                         
         DROP  R1                                                               
         SPACE 1                                                                
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,CLEARP1                                                       
         EX    R1,CLEARP2                                                       
         EX    R1,CLEARP3                                                       
         CLI   1(R2),3                                                          
         BL    TOTOUTX                                                          
         MVC   0(3,R3),=C'ALL'                                                  
         CLI   1(R2),5                                                          
         BL    TOTOUTX                                                          
         MVC   0(5,R3),=C'*ALL*'                                                
         CLI   1(R2),8                                                          
         BL    TOTOUTX                                                          
         MVC   0(8,R3),=C'*TOTALS*'                                             
         CLI   1(R2),16                                                         
         BL    TOTOUTX                                                          
         MVC   0(8,R3),NDSPACES                                                 
         LR    R3,R4                                                            
         SPACE 1                                                                
         MVC   BLOCK(80),NDSPACES                                               
         MVC   BLOCK(10),=C'TOTALS FOR'                                         
         MVC   BLOCK+12(59),OUTAREA                                             
         GOTO1 SQUASHER,DMCB,BLOCK,80                                           
         MVI   ANYTOTST,C'N'                                                    
         ZIC   R4,1(R2)            IF WIDTH OF ALL ROWS IS OVER 20              
         BCTR  R4,0                                                             
******   CLI   1(R2),21                                                         
******   BL    TOTOUT2                                                          
******   LA    R3,4(R3)                                                         
******   SH    R4,=H'4'            INDENT TOTALS BY 4                           
         CLI   NDFLAVOR,C'E'       IF THIS IS NOT ESTIMATES                     
         BE    TOTOUT2                                                          
         TM    NDCOLIND,X'40'      AND TOTSTACK WAS NOT REQUESTED               
         BO    TOTOUT2                                                          
         L     R1,=A(MYDEFLST)                                                  
         CLI   1(R1),0             AND THERE IS MORE THAN 1 LINE                
         BE    TOTOUT2                                                          
         CH    R4,=H'24'           AND WE HAVE ROOM TO SPARE                    
         BL    TOTOUT2                                                          
         MVI   ANYTOTST,C'Y'       SET TOTAL STACK PENDING                      
         SH    R4,=H'9'            AND ALLOW FOR THIS IN THE WIDTH              
         SPACE 1                                                                
TOTOUT2  BCTR  R4,0                                                             
         LA    R1,4                                                             
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198                                                       
         GOTO1 CHOPPER,DMCB,(80,BLOCK),((R4),0(R3))                             
         CLI   ANYTOTST,C'Y'                                                    
         BNE   TOTOUTX                                                          
         AR    R3,R4                                                            
         LA    R3,2(R3)                                                         
         BAS   RE,TOTSTACK                                                      
         SPACE 1                                                                
TOTOUTX  MVC   OUTAREA,NDSPACES                                                 
*                                                                               
*        PRINT AVERAGE DAYS TO DISBURSEMENT IF NEEDED                           
*                                                                               
OTOTADD  DS    0H                                                               
*                                                                               
         LR    R0,R4               SAVE LENGTH                                  
*                                                                               
         L     R4,=A(NOAGADD)      POINT TO AVG DAYS TO DISBURSE                
*                                                                               
         OC    0(8,R4),0(R4)       SKIP IF NOTHING PRESENT                      
         BZ    OTOTADDX                                                         
*                                                                               
         ZIC   RF,NDWDR1+1         TOTAL ROW WIDTH                              
*                                                                               
         CH    RF,=Y(L'NOAGTXTS+6)  MINIMUM POSITIONS NEEDED                    
         BL    OTOTADDX                                                         
*                                                                               
*        FIND A SPARE LINE FOR PRINTING                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         LA    R0,20               CHECK MAX 20 LINES                           
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),NDSPACES    LOOK FOR EMPTY LINE                          
         BNH   *+16                                                             
         LA    R3,198(R3)          BUMP TO NEXT LINE                            
         BCT   R0,*-22                                                          
         B     OTOTADDX            NO ROOM LEFT                                 
*                                                                               
         CH    RF,=Y(L'NOAGTXTL-1+7)  IF ROOM FOR LONG TEXT                     
         BL    *+22                                                             
         L     R1,=A(NOAGTXTL)                                                  
         MVC   0(L'NOAGTXTL,R3),0(R1)       USE IT                              
         LA    RE,L'NOAGTXTL(R3)                                                
         B     *+18                                                             
         L     R1,=A(NOAGTXTS)                                                  
         MVC   0(L'NOAGTXTS,R3),0(R1)     ELSE USE SHORT TEXT                   
         LA    RE,L'NOAGTXTS(R3)                                                
*                                                                               
*        PRINT AVERAGE DAYS TO DISBURSEMENT                                     
*                                                                               
         LR    R2,RE                                                            
*                                                                               
         EDIT  (P8,(R4)),(6,(R2)),1,FLOAT=-,ZERO=NOBLANK                        
*                                                                               
OTOTADDX DS    0H                                                               
*                                                                               
         XC    0(8,R4),0(R4)       RESET DAYS FIELD                             
         LR    R4,R0               RESTORE LENGTH                               
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
CLEARP1  MVC   000(0,R4),NDSPACES                                               
CLEARP2  MVC   198(0,R4),NDSPACES                                               
CLEARP3  MVC   396(0,R4),NDSPACES                                               
         SPACE 1                                                                
NOTOTSTK BAS   RE,TOTSTACK                                                      
         B     XIT                                                              
         EJECT                                                                  
*              TOTAL DEFINITION STACK                                           
         SPACE 3                                                                
TOTSTACK NTR1                                                                   
         L     R2,=A(MYDEFLST)     SET UP TO HANDLE LIST                        
         LA    R0,8                                                             
         SPACE 1                                                                
TOTSTLST MVC   BYTE,0(R2)                                                       
         CLI   BYTE,X'E2'                                                       
         BE    TOTST1A                                                          
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,2                                                           
         BL    TOTST1                                                           
         BE    TOTST2                                                           
         CLI   BYTE,4                                                           
         BL    TOTST3                                                           
         BE    TOTST4                                                           
         B     TOTST5                                                           
         SPACE 1                                                                
TOTST1   MVC   0(3,R3),=C'RAW'                                                  
         TM    0(R2),X'80'                                                      
         BNO   TOTSTNXT                                                         
TOTST1A  MVC   0(7,R3),=C'BASE 30'                                              
         CLI   NDQBASE,60                                                       
         BNE   *+8                                                              
         MVI   5(R3),C'6'                                                       
         B     TOTSTNXT                                                         
         SPACE 1                                                                
TOTST2   MVC   0(7,R3),=C'CPP/CPM'                                              
         B     TOTSTNXT                                                         
         SPACE 1                                                                
TOTST3   DS    0H                  POST INDEX=3                                 
         MVC   0(7,R3),=C'EST/ACT'                                              
         B     TOTSTNXT                                                         
         SPACE 1                                                                
TOTST4   DS    0H                  RAW V EQUIV INDEX =4                         
         MVC   0(6,R3),=C'RAW/30'                                               
         CLI   NDQBASE,60                                                       
         BNE   *+8                                                              
         MVI   5(R3),C'6'                                                       
         B     TOTSTNXT                                                         
         SPACE 1                                                                
TOTST5   MVI   0(R3),0             SPACE=5                                      
         B     TOTSTNXT                                                         
         SPACE 1                                                                
TOTSTNXT LA    R3,198(R3)                                                       
         MVC   DUB,0(R3)                                                        
         MVC   DUB+6(6),NDSPACES                                                
         CLC   0(6,R3),NDSPACES                                                 
         BNE   TOTSTNXT                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         BCT   R0,TOTSTLST                                                      
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,R8,R7                                                         
*                                                                               
* OVERFLOW ROUTINES                                                             
XTRARTN  NMOD1 0,**NEXF**,R8,R7                                                 
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         CLI   NBMODE,NBPROCPK                                                  
         BE    XXIT                EXIT / ROUTINES NEED BILL/UNIT REC           
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     ROUTINE(RF)                                                      
*                                                                               
ROUTINE  DS    0H                                                               
         B     XFILTBH          0  FILTER BILL HEADER READ RECORDS              
         B     XNTIPROG         1  NTI PROGRAM NAME                             
         B     XHICOM           2  HIGH COMMENTS                                
         B     XNIBRP           3  BRP                                          
         B     XNOBRP           4  BRP                                          
         B     XITARGET         5  TARGET DEMO                                  
         B     XDEMOUT          6  DEMOUT                                       
         B     XIUDEF           7  USER DEF INPUT                               
         B     XOUDEF           8  USER DEF OUTPUT                              
         B     XHUDEF           9  USER DEF HEADLINES                           
         B     XNIINV           10 USER DEF HEADLINES                           
*                                                                               
XXIT     XIT1                                                                   
         EJECT                                                                  
*          BILL HEADER READ FILTER                                              
XFILTBH  DS    0H                                                               
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         CLI   GLARGS+13,0         AOR/NOAR FILTER                              
         BE    FB20                                                             
         CLI   GLARGS+13,X'80'     ONLY AOR                                     
         BNE   FB10                                                             
         TM    BHBSTAT,X'20'       IS IT TRUE AOR                               
         BO    FB20                                                             
         B     FBNO                                                             
FB10     TM    BHBSTAT,X'20'       EXCLUDE AOR                                  
         BO    FBNO                                                             
*                                                                               
FB20     CLI   GLARGS+15,0         PERIOD FILTER                                
         BE    FB30                                                             
         ZIC   R1,GLARGS+15        (USE PERIOD NUMBER)                          
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,NDADATES         (TO DISPLACE INTO DATE TABLE)                
         CLC   BHMNSVX,0(R1)      (CHECK START)                                 
         BL    FBNO                                                             
         CLC   BHMNSVX,2(R1)      (CHECK END)                                   
         BH    FBNO                                                             
         SPACE 1                                                                
FB30     CLI   GLARGS,C'$'         CHECK COST TYPE FILTER (TIME ETC)            
         BNE   FB40                NO                                           
         CLC   BHCTYP,GLARGS+1     YES/IS IT A MATCH                            
         BE    FB40                                                             
         CLI   GLARGS+1,X'FF'      .IF ACCEPTING ONLY SPECIALS                  
         BNE   FBNO                                                             
         CLI   BHCTYP,0            .REJECT OLD BILLS                            
         BE    FBNO                                                             
         CLI   BHCTYP,C'T'         .REJECT TIME                                 
         BE    FBNO                                                             
         CLI   BHCTYP,C'I'         .REJECT INTEGRATION                          
         BE    FBNO                                                             
         SPACE 1                                                                
FB40     DS    0H                                                               
         B     FBYES                                                            
         SPACE 1                                                                
FBYES    DS    0H                                                               
         SR    R1,R1                                                            
         B     FB02                                                             
         SPACE 1                                                                
FBNO     DS    0H                                                               
         LA    R1,1                                                             
FB02     LTR   R1,R1                                                            
         B     XXIT                                                             
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
XNTIPROG DS    0H                                                               
         MVC   DMCB+4(4),=X'D9000A17'          GET VNETWEEK                     
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VNETWEEK,DMCB                                                    
         SPACE                                                                  
         MVC   DMCB+4(4),=X'D9000A26'          GET VDEFINE                      
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEFINE,DMCB                                                     
         SPACE                                                                  
         L     RE,ACOMFACS                     GET VDEMAND                      
         USING COMFACSD,RE                                                      
         MVC   VDEMAND,CDEMAND                                                  
*                                                                               
         L     R4,NDADBLCK                                                      
         USING DBLOCK,R4                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'NTI'                                                   
         L     RE,=A(DEMANDIO)                                                  
         STCM  RE,15,DBAREC                 * PASS MY I/O AREA                  
         MVI   DBSELDUR,X'FF'      RETURN ALL TIME DURATION                     
**       CLI   ASCRIBD,C'Y'                                                     
**       BNE   *+8                                                              
**       MVI   DBBTYPE,C'A'       ASCRIBED DATA                                 
         MVC   DBSELAGY,NBSELAGY                                                
         MVC   WORK(2),NBSDAFDT           USE AFFID DATE IF THERE               
         OC    WORK(2),WORK                                                     
         BNZ   *+10                                                             
         MVC   WORK(2),NBACTDAT                                                 
         GOTO1 DATCON,DMCB,(2,WORK),(0,WORK+2)                                  
         GOTO1 VNETWEEK,DMCB,WORK+2,GETDAY,ADDAY                                
         MVC   DBSELBK,DMCB+4                                                   
         MVC   DBSELBK+1(1),DMCB+8     PER ZEN OCT14                            
*                                                                               
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBFUNCT,DBGETNTI                                                 
*          DATA SET NENTVLDEMO AT LEVEL 062 AS OF 03/16/92                      
         MVC   DBSELSTA(4),NBACTNET                                             
         CLI   NBPOSTYP,C'N'                                                    
         BE    *+18                                                             
         CLI   NBNTISTA,X'40'                                                   
         BNH   *+10                                                             
         MVC   DBSELSTA(4),NBNTISTA  IF SYNDICATOR USE NTI STATION              
         MVI   DBSELSTA+4,C'T'                                                  
         CLI   NBPOSTYP,X'40'                                                   
         BNH   *+18                                                             
         CLI   NBPOSTYP,C'N'                                                    
         BE    *+10                                                             
         MVC   DBSELSTA+4(1),NBPOSTYP                                           
*                                                                               
         MVC   DBSELPRG,NBNTI                                                   
         OC    NBNTI,NBNTI         ...IF NO NTI                                 
         BNZ   NIN10                                                            
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELDAY,NBDAY      ...DO DAY/TIME READ                          
*                                                                               
*          DATA SET NENTVLDEMO AT LEVEL 062 AS OF 03/16/92                      
         OC    NBSDAFDT,NBSDAFDT                                                
         BZ    DAF10                                                            
         GOTO1 NBGETDAY,DMCB,WORK+2,FULL               GET AFFID DAY            
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A03'        GET DAYPAK                         
         GOTO1 NBCALLOV,DMCB                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(3,FULL),WORK,WORK+10                                  
         CLI   WORK,0                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DBSELDAY,WORK                                                    
DAF10    DS    0H                                                               
*                                                                               
         CLI   DBSELDAY,X'7C'      IS IT M-F                                    
         BNE   NIN5                                                             
         MVC   DBSEL1WK,NBACTDAT                                                
         OC    NBSDAFDT,NBSDAFDT                                                
         BZ    *+10                                                             
         MVC   DBSEL1WK,NBSDAFDT                                                
         MVI   DBBEST,C'L'                                                      
NIN5     OC    NBAFFTIM,NBAFFTIM                                                
         BZ    NINX                                                             
         MVC   NBTIME(2),NBAFFTIM                                               
         MVC   NBTIME+2(2),NBAFFTIM                                             
NIN7     MVC   DBSELTIM,NBTIME                                                  
*                                                                               
NIN10    GOTO1 VDEMAND,DMCB,NDADBLCK,DEFHK                                      
         OC    DBDIVSOR,DBDIVSOR                                                
         BNZ   *+10                                                             
         XC    0(16,R3),0(R3)                                                   
NINX     B     XITX                                                             
*                                                                               
DEFHK    NTR1                                                                   
         GOTO1 VDEFINE,DMCB,=C'PROGRAM',NDADBLCK,0(R3)                          
XITX     B     XXIT                                                             
         DROP  R4                                                               
VDEMAND  DS    V                                                                
VDEFINE  DS    V                                                                
VNETWEEK DS    V                                                                
         EJECT                                                                  
*                                                                               
XHICOM   DS    0H                                                               
         L     R4,=A(COMMAREA)     SET UP FOR NETCOM                            
         USING NCOMBLKD,R4                                                      
         MVI   NCBTOP,C'Y'         TOP OF PAGE COMMENTS                         
         MVI   NCBKDFL,C'Y'        DEFAULT TO NEXT KEY                          
         MVC   NCBAIO,=A(COMMIO)                                                
         LA    R1,NETBLOCK                                                      
         ST    R1,NCBNETB                                                       
         LA    R1,COMHOOK                                                       
         ST    R1,NCBAHOOK                                                      
         SPACE 1                                                                
         MVC   NCBAM,NBACTAM                                                    
         MVC   NCBID(3),NDCOMDEF   COMMENT ID FROM HIGHCOMM=XX                  
         CLI   NDCOMDEF+2,C'A'     IF NO SIGNIFICANT 3RD CHARACTER,             
         BNL   *+8                                                              
         MVI   NCBID+2,C'1'        ASK FOR TOP COMMENTS                         
         CLI   NDHIGHCM,0          IF NOT SET                                   
         BNE   *+8                                                              
         MVI   NDHIGHCM,X'FF'      SET FOR ALL                                  
                                                                                
         MVI   NCBCLT,C'-'                                                      
         TM    NDHIGHCM,X'01'                                                   
         BNO   *+10                                                             
         MVC   NCBCLT,NBACTCLI                                                  
                                                                                
         MVI   NCBALPRD,C'-'                                                    
         TM    NDHIGHCM,X'02'                                                   
         BNO   HICOM10                                                          
         MVC   NCBPRD,NBSPLPRN                                                  
         MVI   NCBALPRD,0                                                       
                                                                                
HICOM10  MVI   NCBALEST,C'-'                                                    
         TM    NDHIGHCM,X'04'                                                   
         BNO   HICOM12                                                          
         MVC   NCBEST,NBACTEST     CURRENT ESTIMATE                             
         MVI   NCBALEST,0                                                       
                                                                                
HICOM12  MVI   NCBNTWK,C'-'                                                     
         TM    NDHIGHCM,X'08'                                                   
         BNO   *+10                                                             
         MVC   NCBNTWK,NBACTNET                                                 
                                                                                
         MVI   NCBDPT,C'-'                                                      
         TM    NDHIGHCM,X'10'                                                   
         BNO   *+10                                                             
         MVC   NCBDPT,NBACTDP                                                   
                                                                                
         MVI   NCBPKG,C'-'                                                      
         TM    NDHIGHCM,X'20'                                                   
         BNO   *+10                                                             
         MVC   NCBPKG,NBPACK                                                    
                                                                                
         CLC   LASTCMBL,NCBCLT     HAVE WE DONE THIS YET                        
         BE    COMXIT                                                           
         MVI   LASTCOMM,X'40'                                                   
         MVC   LASTCOMM+1(L'LASTCOMM-1),LASTCOMM                                
         MVC   LASTCMBL,NCBCLT          NO - SO NEED NETCOM                     
         GOTO1 =V(NETCOM),DMCB,NCOMBLKD                                         
         MVI   NBFUNCT,NBFRESTR    RESTORE UNIT ON NEXT NETIO                   
         SPACE 1                                                                
COMXIT   MVC   0(144,R3),LASTCOMM                                               
         MVC   DUB,LASTCOMM     SO CAN SEE IN DUMP FOR TESTING                  
         MVC   WORK,NCBAIO      SO CAN SEE IN DUMP (NECOMBLOCK)                 
         B     XXIT                                                             
         SPACE 1                                                                
LASTCOMM DC    CL144' '                                                         
LASTCMBL DC    XL10'00'                                                         
         SPACE 3                                                                
COMHOOK  NTR1                                                                   
         SPACE 1                                                                
         LA    R3,LASTCOMM                                                      
         MVI   LASTCOMM,X'40'                                                   
         MVC   LASTCOMM+1(L'LASTCOMM-1),LASTCOMM                                
         ZIC   R2,GLARGS           MAX LINES                                    
         L     R4,=A(COMMIO)                                                    
*        MVI   ELCODE,X'02'                                                     
         LA    R4,27(R4)                                                        
COMHOOK1 CLI   0(R4),X'02'                                                      
         BE    COMHOOK3                                                         
COMHOOK2 ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    XXIT                                                             
         AR    R4,R1                                                            
         B     COMHOOK1                                                         
         SPACE 1                                                                
         USING NCOMELEM,R4                                                      
COMHOOK3 ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         CH    R1,=H'36'                                                        
         BL    *+8                 MAXIMUM COMM LENGTH = 36                     
         LA    R1,36                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NCOMETXT                                                 
         LA    R3,36(R3)                                                        
         BCT   R2,COMHOOK2                                                      
         B     XXIT                                                             
         EJECT                                                                  
XNIBRP   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),NBACTCLI                                                
         MVC   MYWORK(4),=C'1234'  FUDGE MKT                                    
         MVC   MYWORK+4(4),NBACTNET                                             
         MVI   MYWORK+8,C'N'                                                    
         GOTO1 =V(MSPACK),DMCB,MYWORK,MYWORK+4,MYWORK+10                        
         MVC   KEY+5(3),MYWORK+12                                               
         MVC   KEY+8(6),NBACTPRG                                                
         MVC   KEY+14(2),NBACTDAT                                               
         XC    KEY+14(2),=X'FFFF'       DATE IN COMPLEMENT FORM                 
         MVC   FILENAME,=C'UNTDIR'                                              
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE                                                  
         BNE   NIBRP10                                                          
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R4,27(R4)                                                        
         CLI   0(R4),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NINDCTEL,R4                                                      
         CLI   NINDCCTL,X'80'      IS IT BRP VALUES                             
         BNE   NIBRP10                                                          
NIBRP9   ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),2                                                          
         BNE   NIBRP10                                                          
         USING NINDELEM,R4                                                      
         CLC   NBSPLPRN,NINDPRD                                                 
         BNE   NIBRP9                                                           
         MVC   2(2,R3),NINDBRP                                                  
NIBRP10  MVC   FILENAME,=C'UNTDIR  '      REESTABLISH SEQUENCE                  
         MVC   KEY,NBKEY                                                        
         GOTO1 HIGH                                                             
         B     XXIT                                                             
         DROP  R4                                                               
*                                                                               
XNOBRP   DS    0H                                                               
         EDIT  (B4,0(R2)),(8,0(R3)),1,WRK=MYWORK,ZERO=BLANK                     
         B     XXIT                                                             
         EJECT                                                                  
*                                                                               
XITARGET DS    0H                                                               
         CLI   NBMODE,NBPROCPP     CHECK FOR PUP REPORT                         
         BNE   NITARG4                                                          
*                                                                               
         L     R4,NBADEM                                                        
         LTR   R4,R4                                                            
         BZ    XXIT                                                             
         USING NDDEMBLK,R4                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(3),NDDEMOS                                                   
         MVI   DUB+3,X'FF'                                                      
         ZIC   R0,GLARGS                                                        
         C     R0,=F'1'                                                         
         BE    NITARG5                                                          
         MVC   DUB(3),NDDEMOS+3                                                 
         B     NITARG5                                                          
         DROP  R4                                                               
*                                                                               
NITARG4  L     R4,=A(GETPESTB)                                                  
         USING GETPESTD,R4                                                      
         ST    R4,DMCB                                                          
         GOTO1 =V(GETPEST),DMCB,,NETBLOCK                                       
         L     R1,GTPBAENT                                                      
         MVC   DUB+8(8),0(R1)                                                   
         LA    R1,2                                                             
         ZIC   R0,GLARGS                                                        
         C     R0,=F'1'                                                         
         BE    *+8                                                              
         LA    R1,3(R1)                                                         
         A     R1,GTPBAENT                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(3),0(R1)                                                     
         MVI   DUB+3,X'FF'                                                      
         CLI   DUB+1,X'21'         IF ITS USER DEMO                             
         BE    USERTARG                                                         
NITARG5  MVI   DUB+1,C'T'          SET FOR IMPS SO NAME HAS NO R PREFIX         
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'  A(DEMOCON)                            
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,(10,WORK),NDADBLCK,0                               
NITARG6  LA    RE,WORK                                                          
         LA    R0,7                                                             
         CLI   WORK,X'40'         ..IF BLANK MODIFIER                           
         BNE   NITARG8                                                          
         MVC   WORK(6),WORK+1     ..DROP IT                                     
         SPACE 1                                                                
NITARG8  CLI   0(RE),C'-'          TAKE OUT DASH                                
         BNE   NITARG10                                                         
         MVC   0(6,RE),1(RE)                                                    
         B     NITARG12                                                         
         SPACE 1                                                                
NITARG10 LA    RE,1(RE)                                                         
         BCT   R0,NITARG8                                                       
         SPACE 1                                                                
NITARG12 MVC   0(6,R3),WORK        PASS BACK DEMO NAME                          
         CLI   DUB,0               IS IT NAD                                    
         BE    XXIT                                                             
         MVI   0(R3),X'FF'         SET NAD FLG                                  
         MVC   1(10,R3),WORK                                                    
         B     XXIT                                                             
         SPACE 1                                                                
* GET USER TARGET DEMO NAME FROM NDDEMOS MYSELF                                 
* DUB HAS 3 BYTE USER DEMO                                                      
USERTARG DS    0H                                                               
         L     R4,NBADEM                                                        
         LTR   R4,R4                                                            
         BZ    XXIT                                                             
         USING NDDEMBLK,R4                                                      
         ZIC   R0,NDNDEMOS         CHECK IF DEMO IS IN CONTROL ESTIMATE         
         LTR   R0,R0                                                            
         BZ    XXIT                                                             
UTARG3   CLC   DUB(3),0(R4)                                                     
         BE    UTARG5                                                           
         LA    R4,3(R4)                                                         
         BCT   R0,UTARG3                                                        
         B     XXIT                NOT IN LIST/EXIT                             
UTARG5   ZIC   R2,DUB+2            YES/GET DISPLACEMENT TO USER NAME            
         BCTR  R2,0                                                             
         MH    R2,=H'7'                                                         
         L     R4,NBADEM                                                        
         LA    R4,NDUSRNMS                                                      
         AR    R4,R2                                                            
         MVC   WORK(7),0(R4)                                                    
         B     NITARG6                                                          
         DROP  R4                                                               
         EJECT                                                                  
* - GET DEMO NAME                                                               
XDEMOUT  DS    0H                                                               
         ZIC   R2,GLARGS                                                        
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),=C'HOMES '                                               
         LTR   R2,R2                                                            
         BZ    NHDEM2                                                           
         L     R1,=A(SAVETAR1)                                                  
         MVC   WORK(6),0(R1)      21=TARGET 1                                   
         CLI   GLARGS,21                                                        
         BE    NHDEM2                                                           
         L     R1,=A(SAVETAR2)                                                  
         MVC   WORK(6),0(R1)      22=TARGET 2                                   
         CLI   GLARGS,22                                                        
         BE    NHDEM2                                                           
         BCTR  R2,0                                                             
         NETGO NVDEMCON,DMCB,((R2),NBADEM),(C'C',NDADBLCK),(7,WORK)             
**       NETGO NVDEMCON,DMCB,((R2),NBADEM),(C'C',NDADBLCK),(10,WORK)            
         XC    WORK+12(8),WORK+12                                               
         L     R4,NBADEM                                                        
         MH    R2,=X'0003'                                                      
         AR    R4,R2                                                            
         CLI   0(R4),0             IS IT NAD DEMO                               
         BE    NHDEM1                                                           
         L     R2,=A(PFXTAB)           GET NAME FROM TABLE                      
NADLOOP  CLC   0(1,R4),0(R2)                                                    
         BE    GOTNAD                                                           
         LA    R2,8(R2)                                                         
         CLI   0(R2),X'FF'         NO MATCH                                     
         BNE   NADLOOP                                                          
         MVC   WORK+12(7),=C'**    **'                                          
         B     NHDEM1                                                           
GOTNAD   DS    0H                                                               
         MVC   WORK+12(7),1(R2)                                                 
*                                                                               
NHDEM1   CLI   WORK,C'R'                                                        
         BNE   NHDEM2                                                           
         MVC   WORK(6),WORK+1                                                   
         MVI   WORK+6,X'40'                                                     
*                                                                               
NHDEM2   MVC   0(6,R3),WORK        ONLY NEED AGE/SEX                            
         CLI   WORK+12,0           IF NAD DO TWO LINES                          
         BE    NHDEM3                                                           
         LA    R3,198(R3)                                                       
         LA    R3,198(R3)                                                       
*        ZIC   R1,MYOLEN           MYOLEN = 0 NEED TO CHECK WHY                 
*        C     R1,=F'1'                                                         
*        BL    XIT                                                              
*        C     R1,=F'7'                                                         
*        BNH   *+8                                                              
*NHDEM2B  LA    R1,7                                                            
*        BCTR  R1,0                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
         MVC   0(7,R3),WORK+12                                                  
*        MVC   0(0,R3),WORK+12                                                  
NHDEM3   CLI   GLARGS+1,0                                                       
         BE    XXIT                                                             
         CLI   WORK+12,0           IF NAD DEMO                                  
         BE    NHDEM3B                                                          
*        S     R3,=F'400'          BACK UP FROM NAD ID                          
         S     R3,=F'396'          BACK UP FROM NAD ID                          
NHDEM3B  CLC   0(5,R3),=C'HOMES'                                                
         BNE   *+10                                                             
         MVC   0(6,R3),=C' HOMES'                                               
         LA    R1,WORK             FLOAT DEMO NAME IN HORIZ                     
         LA    R0,6                                                             
         SPACE 1                                                                
NHDEM4   CLI   0(R1),X'40'                                                      
         BH    *+8                                                              
         MVI   0(R1),X'BF'                                                      
         LA    R1,1(R1)                                                         
         BCT   R0,NHDEM4                                                        
         MVC   0(9,R3),=15X'BF'                                                 
         MVC   1(6,R3),WORK                                                     
         CLI   GLARGS+1,1                                                       
         BE    XXIT                                                             
         MVC   0(15,R3),=15X'BF'                                                
         MVC   4(6,R3),WORK                                                     
         B     XXIT                                                             
*                                                                               
         EJECT                                                                  
XIUDEF   DS    0H                                                               
         BAS   RE,GTCLIENT         FILL UDEF TABLE                              
         BAS   RE,GTPROD                                                        
         BAS   RE,GTEST                                                         
         L     RF,NDUDEFD                                                       
         USING SBLOCK,RF                                                        
         LA    R2,GLARGS           LOOP THROUGH ARGS                            
         LA    R0,4                MAX 4 UDEF EXPRESSIONS                       
*                                                                               
IUDEF2   CLI   0(R2),0             TEST END OF ARGS LIST                        
         BE    IUDEF10                                                          
         CLI   1(R2),0             NO-TEST FIELD IS DEFINED FOR CLIENT          
         BE    IUDEF8                                                           
         LA    R5,UDEFTAB          YES-DETERMINE UDEF EXPRESSION                
*                                                                               
IUDEF4   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(R5)                                                    
         BE    *+12                                                             
         LA    R5,L'UDEFTAB(R5)                                                 
         B     IUDEF4                                                           
         SR    R4,R4               R4=A(USER FIELD)                             
         ICM   R4,3,1(R5)                                                       
         LA    R4,SBLOCK(R4)                                                    
         CLI   2(R3),C'D'          TEST DATA TYPE = DATE                        
         BNE   IUDEF6                                                           
         CLC   0(6,R4),=6X'40'     AND FIELD IS SET                             
         BNH   IUDEF6                                                           
         ST    RF,FULL             SAVE ADDRESS OF UDEF BLOCK                   
         GOTO1 DATVAL,DMCB,(0,(R4)),DUB   YES-GET THE DATE                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,DUB,(2,(R3))                                         
         L     RF,FULL             RESET ADDRESS UF UDEF BLOCK                  
         LA    R3,2(R3)            SORT ON DATE                                 
*                                                                               
IUDEF6   DS    0H                                                               
         ZIC   RE,1(R2)            LENGTH OF FIELD                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R3),0(R4)       MOVE INTO DRIVER INPUT FIELD                 
         LA    R3,1(RE,R3)                                                      
*                                                                               
IUDEF8   LA    R2,3(R2)            NEXT UDEF EXPRESSION                         
         BCT   R0,IUDEF2                                                        
*                                                                               
IUDEF10  B     XXIT                                                             
         SPACE 2                                                                
XOUDEF   DS    0H                                                               
         L     RF,NDUDEFD                                                       
         USING SBLOCK,RF                                                        
         LR    R5,R3               R5=A(OUTPUT AREA)                            
         CLI   GLARGS+3,0          TEST MORE THAN 1 UDEF EXPRESSION             
         BNE   OUDEF1                                                           
         L     R5,=A(OUTAREA)                                                   
         USING OUTAREA,R5                                                       
         LA    R5,NAMEAREA         NO-FORMAT TO NAMEAREA                        
         DROP  R5                                                               
OUDEF1   LA    R4,GLARGS           LOOP THROUGH ARGS                            
         LA    R0,4                MAX 4 UDEF EXPRESSIONS                       
*                                                                               
OUDEF2   CLI   0(R4),0             TEST END OF ARGS LIST                        
         BE    OUDEF8                                                           
         MVI   0(R5),0             NON-PRINTABLE CHAR                           
         CLI   1(R4),0             TEST FIELD IS DEFINED FOR CLIENT             
         BE    OUDEF6                                                           
         LA    RE,UDEFTAB          YES-DETERMINE UDEF EXPRESSION                
*                                                                               
OUDEF4   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'UDEFTAB(RE)                                                 
         B     OUDEF4                                                           
         ST    RE,FULL             SAVE ADDR OF UDEFTAB                         
         CLI   2(R4),C'D'          TEST DATA TYPE = DATE                        
         BNE   *+8                                                              
         LA    R2,2(R2)            YES-SKIP PAST COMPRESSED DATE                
         ZIC   R1,1(R4)            R1=L'DATA                                    
         LR    RE,R1                                                            
         L     R1,=A(MYOLEN)       GET ADDRESSABILITY TO MYOLEN                 
         MVC   MYBYTE,0(R1)                                                     
         CLC   MYBYTE,1(R4)        TEST OUTPUT LENGTH IS LESS                   
*        CLC   MYOLEN,1(R4)        TEST OUTPUT LENGTH IS LESS                   
         BNL   *+8                                                              
*        IC    RE,MYOLEN           YES                                          
         IC    RE,MYBYTE           YES                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R5),0(R2)       MOVE DATA TO PRINT LINE                      
         XC    SVUDEF,SVUDEF                                                    
         CH    RE,=H'3'                                                         
         BNH   *+8                                                              
         LH    RE,=H'3'                                                         
         EX    RE,*+4                                                           
         MVC   SVUDEF(0),0(R2)     SAVE THE FIRST 4 BYTES OF UDEF VALUE         
         OC    SVUDEF,=6X'40'                                                   
         LA    R2,0(R1,R2)         ADVANCE TO NEXT UDEF EXPRESSION              
         CLI   GLARGS+3,0          TEST ONLY ONE UDEF EXPRESSION                
         BNE   OUDEF6                                                           
         SR    R1,R1               YES-MOVE DESCRIPTION TO LABLAREA             
         L     RE,FULL             GET UDEFTAB ADDR                             
         ICM   R1,3,3(RE)                                                       
         LA    R1,SBLOCK(R1)                                                    
         L     RE,=A(OUTAREA)                                                   
         USING OUTAREA,RE                                                       
         MVC   LABLAREA,0(R1)                                                   
         DROP  RE                                                               
*                                                                               
OUDEF6   CLI   GLARGS+3,0             TEST ONLY ONE UDEF EXPRESSION             
         BNE   OUDEF7                                                           
         GOTO1 =A(XGENOUT),DMCB,(RA)   YES-EXIT NOW                             
         B     XXIT                                                             
*                                                                               
OUDEF7   LA    R4,3(R4)            NEXT UDEF EXPRESSION                         
         LA    R5,198(R5)          NEXT PRINT LINE                              
         BCT   R0,OUDEF2                                                        
*                                                                               
OUDEF8   B     XXIT                                                             
         SPACE 2                                                                
XHUDEF   DS    0H                                                               
         L     RF,NDUDEFD                                                       
         USING SBLOCK,RF                                                        
         L     R1,GLADTENT                                                      
         ZIC   R4,DRHDWDTH-DRHDD(R1)    R4=COLUMN WIDTH                         
         LA    RE,L'SBUP1DES       SET RE FOR EXEXUTED MOVE                     
         CR    RE,R4                                                            
         BNH   *+6                                                              
         LR    RE,R4                                                            
         BCTR  RE,0                                                             
         SR    R0,R0               R0=N'UDEF EXPRESSIONS                        
         ICM   R0,1,GLARGS                                                      
         BZ    HUDEF8                                                           
         LA    R4,GLARGS+1                                                      
*                                                                               
HUDEF2   MVI   0(R3),0             INIT HEADING WITH NON-PRINTABLE CHAR         
         CLI   0(R4),0             TEST UDEF AT THIS PRINT POSITION             
         BE    HUDEF6                                                           
         LA    R5,UDEFTAB          YES-DETERMINE UDEF EXPRESSION                
*                                                                               
HUDEF4   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),0(R5)                                                    
         BE    *+12                                                             
         LA    R5,L'UDEFTAB(R5)                                                 
         B     HUDEF4                                                           
         SR    R1,R1                                                            
         ICM   R1,3,3(R5)                                                       
         LA    R1,SBLOCK(R1)       R1=A(UDEF DESCRIPTION)                       
         EX    RE,*+4              MOVE DESCRIPTION TO HEADING                  
         MVC   0(0,R3),0(R1)                                                    
*        GOTO1 =V(PRNTBL),DMCB,=C'HUD',0(R3),C'DUMP',50,=C'1D'                  
*                                                                               
HUDEF6   LA    R3,198(R3)          NEXT PRINT LINE                              
         LA    R4,1(R4)            NEXT UDEF EXPRESSION                         
         BCT   R0,HUDEF2                                                        
*                                                                               
HUDEF8   B     XXIT                                                             
         SPACE 2                                                                
UDEFTAB  DS    0CL5                                                             
         DC    X'01',AL2(SBUP1FLD-SBLOCK),AL2(SBUP1DES-SBLOCK)                  
         DC    X'02',AL2(SBUP2FLD-SBLOCK),AL2(SBUP2DES-SBLOCK)                  
         DC    X'03',AL2(SBUE1FLD-SBLOCK),AL2(SBUE1DES-SBLOCK)                  
         DC    X'04',AL2(SBUE2FLD-SBLOCK),AL2(SBUE2DES-SBLOCK)                  
         DC    X'00'                                                            
         DROP  RF                                                               
         EJECT                                                                  
* - GET CLIENT USER DEFINITION DATA                                             
GTCLIENT NTR1                                                                   
***      L     R4,NBAIO                                                         
***      CLI   0(R4),X'04'         MUST BE UNIT REC IN NBAIO                    
***      BNE   XXIT                                                             
         CLC   CURRCLT,NBACTAM    HAVE WE DONE THIS ONE BEFORE?                 
         BE    XXIT                   MAKE SURE WE READ CLIENT                  
         MVC   CURRCLT,NBACTAM                                                  
         USING CLTHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   CKEYAM(3),CURRCLT   READ CLIENT RECORD                           
         BAS   R5,GTREC                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE CLIENT RECORD                        
         L     R4,AIO                                                           
         L     R1,NDUDEFD          GET USER DEF DESCRIPTIONS                    
         USING SBLOCK,R1                                                        
         MVC   SBUP1DES,CPU1       PRODUCT DESCRIPTION                          
         MVC   SBUP1TYP,CPU1TYPE   LENGTH                                       
         MVC   SBUP1LEN,CPU1LEN    TYPE                                         
         MVC   SBUP2DES,CPU2       DESCRIPTION                                  
         MVC   SBUP2TYP,CPU2TYPE   LENGTH                                       
         MVC   SBUP2LEN,CPU2LEN    TYPE                                         
         MVC   SBUE1DES,CEU1       ESTIMATE DESCRIPTION                         
         MVC   SBUE1TYP,CEU1TYPE   LENGTH                                       
         MVC   SBUE1LEN,CEU1LEN    TYPE                                         
         MVC   SBUE2DES,CEU2       DESCRIPTION                                  
         MVC   SBUE2TYP,CEU2TYPE   LENGTH                                       
         MVC   SBUE2LEN,CEU2LEN    TYPE                                         
         DROP  R1                                                               
         SPACE 1                                                                
* - RESET TO READ UNIT FILE                                                     
RESUNTF  XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         L     R5,NDAGBLOK                                                      
         USING NETGOALD,R5                                                      
         CLI   NGOALIOS,1          IF NETGOAL IS ACTIVE                         
         BNE   *+8                                                              
         MVI   NGOALIOS,2             TELL NETGOAL TO REESTABLISH SEQ           
         B     XXIT                                                             
         DROP  R4                                                               
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
*              ROUTINE TO FILL IN PRODUCT DETAILS                               
         SPACE 1                                                                
GTPROD   NTR1                                                                   
*        L     R1,NBAIO                                                         
*        CLI   0(R1),X'04'         MUST BE UNIT RECORD                          
*        BNE   GTPRD1                                                           
         CLI   NBSPLPRN,0          IF UNALLOCATED                               
         BNE   GTPRD2                                                           
         CLI   NBPRD,0             IF UNALLOCATED                               
         BNE   GTPRD2                                                           
         CLI   NBPRD2,0            IF UNALLOCATED                               
         BNE   GTPRD2                                                           
GTPRD1   L     R1,NDUDEFD          CLEAR USER DEFINITION                        
         USING SBLOCK,R1                                                        
         XC    SBUP1FLD,SBUP1FLD                                                
         XC    SBUP2FLD,SBUP2FLD                                                
         MVC   CURRPRD(3),CURRCLT      CLEAR CURRPRD                            
         XC    CURRPRD+3(3),CURRPRD+3                                           
         B     XXIT                                                             
         SPACE 1                                                                
* - HAVE WE DONE IT BEFORE                                                      
GTPRD2   MVC   DUB(3),CURRCLT     SET AM/CLI                                    
         BAS   RE,RETNPRD                                                       
         MVC   DUB+3(3),WORK       SET 3 BYTE PROD                              
         CLI   WORK,0                                                           
         BE    GTPRD1                                                           
         CLC   CURRPRD,DUB        HAVE WE DONE THIS ONE BEFORE?                 
         BE    XXIT                                                             
         MVC   CURRPRD,DUB                                                      
         USING PRDHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   PKEYAM(6),CURRPRD   (AM/CLI/PRODUCT)                             
         BAS   R5,GTREC                                                         
         BNE   GTPRD1                                                           
         L     R4,AIO                                                           
         L     R1,NDUDEFD          GET USER DEFINITION                          
         USING SBLOCK,R1                                                        
         MVC   SBUP1FLD,PUSER1                                                  
         MVC   SBUP2FLD,PUSER2                                                  
GETPRDX  B     RESUNTF                                                          
         DROP  R4,R1                                                            
         EJECT                                                                  
*              ROUTINE TO LOOK UP PRODUCT CODE                                  
*              INPUT               PRODUCT CODE                                 
*              OUTPUT              RETURN 3 BYTE CODE IN WORK                   
         SPACE 1                                                                
RETNPRD  NTR1                                                                   
         CLC   NBSELPRD,=C'POL'    IF POL REQUESTED                             
         BNE   RTN2                                                             
         MVC   WORK(3),=C'POL'     ALWAYS READ POL UDEF RECORD                  
         B     RTNX                                                             
RTN2     XC    WORK(3),WORK                                                     
         MVC   BYTE,NBSPLPRN       SET FOR SPLIT PROD                           
         TM    NBSPLOPT,X'80'      ARE WE SPLITTING PRODUCTS                    
         BO    RTN10                                                            
         MVC   BYTE,NBPRD          NO                                           
RTN10    CLI   BYTE,0                                                           
         BE    RTNX                                                             
         CLI   BYTE,X'FF'                                                       
         BE    RTNX                                                             
         CLI   NDSPLOPT,0          IS SPLIT BILLING ON?                         
         BE    RTN20                                                            
         L     R1,NDASPLBL                                                      
         USING SPLTBLKD,R1                                                      
         MVC   WORK(3),SPLIPRD                                                  
         B     RTNX                                                             
         SPACE 1                                                                
RTN20    L     R4,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R4                                                        
         LA    R2,CLIST                                                         
         LA    R5,220                                                           
         SPACE 1                                                                
RTN30    CLC   BYTE,3(R2)                                                       
         BE    RTN40                                                            
         LA    R2,4(R2)                                                         
         BCT   R5,RTN30                                                         
         B     RTNX                                                             
RTN40    MVC   WORK(3),0(R2)       3-BYTE PRODUCT                               
*                                                                               
RTNX     B     XXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILL IN ESTIMATE USER DEFINITION                      
         SPACE 3                                                                
GTEST    NTR1                                                                   
         MVC   DUB(6),CURRPRD      AM/CLI/PROD                                  
         MVC   DUB+6(1),NBACTEST                                                
         CLC   CURREST,DUB        HAVE WE DONE THIS ONE BEFORE?                 
         BE    XXIT                                                             
* - CLEAR EST USER DEF FIELDS                                                   
         L     R1,NDUDEFD                                                       
         USING SBLOCK,R1                                                        
         XC    SBUE1FLD,SBUE1FLD                                                
         XC    SBUE2FLD,SBUE2FLD                                                
                                                                                
         MVC   CURREST,DUB                                                      
         USING ESTHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   EKEYAM(7),CURREST      (AM/CLI/PROD/EST)                         
         CLC   =C'POL',NBSELPRD                                                 
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         OC    EKEYPRD,EKEYPRD                                                  
         BNZ   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         CLC   EKEYPRD,=C'UNA'                                                  
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         BAS   R5,GTREC                                                         
         BE    GTE4                                                             
*                                  IF NO MATCH/ TRY FOR POL                     
         CLC   KEYSAVE+4(3),=C'POL'                                             
         BE    RESUNTF                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(3),=C'POL'                                                 
         BAS   R5,GTREC                                                         
         BNE   RESUNTF                                                          
         SPACE 1                                                                
GTE4     L     R4,AIO                                                           
         L     R1,NDUDEFD          GET USER FIELDS                              
         USING SBLOCK,R1                                                        
         MVC   SBUE1FLD,EUSER1                                                  
         MVC   SBUE2FLD,EUSER2                                                  
         B     RESUNTF                                                          
         DROP  R4,R1                                                            
         EJECT                                                                  
* - I/O ROUTINE FOR SPOT FILE CALLED FROM UDEF ROUTINES                         
GTREC    NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         BNER  R5                  NO CAN FIND                                  
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         CR    R5,R5               SET EQUAL                                    
         BR    R5                                                               
*                                                                               
         EJECT                                                                  
XNIINV   DS    0H                                                               
         CLI   NBRDBELS,2          ..IF MULTIPLE CALLS TO WRITER                
         BNE   NIV05                                                            
         MVI   0(R3),1             ..SET TO PRINT ONE LINE                      
         B     XXIT                                                             
NIV05    MVI   0(R3),X'FF'         SET DONT PRINT SWITCH                        
         LA    R5,10                                                            
         L     R4,NBAIO                                                         
         CLI   0(R4),X'04'                                                      
         BNE   XXIT                                                             
         LA    R4,27(R4)                                                        
NIV06    CLI   0(R4),X'10'                                                      
         BE    NIV10AA                                                          
NIV10    ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    XXIT                                                             
         AR    R4,R1                                                            
         B     NIV06                                                            
         USING NUBILEL,R4                                                       
NIV10AA  TM    NUBILST,X'20'       IF UNBILLED                                  
         BO    NIV10               REJECT IT                                    
         CLI   GLARGS,X'40'        ..INVOICE FILTERING                          
         BNH   NIV10B                                                           
         CLI   GLARGS,C'S'         .. IF SPECIAL                                
         BNE   NIV10A                                                           
         CLI   NUBILTYP,C'T'       ..REJECT TIME                                
         BE    NIV10                                                            
         CLI   NUBILTYP,C'I'       ..REJECT INTEGRATION                         
         BE    NIV10                                                            
         B     NIV10B              ..ACCEPT ALL ELSE                            
NIV10A   CLC   NUBILTYP,GLARGS                                                  
         BNE   NIV10                                                            
NIV10B   MVC   BYTE,NBSPLPRN       ASSUME SPLIT PROD NUMBER                     
         CLC   NBSELPRD,=C'POL'                                                 
         BE    NIV12                                                            
         TM    NBSPLOPT,X'80'                                                   
         BO    *+10                                                             
         MVC   BYTE,NBPRD          UNLESS SPLIT NOT ON                          
         CLC   NUBILPRD,BYTE                                                    
         BNE   NIV10                                                            
NIV12    MVC   0(1,R3),NUBILTYP    TYPE                                         
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,8(R3))                               
         MVC   1(2,R3),10(R3)                                                   
         MVI   3(R3),C'-'                                                       
         MVC   4(4,R3),NUBILNUM                                                 
         MVC   14(4,R3),NUBILGRS                                                
         MVC   18(4,R3),NUBILNET                                                
         MVC   22(1,R3),NUBILST                                                 
         B     NIV100                                                           
*                                                                               
NIV100   LA    R3,23(R3)                                                        
         BCT   R5,NIV10                                                         
         B     XXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
MYWORK   DS    CL40                                                             
MYBYTE   DS    CL1                                                              
SVUDEF   DS    CL4                                                              
CURRCLT  DS    CL3                 AM/CLT FOR USER DEF READ                     
CURRPRD  DS    CL6                 AM/CLT/PROD                                  
CURREST  DS    CL7                 AM/CLT/PROD/EST                              
DEMANDIO DS    CL1000                                                           
         DROP  RB                                                               
         DROP  R8                                                               
         DROP  R7                                                               
         EJECT                                                                  
*              THIS IS THE START OF DRIVE2                                      
         SPACE 3                                                                
         ENTRY DRIVE2                                                           
         DS    0D                                                               
DRIVE2   LR    RB,RE                                                            
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING DRIVE2,RB,R8,R7,R6                                               
         XC    MYRESULT,MYRESULT                                                
         BR    RF                                                               
         SPACE 1                                                                
*              DEMO INPUT ROUTINES - IMPS GRPS VPH                              
         SPACE 3                                                                
*              ARGUMENTS           1 RELATIVE DEMO NUMBER                       
*                                 13 A(CTUAL) OR E(STIMATED)                    
         SPACE 1                                                                
*--PUP IMP ROUTINE                                                              
NIPIMP   BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         MVC   8(2,R3),NBSPCHRG    PUP UNITS                                    
         MVC   10(2,R3),NBSPCHRG   PUP UNITS                                    
         MVC   12(4,R3),NBACTUAL   BUDGET                                       
         B     NIIMP10             IMPRESSIONS                                  
         SPACE 1                                                                
*--PUP GRP ROUTINE                                                              
NIPGRP   BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         MVC   8(2,R3),NBSPCHRG    PUP UNITS                                    
         MVC   10(2,R3),NBSPCHRG   PUP UNITS                                    
         MVC   12(4,R3),NBACTUAL   BUDGET                                       
         B     NIGRP1              GRP'S                                        
*                                                                               
NIIMP    BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
NIIMP10  BAS   RE,DEMADJ           IMPRESSIONS                                  
         MVC   0(4,R3),MYIMPSR                                                  
         MVC   4(4,R3),MYIMPSE                                                  
         B     NIGRP2                                                           
         SPACE 1                                                                
NIGGRP   BAS   RE,FILTGOAL         GOAL POINTS                                  
         BNE   XIT2                                                             
         B     NIGRP1                                                           
         SPACE 1                                                                
NIGCPP   BAS   RE,FILTGOAL         GOAL CPP                                     
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPP)                          
         BAS   RE,DEMADJ           POINTS                                       
         MVC   2(2,R3),MYGRPSE                                                  
         LA    R3,4(R3)                                                         
         BAS   RE,GETCOST          AND COST                                     
         B     XIT2                                                             
         SPACE 1                                                                
NIGRP    BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         SPACE 1                                                                
NIGRP1   BAS   RE,DEMADJ           POINTS                                       
         MVC   2(2,R3),MYGRPSR                                                  
         MVC   6(2,R3),MYGRPSE                                                  
         SPACE 1                                                                
NIGRP2   OC    0(4,R3),0(R3)                                                    
******   BZ    XIT2                * I FILTERED ABOVE ALREADY? **               
         LA    R3,8(R3)                                                         
         BAS   RE,GETUNIT          UNITS                                        
         LA    R3,4(R3)                                                         
         BAS   RE,GETCOST          AND COST                                     
         B     XIT2                                                             
         SPACE 1                                                                
*--PUP VPH ROUTINE                                                              
NIPVPH   BAS   RE,FILTPUP                                                       
         BE    NIVPH10                                                          
         B     XIT2                                                             
         SPACE 1                                                                
NIVPH    BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
NIVPH10  LA    R3,4(R3)            (LEAVE A SPACE FOR COMPUTE)                  
         BAS   RE,DEMADJ                                                        
         MVC   0(4,R3),MYIMPSE     DEMO IMPRESSIONS                             
         OC    0(4,R3),0(R3)                                                    
         BZ    XIT2                                                             
         CLI   NDVPHOPT,C'P'       OPTION TO GET PROGRAM VPH                    
         BNE   NIVPH2                                                           
         XC    0(4,R3),0(R3)                                                    
         MVC   2(2,R3),MYVPHR      PASS ACTUAL VPH                              
         MVC   6(2,R3),=H'1000'         AND CONSTANT                            
         B     XIT2                                                             
         SPACE 1                                                                
NIVPH2   MVI   GLARGS,0            NOW GET HOMES                                
         BAS   RE,DEMADJ                                                        
         MVC   4(4,R3),MYIMPSE                                                  
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO INPUT ROUTINES - INDEXES                                    
         SPACE 3                                                                
*              ARGUMENTS           1 RELATIVE DEMO NUMBER                       
         SPACE 1                                                                
NIIX     LA    R3,4(R3)            (LEAVE A GAP FOR COMPUTE)                    
*        MVI   GLARGS+12,C'E'      IX RETURN EST/ACT IMPRESSIONS                
         OI    GLARGS+12,X'02'     IX RETURN EST/ACT IMPRESSIONS                
         BAS   RE,FILTUNIT                                                      
         BNE   NIIX2                                                            
         BAS   RE,DEMADJ                                                        
         MVC   0(4,R3),MYIMPSR     EST IMPS                                     
         MVC   4(4,R3),MYIMPSE                                                  
         SPACE 1                                                                
NIIX2    OI    GLARGS+12,X'01'                                                  
         NI    GLARGS+12,X'FF'-X'02' CLEAR EST BIT                              
*        MVI   GLARGS+12,C'A'                                                   
         BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         BAS   RE,DEMADJ                                                        
         MVC   8(4,R3),MYIMPSR     ACT IMPS                                     
         MVC   12(4,R3),MYIMPSE                                                 
         B     XIT2                                                             
         SPACE 1                                                                
NIRX     LA    R3,4(R3)            (LEAVE A GAP FOR COMPUTE)                    
*        MVI   GLARGS+12,C'E'      RX RETURN EST/ACT GRPS                       
         OI    GLARGS+12,X'02'     RX RETURN EST/ACT GRPS                       
         BAS   RE,FILTUNIT                                                      
         BNE   NIRX2                                                            
         BAS   RE,DEMADJ                                                        
         MVC   2(2,R3),MYGRPSR     EST GRPS                                     
         MVC   6(2,R3),MYGRPSE                                                  
         SPACE 1                                                                
NIRX2    OI    GLARGS+12,X'01'                                                  
         NI    GLARGS+12,X'FF'-X'02'   CLEAR EST BIT                            
*        MVI   GLARGS+12,C'A'                                                   
         BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         BAS   RE,DEMADJ                                                        
         MVC   10(2,R3),MYGRPSR     ACT GRPS                                    
         MVC   14(2,R3),MYGRPSE                                                 
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO INPUT ROUTINES - NIEA (ESTIMATED/ACTUAL)                    
         SPACE 3                                                                
*              ARGUMENTS           1 RELATIVE DEMO NUMBER                       
         SPACE 1                                                                
*                                  ESTIMATED VALUES                             
NIEA     OI    GLARGS+12,X'02'                                                  
*        MVI   GLARGS+12,C'E'                                                   
         BAS   RE,FILTUNIT                                                      
         BNE   NIEA4                                                            
         SPACE 1                                                                
         BAS   RE,DEMADJ           EA RETURNS ESTIMATED AND ACTUAL              
         MVC   0(4,R3),MYIMPSR     DEMO IMPRESSIONS                             
         MVC   4(4,R3),MYIMPSE                                                  
         CLI   GLARGS+1,C'R'       OPTIONALLY RATINGS                           
         BNE   NIEA2                                                            
         XC    0(8,R3),0(R3)                                                    
         MVC   2(2,R3),MYGRPSR                                                  
         MVC   6(2,R3),MYGRPSE                                                  
         SPACE 1                                                                
NIEA2    LA    R3,20(R3)                                                        
         BAS   RE,GETCOST                                                       
         SH    R3,=H'20'                                                        
         SPACE 1                                                                
*                                  ACTUAL VALUES                                
NIEA4    LA    R3,8(R3)                                                         
*        MVI   GLARGS+12,C'A'                                                   
         OI    GLARGS+12,X'01'                                                  
         NI    GLARGS+12,X'FF'-X'02'   CLEAR EST BIT                            
         BAS   RE,FILTUNIT                                                      
         BNE   NIEA8                                                            
         BAS   RE,DEMADJ                                                        
         MVC   0(4,R3),MYIMPSR     DEMO IMPRESSIONS                             
         MVC   4(4,R3),MYIMPSE                                                  
         CLI   GLARGS+1,C'R'       OPTIONALLY RATINGS                           
         BNE   NIEA6                                                            
         XC    0(8,R3),0(R3)                                                    
         MVC   2(2,R3),MYGRPSR                                                  
         MVC   6(2,R3),MYGRPSE                                                  
         SPACE 1                                                                
NIEA6    LA    R3,20(R3)                                                        
         BAS   RE,GETCOST                                                       
         SH    R3,=H'20'                                                        
         SPACE 1                                                                
NIEA8    LA    R3,8(R3)                                                         
         BAS   RE,GETUNIT                                                       
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO INPUT ROUTINES - NIGEA (GOAL/ESTIMATED/ACTUAL)              
         SPACE 3                                                                
*              ARGUMENTS           1 RELATIVE DEMO NUMBER                       
         SPACE 1                                                                
*                                  GOAL VALUES                                  
NIGEA    XC    0(48,R3),0(R3)                                                   
         BAS   RE,FILTGOAL                                                      
         BNE   NIGEA2                                                           
         BAS   RE,DEMADJ                                                        
         MVC   2(2,R3),MYGRPSR                                                  
         MVC   6(2,R3),MYGRPSE                                                  
         LR    RF,R3                                                            
         LA    R3,8(R3)                                                         
         BAS   RE,GETCOST                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NIGEA2   LA    R3,16(R3)           (GOAL VALUES COME FIRST)                     
*        MVI   GLARGS+12,C'E'                                                   
         OI    GLARGS+12,X'02'                                                  
         BAS   RE,FILTUNIT                                                      
         BNE   NIGEA4                                                           
         SPACE 1                                                                
*                                  ESTIMATED VALUES                             
         BAS   RE,DEMADJ                                                        
         MVC   2(2,R3),MYGRPSR                                                  
         MVC   6(2,R3),MYGRPSE                                                  
         LA    R3,8(R3)                                                         
         BAS   RE,GETCOST                                                       
         SH    R3,=H'8'                                                         
         SPACE 1                                                                
*                                  ACTUAL VALUES                                
NIGEA4   LA    R3,16(R3)           (GET PAST ESTIMATED)                         
*        MVI   GLARGS+12,C'A'                                                   
         OI    GLARGS+12,X'01'                                                  
         NI    GLARGS+12,X'FF'-X'02'   CLEAR EST BIT                            
         BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         BAS   RE,DEMADJ                                                        
         MVC   2(2,R3),MYGRPSR                                                  
         MVC   6(2,R3),MYGRPSE                                                  
         LA    R3,8(R3)                                                         
         BAS   RE,GETCOST                                                       
         B     XIT2                                                             
         EJECT                                                                  
*              $ INPUT ROUTINES - NIGEADOL (GOAL/ESTIMATED/ACTUAL)              
         SPACE 3                                                                
*                                  GOAL VALUES                                  
NIGEADOL XC    0(24,R3),0(R3)                                                   
         BAS   RE,FILTGOAL                                                      
         BNE   NIGD2                                                            
         BAS   RE,GETCOST                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NIGD2    LA    R3,8(R3)            (GOAL VALUES COME FIRST)                     
*        MVI   GLARGS+12,C'E'                                                   
         OI    GLARGS+12,X'02'                                                  
         BAS   RE,FILTUNIT                                                      
         BNE   NIGD4                                                            
         SPACE 1                                                                
*                                  ESTIMATED VALUES                             
         BAS   RE,GETCOST                                                       
         SPACE 1                                                                
*                                  ACTUAL VALUES                                
NIGD4    LA    R3,8(R3)            (GET PAST ESTIMATED)                         
         OI    GLARGS+12,X'01'                                                  
         NI    GLARGS+12,X'FF'-X'02'   CLEAR EST BIT                            
*        MVI   GLARGS+12,C'A'                                                   
         BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         BAS   RE,GETCOST                                                       
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO INPUT ROUTINES - NIV AND NIIRH                              
         SPACE 3                                                                
*              ARGUMENTS           1 RELATIVE DEMO NUMBER                       
*                                 13 A(CTUAL) OR E(STIMATED)                    
         SPACE 1                                                                
NIV      CLI   NBMODE,NBPROCPP     CHECK PUP PROCESSING                         
         BNE   NIV2                                                             
         BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         B     *+12                                                             
*                                                                               
NIV2     BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         BAS   RE,DEMADJ           V RETURNS IMPS, HOMES & RATING               
         MVC   0(4,R3),MYIMPSR     DEMO IMPRESSIONS                             
         MVC   4(4,R3),MYIMPSE                                                  
         MVC   10(2,R3),MYGRPSR         RATINGS                                 
         MVC   14(2,R3),MYGRPSE                                                 
         MVC   SAVEVPHR,MYVPHR                                                  
         OC    0(16,R3),0(R3)                                                   
         LA    R3,16(R3)                                                        
         BZ    NIV4                                                             
         MVI   GLARGS,0            HOMES                                        
         BAS   RE,DEMADJ                                                        
         MVC   0(4,R3),MYIMPSR                                                  
         MVC   4(4,R3),MYIMPSE                                                  
         CLI   NDVPHOPT,C'P'       IF VPH OPTION IS FOR PROGRAMS,               
         BNE   NIV4                                                             
         XC    0(8,R3),0(R3)                                                    
         MVC   2(2,R3),SAVEVPHR                                                 
         MVC   6(2,R3),=H'1000'                                                 
         SPACE 1                                                                
NIV4     LA    R3,8(R3)                                                         
         CLI   NBMODE,NBPROCPP     CHECK PUP PROCESSING                         
         BE    NIPUCST                                                          
         BAS   RE,GETUNIT                                                       
         LA    R3,4(R3)                                                         
         BAS   RE,GETCOST                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NIIRH    CLI   NBMODE,NBPROCPP     CHECK PUP PROCESSING                         
         BNE   NIIRH2                                                           
         BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         B     *+12                                                             
*                                                                               
NIIRH2   BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         BAS   RE,DEMADJ           IRH RETURNS HOMES & RATING                   
         MVC   0(4,R3),MYIMPSR     DEMO IMPRESSIONS                             
         MVC   4(4,R3),MYIMPSE                                                  
         MVC   10(2,R3),MYGRPSR         RATINGS                                 
         MVC   14(2,R3),MYGRPSE                                                 
*******  OC    0(16,R3),0(R3)                                                   
*******  BZ    XIT2                                                             
         LA    R3,16(R3)                                                        
         CLI   NBMODE,NBPROCPP     CHECK PUP PROCESSING                         
         BE    NIPUCST                                                          
         BAS   RE,GETUNIT                                                       
         LA    R3,4(R3)                                                         
         BAS   RE,GETCOST                                                       
         B     XIT2                                                             
         SPACE 2                                                                
*--MOVE IN PUP UNIT AND COST INFORMATION                                        
*                                                                               
NIPUCST  MVC   2(2,R3),NBSPCHRG    PUP UNITS                                    
*--SWAP LENGTH AND DATE FIELDS FOR FILTERING                                    
         XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
*                                                                               
         XC    NBACTDAT,NBSDAFDT                                                
         XC    NBSDAFDT,NBACTDAT                                                
         XC    NBACTDAT,NBSDAFDT                                                
         BAS   RE,FILTPUP          PUP FILTERING (BUDGET IS SPECIAL)            
         BNE   NIPUC10                                                          
         MVC   4(4,R3),NBACTUAL   PUP BUDGET                                    
         XC    8(4,R3),8(R3)                                                    
*--SWAP LENGTH AND DATE FIELDS BACK                                             
NIPUC10  XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
*                                                                               
         XC    NBACTDAT,NBSDAFDT                                                
         XC    NBSDAFDT,NBACTDAT                                                
         XC    NBACTDAT,NBSDAFDT                                                
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO INPUT ROUTINES - HUT SHARE AND RATING                       
         SPACE 3                                                                
NIPHUT   BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE A GAP FOR AVERAGE)                    
         MVC   2(2,R3),NBSPCHRG    PUP UNITS                                    
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LH    R1,NBESTHUT                                                      
         M     R0,=F'10'                                                        
         STH   R1,6(R3)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
NIHUT    BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE A GAP FOR AVERAGE)                    
         LA    R1,NBESTUN          HUT RETURN UNITS AND HUT                     
*        CLI   GLARGS+12,C'A'                                                   
         TM    GLARGS+12,X'01'                                                  
         BNO   *+8                                                              
         LA    R1,NBACTUN                                                       
         MVC   2(2,R3),0(R1)       (UNITS)                                      
         MVC   6(2,R3),4(R1)       (HUT)                                        
         B     XIT2                                                             
         SPACE 1                                                                
NIPSHR   BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE A GAP FOR AVERAGE)                    
         MVC   2(2,R3),NBSPCHRG    PUP UNITS                                    
         MVC   6(2,R3),NBESTSHR    (SHARE)                                      
         B     XIT2                                                             
         SPACE 1                                                                
NISHR    BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE A GAP FOR AVERAGE)                    
         LA    R1,NBESTUN          SHARE RETURN UNITS AND SHARE                 
*        CLI   GLARGS+12,C'A'                                                   
         TM    GLARGS+12,X'01'                                                  
         BNO   *+8                                                              
         LA    R1,NBACTUN                                                       
         MVC   2(2,R3),0(R1)       (UNITS)                                      
         MVC   6(2,R3),2(R1)       (SHARE)                                      
         B     XIT2                                                             
         SPACE 1                                                                
*--AVERAGE PUP RATING                                                           
NIPRTG   BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE A GAP FOR AVERAGE)                    
         MVC   2(2,R3),NBSPCHRG    PUP UNITS                                    
         B     NIRTG20                                                          
         SPACE 1                                                                
NIRTG    BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE A GAP FOR AVERAGE)                    
         LA    R1,NBESTUN          RATING RETURN UNITS AND POINTS               
**       CLI   GLARGS+12,C'A'                                                   
         TM    GLARGS+12,X'01'                                                  
         BNO   *+8                                                              
         LA    R1,NBACTUN                                                       
         MVC   2(2,R3),0(R1)       (UNITS)                                      
NIRTG20  BAS   RE,DEMADJ                                                        
         MVC   6(2,R3),2(R4)       (POINTS)                                     
         CLI   GLARGS+1,C'I'                                                    
         BNE   XIT2                                                             
         MVC   4(4,R2),4(R4)       (OPTIONALLY, IMPS)                           
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO INPUT ROUTINES - CPM CPP CPU AND RPU                        
         SPACE 3                                                                
*              ARGUMENTS           1 RELATIVE DEMO NUMBER                       
*                                 13 A(CTUAL) OR E(STIMATED)                    
         SPACE 1                                                                
NICPM    BAS   RE,FILTUNIT         CPM                                          
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPM)                          
         BAS   RE,DEMADJ           IMPRESSIONS                                  
         MVC   0(4,R3),MYIMPSE                                                  
         LA    R3,4(R3)                                                         
         BAS   RE,GETCOST          AND COST                                     
         B     XIT2                                                             
         SPACE 1                                                                
NICPP    BAS   RE,FILTUNIT         CPP                                          
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPP)                          
         BAS   RE,DEMADJ           POINTS                                       
         MVC   2(2,R3),MYGRPSE                                                  
         LA    R3,4(R3)                                                         
         BAS   RE,GETCOST          AND COST                                     
         B     XIT2                                                             
         SPACE 1                                                                
NICPU    BAS   RE,FILTUNIT         UNIT                                         
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPU)                          
         MVI   3(R3),1             PASS ONE UNIT                                
         LA    R3,4(R3)                                                         
         BAS   RE,GETCOST          AND COST                                     
         B     XIT2                                                             
         SPACE 1                                                                
NIPCPM   BAS   RE,FILTPUP          CPM                                          
         BNE   XIT2                                                             
         LR    R5,R3                                                            
         LA    R3,4(R3)            (LEAVE GAP FOR CPM)                          
         BAS   RE,DEMADJ           IMPRESSIONS                                  
         MVC   0(4,R3),MYIMPSE                                                  
         LA    R3,4(R3)                                                         
         MVC   0(4,R3),NBACTUAL    AND COST                                     
         B     XIT2                                                             
         SPACE 1                                                                
NIPCPP   BAS   RE,FILTPUP          CPP                                          
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPP)                          
         BAS   RE,DEMADJ           POINTS                                       
         MVC   2(2,R3),MYGRPSE                                                  
         LA    R3,4(R3)                                                         
         MVC   0(4,R3),NBACTUAL    AND COST                                     
         B     XIT2                                                             
         SPACE 1                                                                
NIPCPU   BAS   RE,FILTPUP          CPU                                          
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPU)                          
         MVC   2(2,R3),NBSPCHRG    PUP UNITS                                    
         LA    R3,4(R3)            (LEAVE GAP FOR CPU)                          
         MVC   0(4,R3),NBACTUAL    AND COST                                     
         B     XIT2                                                             
         SPACE 1                                                                
NIRPU    BAS   RE,FILTUNIT         UNIT                                         
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPU)                          
         MVI   3(R3),1             PASS ONE UNIT                                
         LA    R3,4(R3)                                                         
         BAS   RE,DEMADJ                                                        
         MVC   2(2,R3),MYGRPSE                                                  
         B     XIT2                                                             
         SPACE 1                                                                
NIPRPU   BAS   RE,FILTPUP          PUP                                          
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPU)                          
         MVC   2(2,R3),NBSPCHRG    PUP UNITS                                    
         BAS   RE,DEMADJ                                                        
         MVC   6(2,R3),MYGRPSE                                                  
         B     XIT2                                                             
         SPACE 1                                                                
NIPIPU   BAS   RE,FILTPUP          PUP                                          
         BNE   XIT2                                                             
         LA    R3,4(R3)            (LEAVE GAP FOR CPU)                          
         MVC   2(2,R3),NBSPCHRG    PUP UNITS                                    
         BAS   RE,DEMADJ                                                        
         MVC   4(4,R3),MYIMPSE                                                  
         B     XIT2                                                             
         SPACE 1                                                                
NIPBUDG  CLI   NBMODE,NBPROCPP     HANDLE PUP BUDGET                            
         BNE   XIT2                                                             
*--SWAP LENGTH AND DATE FIELDS FOR FILTERING                                    
         XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
*                                                                               
         XC    NBACTDAT,NBSDAFDT                                                
         XC    NBSDAFDT,NBACTDAT                                                
         XC    NBACTDAT,NBSDAFDT                                                
*                                                                               
         BAS   RE,FILTPUP          PUP DOLLARS                                  
         BNE   NIPBD20                                                          
         MVC   0(4,R3),NBACTUAL                                                 
*                                                                               
*--SWAP LENGTH AND DATE FIELDS BACK                                             
NIPBD20  XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
*                                                                               
         XC    NBACTDAT,NBSDAFDT                                                
         XC    NBSDAFDT,NBACTDAT                                                
         XC    NBACTDAT,NBSDAFDT                                                
         B     XIT2                                                             
         SPACE 1                                                                
NIPINT   CLI   NBMODE,NBPROCPP     HANDLE PUP INTEGRATION                       
         BNE   XIT2                                                             
         BAS   RE,FILTPUP          PUP DOLLARS                                  
         BNE   NIPINT20                                                         
         MVC   0(4,R3),NBINTEG                                                  
NIPINT20 B     XIT2                                                             
         EJECT                                                                  
*              DEMO STACK INPUT                                                 
         SPACE 3                                                                
NISTACK  CLI   NBMODE,NBPROCPP     ARE WE PROCESSING PUP                        
         BE    NISTK20                                                          
         BAS   RE,FILTUNIT         STACK - PASS EVERYTHING!                     
         BNE   XIT2                                                             
         BAS   RE,DEMADJ                                                        
         MVC   0(4,R3),MYIMPSR     IMPRESSIONS                                  
         MVC   4(4,R3),MYIMPSE                                                  
         LA    R3,8(R3)                                                         
         MVC   2(2,R3),MYGRPSR     GRPS                                         
         MVC   6(2,R3),MYGRPSE     GRPS                                         
         LA    R3,8(R3)                                                         
         MVI   GLARGS,0            HOMES                                        
         BAS   RE,DEMADJ                                                        
         MVC   0(4,R3),MYIMPSE                                                  
         LA    R3,4(R3)                                                         
*                                                                               
         BAS   RE,GETUNIT          UNITS                                        
         LA    R3,4(R3)                                                         
         MVI   3(R3),1             ACTUAL NUMBER OF UNITS                       
         LA    R3,4(R3)                                                         
         BAS   RE,GETCOST          COST                                         
         B     XIT2                                                             
*--PUP UNITS AND BUDGET                                                         
NISTK20  BAS   RE,FILTPUP          PUP FILTERING                                
         BNE   NISTK40                                                          
         BAS   RE,DEMADJ                                                        
*                                                                               
         MVC   0(4,R3),MYIMPSE     IMPRESSIONS                                  
         MVC   4(4,R3),MYIMPSE                                                  
         MVC   10(2,R3),MYGRPSE    GRPS                                         
         MVC   14(2,R3),MYGRPSE    GRPS                                         
         MVI   GLARGS,0            HOMES                                        
         BAS   RE,DEMADJ                                                        
         MVC   16(4,R3),MYIMPSE                                                 
*                                                                               
         MVC   26(2,R3),NBSPCHRG   PUP UNITS                                    
         MVC   36(4,R3),NBCALCOS   PUP PLAN CPM GUARANTEE                       
*--SWAP LENGTH AND DATE FIELDS FOR FILTERING                                    
NISTK40  XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
*                                                                               
         XC    NBACTDAT,NBSDAFDT                                                
         XC    NBSDAFDT,NBACTDAT                                                
         XC    NBACTDAT,NBSDAFDT                                                
         BAS   RE,FILTPUP          PUP FILTERING (BUDGET IS SPECIAL)            
         BNE   NISTK60                                                          
         MVC   28(4,R3),NBACTUAL   PUP BUDGET                                   
         XC    32(4,R3),32(R3)                                                  
*--SWAP LENGTH AND DATE FIELDS BACK                                             
NISTK60  XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
*                                                                               
         XC    NBACTDAT,NBSDAFDT                                                
         XC    NBSDAFDT,NBACTDAT                                                
         XC    NBACTDAT,NBSDAFDT                                                
         B     XIT2                                                             
         SPACE 1                                                                
*                                  STACK - IMPS                                 
*              ACCUMULATORS        1-2 ESTIMATED IMPS, RAW EQUIV                
*                                  3-4 ESTIMATED COST, DOLLARS & CENTS          
*                                  5-6 ACTUAL IMPS, RAW EQUIV                   
*                                  7-8 ACTUAL COST, DOLLARS & CENTS             
*                                  9   UNITS  NETWORK AND ALL                   
         SPACE 1                                                                
NISTI    OI    GLARGS+12,X'02'     ESTIMATED                                    
*        MVI   GLARGS+12,C'E'      ESTIMATED                                    
         BAS   RE,FILTUNIT                                                      
         BNE   NISTI2                                                           
         BAS   RE,DEMADJ                                                        
         MVC   0(4,R3),MYIMPSR                                                  
         MVC   4(4,R3),MYIMPSE                                                  
         LA    R3,8(R3)                                                         
         BAS   RE,GETCOST          COST                                         
         SH    R3,=H'8'                                                         
         SPACE 1                                                                
NISTI2   LA    R3,16(R3)                                                        
*        MVI   GLARGS+12,C'A'      ACTUAL                                       
         NI    GLARGS+12,X'FF'-X'02' CLEAR ESTIMATE BIT                         
         OI    GLARGS+12,X'01'     ACTUAL                                       
         BAS   RE,FILTUNIT                                                      
         BNE   NISTI4                                                           
         BAS   RE,DEMADJ                                                        
         MVC   0(4,R3),MYIMPSR                                                  
         MVC   4(4,R3),MYIMPSE                                                  
         LA    R3,8(R3)                                                         
         BAS   RE,GETCOST          COST                                         
         SH    R3,=H'8'                                                         
         SPACE 1                                                                
NISTI4   SH    R3,=H'16'      **** ALWAYS USING ACTUAL COSTS ****               
         OC    24(4,R3),24(R3)     IF THERE                                     
         BZ    *+10                                                             
         MVC   8(8,R3),24(R3) **** DESPITE WHAT HAPPENED ABOVE **               
         OC    0(32,R3),0(R3)                                                   
         BZ    XIT2                                                             
         LA    R3,32(R3)           ACCUM 9 CARRIES UNITS                        
         BAS   RE,GETUNIT                                                       
         B     XIT2                                                             
         SPACE 1                                                                
*                                  STACK - RATINGS                              
NISTR    OI    GLARGS+12,X'02'     ESTIMATED                                    
*        MVI   GLARGS+12,C'E'      ESTIMATED                                    
         BAS   RE,FILTUNIT                                                      
         BNE   NISTR2                                                           
         BAS   RE,DEMADJ                                                        
         MVC   2(2,R3),MYGRPSR     GRPS                                         
         MVC   6(2,R3),MYGRPSE                                                  
         LA    R3,8(R3)                                                         
         BAS   RE,GETCOST          COST                                         
         SH    R3,=H'8'                                                         
         SPACE 1                                                                
NISTR2   LA    R3,16(R3)                                                        
*        MVI   GLARGS+12,C'A'            ACTUAL                                 
         NI    GLARGS+12,X'FF'-X'02'     CLEAR EST BIT                          
         OI    GLARGS+12,X'01'           ACTUAL                                 
         BAS   RE,FILTUNIT                                                      
         BNE   NISTR4                                                           
         BAS   RE,DEMADJ                                                        
         MVC   2(2,R3),MYGRPSR     GRPS                                         
         MVC   6(2,R3),MYGRPSE                                                  
         LA    R3,8(R3)                                                         
         BAS   RE,GETCOST          COST                                         
         SH    R3,=H'8'                                                         
         SPACE 1                                                                
NISTR4   SH    R3,=H'16'      **** ALWAYS USING ACTUAL COSTS ****               
         OC    24(4,R3),24(R3)     IF THERE                                     
         BZ    *+10                                                             
         MVC   8(8,R3),24(R3) **** DESPITE WHAT HAPPENED ABOVE **               
         LA    R3,16(R3)                                                        
         LA    R3,16(R3)                                                        
*        MVI   GLARGS+12,0                                                      
         NI    GLARGS+12,X'FF'-X'03'  CLEAR EST/ACT BITS                        
         BAS   RE,FILTGOAL                                                      
         BNE   XIT2                                                             
         BAS   RE,DEMADJ                                                        
         MVC   2(2,R3),MYGRPSR     GRPS                                         
         MVC   6(2,R3),MYGRPSE                                                  
         LA    R3,8(R3)                                                         
         BAS   RE,GETCOST          COST                                         
         B     XIT2                                                             
         SPACE 1                                                                
*                                  STACK - DOLLARS                              
NISTDOL  DS    0H                  ALWAYS USE ACTUAL COST NOW                   
***      MVI   GLARGS+12,C'A'                                                   
         BAS   RE,FILTUNIT                                                      
         BNE   NISTDOL2                                                         
         MVC   GETCOPT,NDE$OPT     (GET COST OPTIONAL PARAMETER)                
         BAS   RE,GETCOST          COST                                         
         SPACE 1                                                                
NISTDOL2 LA    R3,8(R3)                                                         
****     MVI   GLARGS+12,C'A'      ACTUAL                                       
         BAS   RE,FILTUNIT                                                      
         BNE   NISTDOL4                                                         
         MVC   GETCOPT,NDA$OPT     (GET COST OPTIONAL PARAMETER)                
         BAS   RE,GETCOST          COST                                         
         OC    0(4,R3),0(R3)                                                    
         BNZ   NISTDOL4                                                         
         TM    NBUNITST,X'20'      ACT COST OVERRIDE                            
         BNO   NISTDOL4                                                         
         MVC   4(4,R3),=4X'FF' CAUSING PROBLEMS REMOVED 4/9/92                  
         SPACE 1                                                                
NISTDOL4 LA    R3,8(R3)                                                         
         CLI   NDG$OPT,0                                                        
         BNE   NISTDOL6                                                         
**       MVI   GLARGS+12,0                                                      
         NI    GLARGS+12,X'FF'-X'03'   CLEAR EST/ACT BITS                       
         BAS   RE,FILTGOAL                                                      
         BNE   NISTDOL8                                                         
         BAS   RE,GETCOST          COST                                         
         SPACE 1                                                                
NISTDOL6 BAS   RE,FILTUNIT         IF G$ OPTION, TREAT AS UNIT                  
         BNE   NISTDOL8                                                         
         MVC   GETCOPT,NDG$OPT     (GET COST OPTIONAL PARAMETER)                
         BAS   RE,GETCOST          COST                                         
         SPACE 1                                                                
NISTDOL8 LA    R3,8(R3)                                                         
         CLI   NBMODE,NBPROCPP     HANDLE PUP BUDGET                            
         BNE   XIT2                                                             
*--SWAP LENGTH AND DATE FIELDS FOR FILTERING                                    
         XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
*                                                                               
         XC    NBACTDAT,NBSDAFDT                                                
         XC    NBSDAFDT,NBACTDAT                                                
         XC    NBACTDAT,NBSDAFDT                                                
*                                                                               
         BAS   RE,FILTPUP          PUP DOLLARS                                  
         BNE   *+16                                                             
         MVC   0(4,R3),NBACTUAL                                                 
         XC    4(4,R3),4(R3)                                                    
*                                                                               
*--SWAP LENGTH AND DATE FIELDS BACK                                             
         XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
*                                                                               
         XC    NBACTDAT,NBSDAFDT                                                
         XC    NBSDAFDT,NBACTDAT                                                
         XC    NBACTDAT,NBSDAFDT                                                
         B     XIT2                                                             
         SPACE 1                                                                
NISTDATA DS    0H                  DUMMY - JUST PASS SOMETHING                  
***      MVI   3(R3),1   THIS RETURNED EVERY REC WHETHER DATA OR NO             
         MVI   3(R3),0                                                          
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO INPUT SUBROUTINES                                           
         SPACE 3                                                                
GETUNIT  NTR1                                                                   
         CLI   NBMODE,NBPROCPP                                                  
         BE    GTUN1                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   XIT2                                                             
GTUN1    ZIC   R1,NBLEN            PICK UP SECONDS LENGTH                       
         LA    R1,8(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'15'           COMPUTE TO NEAREST 15 SECOND                 
         CLI   GLARGS+1,C'V'       (AVERAGE NEEDS UNITS)                        
         BNE   *+8                                                              
         LA    R1,1                                                             
         ST    R1,0(R3)            RETURN 15 SEC UNITS IN RIGHT HALF            
         CLI   NBSURVEY,C'N'       AND, IF MAIN NETWORK                         
         BE    GTUN2                                                            
         CLI   NBSURVEY,C'S'            OR SYNDICATION                          
         BNE   XIT2                                                             
         SPACE 1                                                                
GTUN2    STH   R1,0(R3)            RETURN IN LEFT HALF AS WELL                  
         B     XIT2                                                             
         SPACE 1                                                                
GETCOST  NTR1                                                                   
         CLI   NBMODE,NBPROCGL                                                  
         BE    GETGCOST                                                         
         CLI   NBMODE,NBPROCPP                                                  
         BE    GETPCOST                                                         
         CLI   NDFLAVOR,C'V'                                                    
         BNE   *+8                                                              
         MVI   SKIPAE,C'Y'                                                      
         BAS   RE,FILTUNIT                                                      
         MVI   SKIPAE,0                                                         
         BNE   XIT2                                                             
         L     R1,NBASSIGN         OVERRIDE OPTION TO USE                       
         TM    NDCPPOPT,X'01'      ASSIGNED                                     
         BO    GETCOST4                                                         
         L     R1,NBACTUAL         OVERRIDE OPTION TO USE                       
         TM    NDCPPOPT,X'02'      ACTUAL                                       
         BO    GETCOST4                                                         
         CLI   NBUSER+8,C'Y'       PROFILE OPTION TO USE ASSIGNED COST          
         BNE   GETCOST2                                                         
         L     R1,NBASSIGN                                                      
         SPACE 1                                                                
GETCOST2 CLI   NBUSER+15,C'Y'      OPTIONALLY PLUS INTEGRATION                  
         BNE   *+8                                                              
         A     R1,NBINTEG                                                       
         B     GETCOST6                                                         
         SPACE 1                                                                
GETCOST4 TM    NDCPPOPT,X'80'      OPTIONALLY PLUS INTEGRATION                  
         BNO   *+8                                                              
         A     R1,NBINTEG                                                       
         SPACE 1                                                                
GETCOST6 CLI   GETCOPT,0           (OPTION TO PASS ACCGEN PARAMETER)            
         BE    GETCOST8                                                         
         XC    DMCB(16),DMCB                                                    
         LA    RE,WORK                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),GETCOPT                                                  
         MVI   GETCOPT,0                                                        
         GOTO1 =V(NETACC),DMCB,,NETBLOCK                                        
         MVC   DUB,WORK+1                                                       
         CVB   R1,DUB                                                           
         SPACE 1                                                                
GETCOST8 BAS   RE,ADJCOST                                                       
         ST    R1,0(R3)            RETURN DOLLARS                               
         ST    R0,4(R3)                   AND CENTS                             
         B     XIT2                                                             
         SPACE 1                                                                
GETGCOST BAS   RE,FILTGOAL         GOAL DOLLARS                                 
         BNE   XIT2                                                             
         L     R5,NDAGBLOK                                                      
         USING NETGOALD,R5                                                      
         L     R1,NGOALDOL                                                      
*******  ST    R1,0(R3)                                                         
         M     R0,=F'100'                                                       
         BAS   RE,ADJCOST                                                       
         ST    R1,0(R3)            RETURN DOLLARS                               
         ST    R0,4(R3)                   AND CENTS                             
         XC    4(4,R3),4(R3)                                                    
         B     XIT2                                                             
         DROP  R5                                                               
         SPACE 1                                                                
GETPCOST BAS   RE,FILTPUP          PUP DOLLARS                                  
         BNE   XIT2                                                             
         L     R1,NBACTUAL                                                      
         ST    R1,0(R3)            RETURN DOLLARS                               
         ST    R0,4(R3)                   AND CENTS                             
         XC    4(4,R3),4(R3)                                                    
         B     XIT2                                                             
         SPACE 1                                                                
GETCOPT  DC    X'00'               OPTIONALLY PASS ACCGEN PARAM HERE            
         SPACE 1                                                                
*                                  R2=A(DOLLAR/CENT FULLWORDS)                  
OUTCOST  L     R0,4(R2)            PICK UP THE CENTS                            
         SRDA  R0,31                                                            
         D     R0,=F'100'          CONVERT TO NEAREST DOLLAR                    
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         A     R1,0(R2)            AND ADD THE DOLLARS                          
         ST    R1,MYCOST           SAVE THIS AND                                
         BR    RE                  RETURN DOLLARS IN R1                         
         SPACE 1                                                                
OTHCOST  BAS   RE,GETCOST          (CAME FROM ECOST IN SECTION 1)               
         L     R1,0(R3)            (RETURNS DOLLARS/CENTS)                      
         CLI   7(R3),50                                                         
         BL    *+8                                                              
         AH    R1,=H'1'                                                         
         CVD   R1,0(R3)                                                         
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO ADJUST FOR PERCENT OVERRIDES                          
         SPACE 3                                                                
*              INPUT               R1=CENTS                                     
*              OUTPUT              R0=ADJUSTED CENTS                            
*                                  R1=ADJUSTED DOLLARS                          
         SPACE 1                                                                
ADJCOST  OC    NDPERCNT,NDPERCNT   CHECK FOR PCT ADJUSTMENT                     
         BZ    ADJ4                                                             
         CLC   NDPERCNT,=C'COST'   (SOFT PERCENT OPTION)                        
         BE    ADJ3                                                             
         M     R0,NDPERCNT         APPLY                                        
         D     R0,=F'1000000'      AND SPLIT                                    
         BR    RE                                                               
         SPACE 1                                                                
ADJ3     DS    0H                  SOFT PERCENT                                 
         MVC   BYTE,NBSPLPRN       ASSUME SPLIT PROD NUMBER                     
         TM    NBSPLOPT,X'80'                                                   
         BO    *+10                                                             
         MVC   BYTE,NBPRD          UNLESS SPLIT NOT ON                          
         ZIC   RF,BYTE                                                          
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         A     RF,NDACPOOL         IN COST POOL TO 4 DEC PLACES                 
         M     R0,0(RF)            APPLY                                        
         D     R0,=F'100000000'                                                 
         BR    RE                                                               
         SPACE 1                                                                
ADJ4     M     R0,=F'1'            NO ADJUST                                    
         D     R0,=F'100'                                                       
         BR    RE                                                               
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - IMPS & GRPS                               
         SPACE 3                                                                
NOIMP    MVC   MYIMPSR,0(R2)       IMPRESSIONS                                  
         MVC   MYIMPSE,4(R2)                                                    
         CLI   GLARGS+1,C'V'       (AVERAGE OPTION)                             
         BNE   NOIMP2                                                           
         LH    RF,10(R2)           (UNITS)                                      
         LTR   RF,RF                                                            
         BZ    NOIMP2                                                           
         L     R1,MYIMPSR                                                       
         M     R0,=F'2'                                                         
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,MYIMPSR                                                       
         L     R1,MYIMPSE                                                       
         M     R0,=F'2'                                                         
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,MYIMPSE                                                       
         SPACE 1                                                                
NOIMP2   MVC   MYALL15,8(R2)                                                    
         LA    R2,12(R2)                                                        
         BAS   RE,OUTCOST                                                       
         ST    R1,MYCOST                                                        
         LA    R1,6                (EXPECTED WIDTH)                             
         BAS   RE,ADJOUT           (MAY BE DIFFERENT)                           
         BAS   RE,DOIMPS                                                        
         B     XIT2                                                             
         SPACE 1                                                                
NOGRP    MVC   MYGRPR4,0(R2)       GRPS                                         
         MVC   MYGRPE4,4(R2)                                                    
         MVC   MYALL15,8(R2)                                                    
         LA    R2,12(R2)                                                        
         BAS   RE,OUTCOST                                                       
         ST    R1,MYCOST                                                        
         LA    R1,6                (EXPECTED WIDTH)                             
         BAS   RE,ADJOUT           (MAY BE DIFFERENT)                           
         BAS   RE,DOGRPS                                                        
         B     XIT2                                                             
         SPACE 1                                                                
NOGCPP   MVC   MYGRPS,4(R2)        COST PER POINT                               
         LA    R2,8(R2)                                                         
         BAS   RE,OUTCOST                                                       
         BAS   RE,CPP                                                           
         SH    R2,=H'8'                                                         
         MVC   0(4,R2),MYRESULT                                                 
         B     XIT2                                                             
         SPACE 1                                                                
ADJOUT   ZIC   RF,MYOLEN2          ACTUAL OUTPUT WIDTH                          
         SR    RF,R1               R1=EXPECTED OUTPUT WIDTH                     
         AR    R3,RF               ADJUST OUTPUT POINTER                        
         BR    RE                  AND RETURN                                   
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - IMPS                                      
         SPACE 3                                                                
DOIMPS   NTR1                                                                   
         LA    R2,MYDEFLST         SET UP TO HANDLE LIST                        
         LA    R0,8                                                             
         SPACE 1                                                                
DOIMPLST MVC   BYTE,0(R2)                                                       
         NI    BYTE,X'0F'                                                       
         MVC   MYIMPS,MYIMPSR      PICK OFF RAW OR EQUIV                        
         TM    0(R2),X'80'         DICTATED BY X'80' BIT                        
         BNO   *+10                                                             
         MVC   MYIMPS,MYIMPSE                                                   
         BAS   RE,IMPDIV10                                                      
         CLI   BYTE,2                                                           
         BL    DOIMP1                                                           
         BE    DOIMP2                                                           
         CLI   BYTE,4                                                           
         BL    DOIMP3                                                           
         BE    DOIMP4                                                           
         B     DOIMP5                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
DOIMP1   BAS   RE,IMP6             IMPS=1                                       
         B     DOIMPNXT                                                         
         SPACE 1                                                                
DOIMP2   BAS   RE,CPM              CPM=2                                        
         B     DOIMPNXT                                                         
         SPACE 1                                                                
DOIMP3   DS    0H                  POST INDEX=3                                 
         ZIC   R1,GLARGS           INDEX INTO CPM POOL                          
         SLL   R1,2                                                             
         LA    R1,SAVEIMPS(R1)                                                  
         CLI   GLARGS+12,C'A'                                                   
         BE    DOIMP3B                                                          
* 'A' AND 'E' IS 01,02 IN DRARGSI BUT THIS IS DRARGSO                           
**       TM    GLARGS+12,X'01'                                                  
**       BO    DOIMP3B                                                          
         MVC   0(4,R1),MYIMPSE     SAVE ESTIMATED IMPS                          
         B     DOIMPNXT                                                         
         SPACE 1                                                                
DOIMP3B  MVC   DUB(4),0(R1)        PICK UP ESTIMATED IMPS                       
         MVC   DUB+4(4),MYIMPSE          AND ACTUAL IMPS                        
         LR    RF,R2                                                            
         LA    R2,DUB                                                           
         BAS   RE,INDEX            AND SHOW INDEX OF ACTUAL/EST IMPS            
         LR    R2,RF                                                            
         B     DOIMPNXT                                                         
         SPACE 1                                                                
DOIMP4   MVC   DUB,MYIMPSE         RAW V EQUIV INDEX =4                         
         MVC   DUB+4(4),MYIMPSR                                                 
         LR    RF,R2                                                            
         LA    R2,DUB                                                           
         BAS   RE,INDEX            AND SHOW INDEX OF ACTUAL/EST IMPS            
         LR    R2,RF                                                            
         B     DOIMPNXT                                                         
         SPACE 1                                                                
DOIMP5   MVI   0(R3),0             SPACE=5                                      
         SPACE 1                                                                
DOIMPNXT LA    R3,198(R3)                                                       
         CLC   0(6,R3),D2SPACES                                                 
         BNE   DOIMPNXT                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XIT2                                                             
         BCT   R0,DOIMPLST                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - GRPS (6 CHARACTERS)                       
         SPACE 3                                                                
DOGRPS   NTR1                                                                   
         LA    R2,MYDEFLST         SET UP TO HANDLE LIST                        
         LA    R0,8                                                             
         SPACE 1                                                                
DOGRPLST MVC   BYTE,0(R2)                                                       
         NI    BYTE,X'0F'                                                       
         XC    MYGRPS,MYGRPS                                                    
         MVC   MYGRPS(4),MYGRPR4   PICK OFF RAW OR EQUIV                        
         TM    0(R2),X'40'         DICTATED BY X'40' BIT                        
         BNO   *+10                                                             
         MVC   MYGRPS(4),MYGRPE4                                                
         CLI   BYTE,2                                                           
         BL    DOGRP1                                                           
         BE    DOGRP2                                                           
         CLI   BYTE,4                                                           
         BL    DOGRP3                                                           
         BE    DOGRP4                                                           
         B     DOGRP5                                                           
         SPACE 1                                                                
DOGRP1   BAS   RE,GRP6             GRPS=1                                       
         B     DOGRPNXT                                                         
         SPACE 1                                                                
DOGRP2   BAS   RE,CPP              CPP=2                                        
         B     DOGRPNXT                                                         
         SPACE 1                                                                
DOGRP3   DS    0H                  POST INDEX=3                                 
         ZIC   R1,GLARGS           INDEX INTO CPP POOL                          
         SLL   R1,2                                                             
         LA    R1,SAVEGRPS(R1)                                                  
         CLI   GLARGS+12,C'A'                                                   
         BE    DOGRP3B                                                          
* A AND E WENT TO 01 AND 02 ON DRARGSI BUT THESE ARE DRARGSO                    
**       TM    GLARGS+12,X'01'                                                  
**       BO    DOGRP3B                                                          
         MVC   0(4,R1),MYGRPE4     SAVE ESTIMATED GRPS                          
         B     DOGRPNXT                                                         
         SPACE 1                                                                
DOGRP3B  MVC   DUB(4),0(R1)        PICK UP ESTIMATED GRPS                       
         MVC   DUB+4(4),MYGRPE4            AND ACTUAL GRPS                      
         LR    RF,R2                                                            
         LA    R2,DUB                                                           
         BAS   RE,INDEX            AND SHOW INDEX OF ACTUAL/EST GRPS            
         LR    R2,RF                                                            
         B     DOGRPNXT                                                         
         SPACE 1                                                                
DOGRP4   MVC   DUB+4(4),MYGRPR4    RAW V EQUIV INDEX =4                         
         MVC   DUB(4),MYGRPE4                                                   
         LR    RF,R2                                                            
         LA    R2,DUB                                                           
         BAS   RE,INDEX            AND SHOW INDEX OF ACTUAL/EST GRPS            
         LR    R2,RF                                                            
         B     DOGRPNXT                                                         
         SPACE 1                                                                
DOGRP5   MVI   0(R3),0             SPACE=5                                      
         SPACE 1                                                                
DOGRPNXT LA    R3,198(R3)                                                       
         CLC   0(6,R3),D2SPACES                                                 
         BNE   DOGRPNXT                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XIT2                                                             
         BCT   R0,DOGRPLST                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - GRPS (4 CHARACTERS)                       
         SPACE 3                                                                
DOGRPS4  NTR1                                                                   
         LA    R2,MYDEFLST         SET UP TO HANDLE LIST                        
         LA    R0,8                                                             
         SPACE 1                                                                
DOGR4LST MVC   BYTE,0(R2)                                                       
         NI    BYTE,X'0F'                                                       
         MVC   MYGRPS(4),MYGRPR4   PICK OFF RAW OR EQUIV                        
         TM    0(R2),X'40'         DICTATED BY X'40' BIT                        
         BNO   *+10                                                             
         MVC   MYGRPS(4),MYGRPE4                                                
         CLI   BYTE,2                                                           
         BL    DOGR41                                                           
         BE    DOGR42                                                           
         CLI   BYTE,4                                                           
         BL    DOGR43                                                           
         BE    DOGR44                                                           
         B     DOGR45                                                           
         SPACE 1                                                                
DOGR41   BAS   RE,GRP4             GRPS=1                                       
         B     DOGR4NXT                                                         
         SPACE 1                                                                
DOGR42   BAS   RE,CPP4             CPP=2                                        
         B     DOGR4NXT                                                         
         SPACE 1                                                                
DOGR43   DS    0H                  POST INDEX=3                                 
DOGR44   DS    0H                  EQUIV INDEX=4                                
DOGR45   MVI   0(R3),0             SPACE=5                                      
         SPACE 1                                                                
DOGR4NXT LA    R3,198(R3)                                                       
         CLC   0(4,R3),D2SPACES                                                 
         BNE   DOGR4NXT                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XIT2                                                             
         BCT   R0,DOGR4LST                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - INDEX CPP CPM CPU                         
         SPACE 3                                                                
NORX     DS    0H                  RATING INDEX                                 
NOIX     LA    R2,4(R2)            IMPRESSION INDEX                             
         BAS   RE,DOINDEX                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NOCPM    MVC   MYIMPS,4(R2)        COST PER THOUSAND                            
         BAS   RE,IMPDIV10                                                      
         LA    R2,8(R2)                                                         
         BAS   RE,OUTCOST                                                       
         BAS   RE,CPM                                                           
         SH    R2,=H'8'                                                         
         MVC   0(4,R2),MYRESULT                                                 
         CLC   =X'40000000',0(R2)  IF 'HIGH' BIT ON                             
         BNE   XIT2                                                             
         MVI   0(R2),0             TURN OFF/ELSE COMPUTES GO NUTSO              
         B     XIT2                                                             
         SPACE 1                                                                
NOCPP    MVC   MYGRPS,4(R2)        COST PER POINT                               
         LA    R2,8(R2)                                                         
         BAS   RE,OUTCOST                                                       
         BAS   RE,CPP                                                           
         SH    R2,=H'8'                                                         
         MVC   0(4,R2),MYRESULT                                                 
         B     XIT2                                                             
         SPACE 1                                                                
NOCPU    MVC   MYUNITS,4(R2)       COST PER UNIT                                
         LA    R2,8(R2)                                                         
         BAS   RE,OUTCOST                                                       
         BAS   RE,CPU                                                           
         SH    R2,=H'8'                                                         
         MVC   0(4,R2),MYRESULT                                                 
         B     XIT2                                                             
         SPACE 1                                                                
NORPU    MVC   MYUNITS,4(R2)       RATINGS PER UNIT                             
         MVC   MYGRPS,8(R2)                                                     
         BAS   RE,RPU                                                           
         MVC   0(4,R2),MYRESULT                                                 
         B     XIT2                                                             
         SPACE 1                                                                
NOIPU    MVC   MYUNITS,4(R2)       RATINGS PER UNIT                             
         MVC   MYIMPS,8(R2)                                                     
         BAS   RE,IPU                                                           
         MVC   0(4,R2),MYRESULT                                                 
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO STACK ROUTINES                                              
         SPACE 3                                                                
NOSTACK  MVC   MYIMPS,0(R2)                                                     
         TM    NBINDS,X'40'        IMP PRECISION                                
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         MVC   MYGRPS,8(R2)                                                     
         MVC   MYHOMES,16(R2)                                                   
         TM    NBINDS,X'40'        IMP PRECISION                                
         BZ    NOSTACK1                                                         
         MVC   WORK(4),MYIMPS      SAVE MYIMPS                                  
         MVC   MYIMPS,MYHOMES      SET TO DO HOMES                              
         BAS   RE,IMPDIV10                                                      
         MVC   MYHOMES,MYIMPS      RESET HOMES                                  
         MVC   MYIMPS,WORK         RESET IMPS                                   
NOSTACK1 MVC   MYALL15,20(R2)                                                   
         MVC   MYUNITS,24(R2)                                                   
         MVC   MYECOST,36(R2)      PUP PLAN CPM GUARANTEE                       
         LA    R2,28(R2)                                                        
         BAS   RE,OUTCOST                                                       
         ST    R1,MYCOST                                                        
         LA    R1,6                (EXPECTED WIDTH)                             
         BAS   RE,ADJOUT           (MAY BE DIFFERENT)                           
         LA    R2,NDSTADEF                                                      
         LA    R4,8                                                             
         SPACE 1                                                                
NOSTACK2 CLI   0(R2),0             FIRST BYTE 0=ALL                             
         BE    NOSTACK6                                                         
         TM    0(R2),X'10'          GRAND TOTAL ONLY                            
         BNO   NOSTACK3                                                         
         CLI   GLLEVEL,0                                                        
         BNE   NOSTNXT2                                                         
NOSTACK3 TM    0(R2),X'80'                    X'80'=DETAILS ONLY                
         BNO   NOSTACK4                                                         
         TM    GLINDS,GLTOTLIN                                                  
         BO    NOSTNXT2                                                         
         B     NOSTACK6                                                         
         SPACE 1                                                                
NOSTACK4 TM    GLINDS,GLTOTLIN                X'40'=TOTALS ONLY                 
         BNO   NOSTNXT2                                                         
         SPACE 1                                                                
NOSTACK6 DS    0H                  SECOND BYTE HAS ROUTINE NUMBER               
         CLI   1(R2),0                                                          
         BE    XIT2                                                             
         CLI   1(R2),2                                                          
         BL    NOSTIMP                                                          
         BE    NOSTGRP                                                          
         CLI   1(R2),4                                                          
         BL    NOSTCPM                                                          
         BE    NOSTCPP                                                          
         CLI   1(R2),6                                                          
         BL    NOSTVPH                                                          
         BE    NOSTCOST                                                         
         CLI   1(R2),8                                                          
         BL    NOSTUNIT                                                         
         BE    NOSTCPU                                                          
         CLI   1(R2),10                                                         
         BL    NOSTRPU                                                          
         BE    NOSTSPAC                                                         
         CLI   1(R2),11                                                         
         BE    NOSTIPU                                                          
         CLI   1(R2),34                                                         
         BE    NOSTCOST                                                         
         CLI   1(R2),35                                                         
         BE    NOSTGCPM                                                         
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTIMP  BAS   RE,IMP6                                                          
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTGRP  BAS   RE,GRP6                                                          
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTCPM  BAS   RE,CPM                                                           
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTCPP  BAS   RE,CPP                                                           
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTVPH  CLI   GLARGS,0            (NOT VPH FOR HOMES)                          
         BE    NOSTNEXT                                                         
         BAS   RE,VPH6                                                          
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTCOST DS    0H                                                               
         OC    MYCOST,MYCOST                                                    
         BNZ   NOSTCST2                                                         
         TM    NDDOWNL,X'80'                                                    
         BNO   *+12                                                             
         MVI   0(R3),C'0'                                                       
         B     NOSTNEXT                                                         
         TM    GLINDS,X'02'        ZERO=YES                                     
         BNO   NOSTNEXT                                                         
         SPACE 1                                                                
NOSTCST2 EDIT  (4,MYCOST),(9,DMCB),FLOAT=$                                      
         MVC   0(6,R3),DMCB+3      (UP TO $1M OK)                               
         CLC   DMCB(2),D2SPACES                                                 
         BE    NOSTNEXT                                                         
         MVC   0(6,R3),D2SPACES                                                 
         SH    R3,=H'3'                                                         
         CLC   0(3,R3),D2SPACES                                                 
         BE    *+8                                                              
         LA    R3,198(R3)                                                       
         MVC   0(9,R3),DMCB                                                     
         LA    R3,3(R3)                                                         
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTGCPM OC    MYECOST,MYECOST                                                  
         BNZ   NOSTGCP2                                                         
         TM    NDDOWNL,X'80'                                                    
         BNO   NOSTNEXT                                                         
         MVI   0(R3),C'0'                                                       
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTGCP2 EDIT  (4,MYECOST),(9,DMCB),2,FLOAT=$                                   
         MVC   0(6,R3),DMCB+3      (UP TO $1M OK)                               
         CLC   DMCB(2),D2SPACES                                                 
         BE    NOSTNEXT                                                         
         MVC   0(6,R3),D2SPACES                                                 
         SH    R3,=H'3'                                                         
         CLC   0(3,R3),D2SPACES                                                 
         BE    *+8                                                              
         LA    R3,198(R3)                                                       
         MVC   0(9,R3),DMCB                                                     
         LA    R3,3(R3)                                                         
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTUNIT EDIT  (4,MYUNITS),(6,(R3))      UNITS                                  
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTCPU  BAS   RE,CPU                                                           
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTRPU  BAS   RE,RPU              RATINGS PER UNIT                             
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTIPU  BAS   RE,IPU              RATINGS PER UNIT                             
         B     NOSTNEXT                                                         
         SPACE 1                                                                
NOSTSPAC MVI   0(R3),0             SPACE                                        
         SPACE 1                                                                
NOSTNEXT LA    R3,198(R3)                                                       
         CLC   0(6,R3),D2SPACES                                                 
         BNE   NOSTNEXT                                                         
         SPACE 1                                                                
NOSTNXT2 LA    R2,2(R2)                                                         
         BCT   R4,NOSTACK2                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              IMPRESSION STACK                                                 
         SPACE 3                                                                
*              ACCUMULATORS        1-2 ESTIMATED IMPS, RAW EQUIV                
*                                  3-4 ESTIMATED COST, DOLLARS & CENTS          
*                                  5-6 ACTUAL IMPS, RAW EQUIV                   
*                                  7-8 ACTUAL COST, DOLLARS & CENTS             
*                                  9   UNITS  NETWORK AND ALL                   
         SPACE 1                                                                
NOSTI    TM    NBINDS,X'40'        PRIMP (IMPRESSION PRECISION)                 
         BZ    NOSTI0                                                           
         MVC   MYIMPS,0(R2)        RAW EST IMPS                                 
         BAS   RE,IMPDIV10                                                      
         MVC   0(4,R2),MYIMPS                                                   
         MVC   MYIMPS,4(R2)        EQUIV EST IMPS                               
         BAS   RE,IMPDIV10                                                      
         MVC   4(4,R2),MYIMPS                                                   
         XC    MYIMPS,MYIMPS                                                    
NOSTI0   XC    LAST,LAST                                                        
         XC    THIS,THIS                                                        
         MVC   MYALL15,32(R2)      ACCUM 9 = UNITS                              
         LA    R1,6                                                             
         BAS   RE,ADJOUT           WIDTH ADJUSTMENT                             
         LA    R2,NDSTADEF                                                      
         LA    R4,8                                                             
         SPACE 1                                                                
NOSTI2   ST    R2,SAVER2                                                        
         CLI   0(R2),0             FIRST BYTE 0=ALL                             
         BE    NOSTI6                                                           
         TM    0(R2),X'10'         GRAND TOTAL ONLY                             
         BNO   NOSTI3                                                           
         CLI   GLLEVEL,0                                                        
         BNE   NOSTINX2                                                         
NOSTI3   TM    0(R2),X'80'                    X'80'=DETAILS ONLY                
         BNO   NOSTI4                                                           
         TM    GLINDS,GLTOTLIN                                                  
         BO    NOSTINX2                                                         
         B     NOSTI6                                                           
         SPACE 1                                                                
NOSTI4   TM    GLINDS,GLTOTLIN                X'40'=TOTALS ONLY                 
         BNO   NOSTINX2                                                         
         SPACE 1                                                                
NOSTI6   DS    0H                  SECOND BYTE HAS ROUTINE NUMBER               
         CLI   1(R2),0                                                          
         BE    XIT2                                                             
         CLI   1(R2),10                                                         
         BE    NOSTISPA                                                         
         CLI   1(R2),21                                                         
         BE    NOSTIE                                                           
         CLI   1(R2),22                                                         
         BE    NOSTIA                                                           
         CLI   1(R2),23                                                         
         BE    NOSTIG                                                           
         CLI   1(R2),24                                                         
         BE    NOSTIEC                                                          
         CLI   1(R2),25                                                         
         BE    NOSTIAC                                                          
         CLI   1(R2),26                                                         
         BE    NOSTISPA                                                         
         CLI   1(R2),27                                                         
         BE    NOSTIER                                                          
         CLI   1(R2),28                                                         
         BE    NOSTIAR                                                          
         CLI   1(R2),29                                                         
         BE    NOSTIERC                                                         
         CLI   1(R2),30                                                         
         BE    NOSTIARC                                                         
         CLI   1(R2),31                                                         
         BE    NOSTID                                                           
         CLI   1(R2),32                                                         
         BE    NOSTII                                                           
         CLI   1(R2),33                                                         
         BE    NOSTII                                                           
         B     NOSTINX2                                                         
         SPACE 1                                                                
NOSTISPA MVI   0(R3),0             SPACE                                        
         B     NOSTINXT                                                         
         SPACE 1                                                                
*                                  IMPRESSIONS                                  
NOSTIE   L     R2,GLAIFLD          EEST                                         
         MVC   MYIMPS,4(R2)                                                     
         B     NOSTIIMP                                                         
         SPACE 1                                                                
NOSTIER  L     R2,GLAIFLD          REST                                         
         MVC   MYIMPS,0(R2)                                                     
         B     NOSTIIMP                                                         
         SPACE 1                                                                
NOSTIA   L     R2,GLAIFLD          EACT                                         
         LA    R2,16(R2)                                                        
         MVC   MYIMPS,4(R2)                                                     
         B     NOSTIIMP                                                         
         SPACE 1                                                                
NOSTIAR  L     R2,GLAIFLD          RACT                                         
         LA    R2,16(R2)                                                        
         MVC   MYIMPS,0(R2)                                                     
         SPACE 1                                                                
NOSTIIMP MVC   LAST,THIS                                                        
         MVC   THIS,MYIMPS                                                      
         BAS   RE,IMP6                                                          
         B     NOSTINXT                                                         
         SPACE 1                                                                
*                                  CPM EXPRESSIONS                              
NOSTIEC  L     R2,GLAIFLD          EEST                                         
         MVC   MYIMPS,4(R2)                                                     
         B     NOSTICPM                                                         
         SPACE 1                                                                
NOSTIERC L     R2,GLAIFLD          REST                                         
         MVC   MYIMPS,0(R2)                                                     
         B     NOSTICPM                                                         
         SPACE 1                                                                
NOSTIAC  L     R2,GLAIFLD          EEST                                         
         LA    R2,16(R2)                                                        
         MVC   MYIMPS,4(R2)                                                     
         B     NOSTICPM                                                         
         SPACE 1                                                                
NOSTIARC L     R2,GLAIFLD          REST                                         
         LA    R2,16(R2)                                                        
         MVC   MYIMPS,0(R2)                                                     
         SPACE 1                                                                
NOSTICPM LA    R2,8(R2)                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,MYCOST                                                        
         BAS   RE,CPM                                                           
         MVC   LAST,THIS                                                        
         MVC   THIS,MYRESULT                                                    
         NI    THIS,X'FF'-X'40'    TURN OFF X'40' BIT SET BY CPM                
         B     NOSTINXT                                                         
         SPACE 1                                                                
NOSTIG   MVC   LAST,THIS           'GOAL' IMPS                                  
         XC    THIS,THIS                                                        
         B     NOSTISPA                                                         
         SPACE 1                                                                
NOSTID   L     R1,THIS             DIFFERENCE                                   
         S     R1,LAST                                                          
         ST    R1,MYIMPS                                                        
         BZ    NOSTINXT                                                         
         BM    NOSTIDM                                                          
         BAS   RE,IMP6                                                          
         BAS   RE,POSFLOAT                                                      
         B     NOSTINXT                                                         
         SPACE 1                                                                
NOSTIDM  LCR   R1,R1                                                            
         ST    R1,MYIMPS                                                        
         BAS   RE,IMP6                                                          
         BAS   RE,MINFLOAT                                                      
         B     NOSTINXT                                                         
         SPACE 1                                                                
NOSTII   OC    LAST,LAST           INDEX                                        
         BZ    NOSTINXT                                                         
         OC    THIS,THIS                                                        
         BZ    NOSTINXT                                                         
         MVC   DUB(4),LAST                                                      
         MVC   DUB+4(4),THIS                                                    
         LA    R2,DUB                                                           
         BAS   RE,INDEX                                                         
         B     NOSTINXT                                                         
         SPACE 1                                                                
NOSTINXT LA    R3,198(R3)                                                       
         CLC   0(6,R3),D2SPACES                                                 
         BNE   NOSTINXT                                                         
         SPACE 1                                                                
NOSTINX2 L     R2,SAVER2                                                        
         LA    R2,2(R2)                                                         
         BCT   R4,NOSTI2                                                        
         B     XIT2                                                             
         SPACE 1                                                                
MINFLOAT MVI   THISSIGN,C'-'                                                    
         B     FLOAT                                                            
         SPACE 1                                                                
POSFLOAT MVI   THISSIGN,C'+'                                                    
         SPACE 1                                                                
FLOAT    LR    R1,R3                                                            
         SPACE 1                                                                
FLOAT2   CLI   0(R1),C' '                                                       
         BNH   FLOAT4                                                           
         BCT   R1,FLOAT2                                                        
         SPACE 1                                                                
FLOAT4   CLI   1(R1),C' '                                                       
         BNE   FLOAT6                                                           
         LA    R1,1(R1)                                                         
         B     FLOAT4                                                           
         SPACE 1                                                                
FLOAT6   MVC   0(1,R1),THISSIGN                                                 
         BR    RE                                                               
         SPACE 1                                                                
THIS     DS    F                                                                
LAST     DS    F                                                                
SAVER2   DS    A                                                                
THISSIGN DS    CL1                                                              
         EJECT                                                                  
*              RATING STACK                                                     
         SPACE 3                                                                
NOSTR    XC    LAST,LAST                                                        
         XC    THIS,THIS                                                        
         LA    R1,6                                                             
         BAS   RE,ADJOUT           WIDTH ADJUSTMENT                             
         LA    R2,NDSTADEF                                                      
         LA    R4,8                                                             
         SPACE 1                                                                
NOSTR2   ST    R2,SAVER2                                                        
         CLI   0(R2),0             FIRST BYTE 0=ALL                             
         BE    NOSTR6                                                           
         TM    0(R2),X'10'         GRAND TOTAL ONLY                             
         BNO   NOSTR3                                                           
         CLI   GLLEVEL,0                                                        
         BNE   NOSTRNX2                                                         
NOSTR3   TM    0(R2),X'80'                    X'80'=DETAILS ONLY                
         BNO   NOSTR4                                                           
         TM    GLINDS,GLTOTLIN                                                  
         BO    NOSTRNX2                                                         
         B     NOSTR6                                                           
         SPACE 1                                                                
NOSTR4   TM    GLINDS,GLTOTLIN                X'40'=TOTALS ONLY                 
         BNO   NOSTRNX2                                                         
         SPACE 1                                                                
NOSTR6   DS    0H                  SECOND BYTE HAS ROUTINE NUMBER               
         CLI   1(R2),0                                                          
         BE    XIT2                                                             
         CLI   1(R2),10                                                         
         BE    NOSTRSPA                                                         
         CLI   1(R2),21                                                         
         BE    NOSTRE                                                           
         CLI   1(R2),22                                                         
         BE    NOSTRA                                                           
         CLI   1(R2),23                                                         
         BE    NOSTRG                                                           
         CLI   1(R2),24                                                         
         BE    NOSTREC                                                          
         CLI   1(R2),25                                                         
         BE    NOSTRAC                                                          
         CLI   1(R2),26                                                         
         BE    NOSTRGC                                                          
         CLI   1(R2),27                                                         
         BE    NOSTRER                                                          
         CLI   1(R2),28                                                         
         BE    NOSTRAR                                                          
         CLI   1(R2),29                                                         
         BE    NOSTRERC                                                         
         CLI   1(R2),30                                                         
         BE    NOSTRARC                                                         
         CLI   1(R2),31                                                         
         BE    NOSTRD                                                           
         CLI   1(R2),32                                                         
         BE    NOSTRI                                                           
         CLI   1(R2),33                                                         
         BE    NOSTRI                                                           
         B     NOSTRNX2                                                         
         SPACE 1                                                                
NOSTRSPA MVI   0(R3),0             SPACE                                        
         B     NOSTRNXT                                                         
         SPACE 1                                                                
*                                  GRP EXPRESSIONS                              
NOSTRE   L     R2,GLAIFLD          EEST                                         
         MVC   MYGRPS,4(R2)                                                     
         B     NOSTRGRP                                                         
         SPACE 1                                                                
NOSTRER  L     R2,GLAIFLD          REST                                         
         MVC   MYGRPS,0(R2)                                                     
         B     NOSTRGRP                                                         
         SPACE 1                                                                
NOSTRA   L     R2,GLAIFLD          EACT                                         
         LA    R2,16(R2)                                                        
         MVC   MYGRPS,4(R2)                                                     
         B     NOSTRGRP                                                         
         SPACE 1                                                                
NOSTRAR  L     R2,GLAIFLD          RACT                                         
         LA    R2,16(R2)                                                        
         MVC   MYGRPS,0(R2)                                                     
         B     NOSTRGRP                                                         
         SPACE 1                                                                
NOSTRG   L     R2,GLAIFLD          GOAL                                         
         LA    R2,32(R2)                                                        
         MVC   MYGRPS,4(R2)                                                     
         SPACE 1                                                                
NOSTRGRP MVC   LAST,THIS                                                        
         MVC   THIS,MYGRPS                                                      
         BAS   RE,GRP6                                                          
         B     NOSTRNXT                                                         
         SPACE 1                                                                
*                                  CPP EXPRESSIONS                              
NOSTREC  L     R2,GLAIFLD          EEST                                         
         MVC   MYGRPS,4(R2)                                                     
         B     NOSTRCPP                                                         
         SPACE 1                                                                
NOSTRERC L     R2,GLAIFLD          REST                                         
         MVC   MYGRPS,0(R2)                                                     
         B     NOSTRCPP                                                         
         SPACE 1                                                                
NOSTRAC  L     R2,GLAIFLD          REST                                         
         LA    R2,16(R2)                                                        
         MVC   MYGRPS,4(R2)                                                     
         B     NOSTRCPP                                                         
         SPACE 1                                                                
NOSTRARC L     R2,GLAIFLD          RACT                                         
         LA    R2,16(R2)                                                        
         MVC   MYGRPS,0(R2)                                                     
         B     NOSTRCPP                                                         
         SPACE 1                                                                
NOSTRGC  L     R2,GLAIFLD          GOAL                                         
         LA    R2,32(R2)                                                        
         MVC   MYGRPS,4(R2)                                                     
         B     NOSTRCPP                                                         
         SPACE 1                                                                
NOSTRCPP LA    R2,8(R2)                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,MYCOST                                                        
         BAS   RE,CPP                                                           
         MVC   LAST,THIS                                                        
         MVC   THIS,MYRESULT                                                    
         NI    THIS,X'FF'-X'40'    TURN OFF X'40' BIT SET BY CPM                
         B     NOSTRNXT                                                         
         SPACE 1                                                                
         SPACE 1                                                                
NOSTRD   L     R1,THIS             DIFFERENCE                                   
         S     R1,LAST                                                          
         ST    R1,MYGRPS                                                        
         BZ    NOSTRNXT                                                         
         BM    NOSTRDM                                                          
         MVI   GRPSIGN,C'+'                                                     
         BAS   RE,GRP6                                                          
         BAS   RE,POSFLOAT                                                      
         B     NOSTRNXT                                                         
         SPACE 1                                                                
NOSTRDM  LCR   R1,R1                                                            
         ST    R1,MYGRPS                                                        
         MVI   GRPSIGN,C'-'                                                     
         BAS   RE,GRP6                                                          
         BAS   RE,MINFLOAT                                                      
         B     NOSTRNXT                                                         
         SPACE 1                                                                
NOSTRI   OC    LAST,LAST           INDEX                                        
         BZ    NOSTRNXT                                                         
         OC    THIS,THIS                                                        
         BZ    NOSTRNXT                                                         
         MVC   DUB(4),LAST                                                      
         MVC   DUB+4(4),THIS                                                    
         LA    R2,DUB                                                           
         BAS   RE,INDEX                                                         
         B     NOSTRNXT                                                         
         SPACE 1                                                                
NOSTRNXT LA    R3,198(R3)                                                       
         CLC   0(6,R3),D2SPACES                                                 
         BNE   NOSTRNXT                                                         
         SPACE 1                                                                
NOSTRNX2 L     R2,SAVER2                                                        
         LA    R2,2(R2)                                                         
         BCT   R4,NOSTR2                                                        
         B     XIT2                                                             
         EJECT                                                                  
*              DOLLAR STACK                                                     
         SPACE 3                                                                
NOSTDOL  XC    LAST,LAST                                                        
         XC    THIS,THIS                                                        
****     LA    R1,9                                                             
****     BAS   RE,ADJOUT           WIDTH ADJUSTMENT                             
         LA    R2,NDSTADEF                                                      
         LA    R4,8                                                             
         SPACE 1                                                                
NOSTD2   ST    R2,SAVER2                                                        
         CLI   0(R2),0             FIRST BYTE 0=ALL                             
         BE    NOSTD6                                                           
         TM    0(R2),X'10'         GRAND TOTAL ONLYU                            
         BNO   NOSTD3                                                           
         CLI   GLLEVEL,0                                                        
         BNE   NOSTDNX2                                                         
NOSTD3   TM    0(R2),X'80'                    X'80'=DETAILS ONLY                
         BNO   NOSTD4                                                           
         TM    GLINDS,GLTOTLIN                                                  
         BO    NOSTDNX2                                                         
         B     NOSTD6                                                           
         SPACE 1                                                                
NOSTD4   TM    GLINDS,GLTOTLIN                X'40'=TOTALS ONLY                 
         BNO   NOSTDNX2                                                         
         SPACE 1                                                                
NOSTD6   DS    0H                  SECOND BYTE HAS ROUTINE NUMBER               
         CLI   1(R2),0                                                          
         BE    XIT2                                                             
         CLI   1(R2),10                                                         
         BE    NOSTDSPA                                                         
         CLI   1(R2),21                                                         
         BE    NOSTDE                                                           
         CLI   1(R2),22                                                         
         BE    NOSTDA                                                           
         CLI   1(R2),23                                                         
         BE    NOSTDG                                                           
         CLI   1(R2),31                                                         
         BE    NOSTDD                                                           
         CLI   1(R2),32                                                         
         BE    NOSTDI                                                           
         CLI   1(R2),34                                                         
         BE    NOSTDB                                                           
         SPACE 1                                                                
NOSTDSPA MVI   0(R3),0             SPACE                                        
         B     NOSTDNXT                                                         
         SPACE 1                                                                
NOSTDE   L     R2,GLAIFLD          ESTIMATED DOLLARS                            
         SPACE 1                                                                
NOSTDCST MVC   LAST,THIS                                                        
         BAS   RE,OUTCOST                                                       
         ST    R1,THIS                                                          
         SPACE 1                                                                
NOSTDCS2 BAS   RE,DOWNZERO                                                      
         CLI   0(R3),C'0'          DOWNLOAD ZERO GETS OUT HERE                  
         BE    NOSTDNXT                                                         
         LR    RF,R1                                                            
         EDIT  (RF),(6,(R3)),FLOAT=-                                            
         CLI   MYOLEN2,6                                                        
         BE    NOSTDNXT                                                         
         EDIT  (RF),(7,(R3)),FLOAT=-                                            
         CLI   MYOLEN2,7                                                        
         BE    NOSTDNXT                                                         
         EDIT  (RF),(8,(R3)),FLOAT=-                                            
         CLI   MYOLEN2,8                                                        
         BE    NOSTDNXT                                                         
         EDIT  (RF),(9,(R3)),FLOAT=-                                            
         B     NOSTDNXT                                                         
         SPACE 1                                                                
NOSTDA   L     R2,GLAIFLD          ACTUAL DOLLARS                               
         LA    R2,8(R2)                                                         
         CLC   4(4,R2),=4X'FF'     IF ACTUAL COST OVERRIDE                      
         BNE   NOSTDCST                                                         
         XC    4(4,R2),4(R2)       HANDLE IT HERE                               
         OC    0(4,R2),0(R2)       UNLESS THERE ARE DOLLARS IN BUCKET           
         BNZ   NOSTDCST                                                         
         ZIC   R1,MYOLEN2                                                       
         AR    R1,R3                                                            
         BCTR  R1,0                                                             
         MVI   0(R1),C'0'                                                       
         MVC   LAST,THIS                                                        
         XC    THIS,THIS                                                        
         B     NOSTDNXT                                                         
         SPACE 1                                                                
NOSTDG   L     R2,GLAIFLD          GOAL DOLLARS                                 
         LA    R2,16(R2)                                                        
         B     NOSTDCST                                                         
         SPACE 1                                                                
NOSTDB   L     R2,GLAIFLD          PUP BUDGET DOLLARS                           
         LA    R2,24(R2)                                                        
         B     NOSTDCST                                                         
         SPACE 1                                                                
NOSTDD   L     R1,THIS             DIFFERENCE                                   
         S     R1,LAST                                                          
         BNP   NOSTDCS2                                                         
         LR    RF,R1                                                            
         EDIT  (RF),(6,(R3)),FLOAT=+                                            
         CLI   MYOLEN2,6                                                        
         BE    NOSTDNXT                                                         
         EDIT  (RF),(7,(R3)),FLOAT=+                                            
         CLI   MYOLEN2,7                                                        
         BE    NOSTDNXT                                                         
         EDIT  (RF),(8,(R3)),FLOAT=+                                            
         CLI   MYOLEN2,8                                                        
         BE    NOSTDNXT                                                         
         EDIT  (RF),(9,(R3)),FLOAT=+                                            
         B     NOSTDNXT                                                         
         SPACE 1                                                                
NOSTDI   OC    LAST,LAST           INDEX                                        
         BZ    NOSTDNXT                                                         
         OC    THIS,THIS                                                        
         BZ    NOSTDNXT                                                         
         MVC   DUB(4),LAST                                                      
         MVC   DUB+4(4),THIS                                                    
         LA    R2,DUB                                                           
         ZIC   R1,MYOLEN2                                                       
         SH    R1,=H'6'                                                         
         AR    R3,R1                                                            
         BAS   RE,INDEX                                                         
         SR    R3,R1                                                            
         B     NOSTDNXT                                                         
         SPACE 1                                                                
NOSTDNXT LA    R3,198(R3)                                                       
         CLC   0(6,R3),D2SPACES                                                 
         BNE   NOSTDNXT                                                         
         SPACE 1                                                                
NOSTDNX2 L     R2,SAVER2                                                        
         LA    R2,2(R2)                                                         
         BCT   R4,NOSTD2                                                        
         B     XIT2                                                             
         EJECT                                                                  
*              STACK DATA DEFINITION                                            
         SPACE 3                                                                
NOSTDATA LA    R4,NDSTADEF                                                      
         LA    R0,8                                                             
         SPACE 1                                                                
NOSTDA2  CLI   0(R4),0             FIRST BYTE 0=ALL                             
         BE    NOSTDA6                                                          
         TM    0(R4),X'10'         GRAND TOT ONLY                               
         BNO   NOSTDA3                                                          
         CLI   GLLEVEL,0                                                        
         BNE   NOSTDAX                                                          
NOSTDA3  TM    0(R4),X'80'                    X'80'=DETAILS ONLY                
         BNO   NOSTDA4                                                          
         TM    GLINDS,GLTOTLIN                                                  
         BO    NOSTDAX                                                          
         B     NOSTDA6                                                          
         SPACE 1                                                                
NOSTDA4  TM    GLINDS,GLTOTLIN                X'40'=TOTALS ONLY                 
         BNO   NOSTDAX                                                          
         SPACE 1                                                                
NOSTDA6  CLI   1(R4),0                                                          
         BE    XIT2                                                             
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
*        LA    R1,NOSTDLST(R1)                                                  
         L     RE,=A(NOSTDLST)     *                                            
         AR    R1,RE               *                                            
         MVC   0(4,R3),0(R1)                                                    
         LA    R3,198(R3)                                                       
         SPACE 1                                                                
NOSTDAX  LA    R4,2(R4)                                                         
         BCT   R0,NOSTDA2                                                       
         B     XIT2                                                             
         EJECT                                                                  
*              ACCOUNTING STACK INPUT                                           
         SPACE 3                                                                
NIAST    LA    R0,8                                                             
         LR    R1,R3                                                            
         SPACE 1                                                                
NIAST2   ZAP   0(9,R1),=P'0'       PRE-CLEAR                                    
         LA    R1,9(R1)                                                         
         BCT   R0,NIAST2                                                        
         BAS   RE,FILTUNIT         FILTER UNIT                                  
         BNE   XIT2                                                             
         LA    R0,8                                                             
         LA    R5,PREVP9                                                        
         LA    R4,GLARGS                                                        
         SPACE 1                                                                
NIAST4   MVC   ACCGENNO,0(R4)      PICK OFF ACCGEN NUMBER                       
         CLI   ACCGENNO,219        IF PCT OF PREV COL                           
         BNE   NIAST10                                                          
         MVC   0(9,R3),0(R5)       SET PREVIOUS                                 
         BAS   RE,PCTRTN                                                        
         B     NIASTX                                                           
NIAST10  CLI   ACCGENNO,200                                                     
         BH    NIASTX              (SKIP PHONY NUMBER)                          
         BAS   RE,DOACCGEN                                                      
         MVI   0(R3),0                                                          
         MVC   0(9,R5),0(R3)                                                    
         SPACE 1                                                                
NIASTX   LA    R3,9(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,9(R5)                                                         
         BCT   R0,NIAST4                                                        
         B     XIT2                                                             
*                                                                               
PREVP9   DS    CL72            8X9                                              
*                                                                               
PCTRTN   NTR1                                                                   
         ZAP   DUB,1(8,R3)          PCT  (ASSUME FIRST BYTE=ZEROS)              
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         OC    NDPERCNT,NDPERCNT                                                
         BZ    XIT2                                                             
*          DATA SET NENETACC   AT LEVEL 056 AS OF 03/07/91                      
         MVC   FULL,=F'10000'      2 DECIMAL DEFAULT                            
         MVC   BYTE,NDPERCNT                                                    
         CLI   BYTE,2                                                           
         BNH   PRT5                                                             
         L     RE,FULL                                                          
PRTNLOP  MH    RE,=H'10'                                                        
         ST    RE,FULL                                                          
         ZIC   R0,BYTE                                                          
         BCTR  R0,0                                                             
         CH    R0,=H'2'                                                         
         BE    PRT5                                                             
         STC   R0,BYTE                                                          
         B     PRTNLOP                                                          
PRT5     ICM   R0,15,NDPERCNT                                                   
         SLL   R0,8                                                             
         SRL   R0,7                                                             
         MR    R0,R0                                                            
         D     R0,FULL                                                          
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   1(8,R3),DUB                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              ACCOUNTING STACK OUTPUT                                          
         SPACE 3                                                                
NOAST    LA    R4,NDASTDEF                                                      
         ZAP   LASTP9,=P'0'                                                     
         ZAP   THISP9,=P'0'                                                     
         LA    R0,8                                                             
         SPACE 1                                                                
NOAST2   DS    0H                                                               
         CLI   0(R4),0             FIRST BYTE 0=ALL                             
         BE    NOAST6                                                           
         TM    0(R4),X'01'         NO PRINT                                     
         BO    NOAST6                                                           
         TM    0(R4),X'80'                    X'80'=DETAILS ONLY                
         BNO   NOAST4                                                           
         TM    GLINDS,GLTOTLIN                                                  
         BO    NOASTX                                                           
         B     NOAST6                                                           
         SPACE 1                                                                
NOAST4   TM    GLINDS,GLTOTLIN                X'40'=TOTALS ONLY                 
         BNO   NOASTX                                                           
         SPACE 1                                                                
NOAST6   CLI   1(R4),0             CHECK END                                    
         BE    XIT2                                                             
         CLI   1(R4),251           SPACE                                        
         BE    NOASTSPA                                                         
         CLI   1(R4),252           INDEX                                        
         BE    NOASTIX                                                          
         CLI   1(R4),253           DIFF                                         
         BE    NOASTDIF                                                         
         CLI   1(R4),255           N/A = SPACE                                  
         BE    NOASTSPA                                                         
         CLI   1(R4),254           PERCENT                                      
         BE    NOASTPCT                                                         
         TM    0(R4),X'01'                    X'01'=NO PRINT                    
         BNO   NOAST8                         SET THIS/ BUT NO EDIT             
         MVC   LASTP9,THISP9                                                    
         MVC   THISP9,0(R2)                                                     
         B     NOASTX                                                           
NOAST8   BAS   RE,DOEDIT           ELSE REGULAR TERM                            
         MVC   LASTP9,THISP9       PXZ                                          
         MVC   THISP9,0(R2)        PXZ                                          
         BC    0,NOASKIP                                                        
         OI    *-3,X'F0'                                                        
         MVC   LASTP9,THISP9                                                    
NOASKIP  LA    R3,198(R3)                                                       
         B     NOASTX                                                           
         SPACE 1                                                                
NOASTSPA MVI   0(R3),0             SPACE                                        
         LA    R3,198(R3)                                                       
         B     NOASTX                                                           
         SPACE 1                                                                
NOASTIX  CP    THISP9,=P'0'        INDEX                                        
         BE    NOASTSPA                                                         
         CP    LASTP9,=P'0'                                                     
         BE    NOASTSPA                                                         
         OC    THISP9(4),THISP9    (AVOID > $10M)                               
         BNZ   NOASTSPA                                                         
         OC    LASTP9(4),LASTP9    (AVOID > $10M)                               
         BNZ   NOASTSPA                                                         
         ZAP   DUB,LASTP9                                                       
         CVB   RE,DUB                                                           
         ZAP   DUB,THISP9                                                       
         CVB   RF,DUB                                                           
         STM   RE,RF,DUB                                                        
         ZIC   R1,MYOLEN2                                                       
         SH    R1,=H'6'                                                         
         AR    R3,R1                                                            
         LR    RF,R2                                                            
         LA    R2,DUB                                                           
         BAS   RE,INDEX                                                         
         SR    R3,R1                                                            
         LR    R2,RF                                                            
         LA    R3,198(R3)                                                       
         B     NOASTX                                                           
         SPACE 1                                                                
NOASTDIF ZAP   WORKP9,LASTP9       DIFF                                         
         SP    WORKP9,THISP9                                                    
         LR    RF,R2                                                            
         LA    R2,WORKP9                                                        
         BAS   RE,DOEDIT                                                        
         LR    R2,RF                                                            
         LA    R3,198(R3)                                                       
         B     NOASTX                                                           
         SPACE 1                                                                
NOASTPCT ZAP   DUB,THISP9+1(8)      PCT  (ASSUME FIRST BYTE=ZEROS)              
         CP    DUB,=PL8'0'                                                      
         BE    NOASTSPA                                                         
         OC    NDPERCNT,NDPERCNT                                                
         BZ    NOASTSPA                                                         
*          DATA SET NENETACC   AT LEVEL 056 AS OF 03/07/91                      
NOAPCT1  MVC   FULL,=F'10000'      2 DECIMAL DEFAULT                            
         MVC   BYTE,NDPERCNT                                                    
         CLI   BYTE,2                                                           
         BNH   NOAP5                                                            
         L     RE,FULL                                                          
NOALOOP  MH    RE,=H'10'                                                        
         ST    RE,FULL                                                          
         ZIC   R0,BYTE                                                          
         BCTR  R0,0                                                             
         CH    R0,=H'2'                                                         
         BE    NOAP5                                                            
         STC   R0,BYTE                                                          
         B     NOALOOP                                                          
NOAP5    ICM   R0,15,NDPERCNT       MULTIPLY BY PERCENT                         
         SLL   R0,8                                                             
         SRL   R0,7                                                             
         CVD   R0,WORKP8                                                        
         ZAP   WORKP16,DUB                                                      
         MP    WORKP16,WORKP8                                                   
*                                                                               
         L     R1,FULL              DIVIDE BY FULL                              
         CVD   R1,DUB                                                           
         DP    WORKP16,DUB                                                      
                                                                                
         ZAP   DUB,WORKP16(8)       GET CLEAN DIVIDEND IN WORKP16               
         ZAP   WORKP16,DUB                                                      
         AP    WORKP16,=P'1'        ADD 1                                       
         DP    WORKP16,=P'2'        DIVIDE BY 2                                 
         ZAP   DUB,WORKP16+7(8)     AND GET IT INTO DUB                         
*                                                                               
         ZAP   WORKP9,DUB                                                       
         LR    RF,R2                                                            
         LA    R2,WORKP9                                                        
         BAS   RE,DOEDIT                                                        
         LR    R2,RF                                                            
NOAPCTX  LA    R3,198(R3)                                                       
         B     NOASTX                                                           
         SPACE 1                                                                
NOASTX   DS    0H                                                               
         LA    R2,9(R2)                                                         
         LA    R4,2(R4)                                                         
         BCT   R0,NOAST2                                                        
         B     XIT2                                                             
         SPACE 1                                                                
WORKP9   DC    PL9'0'                                                           
THISP9   DC    PL9'0'                                                           
LASTP9   DC    PL9'0'                                                           
         DS    0D                                                               
WORKP8   DC    PL8'0'                                                           
WORKP16  DC    PL16'0'                                                          
         EJECT                                                                  
*              GENERAL EDITING HELP                                             
         SPACE 3                                                                
*              INPUT               R2=A(INPUT)                                  
*                                  R3=A(OUTPUT)                                 
         SPACE 1                                                                
DOEDIT   NTR1                                                                   
         XC    EBLOCK,EBLOCK                                                    
         ST    R2,EBAIN                                                         
         ST    R3,EBAOUT                                                        
         L     R3,GLADTENT                                                      
         USING DROD,R3                                                          
         L     R2,DROIADD                                                       
         USING DRIND,R2                                                         
         MVC   EBTIN,DRINTYPE                                                   
         MVC   EBLIN,DRINFLEN                                                   
         MVC   EBLOUT,DROLEN                                                    
         MVC   EBDECS,DRODEC                                                    
         MVC   EBFILL,DROFILL                                                   
         MVC   EBFLOAT,DROFLOAT                                                 
         MVC   EBROUND,DRODIV                                                   
         MVC   EBOPT,DROEDIT                                                    
         MVC   EBTRIM,DROFORM                                                   
         MVC   EBALIGN,DROALIGN                                                 
         MVI   EBPWIDTH,198                                                     
         MVC   EBSCOUT,DROSCALE                                                 
         MVC   EBTRAIL,DROTRAIL                                                 
         BAS   RE,ADJEDIT                                                       
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         B     XIT2                                                             
         DROP  R3                                                               
         SPACE 1                                                                
ADJEDIT  NTR1                                                                   
         TM    GLINDS3,GLRNDALL    ROUND DET AND TOTS                           
         BNO   ADJE10                                                           
         TM    GLINDS,GLTOTLIN     IS IT TOT LINE                               
         BNO   ADJE12                                                           
         ZIC   R1,EBLOUT           REDUCE OUTPUT LENGTH                         
         SH    R1,=H'3'                                                         
         STC   R1,EBLOUT                                                        
         MVI   EBDECS,0            NO DECIMAL PLACES                            
         B     XIT2                                                             
ADJE10   TM    GLINDS,GLRNDOPT     ROUND OPTION                                 
         BNO   XIT2                                                             
         TM    GLINDS,GLTOTLIN     ONLY APPLIES TO TOTALS                       
         BNO   XIT2                                                             
ADJE12   ZIC   R1,EBLOUT           REDUCE OUTPUT LENGTH                         
         SH    R1,=H'3'                                                         
         STC   R1,EBLOUT                                                        
         MVI   EBROUND,2           FORCE ROUND BY 100                           
         MVI   EBDECS,0                  AND NO DECIMAL PLACES                  
         B     XIT2                                                             
         EJECT                                                                  
*              EDITOR BLOCK                                                     
         SPACE 3                                                                
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
*              ACCOUNTING STACK DEFINITION                                      
         SPACE 3                                                                
NOASTDTA LA    R4,NDASTDEF                                                      
         LA    R0,8                                                             
         SPACE 1                                                                
NOASTDA2 CLI   0(R4),0             FIRST BYTE 0=ALL                             
         BE    NOASTDA6                                                         
         TM    0(R4),X'01'                    X'01'=DON'T PRINT                 
         BO    NOASTDA6                                                         
         TM    0(R4),X'80'                    X'80'=DETAILS ONLY                
         BNO   NOASTDA4                                                         
         TM    GLINDS,GLTOTLIN                                                  
         BO    NOASTDAX                                                         
         B     NOASTDA6                                                         
         SPACE 1                                                                
NOASTDA4 TM    GLINDS,GLTOTLIN                X'40'=TOTALS ONLY                 
         BNO   NOASTDAX                                                         
         SPACE 1                                                                
NOASTDA6 CLI   1(R4),0                                                          
         BE    XIT2                                                             
         L     R1,=A(ASDATTAB)                                                  
         CLI   GLARGS,C'S'                                                      
         BNE   NOASTDA7                                                         
         L     R1,=A(SUMDTTAB)                                                  
         SPACE 1                                                                
NOASTDA7 CLI   7(R1),X'FF'         LOOK UP IN TABLE                             
         BE    NOASTDA8                                                         
         CLC   7(1,R1),1(R4)                                                    
         BE    NOASTDA8                                                         
         LA    R1,8(R1)                                                         
         B     NOASTDA7                                                         
         SPACE 1                                                                
NOASTDA8 MVC   0(7,R3),0(R1)                                                    
         TM    0(R4),X'01'         PRINT WITH PCT                               
         BNO   NOASTDA9                                                         
         MVI   6(R3),C'%'                                                       
         B     NOASTD10                                                         
*                                                                               
NOASTDA9 CLI   1(R4),254            IF = PCT                                    
         BNE   NOASTD10                                                         
         LR    RE,R4                                                            
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         TM    0(RE),X'01'         WAS PREVIOUS FIELD PRINT EITH PCT            
         BO    NOASTDAX            YES/EXIT                                     
         BAS   RE,NOASTDPC          EDIT THE PCT                                
*                                                                               
NOASTD10 LA    R3,198(R3)                                                       
         SPACE 1                                                                
NOASTDAX LA    R4,2(R4)                                                         
         BCT   R0,NOASTDA2                                                      
         B     XIT2                                                             
*                                                                               
NOASTDPC NTR1                                                                   
         EDIT  (4,NDPERCNT),(6,0(R3)),2                                         
         MVI   6(R3),C'%'                                                       
         CLI   5(R3),C'0'          GET RID OF TRAILING ZEROS                    
         BNE   XIT2                                                             
         MVC   5(2,R3),=C'% '                                                   
         CLI   4(R3),C'0'                                                       
         BNE   XIT2                                                             
         MVC   3(3,R3),=C'%  '                                                  
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - NOEA (ESTIMATED/ACTUAL EXPS.)             
         SPACE 3                                                                
NOEA     DS    0H                  ESTIMATED ACTUAL (INDEX)                     
         LA    R0,13               POSSIBLE WIDTH ADJUSTMENT                    
         CLI   GLARGS+2,C'I'                                                    
         BNE   *+8                                                              
         LA    R0,17                                                            
         ZIC   R4,MYOLEN2                                                       
         SR    R4,R0                                                            
         SRA   R4,1                                                             
         AR    R3,R4                                                            
         LA    R2,16(R2)                                                        
         MVC   MYALL15,0(R2)                                                    
         LA    R2,4(R2)                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,MYECOST                                                       
         LA    R2,8(R2)                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,MYACOST                                                       
         SH    R2,=H'28'                                                        
         CLI   GLARGS+1,C'R'       DO GRPS BELOW                                
         BE    NOEAR                                                            
         SPACE 1                                                                
         MVC   MYIMPSR,0(R2)                                                    
         MVC   MYIMPSE,4(R2)                                                    
         MVC   MYCOST,MYECOST                                                   
         BAS   RE,DOIMPS           HANDLE ESTIMATED                             
         LA    R2,8(R2)                                                         
         LA    R3,7(R3)                                                         
         AR    R3,R4                                                            
         MVC   MYIMPSR,0(R2)                                                    
         MVC   MYIMPSE,4(R2)                                                    
         MVC   MYCOST,MYACOST                                                   
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         MVI   DROARGS+12,C'A'     TELL DOIMPS THIS IS ACTUAL DATA              
         BAS   RE,DOIMPS           ACTUAL                                       
         MVI   DROARGS+12,0        CLEAR IT                                     
         DROP  R1                                                               
         B     NOEAR2                                                           
         SPACE 1                                                                
NOEAR    MVC   MYGRPR4,0(R2)                                                    
         MVC   MYGRPE4,4(R2)                                                    
         MVC   MYCOST,MYECOST                                                   
         BAS   RE,DOGRPS           HANDLE ESTIMATED                             
         LA    R2,8(R2)                                                         
         LA    R3,7(R3)                                                         
         MVC   MYGRPR4,0(R2)                                                    
         MVC   MYGRPE4,4(R2)                                                    
         MVC   MYCOST,MYACOST                                                   
         BAS   RE,DOGRPS           ACTUAL                                       
         SPACE 1                                                                
NOEAR2   CLI   GLARGS+2,C'I'                                                    
         BNE   XIT2                                                             
         L     R2,GLAIFLD                                                       
         LA    R3,4(R3)                                                         
         BAS   RE,DOINDEX          OPTIONALLY INDEX                             
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - NOGEA (GOAL/EST/ACTUAL EXPS.)             
         SPACE 3                                                                
*              INPUT               GLARGS IS A LIST OF FUNCTIONS                
*                                         1 GOAL POINTS                         
*                                         2 EST POINTS                          
*                                         3 ACTUAL POINTS                       
*                                         4 GOAL-EST DIFF                       
*                                         5 GOAL-ACT DIFF                       
*                                         6 EST-ACT DIFF                        
*                                         7 GOAL-EST INDEX                      
*                                         8 GOAL-ACT INDEX                      
*                                         9 EST-ACT INDEX                       
         SPACE 1                                                                
NOGEA    LA    R5,GLARGS                                                        
         SPACE 1                                                                
NOGEA2   CLI   0(R5),0             END OF ARGS                                  
         BE    XIT2                                                             
         L     R2,GLAIFLD                                                       
         CLI   0(R5),1             GOAL POINTS                                  
         BE    NOGEAGRP                                                         
         LA    R2,16(R2)                                                        
         CLI   0(R5),2             EST POINTS                                   
         BE    NOGEAGRP                                                         
         LA    R2,16(R2)                                                        
         CLI   0(R5),3             ACT POINTS                                   
         BE    NOGEAGRP                                                         
         SPACE 1                                                                
         L     R2,GLAIFLD                                                       
         MVC   CHUNK+8(8),0(R2)    GOAL V ESTIMATED                             
         MVC   CHUNK(8),16(R2)                                                  
         CLI   0(R5),4             DIFF                                         
         BE    NOGEADIF                                                         
         SPACE 1                                                                
*                                  GOAL V ACTUAL                                
         MVC   CHUNK(8),32(R2)                                                  
         CLI   0(R5),5             DIFF                                         
         BE    NOGEADIF                                                         
         SPACE 1                                                                
         MVC   CHUNK+8(8),16(R2)   EST V ACTUAL                                 
         CLI   0(R5),6             DIFF                                         
         BE    NOGEADIF                                                         
         SPACE 1                                                                
         L     R2,GLAIFLD                                                       
         MVC   CHUNK(8),0(R2)      GOAL V ESTIMATED                             
         MVC   CHUNK+8(8),16(R2)                                                
         CLI   0(R5),7             INDEX                                        
         BE    NOGEAIX                                                          
         SPACE 1                                                                
*                                  GOAL V ACTUAL                                
         MVC   CHUNK+8(8),32(R2)                                                
         CLI   0(R5),8             INDEX                                        
         BE    NOGEAIX                                                          
         SPACE 1                                                                
         MVC   CHUNK(8),16(R2)     EST V ACTUAL                                 
         CLI   0(R5),9             INDEX                                        
         BE    NOGEAIX                                                          
         B     NOGEANXT                                                         
         SPACE 1                                                                
NOGEAGRP MVC   MYGRPR4,0(R2)                                                    
         MVC   MYGRPE4,4(R2)                                                    
         LA    R2,8(R2)                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,MYCOST                                                        
         BAS   RE,DOGRPS                                                        
         LA    R3,7(R3)                                                         
         B     NOGEANXT                                                         
         SPACE 1                                                                
NOGEADIF LA    R2,CHUNK                                                         
         BAS   RE,DODIFF                                                        
         LA    R3,7(R3)                                                         
         B     NOGEANXT                                                         
         SPACE 1                                                                
NOGEAIX  LA    R2,CHUNK                                                         
         SH    R3,=H'3'                                                         
         BAS   RE,DOINDEX                                                       
         LA    R3,7(R3)                                                         
         B     NOGEANXT                                                         
         SPACE 1                                                                
NOGEANXT LA    R5,1(R5)                                                         
         B     NOGEA2                                                           
         SPACE 1                                                                
         DS    0F                                                               
CHUNK    DS    CL16                                                             
         EJECT                                                                  
*              $ OUTPUT ROUTINES - NOGEADOL (GOAL/EST/ACTUAL EXPS.)             
         SPACE 3                                                                
*              INPUT               GLARGS IS A LIST OF FUNCTIONS                
*                                         1 GOAL $                              
*                                         2 EST $                               
*                                         3 ACTUAL $                            
*                                         4 GOAL-EST DIFF                       
*                                         5 GOAL-ACT DIFF                       
*                                         6 EST-ACT DIFF                        
*                                         7 GOAL-EST INDEX                      
*                                         8 GOAL-ACT INDEX                      
*                                         9 EST-ACT INDEX                       
         SPACE 1                                                                
NOGEADOL LA    R5,GLARGS                                                        
         SPACE 1                                                                
NOGD2    CLI   0(R5),0             END OF ARGS                                  
         BE    XIT2                                                             
         L     R2,GLAIFLD                                                       
         CLI   0(R5),1             GOAL $                                       
         BE    NOGDDOL                                                          
         LA    R2,8(R2)                                                         
         CLI   0(R5),2             EST $                                        
         BE    NOGDDOL                                                          
         LA    R2,8(R2)                                                         
         CLI   0(R5),3             ACT $                                        
         BE    NOGDDOL                                                          
         SPACE 1                                                                
         L     R2,GLAIFLD                                                       
         MVC   CHUNK(8),0(R2)      GOAL V ESTIMATED                             
         MVC   CHUNK+8(8),8(R2)                                                 
         CLI   0(R5),4             DIFF                                         
         BE    NOGDDIF                                                          
         SPACE 1                                                                
*                                  GOAL V ACTUAL                                
         MVC   CHUNK+8(8),16(R2)                                                
         CLI   0(R5),5             DIFF                                         
         BE    NOGDDIF                                                          
         SPACE 1                                                                
         MVC   CHUNK+8(8),8(R2)    EST V ACTUAL                                 
         MVC   CHUNK(8),16(R2)                                                  
         CLI   0(R5),6             DIFF                                         
         BE    NOGDDIF                                                          
         SPACE 1                                                                
         L     R2,GLAIFLD                                                       
         MVC   CHUNK(8),0(R2)      GOAL V ESTIMATED                             
         MVC   CHUNK+8(8),8(R2)                                                 
         CLI   0(R5),7             INDEX                                        
         BE    NOGDIX                                                           
         SPACE 1                                                                
*                                  GOAL V ACTUAL                                
         MVC   CHUNK+8(8),16(R2)                                                
         CLI   0(R5),8             INDEX                                        
         BE    NOGDIX                                                           
         SPACE 1                                                                
         MVC   CHUNK(8),8(R2)      EST V ACTUAL                                 
         CLI   0(R5),9             INDEX                                        
         BE    NOGDIX                                                           
         B     NOGDNXT                                                          
         SPACE 1                                                                
NOGDDOL  BAS   RE,OUTCOST                                                       
         SPACE 1                                                                
NOGDDOL2 BAS   RE,DOWNZERO                                                      
         CLI   0(R3),C'0'          DOWNLOAD ZERO                                
         BE    NOGDDOL4                                                         
         EDIT  (R1),(8,0(R3)),FLOAT=-                                           
         SPACE 1                                                                
NOGDDOL4 LA    R3,9(R3)                                                         
         B     NOGDNXT                                                          
         SPACE 1                                                                
NOGDDIF  LA    R2,CHUNK                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,CHUNK                                                         
         LA    R2,CHUNK+8                                                       
         BAS   RE,OUTCOST                                                       
         S     R1,CHUNK                                                         
         ST    R1,MYCOST                                                        
         B     NOGDDOL2                                                         
         SPACE 1                                                                
NOGDIX   LA    R2,CHUNK                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,CHUNK                                                         
         LA    R2,CHUNK+8                                                       
         BAS   RE,OUTCOST                                                       
         ST    R1,CHUNK+4                                                       
         LA    R2,CHUNK                                                         
         SH    R3,=H'3'                                                         
         BAS   RE,INDEX                                                         
         LA    R3,7(R3)                                                         
         SPACE 1                                                                
NOGDNXT  LA    R5,1(R5)                                                         
         B     NOGD2                                                            
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - NOV AND NOIRH                             
         SPACE 3                                                                
NOV      DS    0H                  VPH IMPS POINTS                              
         MVC   MYIMPSR,0(R2)                                                    
         MVC   MYIMPSE,4(R2)                                                    
         LA    R2,8(R2)                                                         
         MVC   MYGRPR4,0(R2)                                                    
         MVC   MYGRPE4,4(R2)                                                    
         LA    R2,8(R2)                                                         
         MVC   MYHOMESR,0(R2)                                                   
         MVC   MYHOMESE,4(R2)                                                   
         LA    R2,8(R2)                                                         
         MVC   MYALL15,0(R2)                                                    
         LA    R2,4(R2)                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,MYCOST                                                        
         MVC   MYIMPS,MYIMPSE      (USE EQUIV FOR VPH COMPS)                    
         MVC   MYHOMES,MYHOMESE                                                 
         CLI   NDVPHOPT,C'P'       SPECIAL FOR PROGRAM VPHS                     
         BNE   *+10                  HOMES RAW HAS VPH,                         
         MVC   MYIMPS,MYHOMESR       HOMES EQUIV HAS COUNT (000)                
         BAS   RE,VPH4                                                          
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         MVC   0(4,R3),1(R3)                                                    
         MVI   BYTE,0                                                           
         LA    R1,16               (EXPECTED WIDTH)                             
         ZIC   RE,MYOLEN2          (MAY BE DIFFERENT)                           
         CR    R1,RE                (LET'S SEE)                                 
         BE    NOV10               (NO CHANGE)                                  
         BH    NOV10               (OR LESS)                                    
* -  SPLIT NEW OUTLENGTH FOR VPH/IMP/GRP                                        
         MVC   BYTE,MYOLEN2         SAVE OUTPUT LENGTH                          
         SR    RE,R1                GET EXTRA LENGTH                            
         SRA   RE,1                 AND DIVIDE IT                               
         AR    R1,RE                                                            
         STC   R1,MYOLEN2                                                       
         LA    R1,16               (EXPECTED WIDTH)                             
NOV10    BAS   RE,ADJOUT                                                        
         LA    R3,5(R3)                                                         
         BAS   RE,DOIMPS                                                        
         CLI   BYTE,0              ARE WE DIVIDING OUT LENGTHS                  
         BE    NOV12               NO                                           
         LA    R1,16               YES/(EXPECTED WIDTH)                         
         BAS   RE,ADJOUT                                                        
NOV12    LA    R3,7(R3)                                                         
         BAS   RE,DOGRPS4                                                       
         CLI   BYTE,0                                                           
         BE    *+10                                                             
         MVC   MYOLEN2,BYTE        RESTORE OUT LENGTH                           
         B     XIT2                                                             
         SPACE 1                                                                
NOVPH    MVC   MYIMPS,4(R2)                                                     
         MVC   MYHOMES,8(R2)                                                    
         BAS   RE,VPH6                                                          
         MVC   0(4,R2),MYRESULT                                                 
         B     XIT2                                                             
*                                                                               
* VPH IN R1                                                                     
VPHRND   NTR1               ROUND TO NEAREST 10TH                               
         SR    R0,R0                                                            
         A     R1,=F'5'                                                         
         D     R0,=F'10'                                                        
         SR    R0,R0                                                            
         M     R0,=F'10'                                                        
         XIT1  REGS=(R1)                                                        
         SPACE 1                                                                
NOIRH    MVC   MYIMPSR,0(R2)       IMPS POINTS                                  
         MVC   MYIMPSE,4(R2)                                                    
         LA    R2,8(R2)                                                         
         MVC   MYGRPR4,0(R2)                                                    
         MVC   MYGRPE4,4(R2)                                                    
         LA    R2,8(R2)                                                         
         MVC   MYALL15,0(R2)                                                    
         LA    R2,4(R2)                                                         
         BAS   RE,OUTCOST                                                       
         ST    R1,MYCOST                                                        
         LA    R1,11               (EXPECTED WIDTH)                             
         BAS   RE,ADJOUT           (MAY BE DIFFERENT)                           
         BAS   RE,DOIMPS                                                        
         LA    R3,7(R3)                                                         
         BAS   RE,DOGRPS4                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NORTG    DS    0H                  AVERAGE RATING                               
         MVI   BYTE,C'R'           SET RATING INDICATOR                         
NOSHR    DS    0H                  AVERAGE SHARE                                
NOHUT    DS    0H                  AVERAGE HUT                                  
         LA    R2,4(R2)                                                         
         BAS   RE,AVE              AVERAGE                                      
         SH    R2,=H'4'                                                         
         CLI   BYTE,C'R'                                                        
         BNE   *+12                                                             
         BAS   RE,RNDDMRAT         RATING POINT ROUND                           
         ST    R1,MYRESULT                                                      
         MVC   0(4,R2),MYRESULT                                                 
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - INDEX STACK                               
         SPACE 3                                                                
*              INPUT               R2=A(16 BYTE AREA - 4 FIELDS)                
*                                  EST RAW/EQUIV, ACTUAL RAW/EQUIV              
DOINDEX  NTR1                                                                   
         LR    R4,R2                                                            
         LA    R2,MYDEFLST         SET UP TO HANDLE LIST                        
         LA    R0,8                                                             
         SPACE 1                                                                
DOINXLST MVC   BYTE,0(R2)                                                       
         NI    BYTE,X'0F'                                                       
         MVC   DUB(4),0(R4)        PICK OFF RAW ESTIMATED                       
         MVC   DUB+4(4),8(R4)                   ACTUAL                          
         TM    0(R2),X'80'                                                      
         BNO   *+16                                                             
         MVC   DUB(4),4(R4)            OR EQUIV ESTIMATED                       
         MVC   DUB+4(4),12(R4)                  ACTUAL                          
         CLI   BYTE,2                                                           
         BL    DOINX1                                                           
         BE    DOINX2                                                           
         CLI   BYTE,4                                                           
         BL    DOINX3                                                           
         BE    DOINX4                                                           
         B     DOINX5                                                           
         SPACE 1                                                                
DOINX1   LR    RF,R2               STRAIGHT INDEX=1                             
         LA    R2,DUB                                                           
         BAS   RE,INDEX                                                         
         LR    R2,RF                                                            
         B     DOINXNXT                                                         
         SPACE 1                                                                
DOINX2   DS    0H                  CPP=2                                        
DOINX3   DS    0H                  POST INDEX=3                                 
DOINX4   DS    0H                  INDEX INDEX=4                                
         SPACE 1                                                                
DOINX5   MVI   3(R3),0             SPACE=5                                      
         SPACE 1                                                                
DOINXNXT LA    R3,198(R3)                                                       
         LA    R2,1(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XIT2                                                             
         BCT   R0,DOINXLST                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              DEMO OUTPUT ROUTINES - DIFFERENCE STACK                          
         SPACE 3                                                                
*              INPUT               R2=A(16 BYTE AREA - 4 FIELDS)                
*                                  EST RAW/EQUIV, ACTUAL RAW/EQUIV              
         SPACE 1                                                                
DODIFF   NTR1                                                                   
         LR    R4,R2                                                            
         LA    R2,MYDEFLST         SET UP TO HANDLE LIST                        
         LA    R0,8                                                             
         SPACE 1                                                                
DODIFLST MVC   BYTE,0(R2)                                                       
         NI    BYTE,X'0F'                                                       
         MVC   DUB(4),0(R4)        PICK OFF RAW ESTIMATED                       
         MVC   DUB+4(4),8(R4)                   ACTUAL                          
         TM    0(R2),X'80'                                                      
         BNO   *+16                                                             
         MVC   DUB(4),4(R4)            OR EQUIV ESTIMATED                       
         MVC   DUB+4(4),12(R4)                  ACTUAL                          
         CLI   BYTE,2                                                           
         BL    DODIF1                                                           
         BE    DODIF2                                                           
         CLI   BYTE,4                                                           
         BL    DODIF3                                                           
         BE    DODIF4                                                           
         B     DODIF5                                                           
         SPACE 1                                                                
DODIF1   L     R1,DUB              STRAIGHT DIF=1                               
         S     R1,DUB+4                                                         
         BZ    DODIFEQU                                                         
         BM    DODIFMIN                                                         
         ST    R1,MYGRPS           POSITIVE DIFFERENCE                          
         MVI   GRPSIGN,C'+'                                                     
         BAS   RE,GRP6                                                          
         BAS   RE,POSFLOAT                                                      
         B     DODIFNXT                                                         
         SPACE 1                                                                
DODIFEQU TM    NDDOWNL,X'80'                                                    
         BNO   DODIFNXT                                                         
         MVI   0(R3),C'0'                                                       
         B     DODIFNXT                                                         
         SPACE 1                                                                
DODIFMIN LCR   R1,R1               MINUS DIFFERENCE                             
         ST    R1,MYGRPS                                                        
         MVI   GRPSIGN,C'-'                                                     
         BAS   RE,GRP6                                                          
         BAS   RE,MINFLOAT                                                      
         B     DODIFNXT                                                         
         SPACE 1                                                                
DODIF2   DS    0H                  CPP=2                                        
DODIF3   DS    0H                  POST INDEX=3                                 
DODIF4   DS    0H                  INDEX INDEX=4                                
         SPACE 1                                                                
DODIF5   MVI   0(R3),0             SPACE=5                                      
         SPACE 1                                                                
DODIFNXT LA    R3,198(R3)                                                       
         LA    R2,1(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XIT2                                                             
         BCT   R0,DODIFLST                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              SUBSIDIARY EDITING  - VPH                                        
         SPACE 3                                                                
*              INPUT               R2=A(IMPS/HOMES)                             
*              OUTPUT              VVVV                                         
         SPACE 1                                                                
VPH4     NTR1                      VPH 4 POSITIONS LEFT ALIGN                   
         L     R1,MYIMPS           DEMO IMPS                                    
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'2000'                                                      
         L     RF,MYHOMES          HOME IMPS                                    
         BAS   RE,DOWNZRF                                                       
         LTR   RF,RF                                                            
         BZ    XIT2                                                             
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CLI   NDRNDVPH,C'Y'       ROUND VPH                                    
         BNE   *+8                                                              
         BAS   RE,VPHRND                                                        
         ST    R1,MYRESULT                                                      
         EDIT  (R1),(4,(R3)),ALIGN=LEFT                                         
         B     XIT2                                                             
         SPACE 1                                                                
VPH6     NTR1                      VPH 6 POSITIONS RIGHT ALIGN                  
         L     R1,MYIMPS           DEMO IMPS                                    
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'2000'                                                      
         L     RF,MYHOMES          HOME IMPS                                    
         LTR   RF,RF                                                            
         BZ    XIT2                                                             
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CLI   NDRNDVPH,C'Y'       ROUND VPH                                    
         BNE   *+8                                                              
         BAS   RE,VPHRND                                                        
         ST    R1,MYRESULT                                                      
         EDIT  (R1),(6,(R3))                                                    
         B     XIT2                                                             
         EJECT                                                                  
*              SUBSIDIARY EDITING  - IMPRESSIONS                                
         SPACE 3                                                                
*              OUTPUT              IIII.I OR IIIIII                             
         SPACE 1                                                                
IMP6     NTR1                                                                   
         L     R1,MYIMPS                                                        
         BAS   RE,DOWNZERO                                                      
         BAS   RE,AVE30                                                         
         TM    NDDOWNL,X'80'       IF DOWNLOADING                               
         BNO   *+10                                                             
         LTR   R1,R1               AND IMPS ZERO                                
         BZ    XIT2                GET OUT HERE                                 
         CLI   NBHUNOPT,C'Y'       IF IMPRESSIONS ARE IN 00                     
         BNE   IMP61                                                            
         CLC   MYN15S,MY15S        IF ALL THE UNITS ARE NOT NETWORK             
         BE    IMP6H4                                                           
*                                  WE NEED TO SHOW DECIMAL                      
         EDIT  (R1),(10,DMCB),1,ZERO=BLANK                                      
         MVC   0(6,R3),DMCB+4      WILL IT FIT IN 6 CHARACTERS?                 
         CLC   DMCB(4),D2SPACES                                                 
         BE    XIT2                                                             
         MVC   0(6,R3),D2SPACES                                                 
         SPACE 1                                                                
IMP6H4   AH    R1,=H'5'            RETURN TO 000 BASE                           
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         SPACE 1                                                                
IMP61    EDIT  (R1),(10,DMCB)                                                   
         CLI   DMCB+3,C' '         CHECK LENGTH OF OUTPUT                       
         BNE   IMP7                                                             
         MVC   0(6,R3),DMCB+4      OK - NO MORE THAN 6                          
         MVC   WORK(10),DMCB                                                    
         B     XIT2                                                             
         SPACE 1                                                                
IMP7     CLI   DMCB+2,C' '         7 CHARACTERS                                 
         BNE   IMP8                                                             
         SH    R3,=H'2'                                                         
         SPACE 1                                                                
IMP7B    CLC   0(2,R3),D2SPACES    IS THERE ROOM ON THIS LINE?                  
         BE    *+12                                                             
         LA    R3,198(R3)          NO - USE LINE BELOW                          
         B     IMP7B                                                            
         MVC   1(7,R3),DMCB+3                                                   
         B     XIT2                                                             
         SPACE 1                                                                
IMP8     CLI   DMCB+1,C' '         8 CHARACTERS                                 
         BNE   IMP9                                                             
         SH    R3,=H'3'                                                         
         SPACE 1                                                                
IMP8B    CLC   0(3,R3),D2SPACES    IS THERE ROOM ON THIS LINE?                  
         BE    *+12                                                             
         LA    R3,198(R3)          NO - USE LINE BELOW                          
         B     IMP8B                                                            
         MVC   1(8,R3),DMCB+2                                                   
         B     XIT2                                                             
         SPACE 1                                                                
IMP9     SH    R3,=H'4'                                                         
         SPACE 1                                                                
IMP9B    CLC   0(3,R3),D2SPACES    IS THERE ROOM ON THIS LINE?                  
         BE    *+12                                                             
         LA    R3,198(R3)          NO - USE LINE BELOW                          
         B     IMP9B                                                            
         MVC   1(9,R3),DMCB+1                                                   
         B     XIT2                                                             
         EJECT                                                                  
*              SUBSIDIARY EDITING  - GRPS                                       
         SPACE 3                                                                
GRP4     NTR1                                                                   
         L     R1,MYGRPS           GRPS TO 4 CHARS                              
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         BAS   RE,AVE30                                                         
         BAS   RE,RNDDMRAT         RATING POINT ROUND                           
         CLI   NDPREOPT,C'Y'                                                    
         BNE   GRP42                                                            
         EDIT  (R1),(8,DMCB),2     TRY 2 DEC                                    
         CLI   DMCB+3,C' '                                                      
         BNE   GRP41                                                            
         MVC   0(4,R3),DMCB+4                                                   
         B     XIT2                                                             
GRP41    LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         SPACE 1                                                                
GRP42    EDIT  (R1),(8,DMCB),1     TRY 1 DEC                                    
         CLI   DMCB+3,C' '                                                      
         BNE   GRP44                                                            
         MVC   0(4,R3),DMCB+4                                                   
         B     XIT2                                                             
         SPACE 1                                                                
GRP43    LA    R1,5(R1)            TRY 0 DEC                                    
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(8,DMCB)                                                    
         CLI   DMCB+3,C' '                                                      
         BNE   GRP44                                                            
         MVC   0(4,R3),DMCB+4                                                   
         B     XIT2                                                             
         SPACE 1                                                                
GRP44    SH    R3,=H'4'            MORE THAN 4 CHARACTERS                       
         SPACE 1                                                                
GRP46    CLC   0(4,R3),D2SPACES    IS THERE ANYTHING TO THE LEFT?               
         BE    GRP48                                                            
         LA    R3,198(R3)                                                       
         B     GRP46                                                            
         SPACE 1                                                                
GRP48    MVC   0(8,R3),DMCB                                                     
         B     XIT2                                                             
         SPACE 1                                                                
GRP6     NTR1                                                                   
         L     R1,MYGRPS           GRPS TO 6 CHARS                              
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         BAS   RE,AVE30                                                         
         BAS   RE,RNDDMRAT         RATING POINT ROUND                           
         BCTR  R3,0                                                             
         SPACE 1                                                                
         CLI   NDPREOPT,C'Y'                                                    
         BNE   GRP6C                                                            
         EDIT  (R1),(7,(R3)),2     TRY 2 DEC PLACE                              
         CLI   1(R3),C' '                                                       
         BE    GRP6X                                                            
         CLI   GRPSIGN,0                                                        
         BNE   GRP6B                                                            
         CLI   0(R3),C' '                                                       
         BE    GRP6X                                                            
GRP6B    LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         SPACE 1                                                                
GRP6C    EDIT  (R1),(7,(R3)),1     TRY 1 DEC PLACE                              
         CLI   1(R3),C' '                                                       
         BE    GRP6X                                                            
         CLI   GRPSIGN,0                                                        
         BNE   GRP6D                                                            
         CLI   0(R3),C' '                                                       
         BE    GRP6X                                                            
         SPACE 1                                                                
GRP6D    LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,(R3))                                                    
         SPACE 1                                                                
GRP6X    MVI   GRPSIGN,0                                                        
         B     XIT2                                                             
         SPACE 1                                                                
GRPSIGN  DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE TO AVERAGE                                               
         SPACE 3                                                                
AVE30    CLI   NDAVEOPT,C'Y'       OPTION FOR AVERAGE ON 30S UNITS              
         BNER  RE                                                               
         M     R0,=F'4'            R1 HAS AMOUNT TO BE AVERAGED                 
         LH    RF,MY15S                                                         
         LTR   RF,RF                                                            
         BNZ   AVE30B                                                           
         SR    R1,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
AVE30B   DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         SPACE 1                                                                
*                                  R2=A(UNITS AND VALUE)                        
AVE      L     R1,4(R2)            AVERAGE HUT/SHR/RTG                          
         L     RF,0(R2)                                                         
         LTR   RF,RF                                                            
         BZ    AVE2                                                             
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,MYRESULT                                                      
         EDIT  (R1),(3,(R3))                                                    
         BR    RE                                                               
         SPACE 1                                                                
AVE2     TM    NDDOWNL,X'80'                                                    
         BNOR  RE                                                               
         MVI   0(R3),C'0'                                                       
         BR    RE                                                               
         EJECT                                                                  
*              SUBSIDIARY EDITING  - CPM                                        
         SPACE 3                                                                
CPM      NTR1                                                                   
         XC    MYRESULT,MYRESULT                                                
         MVI   MYRESULT,X'40'      (SET HIGH RESULT FOR HIGH)                   
         L     RF,MYIMPS           COST PER THOUSAND                            
         BAS   RE,DOWNZRF                                                       
         CLI   NBHUNOPT,C'Y'                                                    
         BE    CPM2                                                             
         MH    RF,=H'10'                                                        
         SPACE 1                                                                
CPM2     LTR   RF,RF                                                            
         BZ    XIT2                                                             
         MVI   MYRESULT,0                                                       
         L     R1,MYCOST                                                        
         BAS   RE,DOWNZERO                                                      
         SPACE 1                                                                
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'2000'                                                      
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                NOW HAVE CPM IN PENNIES                      
         ST    R1,MYRESULT                                                      
         BCTR  R3,0                                                             
         CLC   0(7,R3),D2SPACES                                                 
         BE    *+12                                                             
         LA    R3,198(R3)                                                       
         B     *-14                                                             
         LR    RF,R1                                                            
         CLI   MYOLEN2,8                                                        
         BNE   CPM5                                                             
         EDIT  (R1),(9,(R3)),2,FLOAT=$                                          
         B     XIT2                                                             
CPM5     EDIT  (R1),(7,(R3)),2,FLOAT=$                                          
         CLI   0(R3),C' '                                                       
         BE    XIT2                                                             
         LA    R1,50(RF)                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(7,(R3)),FLOAT=$                                            
         B     XIT2                                                             
         EJECT                                                                  
*              SUBSIDIARY EDITING  - CPP                                        
         SPACE 3                                                                
CPP      NTR1                                                                   
         XC    MYRESULT,MYRESULT                                                
         MVI   MYRESULT,X'40'      (SET HIGH RESULT FOR HIGH)                   
         L     RF,MYGRPS           COST PER POINT                               
         BAS   RE,DOWNZRF                                                       
         LTR   RF,RF                                                            
         BZ    XIT2                                                             
         L     R1,MYGRPS           COST PER POINT                               
         BAS   RE,RNDDMRAT         RATING POINT ROUND                           
         LR    RF,R1                                                            
         LTR   RF,RF                                                            
         BZ    XIT2                                                             
         SPACE 1                                                                
         MVI   MYRESULT,0                                                       
         L     R1,MYCOST           GET COST                                     
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'20'                                                        
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                NOW HAVE CPP                                 
         CLI   NDPREOPT,C'Y'       ROUND BY TEN IF PRECISSION TO 2 DEC          
         BNE   CPP1                                                             
         SR    R0,R0                                                            
         M     R0,=F'10'           PXZ                                          
         B     CPP1                PXZ                                          
         LA    R1,5(R1)                                                         
         D     R0,=F'10'                                                        
CPP1     ST    R1,MYRESULT                                                      
         BCTR  R3,0                                                             
         CLC   0(7,R3),D2SPACES                                                 
         BE    *+12                                                             
         LA    R3,198(R3)                                                       
         B     *-14                                                             
         EDIT  (R1),(7,(R3)),FLOAT=$                                            
         B     XIT2                                                             
         SPACE 1                                                                
CPP4     NTR1                                                                   
         L     RF,MYGRPS           COST PER POINT                               
         BAS   RE,DOWNZRF                                                       
         LTR   RF,RF                                                            
         BZ    XIT2                                                             
         L     R1,MYGRPS           COST PER POINT                               
         BAS   RE,RNDDMRAT         RATING POINT ROUND                           
         LR    RF,R1                                                            
         SPACE 1                                                                
         L     R1,MYCOST           GET COST                                     
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'20'                                                        
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                NOW HAVE CPP                                 
         CLI   NDPREOPT,C'Y'       ROUND BY TEN IF PRECISSION TO 2 DEC          
         BNE   CPP5                                                             
         SR    R0,R0                                                            
         M     R0,=F'10'           PXZ                                          
         B     CPP5                PXZ                                          
         LA    R1,5(R1)                                                         
         D     R0,=F'10'                                                        
CPP5     ST    R1,MYRESULT                                                      
         EDIT  (R1),(6,DMCB),FLOAT=$                                            
         MVC   0(4,R3),DMCB+2                                                   
         CLI   DMCB,C' '                                                        
         BE    XIT2                                                             
         MVI   0(R3),C'0'                                                       
         TM    NDDOWNL,X'80'                                                    
         BO    XIT2                                                             
         MVC   0(4,R3),=C'HIGH'                                                 
         B     XIT2                                                             
         EJECT                                                                  
*              SUBSIDIARY EDITING  - CPU AND RPU                                
         SPACE 3                                                                
CPU      NTR1                                                                   
         L     RF,MYUNITS          COST PER UNIT                                
         BAS   RE,DOWNZRF                                                       
         LTR   RF,RF                                                            
         BZ    XIT2                                                             
         SPACE 1                                                                
         L     R1,MYCOST           GET COST                                     
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'2'                                                         
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                NOW HAVE CPU IN DOLLARS                      
         ST    R1,MYRESULT                                                      
         BCTR  R3,0                                                             
         EDIT  (R1),(7,(R3)),FLOAT=$                                            
         CLI   0(R3),C'$'                                                       
         BNE   XIT2                                                             
         MVI   0(R3),C' '                                                       
         B     XIT2                                                             
         SPACE 3                                                                
IPU      NTR1                                                                   
         L     RF,MYUNITS          COST PER UNIT                                
         BAS   RE,DOWNZRF                                                       
         LTR   RF,RF                                                            
         BZ    XIT2                                                             
         SPACE 1                                                                
         L     R1,MYIMPS           GET COST                                     
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'2'                                                         
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                NOW HAVE IMPS                                
         STCM  R1,15,MYIMPS                                                     
         BAS   RE,IMP6                                                          
         ST    R1,MYRESULT                                                      
         B     XIT2                                                             
         SPACE 3                                                                
RPU      NTR1                                                                   
         L     RF,MYUNITS          RATINGS PER UNIT                             
         BAS   RE,DOWNZRF                                                       
         LTR   RF,RF                                                            
         BZ    XIT2                                                             
         SPACE 1                                                                
         L     R1,MYGRPS           GET POINTS                                   
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'2'                                                         
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                NOW HAVE RPU                                 
         BAS   RE,RNDDMRAT         RATING POINT ROUND                           
         ST    R1,MYRESULT                                                      
         CLI   NDPREOPT,C'Y'                                                    
         BNE   RPU10               SINGLE DECIMAL DISPLAY                       
         EDIT  (R1),(6,(R3)),2                                                  
         B     XIT2                                                             
RPU10    EDIT  (R1),(6,(R3)),1                                                  
         B     XIT2                                                             
         SPACE 1                                                                
*--ROUND DEMO ROUNDING WHEN USING 2 DEC PRECISSION                              
RNDDMRAT CLI   NDPREOPT,C'Y'       CHECK 2 DEC. OPTION                          
         BNE   RNDDMEX                                                          
         CLI   NDMEDCOD,X'40'      REQUIRE NETWORK AS ROW                       
         BNH   RNDDM20             FOR PRE=CAB OPTION                           
         CLI   NDMEDCOD,C'N'       ROUND FOR NETWORK RATINGS                    
         BE    RNDDM20                                                          
         CLI   NDMEDCOD,C'S'       ROUND FOR SYNDICATION RATINGS                
         BNE   RNDDMEX                                                          
*                                                                               
RNDDM20  SR    R0,R0                                                            
         LA    R1,5(R1)                                                         
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
RNDDMEX  BR    RE                                                               
         EJECT                                                                  
*              SUBSIDIARY EDITING  - INDEX                                      
         SPACE 3                                                                
*              INPUT               R2=A(ACTUAL/ESTIMATE)                        
         SPACE 1                                                                
INDEX    NTR1                                                                   
         L     R1,0(R2)                                                         
         BAS   RE,DOWNZERO                                                      
         L     R1,4(R2)            ACTUAL VALUE                                 
         BAS   RE,DOWNZERO                                                      
         LTR   R1,R1                                                            
         BZ    XIT2                                                             
         M     R0,=F'1000'         X 1000  (FOR ROUNDING)                       
         OC    0(4,R2),0(R2)                                                    
         BZ    XIT2                                                             
         D     R0,0(R2)            / ESTIMATED VALUE                            
         AH    R1,=H'5'            ROUND RESULT TO NEAREST POINT                
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,MYRESULT                                                      
         CH    R1,=H'999'                                                       
         BH    XIT2                                                             
         EDIT  (R1),(3,3(R3))                                                   
         B     XIT2                                                             
         SPACE 1                                                                
*OWNZERO MVI   0(R3),C' '          THIS WAS CAUSING PROBLEM                     
DOWNZERO LTR   R1,R1               CHECK FOR DOWNLOADING ZEROS                  
         BNZR  RE                                                               
         B     DOWNZALL                                                         
         SPACE 1                                                                
DOWNZRF  LTR   RF,RF                                                            
         BNZR  RE                                                               
         SPACE 1                                                                
DOWNZALL TM    NDDOWNL,X'80'       RESULT IS ZERO                               
         BNOR  RE                  IF WE ARE DOWNLOADING,                       
         MVI   0(R3),C'0'          POP IN A ZERO FOR LOTUS                      
         BR    RE                                                               
         EJECT                                                                  
         SPACE 1                                                                
*--PUP UNITS                                                                    
NIPUNIT  CLI   NBMODE,NBPROCPP     PUP MODE                                     
         BNE   XIT2                                                             
         BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         SR    RE,RE                                                            
         ICM   RE,3,NBSPCHRG                                                    
         MH    RE,=H'10'                                                        
         STH   RE,2(R3)               PUP UNITS                                 
         CLI   GLARGS+7,C'E'       FORCED TO EQUIV                              
         BNE   *+10                                                             
         MVC   2(2,R3),NBSPCHRG+2  PUP EQUIV UNITS                              
         B     XIT2                                                             
*                                                                               
*              NUMBER OF UNITS                                                  
         SPACE 3                                                                
*              ARGUMENTS 1-4       LIST OF SEC. LENGTHS (255=ALL)               
*                        8-16      SEE FILTUNIT                                 
         SPACE 1                                                                
NIUNIT   DS    0H                                                               
         CLI   NBRDBELS,2      IF READING BILL/PAY ELEMENTS                     
         BE    XIT2            2=WE'VE ALREADY BEEN HERE ONCE FOR UNIT          
         CLI   NBMODE,NBPROCPK                                                  
         BE    XIT2                                                             
         CLI   GLARGS+1,C'P'       ...IF PFB FILTERING                          
         BNE   NIUNIT0                                                          
         TM    NBUNITST,X'04'      ...ACCEPT PFB                                
         BNO   NIUNIT1                                                          
         TM    NBUNITST,X'01'      ...IF ALSO MAKEGOOD                          
         BNO   XIT2                                                             
         TM    NBMGFSTA,X'04'      ...UNLESS IT'S MAKEGOOD FOR PFB              
         BO    XIT2                                                             
         B     NIUNIT1                                                          
NIUNIT0  CLI   GLARGS+1,C'A'       IF ACCOUNTING UNIT                           
         BNE   NIUNIT1                                                          
         TM    NBUNITST,X'42'      AND PREEMPTED OR MISSED                      
         BNZ   XIT2                IGNORE                                       
         CLC   NBSPLPRN,NBPRD      ..IF WORKING PRD DOES NOT MATCH              
         BE    NIUNIT1             ..ONE OF ALLOCATED PRDS                      
         CLC   NBSPLPRN,NBPRD2     ..(E.G. BILLED AND PROD CHANGE)              
         BNE   XIT2                ..IGNORE                                     
NIUNIT1  CLI   NDFLAVOR,C'V'                                                    
         BNE   *+8                                                              
         MVI   SKIPAE,C'Y'                                                      
         CLI   NDFLAVOR,C'E'                                                    
         BNE   *+8                                                              
         MVI   SKIPAE,C'Y'                                                      
         BAS   RE,FILTUNIT                                                      
         MVI   SKIPAE,0                                                         
         BNE   XIT2                                                             
         LA    R2,GLARGS                                                        
         LA    R0,4                                                             
         SPACE 1                                                                
NIUNIT2  CLI   0(R2),0                                                          
         BE    XIT2                                                             
         CLI   0(R2),255           255=TOTAL                                    
         BE    NIUNIT4                                                          
         CLC   0(1,R2),NBLEN       ELSE MATCH ON LENGTH                         
         BNE   NIUNIT6                                                          
         SPACE 1                                                                
NIUNIT4  MVI   3(R3),30            PASS 30 FOR NUMBER OF UNITS                  
         CLI   GLARGS+7,C'E'       FORCED TO EQUIV                              
         BNE   *+10                                                             
         MVC   3(1,R3),NBLEN                                                    
         MVC   7(1,R3),NBLEN       AND LENGTH FOR EQUIV                         
         CLI   GLARGS+7,C'R'       FORCED TO RAW                                
         BNE   *+8                                                              
         MVI   7(R3),30                                                         
         CLI   GLARGS+1,C'A'       IF ACCOUNTING UNIT                           
         BNE   NIUNIT6                                                          
         TM    NBUNITST,X'42'      AND PREEMPTED OR MISSED                      
         BZ    NIUNIT6                                                          
         MVI   3(R3),0           SET TO 0 UNITS COUNT                           
         SPACE 1                                                                
NIUNIT6  LA    R2,1(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,NIUNIT2                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NOUNIT   LA    R1,6                (EXPECTED WIDTH)                             
         CLI   GLARGS+4,0                                                       
         BE    *+8                                                              
         LA    R1,5                                                             
         CLI   GLARGS+1,0                                                       
         BNE   *+8                 DON'T ADJUST IF MULTI COLUMN                 
         BAS   RE,ADJOUT           (MAY BE DIFFERENT)                           
         LA    R4,GLARGS                                                        
         LA    R0,4                                                             
         SPACE 1                                                                
NOUNIT2  CLI   0(R4),0                                                          
         BE    XIT2                                                             
         MVC   MYUNITSR,0(R2)                                                   
         MVC   MYUNITSE,4(R2)                                                   
         BAS   RE,DOUNIT                                                        
         LA    R2,8(R2)                                                         
         LA    R3,6(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,NOUNIT2                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NHUNIT   LA    R2,GLARGS           HEADING ROUTINE                              
         LA    R3,192(R3)          STARTS ON SECOND LINE                        
         LA    R4,4                                                             
         SPACE 1                                                                
NHUNIT2  CLI   0(R2),0                                                          
         BE    XIT2                                                             
         MVC   0(6,R3),=C' TOTAL'                                               
         CLI   0(R2),255                                                        
         BE    NHUNIT4                                                          
         EDIT  (1,0(R2)),(5,0(R3))                                              
         MVI   5(R3),C'S'                                                       
         LA    R1,2(R3)                                                         
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R3)                                                         
         MVI   0(R1),C':'                                                       
         SPACE 1                                                                
NHUNIT4  LA    R2,1(R2)                                                         
         LA    R3,6(R3)                                                         
         BCT   R4,NHUNIT2                                                       
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO HANDLE A STACK OF UNITS                               
         SPACE 3                                                                
DOUNIT   NTR1                                                                   
         LA    R2,MYDEFLST         SET UP TO HANDLE LIST                        
         LA    R4,8                                                             
         SPACE 1                                                                
DOUNTLST MVC   BYTE,0(R2)                                                       
         NI    BYTE,X'0F'                                                       
         MVC   MYUNITS,MYUNITSR    PICK OFF RAW OR EQUIV                        
         TM    0(R2),X'20'         DICTATED BY X'20' BIT                        
         BNO   *+10                                                             
         MVC   MYUNITS,MYUNITSE                                                 
         CLI   BYTE,2                                                           
         BL    DOUNT1                                                           
         BE    DOUNT2                                                           
         CLI   BYTE,4                                                           
         BL    DOUNT3                                                           
         BE    DOUNT4                                                           
         B     DOUNT5                                                           
         SPACE 1                                                                
DOUNT1   L     RF,MYUNITS          DATA=1                                       
         LTR   RF,RF                                                            
         BZ    DOUNTNXT                                                         
         SR    RE,RE                                                            
         CLI   GLARGS+4,1          OPTIONAL EDITING 1=0 DECS!                   
         BE    DOUNT10                                                          
         D     RE,=F'3'                                                         
         EDIT  (RF),(8,DMCB),1                                                  
         MVC   0(6,R3),DMCB+2                                                   
         CLI   DMCB+1,C' '                                                      
         BE    DOUNTNXT                                                         
         SH    R3,=H'2'                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         MVC   0(2,R3),DMCB                                                     
         LA    R3,2(R3)                                                         
         B     DOUNTNXT                                                         
         SPACE 1                                                                
DOUNT10  D     RE,=F'30'           0 DEC                                        
         EDIT  (RF),(5,(R3))                                                    
         B     DOUNTNXT                                                         
         SPACE 1                                                                
DOUNT4   MVC   DUB,MYUNITSE        RAW V EQUIV INDEX =4                         
         MVC   DUB+4(4),MYUNITSR                                                
         LR    RF,R2                                                            
         LA    R2,DUB                                                           
         BAS   RE,INDEX            AND SHOW INDEX                               
         LR    R2,RF                                                            
         B     DOUNTNXT                                                         
         SPACE 1                                                                
DOUNT2   DS    0H                  CPU=2                                        
DOUNT3   DS    0H                  POST INDEX=3                                 
DOUNT5   MVI   0(R3),0             SPACE=5                                      
         SPACE 1                                                                
DOUNTNXT LA    R3,198(R3)                                                       
         LA    R2,1(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XIT2                                                             
         BCT   R4,DOUNTLST                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              SOME NUMERIC UNIT FIELDS                                         
         SPACE 3                                                                
*              ARGUMENTS           SEE FILTUNIT                                 
         SPACE 1                                                                
NILEN    CLI   NBMODE,NBPROCPP                                                  
         BNE   NILEN2                                                           
         BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         B     NILEN3                                                           
         SPACE 1                                                                
NILEN2   BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
NILEN3   CLI   GLARGS,C'D'         DL DOWNLOAD (DLLEN ENTRY)                    
         BNE   NILEN4                                                           
         EDIT  (B1,NBLEN),(3,0(R3))                                             
         B     XIT2                                                             
NILEN4   MVC   3(1,R3),NBLEN                                                    
         B     XIT2                                                             
         SPACE 1                                                                
*--BUDGET LENGTH                                                                
NIBLEN   XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
         BAS   RE,FILTPUP                                                       
         BNE   XIT2                                                             
         XC    NBSDRTCV,NBLEN                                                   
         XC    NBLEN,NBSDRTCV                                                   
         XC    NBSDRTCV,NBLEN                                                   
         MVC   3(1,R3),NBSDRTCV                                                 
         B     XIT2                                                             
         SPACE 1                                                                
*--PUP PLAN COSTS                                                               
         SPACE 1                                                                
*--PACKAGE ADJUSTMENT                                                           
NIPPKGA  CLI   NBMODE,NBPROCPP     PUP MODE                                     
         BNE   XIT2                                                             
         MVC   0(4,R3),NBNGUFAC                                                 
         B     XIT2                                                             
         SPACE 1                                                                
*--DEMO ADJUSTMENT                                                              
NIPDEMA  CLI   NBMODE,NBPROCPP     PUP MODE                                     
         BNE   XIT2                                                             
         MVC   0(4,R3),NBPAKCST                                                 
         B     XIT2                                                             
         SPACE 1                                                                
*--CPM GUARANTEE                                                                
NIPCPMG  CLI   NBMODE,NBPROCPP     PUP MODE                                     
         BNE   XIT2                                                             
         MVC   0(4,R3),NBPAKCPM                                                 
         B     XIT2                                                             
         SPACE 3                                                                
NICOST   XC    0(8,R3),0(R3)                                                    
         CLI   NBMODE,NBPROCGL                                                  
         BE    XIT2                                                             
         BAS   RE,GETCOST          RETURNS DOLLARS AND CENTS                    
         BAS   RE,SPLITR3                                                       
         LA    R3,4(R3)                                                         
         BAS   RE,SPLITR3                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NIGCOST  NI    GLARGS+12,X'FF'-X'03'   CLEAR EST/ACT BITS                       
*        MVI   GLARGS+12,0                                                      
         BAS   RE,FILTGOAL                                                      
         BNE   XIT2                                                             
         BAS   RE,GETCOST          RETURNS DOLLARS AND CENTS                    
         CLI   GLARGS,C'P'         STORE IN PACKED FORMAT                       
         BNE   XIT2                                                             
         XC    0(8,R3),0(R3)                                                    
         L     RE,NDAGBLOK                                                      
         USING NETGOALD,RE                                                      
         L     R1,NGOALDOL                                                      
         CVD   R1,0(R3)                                                         
         B     XIT2                                                             
         DROP  RE                                                               
         SPACE 1                                                                
NOCOST   BAS   RE,OUTCOST                                                       
         BAS   RE,DOWNZERO                                                      
         CLI   0(R3),C'0'          DOWNLOAD ZERO GETS OUT HERE                  
         BE    XIT2                                                             
         CLI   GLARGS,2                                                         
         BE    NOCOST2                                                          
         BH    NOCOST3                                                          
         EDIT  (R1),(9,(R3)),FLOAT=-                                            
         B     XIT2                                                             
         SPACE 1                                                                
NODLCST  EDIT  (P8,1(R2)),(10,0(R3)),2,FILL=0,ZERO=NOBLANK                      
         B     XIT2                                                             
         SPACE 1                                                                
NIDLGAP  MVI   0(R3),X'40'                                                      
         B     XIT2                                                             
         SPACE 1                                                                
NIDLTYPO MVI   0(R3),C'O'                                                       
         B     XIT2                                                             
         SPACE 1                                                                
NIDLTYPA MVI   0(R3),C'A'                                                       
         B     XIT2                                                             
NODLGAP  MVC   0(4,R3),D2SPACES                                                 
         B     XIT2                                                             
         SPACE 1                                                                
NOCOST2  AH    R1,=H'50'           COST TO NEAREST 00                           
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(8,(R3)),1,FLOAT=-                                          
         B     XIT2                                                             
         SPACE 1                                                                
NOCOST3  AH    R1,=H'500'          COST TO NEAREST 000                          
         SR    R0,R0                                                            
         D     R0,=F'1000'                                                      
         BAS   RE,DOWNZERO                                                      
         EDIT  (R1),(6,(R3)),FLOAT=-                                            
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINES TO SPLIT COST FOR SPLIT BILLING                         
         SPACE 3                                                                
SPLITR3  NTR1                                                                   
         CLI   NDSPLOPT,0                                                       
         BE    XIT2                                                             
         L     R4,NDASPLBL                                                      
         USING SPLTBLKD,R4                                                      
         MVC   SPLAMT,0(R3)        R3=A(AMOUNT TO BE SPLIT)                     
         BAS   RE,SPLITEM                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(R3)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
SPLITDUB NTR1                                                                   
         CLI   NDSPLOPT,0                                                       
         BE    XIT2                                                             
         L     R4,NDASPLBL                                                      
         USING SPLTBLKD,R4                                                      
         CVB   R1,DUB              DUB HAS AMOUNT TO BE SPLIT                   
         ST    R1,SPLAMT                                                        
         BAS   RE,SPLITEM                                                       
         B     XIT2                                                             
         SPACE 1                                                                
SPLITEM  NTR1                                                                   
         MVI   SPLPRDO,0                                                        
         MVI   SPLPRECI,X'02'      DEFAULT PRECISION CENTS                      
         CLI   NDSPLOPT,100                                                     
         BNE   *+8                                                              
         MVI   SPLPRECI,X'04'      OPTIONALLY DOLLARS                           
         LA    R1,SPLITEM2                                                      
         ST    R1,SPLAHOOK                                                      
         GOTO1 =V(NETSPB),DMCB,(R4)                                             
         B     XIT2                                                             
         SPACE 1                                                                
SPLITEM2 NTR1                                                                   
         MVC   DUB,SPLODOLS                                                     
         B     XIT2                                                             
         DROP  R4                                                               
         EJECT                                                                  
*              ACCGEN - GENERAL COST EXTRACT                                    
         SPACE 3                                                                
*              ARGUMENTS           SEE FILTUNIT                                 
         SPACE 1                                                                
NIACCGEN ZAP   0(9,R3),=P'0'                                                    
         CLI   NBRDBELS,2          CHECK IF SKIPPING ACCGEN ENTRIES             
         BE    XIT2                                                             
         CLI   NDACCPRD,0         CHECK ACC=PRD OPTION FILTER                   
         BE    *+14                                                             
         CLC   NBSPLPRN,NDACCPRD                                                
         BNE   XIT2                                                             
         BAS   RE,FILTUNIT                                                      
         BNE   XIT2                                                             
         MVC   ACCGENNO,GLARGS                                                  
         BAS   RE,DOACCGEN                                                      
         B     XIT2                                                             
         SPACE 1                                                                
ACCGENNO DC    X'00'                                                            
         SPACE 1                                                                
DOACCGEN NTR1                                                                   
         XC    DMCB(16),DMCB                                                    
         ST    R3,DMCB             A(9 BYTE OUTPUT)                             
         MVC   DMCB(1),ACCGENNO    PASS ROUTINE NUMBER                          
         CLI   ACCGENNO,219        ASTPPCT ENTRY                                
         BE    ACG2                HAS SOFT PERCENT                             
         CLI   ACCGENNO,157        BLTG% HAS SOFT PERCENT                       
         BE    ACG2                HAS SOFT PERCENT                             
         CLI   ACCGENNO,141        141-150 HAVE SOFT PERCENTS                   
         BL    ACG4                                                             
         CLI   ACCGENNO,150                                                     
         BH    ACG4                                                             
         SPACE 1                                                                
ACG2     OC    NDPERCNT,NDPERCNT   IF THERE WERE SOFT PERCENTS                  
         BZ    ACG4                                                             
         MVC   DMCB+8(4),NDPERCNT  PASS PERCENT ADJUSTMENT                      
         CLC   NDPERCNT,=C'COST'   IF PERCENTS ARE SOFT BY BRAND                
         BNE   ACG4                                                             
         ZIC   R1,NBSPLPRN                                                      
         BCTR  R1,0                   DISPLACE INTO COST POOL                   
         SLL   R1,2                                                             
         A     R1,NDACPOOL                                                      
         MVC   DMCB+8(4),0(R1)        PASS PERCENT FOR THIS BRAND               
         MVI   DMCB+8,4               WHICH HAS 4 DEC PLACES                    
         SPACE 1                                                                
ACG4     DS    0H                                                               
         CLI   GLARGS+7,0          IF BILLDATE COLUMN FILTERING                 
         BNH   ACG4A                                                            
         CLI   GLARGS+7,10                                                      
         BH    ACG4A                                                            
         L     R1,NBABDFLT         GET BILLDATE FILTER TABLE OF DATES           
         ZIC   RE,GLARGS+7         GET POSITION                                 
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         AR    R1,RE                                                            
         MVC   NBBILSTR,0(R1)      SET DATES FROM BDF TABLE                     
         MVC   NBBILEND,2(R1)                                                   
ACG4A    GOTO1 =V(NETACC),DMCB,,NETBLOCK                                        
         CLI   GLARGS+7,0          ..IF USING COLUMN FILTERS                    
         BNH   ACG5                                                             
         CLI   GLARGS+7,10                                                      
         BH    ACG5                                                             
         XC    NBBILSTR,NBBILSTR    ..CLEAR FILTER FIELDS                       
         XC    NBBILEND,NBBILEND                                                
ACG5     MVC   DUB,1(R3)           MAY ADJUST FOR SPLIT BILLING                 
         BAS   RE,SPLITDUB                                                      
         MVC   1(8,R3),DUB                                                      
         LR    R4,R3                                                            
         S     R4,=F'100'                                                       
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO FILTER UNITS                                          
         SPACE 3                                                                
*              ARGUMENTS                                                        
*                                   9 DAY CODE                                  
*                                  10 PRODUCT NUMBER                            
*                                  11 OFFICE CODE                               
*                                  12 1=ABC  2=CBS  3=NBC                       
*                                     11=NET 12=CAB 13=SYD 14=OTH               
*                                  13 A=ACTUAL E=ESTIMATED                      
*                                  14 SPOT LENGTH                               
*                                  15 DAYPART                                   
*                                  16 PERIOD NUMBER                             
         SPACE 1                                                                
FILTUNIT NTR1                                                                   
         CLI   NBMODE,NBPROCUN     CHECK WE ARE PROCESSING A UNIT               
         BNE   FILTNO                                                           
         B     FILTDAY                                                          
         SPACE 1                                                                
FILTGOAL NTR1                                                                   
         CLI   NBMODE,NBPROCGL     CHECK WE ARE PROCESSING A GOAL               
         BNE   FILTNO                                                           
         B     FILTDAY                                                          
         SPACE 1                                                                
FILTPUP  NTR1                                                                   
         CLI   NBMODE,NBPROCPP     CHECK WE ARE PROCESSING A PUP                
         BNE   FILTNO                                                           
         SPACE 1                                                                
FILTDAY  CLI   GLARGS+8,0          DAY CODE FILTER                              
         BE    FILTPROD                                                         
         CLC   NBDAY,GLARGS+8                                                   
         BNE   FILTNO                                                           
         SPACE 1                                                                
* - PRODUCT FILTER/PRODUCT GROUP FILTER                                         
FILTPROD CLI   NBMODE,NBPROCPP     CHECK WE ARE PROCESSING A PUP                
         BE    FILTPLF                                                          
         CLI   GLARGS+9,0                                                       
         BE    FILTOFF                                                          
         L     R3,NBAPLIST         IS IT PROD GROUP FILTER                      
         LTR   R3,R3                                                            
         BZ    FPRD10              NO                                           
         CLI   0(R3),0                                                          
         BE    FPRD10              NO                                           
         ZIC   R1,GLARGS+9                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'256'                                                       
         AR    R3,R1                                                            
         LA    R1,256                                                           
         MVC   BYTE,NBSPLPRN       ASSUME SPLIT PROD NUMBER                     
         TM    NBSPLOPT,X'80'                                                   
         BO    *+10                                                             
         MVC   BYTE,NBPRD          UNLESS SPLIT NOT ON                          
         CLC   BYTE,0(R3)                                                       
         BE    FILTOFF                                                          
         LA    R3,1(R3)                                                         
         BCT   R1,*-14                                                          
         B     FILTNO                                                           
*                                                                               
*                                                                               
FPRD10   CLI   GLARGS+9,0          PRODUCT FILTER                               
         BE    FILTOFF                                                          
         MVC   BYTE,NBSPLPRN       ASSUME SPLIT PROD NUMBER                     
         TM    NBSPLOPT,X'80'                                                   
         BO    *+10                                                             
         MVC   BYTE,NBPRD          UNLESS SPLIT NOT ON                          
         CLC   BYTE,GLARGS+9                                                    
         BNE   FILTNO                                                           
         B     FILTOFF                                                          
         EJECT                                                                  
* - PLAN FILTER                                                                 
FILTPLF  CLI   GLARGS+9,0                                                       
         BE    FILTOFF                                                          
         LA    R3,NDPRGFLT         YES/GET PROG GROUP TABLE                     
         SR    R1,R1                                                            
         IC    R1,GLARGS+9         GET DISPLACEMENT                             
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         AR    R3,R1                                                            
         LA    RE,NBPKFILT                                                      
         LA    RF,3                                                             
FLTPLF10 CLI   0(R3),C'*'          CHECK FOR BYPASS                             
         BE    FLTPLF20                                                         
         CLC   0(1,R3),0(RE)       CHECK FOR PLAN FILTER MATCH                  
         BNE   FILTNO                                                           
FLTPLF20 LA    R3,1(R3)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,FLTPLF10                                                      
         SPACE 1                                                                
FILTOFF  CLI   GLARGS+10,0         OFFICE FILTER                                
         BE    FILTNET                                                          
         CLC   NBEFFOFF,GLARGS+10                                               
         BNE   FILTNO                                                           
         SPACE 1                                                                
FILTNET  CLI   GLARGS+11,0         NETWORK FILTERS                              
         BE    FILTAE                                                           
         CLI   GLARGS+11,2                                                      
         BL    FILTNET1                                                         
         BE    FILTNET2                                                         
         CLI   GLARGS+11,4                                                      
         BL    FILTNET3                                                         
         BE    FILTNET4                                                         
         B     FILTMED             (HIGH VALUES ARE MEDIA)                      
         SPACE 1                                                                
FILTNET1 CLC   NBACTNET(3),=C'ABC' 1=ABC                                        
         BNE   FILTNO                                                           
         B     FILTAE                                                           
         SPACE 1                                                                
FILTNET2 CLC   NBACTNET(3),=C'CBS' 2=CBS                                        
         BNE   FILTNO                                                           
         B     FILTAE                                                           
         SPACE 1                                                                
FILTNET3 CLC   NBACTNET(3),=C'NBC' 3=NBC                                        
         BNE   FILTNO                                                           
         B     FILTAE                                                           
         SPACE 1                                                                
FILTNET4 CLC   NBACTNET(3),=C'FOX' 4=FOX                                        
         BE    FILTAE                                                           
         CLC   NBACTNET(3),=C'FBC'                                              
         BNE   FILTNO                                                           
         B     FILTAE                                                           
         SPACE 1                                                                
         CLI   GLARGS+11,2                 2=CBS                                
         BNE   *+14                                                             
         CLC   NBACTNET(3),=C'CBS'                                              
         BNE   FILTNO                                                           
         CLI   GLARGS+11,3                 3=NBC                                
         BNE   *+14                                                             
         CLC   NBACTNET(3),=C'NBC'                                              
         BNE   FILTNO                                                           
         CLI   GLARGS+11,4                 4=FOX                                
         BNE   *+14                                                             
         CLC   NBACTNET(3),=C'FOX'                                              
         BNE   FILTNO                                                           
         B     FILTAE                                                           
         SPACE 1                                                                
FILTMED  CLC   NBEFFMED,GLARGS+11  MEDIA FILTER                                 
         BNE   FILTNO                                                           
         SPACE 1                                                                
FILTAE   TM    GLARGS+12,X'03'     ACTUAL/ESTIMATED                             
*        CLI   GLARGS+12,0         ACTUAL/ESTIMATED                             
         BZ    FILTSL                                                           
         B     FILTSL              BYPASS THIS ROUTINE (FOR NOW)                
******   CLI   SKIPAE,C'Y'         (INTERNAL SWITCH TO SKIP THIS)               
******   BE    FILTSL                                                           
         CLI   NDFLAVOR,C'E'        ONLY TEST FOR M OR P                        
         BE    FILTSL                                                           
         CLI   NDFLAVOR,C'V'                                                    
         BE    FILTSL                                                           
         TM    GLARGS+12,X'01'                                                  
         BNO   *+14                                                             
         OC    NBACTUN,NBACTUN                                                  
         BZ    FILTNO                                                           
         TM    GLARGS+12,X'02'                                                  
         BNO   *+14                                                             
         OC    NBESTUN,NBESTUN                                                  
         BZ    FILTNO                                                           
         SPACE 1                                                                
FILTSL   CLI   GLARGS+13,0         'SPOT' LENGTH FILTER                         
         BE    FILTDPT                                                          
         CLC   NBLEN,GLARGS+13                                                  
         BNE   FILTNO                                                           
         SPACE 1                                                                
FILTDPT  CLI   GLARGS+14,0         DAYPART FILTER                               
         BE    FILTPER             (GLRGX+0)                                    
         ZIC   R1,GLARGS+14        (INDEX)                                      
         MH    R1,=Y(NDGLRGXE)     X (LENGTH)                                   
         L     RE,NDAGLARG                                                      
         AR    RE,R1                                                            
         CLC   NBACTDP,0(RE)                                                    
*        CLC   NBACTDP,GLARGS+14                                                
         BNE   FILTNO                                                           
         SPACE 1                                                                
FILTPER  CLI   GLARGS+15,0         PERIOD FILTER                                
         BE    FILTMAT                                                          
         ZIC   R1,GLARGS+15        (USE PERIOD NUMBER)                          
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,NDADATES         (TO DISPLACE INTO DATE TABLE)                
         CLC   NBACTDAT,0(R1)      (CHECK START)                                
         BL    FILTNO                                                           
         CLC   NBACTDAT,2(R1)      (CHECK END)                                  
         BH    FILTNO                                                           
         SPACE 1                                                                
FILTMAT  TM    GLARGS+12,X'0C'     MATCH/UNMATCH FILTER                         
         BZ    FILTYES                                                          
         TM    GLARGS+12,X'04'     MATCHED ONLY                                 
         BNO   *+16                                                             
         CLI   NBAFFTIM,0          MUST HAVE AFFID TIME                         
         BE    FILTNO                                                           
         B     FILTYES                                                          
         TM    GLARGS+12,X'08'     UNMATCHED ONLY                               
         BO    *+6                                                              
         DC    H'0'                                                             
         CLI   NBAFFTIM,0                                                       
         BNE   FILTNO                                                           
         B     FILTYES                                                          
         SPACE 1                                                                
FILTYES  DS    0H                                                               
         SR    R1,R1                                                            
         B     FLTN02                                                           
         SPACE 1                                                                
FILTNO   DS    0H                                                               
         LA    R1,1                                                             
FLTN02   LTR   R1,R1                                                            
         B     XIT2                                                             
         EJECT                                                                  
*                                                                               
NIBHCTP  DS    0H                  COST TYPE                                    
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   2(1,R3),BHCTYP                                                   
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO ADDRESS CORRECT 16 BYTE DEMOS                         
         SPACE 3                                                                
*              INPUT (GLARGS)      1 RELATIVE DEMO NO (0=HOMES)                 
*                                  8  E(QUIV) R(AW) X'00' BOTH                  
*                                  13 E OR A                                    
*              OUTPUT              (R4)=A(16-BYTE DEMOS)                        
*                                  VPH(2) GRP(2) IMPS(4)                        
*                                  8 BYTES RAW, 8 BYTES EQUIV                   
         SPACE 1                                                                
DEMADJ   NTR1                                                                   
         CLI   NBMODE,NBPROCGL     (ELSEWHERE FOR GOALS)                        
         BE    GOALADJ                                                          
         L     R5,NBADEM                                                        
         USING NDDEMBLK,R5                                                      
         L     R1,NDARAWEX                                                      
         USING RAWDATA,R1                                                       
         XC    DADJAREA,DADJAREA                                                
         CLI   GLARGS,0                                                         
         BE    DEMADJH                                                          
         CLI   GLARGS,255                                                       
         BNE   DEMADJD                                                          
         B     DADJXIT                                                          
         SPACE 1                                                                
DEMADJH  MVC   DADJRAW,RWESTHOM    HOMES                                        
         MVC   DADJEQU,NBESTHOM                                                 
*        CLI   GLARGS+12,C'A'                                                   
         TM    GLARGS+12,X'01'                                                  
         BNO   DADJXIT                                                          
         MVC   DADJRAW,RWACTHOM                                                 
         MVC   DADJEQU,NBACTHOM                                                 
         LA    R4,NBACTHOM                                                      
         B     DADJXIT                                                          
         SPACE 1                                                                
DEMADJD  BAS   RE,SETDEMNO         SET RELATIVE DEMO NUMBER                     
         ZIC   R2,THISRELN         PICK UP RELATIVE DEMO NUMBER                 
         CLI   THISRELN,255        .IF NO MATCH                                 
         BE    DADJXIT             .KICK OUT                                    
         LTR   R2,R2                                                            
         BZ    DEMADJH                                                          
DEMADJ3  BCTR  R2,0                CALCULATE OFFSET                             
         SLL   R2,3                                                             
         LA    R4,RWESTDEM         GET RAW DEMOS                                
**       CLI   GLARGS+12,C'A'          FOR EST OR ACTUAL                        
         TM    GLARGS+12,X'01'         FOR EST OR ACTUAL                        
         BNO   *+8                                                              
         LA    R4,RWACTDEM                                                      
         AR    R4,R2                                                            
         MVC   DADJRAW,0(R4)                                                    
         LA    R4,NDESTDEM         GET EQUIVALENT DEMOS                         
*        CLI   GLARGS+12,C'A'          FOR EST OR ACTUAL                        
         TM    GLARGS+12,X'01'         FOR EST OR ACTUAL                        
         BNO   *+8                                                              
         LA    R4,NDACTDEM                                                      
         AR    R4,R2                                                            
         MVC   DADJEQU,0(R4)                                                    
         B     DADJXIT                                                          
         EJECT                                                                  
*              ADDRESS DEMOS FOR GOALS                                          
         SPACE 3                                                                
GOALADJ  XC    DADJAREA,DADJAREA                                                
         L     R5,NDAGBLOK                                                      
         USING NETGOALD,R5                                                      
         SR    R1,R1                                                            
         CLI   GLARGS,21                                                        
         BE    GLADJ5                                                           
         CLI   GLARGS,1                                                         
         BNE   *+8                                                              
GLADJ5   L     R1,NGOALGRP         GET FIRST                                    
         CLI   GLARGS,2                                                         
         BE    GLADJ10                                                          
         CLI   GLARGS,22                                                        
         BNE   *+8                                                              
GLADJ10  L     R1,NGOALG2          OR SECOND TARGET                             
         STH   R1,MYGRPSR          THESE ARE RAW                                
******                             EQUIVALENCE THIS                             
*****    ZIC   R0,NGOALSL          NO - GOALS S/B RAW                           
*****    MR    R0,R0                                                            
*****    ZIC   RF,NDQBASE                                                       
*****    LTR   RF,RF                                                            
*****    BNZ   *+8                                                              
*****    LA    RF,30                                                            
*****    DR    R0,RF                                                            
         STH   R1,MYGRPSE          AND SAVE                                     
         DROP  R1                                                               
         DROP  R5                                                               
         SPACE 1                                                                
DADJXIT  LA    R4,DADJAREA                                                      
         CLI   GLARGS+7,C'R'       IF RAW REQUESTED,                            
         BNE   *+10                                                             
         MVC   DADJEQU,DADJRAW        JUST PASS THAT                            
         CLI   GLARGS+7,C'E'       IF EQUIV REQUESTED,                          
         BNE   *+10                                                             
         MVC   DADJRAW,DADJEQU        JUST PASS THAT                            
         XIT1  REGS=(R4)                                                        
         SPACE 1                                                                
         DS    0F                                                               
DADJAREA DS    0CL16               PASSED BACK TO CALLER                        
DADJRAW  DS    0CL8                RAW DATA                                     
MYVPHR   DS    CL2                                                              
MYGRPSR  DS    CL2                                                              
MYIMPSR  DS    CL4                                                              
DADJEQU  DS    0CL8                EQUIVALENT DATA                              
MYVPHE   DS    CL2                                                              
MYGRPSE  DS    CL2                                                              
MYIMPSE  DS    CL4                                                              
MYGRPR4  DS    F                                                                
MYGRPE4  DS    F                                                                
THISDEMN DS    XL3                                                              
THISRELN DS    XL1                                                              
SAVEVPHR DS    XL2                                                              
         EJECT                                                                  
*              CONVERT TARGET DEMO NUMBERS TO RELATIVE                          
         SPACE 3                                                                
*              INPUT               GLARGS(1)                                    
*              OUTPUT              THISRELN                                     
         SPACE 1                                                                
SETDEMNO NTR1                                                                   
         MVC   THISRELN,GLARGS     NUMBERS 1-20 ARE RELATIVE                    
         CLI   THISRELN,21                                                      
         BL    XIT2                                                             
*                                                                               
         CLI   NBMODE,NBPROCPP     CHECK PUP REPORT                             
         BNE   SETDN1                                                           
         MVI   THISRELN,1                                                       
         CLI   GLARGS,21                                                        
         BE    XIT2                                                             
         MVI   THISRELN,2                                                       
         B     XIT2                                                             
*                                                                               
SETDN1   L     R4,=A(GETPESTB)     21 AND 22 ARE TARGETS                        
*                                  NEED TO LOOK UP TARGETS                      
         USING GETPESTD,R4                                                      
         ST    R4,DMCB                                                          
         GOTO1 =V(GETPEST),DMCB,,NETBLOCK                                       
         L     R1,GTPBAENT                                                      
         MVC   DUB,0(R1)           TO SEE WHAT IS THERE *** TEST                
         DROP  R4                                                               
         LA    R1,2(R1)                                                         
         CLI   GLARGS,22                                                        
         BNE   *+8                                                              
         LA    R1,3(R1)                                                         
         MVC   THISDEMN,0(R1)      PICK UP 3 BYTE DEMO CODE                     
         MVC   DUB+8(3),0(R1)      *** TEST                                     
         SPACE 1                                                                
         MVI   THISRELN,0                                                       
         CLI   THISDEMN+1,X'21'    IS IT USER DEMOS                             
         BE    SETUSERD                                                         
         CLI   THISDEMN+2,1                                                     
         BE    XIT2                                                             
         L     R5,NBADEM                                                        
         USING NDDEMBLK,R5                                                      
         LA    R2,NDDEMOS+2        NOW CONVERT THIS TO RELATIVE NO              
         LA    R1,1                ** POINTS TO DEMO CODE                       
         LA    R0,20                                                            
         SPACE 1                                                                
SETDN2   CLC   THISDEMN+2(1),0(R2) ** POINTS TO DEMO CODE                       
         BE    SETDN4                                                           
         LA    R2,3(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SETDN2                                                        
         MVI   THISRELN,255        PXZ 11/19/90 IF NO MATCH OF TARGET           
         B     XIT2                                                             
         SPACE 1                                                                
SETDN4   STC   R1,THISRELN                                                      
         B     XIT2                                                             
*                                                                               
SETUSERD L     R5,NBADEM           IS USER DEMO IN CONTROL ESTIMATE             
         USING NDDEMBLK,R5                                                      
         LA    R1,1                                                             
         LA    R4,NDDEMOS                                                       
         ZIC   R0,NDNDEMOS                                                      
SETU1    CLC   THISDEMN,0(R4)                                                   
         BE    SETU5                                                            
         LA    R4,3(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SETU1                                                         
         MVI   THISRELN,255        NO/EXIT                                      
         B     XIT2                                                             
SETU5    STC   R1,THISRELN        YES/PASS BACK RELATIVE POSITION               
         B     XIT2                                                             
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
NICSTAT  MVC   2(2,R3),=C'$$'      UNIT COST STATUS                             
         OC    NBACTUAL,NBACTUAL   IF NO ACTUAL DOLLARS                         
         BNZ   XIT2                                                             
         MVC   2(2,R3),=C'0$'      MARK IT AS 'FREE'                            
         B     XIT2                                                             
*                                                                               
IMPDIV10 NTR1                                                                   
         TM    NBINDS,X'40'        INCREASE PRECISION SET                       
         BZ    IMP10X                                                           
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         CLI   DROARGS+12,C'A'     IF DEALING WITH ACTUALS                      
         BE    IMP10X              SKIP                                         
         DROP  R1                                                               
         ICM   R1,15,MYIMPS                                                     
         LTR   R1,R1                                                            
         BZ    IMP10X                                                           
         SR    R0,R0                                                            
         A     R1,=F'50'          ..ROUND TO NEAREST TENTH                      
         D     R0,=F'100'         ..SO NET IMPS END IN ZERO                     
         SR    R0,R0                                                            
         ST    R1,MYIMPS                                                        
         CLC   MYN15S,MY15S        ..BUT IF NOT NETWORK                         
         BNE   IMP10X              ..LEAVE AS IS                                
*                                                                               
         CLI   NDPREOPT,C'Y'       ..IF ALSO  PRE=CAB                           
         BNE   IMP10AX                                                          
         SR    R0,R0                                                            
         A     R1,=F'50'          ..ROUND TO NEAREST TENTH                      
         D     R0,=F'100'         ..SO NET IMPS END IN ZERO                     
*                                                                               
IMP10AX  M     R0,=F'100'          ..ELSE IF NET PUT BACK OUT TO 000            
         ST    R1,MYIMPS                                                        
IMP10X   XIT1                                                                   
*                                                                               
NOCOMMNT LA    R1,197(R2)          COMMENTS                                     
         LA    R0,198              GET LENGTH OF COMMENT                        
         CLI   NBMODE,NBPROCPP                                                  
         BNE   NOCMN10                                                          
         LA    R1,67(R2)           PUP PARAMETERS                               
         LA    R0,68                                                            
NOCMN10  CLI   0(R1),X'40'                                                      
         BH    NOCMN12                                                          
         BCTR  R1,0                                                             
         BCT   R0,NOCMN10                                                       
         B     XIT2                                                             
NOCMN12  ST    R2,DMCB             A(INPUT)                                     
         STC   R0,DMCB             L'INPUT                                      
         CLI   MYLTYP2,C'H'                                                     
         BNE   *+8                                                              
         MVI   NDCMHEAD,C'Y'                                                    
         ST    R3,DMCB+4           A(OUTPUT)                                    
         MVI   DMCB+4,48           L'OUTPUT                                     
         CLI   NBMODE,NBPROCPP                                                  
         BNE   *+8                                                              
         MVI   DMCB+4,34           L'PUP OUTPUT LINE                            
         CLI   MYOLEN2,0                                                        
         BE    *+10                                                             
         MVC   DMCB+4(1),MYOLEN2                                                
         LA    R1,4                N'LINES                                      
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198          L'PRINT LINE                                 
         GOTO1 CHOPPER,DMCB                                                     
         B     XIT2                                                             
*                                                                               
XIT2     XIT1                                                                   
         EJECT                                                                  
         SPACE 1                                                                
NISCHED  MVI   0(R3),1            DUMMY - ALL DONE ON OUTPUT                    
         B     XIT2                                                             
         SPACE 1                                                                
NOSCHED  DS    0H                                                               
         GOTO1 =A(NOSCHD),DMCB,(RA)                                             
         B     XIT2                                                             
         EJECT                                                                  
NIBHPK   DS    0H                  PACKAGE NUMBER                               
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         MVC   0(1,R3),BHPKNUM                                                  
         B     XIT2                                                             
         DROP  R4                                                               
NOBHPK   DS    0H                                                               
         EDIT  (B1,0(R2)),(3,0(R3)),ZERO=BLANK                                  
         B     XIT2                                                             
NIBHPKNM DS    0H                  PACKAGE NAME                                 
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         CLI   GLARGS,C'B'                                                      
         BNE   NIPK10                                                           
         EDIT  (B1,BHPKNUM),(3,0(R3)),ZERO=BLANK                                
         LA    R3,4(R3)                                                         
NIPK10   MVC   0(16,R3),BHPKNAM                                                 
         B     XIT2                                                             
         EJECT                                                                  
*                                                                               
NIBRP    DS    0H                                                               
         PRINT GEN                                                              
         GOTO1 =A(XTRARTN),DMCB,(3,(RA))                                        
         PRINT NOGEN                                                            
         B     XIT2                                                             
*                                                                               
NOBRP    DS    0H                                                               
         GOTO1 =A(XTRARTN),DMCB,(4,(RA))                                        
         B     XIT2                                                             
*                                                                               
NICHQ    DS    0H                                                               
         GOTO1 =A(CHQRTN),DMCB,(RA)                                             
         B     XIT2                                                             
                                                                                
NOCHQ    DS    0H                                                               
         GOTO1 =A(CHQORTN),DMCB,(RA)                                            
         B     XIT2                                                             
         EJECT                                                                  
NIKID    L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
         CLI   0(R4),X'04'         UNITS ONLY                                   
         BNE   XIT2                                                             
         GOTO1 HEXOUT,DMCB,NUKDATE,0(R3),3,0   AIRDATE + START QRT HR           
         LA    R3,6(R3)                                                         
         MVC   0(4,R3),NUKNET                 NETWORK                           
         LA    R3,3(R3)                                                         
         CLI   0(R3),X'40'         SUPPRESS LAST BLANK OF NETWORK               
         BNH   *+8                                                              
         LA    R3,1(R3)                                                         
         MVC   0(6,R3),NUKPROG                 PROGRAM                          
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'40'         GET TO END OF PROG CODE                      
         BH    *-8                                                              
         GOTO1 HEXOUT,DMCB,NUKEST,0(R3),2     EST + SUBLINE                     
         B     XIT2                                                             
         DROP  R4                                                               
                                                                                
                                                                                
         EJECT                                                                  
*              LTORG                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ODDMENTS AND STORAGE FOR DRIVE 2                                 
         SPACE 1                                                                
         ENTRY MYOLEN2                                                          
MYOLEN2  DS    CL1                                                              
MYLTYP2  DS    CL1                                                              
SKIPAE   DC    X'00'                                                            
         DS    0F                                                               
MYIMPS   DS    F                                                                
MYGRPS   DS    F                                                                
MYHOMES  DS    F                                                                
MYHOMESR DS    F                                                                
MYHOMESE DS    F                                                                
MYUNITSR DS    F                                                                
MYUNITSE DS    F                                                                
MYUNITS  DS    F                                                                
MYALL15  DS    0F                                                               
MY15S    DS    H                                                                
MYN15S   DS    H                                                                
MYCOST   DS    F                                                                
MYECOST  DS    F                                                                
MYACOST  DS    F                                                                
MYRESULT DS    F                                                                
         ENTRY MYDEFLST                                                         
MYDEFLST DS    CL8                 SET FROM DETAIL, SUB OR TOTAL                
SAVEGRPS DS    CL96                                                             
SAVEIMPS DS    CL96                                                             
D2SPACES DC    CL198' '                                                         
         EJECT                                                                  
         SPACE 1                                                                
NOSTDLST DC    C'IMP '             1                                            
         DC    C'GRP '             2                                            
         DC    C'CPM '             3                                            
         DC    C'CPP '             4                                            
         DC    C'VPH '             5                                            
         DC    C'COST'             6                                            
         DC    C'UNIT'             7                                            
         DC    C'CPU '             8                                            
         DC    C'RPU '             9                                            
         DC    X'00'              10 (SPACE)                                    
         DC    C'   '                                                           
         DC    C'IPU '            11                                            
         DC    C'    '            12                                            
         DC    C'    '            13                                            
         DC    C'    '            14                                            
         DC    C'    '            15                                            
         DC    C'    '            16                                            
         DC    C'    '            17                                            
         DC    C'    '            18                                            
         DC    C'    '            19                                            
         DC    C'    '            20                                            
         DC    C'EST '            21                                            
         DC    C'ACT '            22                                            
         DC    C'GOAL'            23                                            
         DC    C'ECP.'            24                                            
         DC    C'ACP.'            25                                            
         DC    C'GCPP'            26                                            
         DC    C'REST'            27                                            
         DC    C'RACT'            28                                            
         DC    C'RECP'            29                                            
         DC    C'RACP'            30                                            
         DC    C'DIFF'            31                                            
         DC    C'INDX'            32                                            
         DC    C'CPX.'            33                                            
         DC    C'BDGT'            34                                            
         DC    C'CPMG'            35                                            
         EJECT                                                                  
         DROP  RB,R8,R7,R6                                                      
* SCHEDULE HEADERS                                                              
NOSCHD   NMOD1 0,**NOSCH*,R8,R7                                                 
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
*                                                                               
         MVC   0(27,R3),=C'ACTUAL SCHED./ACTUAL DEMOS.'                         
         CLC   NDFLAVOR(2),=C'V3'                                               
         BE    NOSCHX                                                           
         MVC   0(30,R3),=C'ACTUAL SCHED./ESTIMATED DEMOS.'                      
         CLC   NDFLAVOR(2),=C'V2'                                               
         BE    NOSCHX                                                           
         MVI   0(R3),X'40'                                                      
         MVC   1(29,R3),0(R3)                                                   
         MVC   0(09,R3),=C'PFBS ONLY'                                           
         CLI   NDFLAVOR+1,C'P'                                                  
         BE    NOSCHX                                                           
         MVC   0(20,R3),=C'MISSED AND MAKEGOODS'                                
         CLI   NDFLAVOR+1,C'M'                                                  
         BE    NOSCHX                                                           
         MVC   0(20,R3),=C'ACTUAL SCHEDULE     '                                
         CLI   NBSELUOP,C'A'                                                    
         BE    NOSCHX                                                           
         MVC   0(20,R3),=C'ESTIMATED SCHEDULE  '                                
         CLI   NBSELUOP,C'E'                                                    
         BE    NOSCHX                                                           
         MVI   0(R3),X'40'                                                      
         MVC   1(29,R3),0(R3)                                                   
         B     NOSCHX                                                           
*                                                                               
NOSCHX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DROP  RB,R8,R7                                                         
* CHECK INPUT ROUTINES                                                          
CHQRTN   NMOD1 0,**NECHQ*,R8,R7                                                 
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R4,NDCIDTBL                                                      
         USING BHBLOCK,R4                                                       
         ZIC   RF,GLARGS                                                        
         SLL   RF,2                                                             
         B     CHQRTNS(RF)                                                      
*                                                                               
CHQRTNS  DS    0H                                                               
         B     RCHQPRD             (0) CHQ PRODUCT                              
         B     RCHQPRD2            (1) CHQ PRODUCT PARTNER                      
         B     RCHQSPRD            (2) SPLIT CHECK PRODUCT                      
         B     RCHQCLT             (3) CHQ CLIENT                               
         B     RCHQMKST            (4) CHQ MKT/STATION (CL5)                    
         B     RCHQDAT             (5) CHQ DATE                                 
         B     RCHQNUM             (6) CHQ NUMBER                               
         B     RCHQCLDT            (7) CHQ CLEARANCE DATE                       
         B     RCHQGRS             (8) CHQ GRS                                  
         B     RCHQNET             (9) CHQ NET                                  
         B     RCHQSGRS           (10) SPLIT CHQ GROSS                          
         B     RCHQSNET           (11) SPLIT CHQ NET                            
*                                                                               
CHQX     XIT1                                                                   
         EJECT                                                                  
RCHQPRD  MVC   WORK(1),CHQPRD      PRODUCT                                      
         BAS   RE,GETPRD3                                                       
         MVC   0(3,R3),WORK                                                     
         B     CHQX                                                             
*                                                                               
RCHQPRD2 MVC   WORK(1),CHQPRD2     PRODUCT PARTNER                              
         BAS   RE,GETPRD3                                                       
         MVC   0(3,R3),WORK                                                     
         B     CHQX                                                             
*                                                                               
RCHQSPRD DS    0H                 SPLIT PRODUCT (DELETED)                       
         B     CHQX                                                             
*                                                                               
RCHQCLT  MVC   0(2,R3),CHQCLT      CLIENT                                       
         B     CHQX                                                             
*                                                                               
RCHQMKST MVC   0(5,R3),CHQMKT      MKT/STATION                                  
         B     CHQX                                                             
*                                                                               
RCHQDAT  MVC   0(2,R3),CHQDATE     CHECK DATE                                   
         B     CHQX                                                             
*                                                                               
RCHQNUM  MVC   0(7,R3),CHQNUM      CHECK NUMBER                                 
         CLI   0(R3),X'40'                                                      
         BH    CHQX                                                             
         MVC   0(7,R3),=X'40404040404040'                                       
         B     CHQX                                                             
*                                                                               
RCHQCLDT MVC   0(2,R3),CHQCLRDT    CHECK CLEARANCE DATE                         
         B     CHQX                                                             
*                                                                               
RCHQGRS  MVC   0(4,R3),CHQGRS      GROSS                                        
         B     CHQX                                                             
*                                                                               
RCHQNET  MVC   0(4,R3),CHQNET      NET                                          
         B     CHQX                                                             
*                                                                               
RCHQSGRS DS    0H                  SPLIT GROSS                                  
         B     CHQX                                                             
*                                                                               
RCHQSNET DS    0H                  SPLIT NET                                    
         B     CHQX                                                             
*                                                                               
* - EXPECTS 1 BYTE PROD CODE IN WORK                                            
* - RETURNS 3 BYTE PROD CODE IN WORK                                            
GETPRD3  NTR1                                                                   
         L     R2,NBACLI                                                        
         LA    R3,220                                                           
GP3      CLC   3(1,R2),WORK                                                     
         BE    GP5                                                              
         LA    R2,4(R2)                                                         
         BCT   R3,GP3                                                           
         MVC   WORK(3),=C'***'                                                  
         B     *+10                                                             
GP5      MVC   WORK(3),0(R2)                                                    
         XIT1                                                                   
         EJECT                                                                  
* CHECK OUTPUT ROUTINES                                                         
CHQORTN  NMOD1 0,**NECHQ*,R8,R7                                                 
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         ZIC   RF,GLARGS                                                        
         SLL   RF,2                                                             
         B     CHQORTNS(RF)                                                     
*                                                                               
CHQORTNS DS    0H                                                               
         B     OCHQPRD             (0) CHQ PRODUCT                              
         B     OCHQPRD2            (1) CHQ PRODUCT PARTNER                      
         B     OCHQSPRD            (2) SPLIT CHECK PRODUCT                      
         B     OCHQCLT             (3) CHQ CLIENT                               
         B     OCHQMKST            (4) CHQ MKT/STATION (CL5)                    
         B     OCHQDAT             (5) CHQ DATE                                 
         B     OCHQNUM             (6) CHQ NUMBER                               
         B     OCHQCLDT            (7) CHQ CLEARANCE DATE                       
         B     OCHQGRS             (8) CHQ GRS                                  
         B     OCHQNET             (9) CHQ NET                                  
         B     OCHQSGRS           (10) SPLIT CHQ GROSS                          
         B     OCHQSNET           (11) SPLIT CHQ NET                            
*                                                                               
CHQOX    XIT1                                                                   
         EJECT                                                                  
OCHQPRD  DS    0H                  PRODUCT                                      
         MVC   0(3,R3),0(R2)                                                    
         B     CHQOX                                                            
*                                                                               
OCHQPRD2 DS    0H                  PRODUCT PARTNER                              
         MVC   0(3,R3),0(R2)                                                    
         B     CHQOX                                                            
*                                                                               
OCHQSPRD DS   0H                  SPLIT PRODUCT                                 
         MVC   0(3,R3),0(R2)                                                    
         B     CHQOX                                                            
*                                                                               
OCHQCLT  DS    0H                  CLIENT                                       
         GOTO1 NBCLUNPK,DMCB,0(R2),0(R3)                                        
         B     CHQOX                                                            
*                                                                               
OCHQMKST DS    0H                   MARKET/STATION                              
         GOTO1 =V(MSUNPK),DMCB,0(R2),DUB                                        
         MVC   0(4,R3),DUB+4        SET STATION                                 
         B     CHQOX                                                            
*                                                                               
OCHQDAT  GOTO1 DATCON,DMCB,(2,0(R2)),(0,0(R3))   CHECK DATE                     
         B     CHQOX                                                            
*                                                                               
OCHQNUM  MVC   0(7,R3),0(R2)       CHECK NUMBER                                 
         B     CHQOX                                                            
*                                                                               
OCHQCLDT DS    0H                  CLEARANCE DATE                               
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,0(R3))                                  
         B     CHQOX                                                            
*                                                                               
OCHQGRS  EDIT  (B4,0(R2)),(12,0(R3)),2,MINUS=YES,ZERO=BLANK                     
         B     CHQOX                                                            
*                                                                               
OCHQNET  DS    0H                  NET                                          
         EDIT  (B4,0(R2)),(12,0(R3)),2,MINUS=YES,ZERO=BLANK                     
         B     CHQOX                                                            
*                                                                               
OCHQSGRS DS    0H                  SPLIT GROSS                                  
         EDIT  (B4,0(R2)),(12,0(R3)),2,MINUS=YES                                
         B     CHQOX                                                            
*                                                                               
OCHQSNET DS    0H                  SPLIT NET                                    
         EDIT  (B4,0(R2)),(12,0(R3)),2,MINUS=YES                                
         B     CHQOX                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              THIS IS THE START OF DRIVE3                                      
         SPACE 3                                                                
         DROP  R4                                                               
         ENTRY DRIVE3                                                           
         DS    0D                                                               
DRIVE3   LR    RB,RE                                                            
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         USING DRIVE3,RB,R8,R7                                                  
         L     R6,=A(OUTAREA)                                                   
         USING OUTAREA,R6                                                       
         BR    RF                                                               
*                                                                               
XIT3     XIT1                                                                   
*                                                                               
         SPACE 1                                                                
NINSI    MVC   0(2,R3),NBNSI       NSI NUMBER                                   
         B     XIT3                                                             
         SPACE 1                                                                
NIUNCODE MVC   0(2,R3),NBUNCODE    UNIVERSE CODE                                
         B     XIT3                                                             
         SPACE 1                                                                
NIMARKET MVC   0(2,R3),NBMARKET    MARKET NUM                                   
         B     XIT3                                                             
         SPACE 1                                                                
NISREP   MVC   0(2,R3),NBSREP      SPECIAL REP                                  
         B     XIT3                                                             
         SPACE 1                                                                
NIHUTPCT MVC   0(2,R3),NBHUTPCT    HUT %                                        
         B     XIT3                                                             
         SPACE 1                                                                
NIFEED   MVC   0(2,R3),NBFEED      FEED %                                       
         B     XIT3                                                             
         SPACE 1                                                                
NIUNIV   MVC   0(2,R3),NBUNIV      UNIV %                                       
         B     XIT3                                                             
         SPACE 1                                                                
NIIMPACT MVC   0(2,R3),NBIMPACT    IMPACT %                                     
         B     XIT3                                                             
         SPACE 1                                                                
NIUNITST MVC   0(1,R3),NBUNITST    UNIT STATUS                                  
         B     XIT3                                                             
         SPACE 1                                                                
NIPACKST MVC   0(1,R3),NBPACKST    PACKAGE STATUS                               
         B     XIT3                                                             
         SPACE 1                                                                
NIACTWHY MVC   0(1,R3),NBACTWHY    REASON FOR LAST ACTIVITY                     
         B     XIT3                                                             
         SPACE 1                                                                
NISL     MVC   0(1,R3),NBLEN       'SPOT' LENGTH                                
         B     XIT3                                                             
*                                                                               
NIUCNT   DS    0H                                                               
         TM    NBUNITST,X'42'      MISSED/PREEMPTED                             
         BNZ   XIT3                                                             
         MVI   3(R3),1                                                          
         B     XIT3                                                             
*                                                                               
NOUCNT   DS    0H                                                               
         EDIT  (B4,0(R2)),(12,0(R3))                                            
         B     XIT3                                                             
         EJECT                                                                  
         SPACE 1                                                                
NICPCT   MVC   0(8,R3),NDSPACES                                                 
         ZIC   R1,NBSPLPRN         LOOK UP COST POOL                            
         LTR   R1,R1                                                            
         BZ    XIT3                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,NDACPOOL                                                      
         L     R1,0(R1)                                                         
         LTR   R1,R1                                                            
         BZ    XIT3                                                             
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,0(R3)),3                                                 
         MVI   7(R2),C'%'                                                       
         B     XIT3                                                             
         EJECT                                                                  
NICLIINT DS    0H                  CLIENT INTERFACE CODE                        
         CLC   CLIINTSV(3),NBACTAM  DO WE ALREADY HAVE IT                       
         BE    NICI10                                                           
         NETGO NVSETSPT                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM                                                 
         BAS   R5,GTSPTHI                                                       
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   R5,GTSPTREC                                                      
         L     R5,AIO                                                           
         USING CLTHDR,R5                                                        
         MVC   CLIINTSV(3),KEY+1             SAVE A/M,CLT                       
         MVC   CLIINTSV+3(8),CCLTIFC         SAVE CLIENT INTERFACE CODE         
NICI10   MVC   0(8,R3),CLIINTSV+3                                               
         NETGO NVSETUNT                                                         
         MVI   NBFUNCT,NBFRDHI                                                  
         B     XIT3                                                             
         DROP  R5                                                               
         SPACE 2                                                                
NOCLIINT DS    0H                                                               
         MVC   0(8,R3),0(R2)                                                    
         OC    0(8,R3),NDSPACES                                                 
         GOTO1 CENTER,DMCB,0(R3),8                                              
         B     XIT3                                                             
         EJECT                                                                  
*        COMMERCIAL INPUT ROUTINE FOR DISPLAYING BOTH PIGGY BACKS               
         SPACE 3                                                                
*              ARGUMENT 1          1=COMMERCIAL NUMBER                          
*                                  2=COMMERCIAL NAME                            
         SPACE 1                                                                
*NICOMMB  BAS   RE,INGOAL3     ***************                                  
         MVC   0(8,R3),NDSPACES                                                 
         L     R4,NBAIO            COMMERCIAL SCHEDULED                         
         CLI   0(R4),X'04'                                                      
         BNE   NICBX                                                            
         MVI   ELCODE,X'21'                                                     
****     BAS   RE,GETEL       ********************8                             
         BNE   XIT3                                                             
         XC    WORK(48),WORK                                                    
         USING NUCMLEL,R4                                                       
         MVC   BYTE,NUCMLFLG    SAVE FLAG CODE                                  
         MVC   WORK(8),NUCML1                                                   
         MVC   WORK+8(8),NUCML2                                                 
         SPACE 1                                                                
* - COPY SPLITS                                                                 
         TM    NBUNST3,X'40'                                                    
         BNO   NICB10                                                           
         L     R4,NBAIO               GET COMMERCIAL FROM FEED ELEMENT          
         MVI   ELCODE,X'23'                                                     
*******  BAS   RE,GETEL ********************                                    
         BNE   NICBX                                                            
         USING NUFDCEL,R4                                                       
         MVC   WORK(8),NUFDCML1                                                 
         MVC   WORK+8(8),NUFDCML2                                               
*                                                                               
NICB10   TM    BYTE,X'E0'          CHK PROD,LEN.DATE CHANGE                     
         BZ    NICB12                                                           
         MVC   0(8,R3),=C'REASSIGN'                                             
         B     NICBX                                                            
*                                                                               
NICB12   MVC   0(8,R3),WORK                                                     
         MVC   11(8,R3),WORK+8                                                  
         CLI   GLARGS,1            1=COMMERCIAL NUMBER                          
         BE    NICBX                                                            
         BAS   RE,GETCNAMS         GET COMMERCIAL NAMES                         
* NEEEEEEEEEEEDDDDDDDDDDDD TOOOOOOO WORKKKKKKK ON BELOWWWW                      
****     CLI   GLARGS+1,C'B'      (ARG 2 - NAME AS WELL)                        
****     BE    *+                                                               
*****    MVC   0(48,R3),COMMNAME                                                
         B     NICBX                                                            
NICBX    B     XIT3                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* - NICSH    - CASH APPLIED DATA - INPUT                              *         
*                                                                     *         
*NTRY    GLARGS   0   - C'D' DATE                                     *         
*                  1  -  C'C' CLIENT CHECK DATE                       *         
*                     -  C'D' DEPOSIT DATE                            *         
*                     - C'#' CHECK NUMBER                             *         
*                     - C'$' AMOUNTS                                  *         
*                     -  C'G' GROSS                                   *         
*                     -  C'N' NET                                     *         
*                                                                     *         
*                                                                     *         
*EXIT    R3==>  DATES - XL3 YMD                                       *         
*               NUMBER- CL6                                           *         
*               AMOUNT- PL8                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NICSH    DS    0H                                                               
*                                                                               
         ICM   R4,15,NDCIDTBL      POINT TO BHBLOCK                             
         BZ    NICSHX                 SKIP IF NO POINTER GIVEN                  
         USING BHBLOCK,R4          ESTABLISH BHBLOCK                            
*                                                                               
*        DETERMINE FIELD TYPE BASED ON GLARGS+0                                 
*                                                                               
         CLI   GLARGS+0,C'D'       DATE                                         
         BE    NICSHDT                                                          
*                                                                               
         CLI   GLARGS+0,C'#'       CHECK NUMBER                                 
         BE    NICSH#                                                           
*                                                                               
         CLI   GLARGS+0,C'$'       CHECK AMOUNT                                 
         BE    NICSH$                                                           
*                                                                               
         CLI   GLARGS+0,C'%'       PERDCENTAGE APPLIED                          
         BE    NICSHPCT                                                         
*                                                                               
         B     NICSHX              UNKNOWN                                      
*                                                                               
NICSHDT  DS    0H                                                               
*                                                                               
         LA    RF,CSHCHKDT         ASSUME CHECK DATE WANTED                     
*                                                                               
         CLI   GLARGS+1,C'D'       IF DEPOSIT DATE WANTED                       
         BNE   *+8                                                              
         LA    RF,CSHDEPDT            SWITCH POINTER                            
*                                                                               
         MVC   0(3,R3),0(RF)       RETURN DATE                                  
*                                                                               
         CLI   GLARGS+1,C'C'       IF CHECK DATE                                
         BNE   *+10                                                             
         MVC   3(1,R3),CSHUNAPP       RETURN UNAPPLIED INDICATOR                
*                                                                               
         B     NICSHX                                                           
*                                                                               
NICSH#   DS    0H                  CHECK NUMBER                                 
*                                                                               
         MVC   0(6,R3),CSHCHK      RETURN CHECK NUMBER                          
*                                                                               
         B     NICSHX                                                           
*                                                                               
NICSH$   DS    0H                                                               
*                                                                               
         LA    RF,CSHGRS           ASSUME GROSS                                 
*                                                                               
         CLI   GLARGS+1,C'N'       CHECK FOR NET                                
         BNE   *+8                                                              
         LA    RF,CSHNET                                                        
*                                                                               
         MVC   0(8,R3),0(RF)       RETURN AMOUNT                                
*                                                                               
         OC    0(8,R3),0(R3)       IF NO DATA                                   
         BNZ   *+10                                                             
         ZAP   0(8,R3),=P'0'          FORCE ZERO                                
*                                                                               
         B     NICSHX                                                           
*                                                                               
NICSHPCT DS    0H                                                               
*                                                                               
         MVC   0(L'CSHPCT,R3),CSHPCT  RETURN PERCENTAGE                         
*                                                                               
         OC    0(8,R3),0(R3)       IF NO DATA                                   
         BNZ   *+10                                                             
         ZAP   0(8,R3),=P'0'          FORCE ZERO                                
*                                                                               
         B     NICSHX                                                           
*                                                                               
NICSHX   DS    0H                                                               
         B     XIT3                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* - NOCSH    - CASH APPLIED DATA - OUTPUT                             *         
*                                                                     *         
*NTRY    GLARGS   0   - C'D' DATE                                     *         
*                  1  -  C'C' CLIENT CHECK DATE                       *         
*                     -  C'D' DEPOSIT DATE                            *         
*                                                                     *         
*        R2==>  DATES - XL3 YMD                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NOCSH    DS    0H                                                               
*                                                                               
*        DETERMINE FIELD TYPE BASED ON GLARGS+0                                 
*                                                                               
         CLI   GLARGS+0,C'D'       DATE                                         
         BE    NOCSHDT                                                          
*                                                                               
         B     NOCSHX              UNKNOWN                                      
*                                                                               
NOCSHDT  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,(R2)),(X'20',(R3))   PRINT DATE                   
*                                                                               
         CLI   GLARGS+1,C'C'       IF CHECK DATE                                
         BNE   *+10                                                             
         MVC   6(1,R3),3(R2)          DISPLAY UNAPPLIED INDICATOR               
*                                                                               
         B     NOCSHX                                                           
*                                                                               
NOCSHX   DS    0H                                                               
         B     XIT3                                                             
*                                                                               
         TITLE 'NEWRIDRIVE - DATE FIELD - OUTPUT - NODATOUT'                    
***********************************************************************         
*                                                                     *         
*        ROUTINE TO PRINT A DATE                                      *         
*                                                                     *         
*NTRY    GLARGS   = INPUT  DATE FORMAT FOR DATCON                     *         
*        GLARGS+1 = OUTPUT DATE FORMAT FOR DATCON                     *         
*        GLARGS+2 = DISPLACEMENT OF DATE IN INPUT                     *         
*        GLARGS+3 = DISPLACEMENT OF POSSIBLE TRAILING CH IN INPUT     *         
*        NDLOCAL    NDYYMMDD   DATE IN YYMMDD FORMAT                  *         
*                   NDCCYY     DATE HAS CENTURY IN YEAR               *         
*                                                                     *         
*        R2==>  DRIVER INPUT                                          *         
*        R3==>  DRIVER PRINT AREA                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         SPACE 2                                                                
NODATOUT DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GLARGS+2         DISPLACEMENT OF DATE IN INPUT                
         LA    R2,0(RF,R2)         POINT TO DATE                                
*                                                                               
         SR    R0,R0                                                            
         IC    R0,GLARGS           INPUT DATE FORMAT                            
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,GLARGS+1       OUTPUT DATE FORMAT                           
         BZ    NODTOUT1            NUMERIC OUTPUT YYMMDD                        
*                                                                               
         CLI   GLARGS,3            IF BINARY INPUT                              
         BNE   *+8                                                              
         CLI   2(R2),0             AND NO DAYS                                  
         BNE   *+8                                                              
         LA    R5,1(R5)                CHANGE TO NEXT OUTPUT TYPE               
*                                                                               
NODTOUT1 DS    0H                                                               
*                                                                               
         TM    NDLOCAL,NDYYMMDD+NDCCYY    IF NUMERIC OUTPUT WANTED              
         BZ    *+8                                                              
         LA    R5,0                   RESET OUTPUT DATE FORMAT                  
*                                                                               
         LA    R4,0(R3)            DEFAULT PRINT POSITION                       
*                                                                               
         TM    NDLOCAL,NDCCYY    IF NUMERIC OUTPUT WANTED WITH CENTURY          
         BZ    *+8                                                              
         LA    R4,2(R4)            BUMP OUTPUT POINTER                          
*                                                                               
         GOTO1 DATCON,DMCB,((R0),(R2)),((R5),(R4))  PRINT DATE                  
*                                                                               
         TM    NDLOCAL,NDYYMMDD+NDCCYY    IF NUMERIC OUTPUT WANTED              
         BZ    NODTOUT6                                                         
*                                                                               
         TM    NDLOCAL,NDCCYY    IF NUMERIC OUTPUT WANTED WITH CENTURY          
         BNO   NODTOUT5                                                         
*                                                                               
         MVC   0(2,R3),=C'19'       TWENTIETH IS DEFAULT CENTURY                
*                                                                               
         CLC   2(2,R3),=C'50'       IF BEFORE MID-CENTURY                       
         BNL   *+10                                                             
         MVC   0(2,R3),=C'20'          USE TWENTY-FIRST CENTURY                 
*                                                                               
NODTOUT5 DS    0H                                                               
*                                                                               
         CLI   GLARGS,3            IF BINARY INPUT                              
         BNE   *+8                                                              
         CLI   2(R2),0             AND NO DAYS                                  
         BNE   *+10                                                             
         MVC   4(2,R4),=C'  '          KILL DAYS IN DATE                        
*                                                                               
NODTOUT6 DS    0H                                                               
*                                                                               
         CLI   GLARGS+3,0          IF THERE IS A TRAILING CHARACTER             
         BE    NODTOUT7                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GLARGS+3            GET DISPLACEMENT IN INPUT                 
*                                                                               
         LA    RE,0(RF,R2)            POINT TO TRAILING CHARACTER               
*                                                                               
         LA    RF,8                   MAX OUTPUT LENGTH                         
         LA    R1,7(R4)               LAST OF OUTPUT                            
*                                                                               
         CLI   0(R1),C' '             FIND END OF DATE                          
         BH    *+10                                                             
         BCTR  R1,0                   BACK UP A BYTE                            
         BCT   RF,*-10                                                          
*                                                                               
         MVC   1(1,R1),0(RE)          ADD ON TRAILING CHARACTER                 
*                                                                               
NODTOUT7 DS    0H                                                               
*                                                                               
NODTOUTX DS    0H                                                               
*                                                                               
         B     XIT3                                                             
*                                                                               
         TITLE 'NEWRIDRIVE - DATE FIELD - OUTPUT - NICSHSQN'                    
***********************************************************************         
*                                                                     *         
* - NICSHSQN - LINE SEQUENCE NUMBER FOR CASHFLOW                      *         
*                                                                     *         
*EXIT    R3==>  XL2   - SEQUENCE NUMBER                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NICSHSQN DS    0H                                                               
*                                                                               
         L     R5,NBAIO            SKIP UNLESS WE HAVE UNIT RECORD              
         CLI   0(R5),X'04'         IS IT UNIT                                   
         BNE   NICSHSQX                                                         
*                                                                               
         CLC   NICNUKEY,0(R5)      IF KEY HAS CHANGED                           
         BE    *+16                                                             
         XC    NICSAVE(NICSAVEL),NICSAVE   CLEAR SAVEAREA                       
         MVC   NICNUKEY,0(R5)         SAVE CURRENT KEY                          
*                                                                               
         ICM   R4,15,NDCIDTBL      SKIP IF NO BHBLOCK GIVEN                     
         BZ    NICSHSQX                                                         
         USING BHBLOCK,R4          ESTABLISH BHBLOCK                            
*                                                                               
         OC    CHQDATE,CHQDATE     IF DOING A PAY ELEMENT                       
         BZ    NICPAYN                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,NICPYSQN          GET    CURRENT PAY SQN                    
*                                                                               
         STCM  RF,3,0(R3)             RETURN CURRENT PAY SQN                    
*                                                                               
         LA    RF,1(RF)               BUMP   CURRENT PAY SQN                    
         STCM  RF,3,NICPYSQN                                                    
*                                                                               
         B     NICSHSQX                                                         
*                                                                               
NICPAYN  DS    0H                                                               
*                                                                               
         OC    BHBDATE,BHBDATE     IF DOING A BILL ELEMENT                      
         BZ    NICBLLN                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,NICBLSQN          GET    CURRENT BILL SQN                   
*                                                                               
         STCM  RF,3,0(R3)             RETURN CURRENT BILL SQN                   
*                                                                               
         LA    RF,1(RF)               BUMP   CURRENT BILL SQN                   
         STCM  RF,3,NICBLSQN                                                    
*                                                                               
         B     NICSHSQX                                                         
*                                                                               
NICBLLN  DS    0H                                                               
*                                                                               
NICSHSQX DS    0H                                                               
         B     XIT3                                                             
*                                                                               
NICSAVE  DS    0X                  SAVE AREA FOR SEQUENCE NUMBER                
NICNUKEY DS    XL(L'NUKEY)         UNIT RECORD KEY                              
NICPYSQN DS    XL2                 PAY  ELEMENT SEQUENCE NUMBER                 
NICBLSQN DS    0XL2                BILL ELEMENT SEQUEMCE NUMBER                 
NICBLSQ1 DS    XL1                 MAJOR PART OF SEQUENCE NUMBER                
NICBLSQ2 DS    XL1                 MINOR PART OF SEQUENCE NUMBER                
NICSAVEL EQU   *-NICSAVE           SAVEAREA LENGTH                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  - AGING DAYS TO DISBURSEMENT - INPUT                               *         
*                                                                     *         
*EXIT    R3==>  6PL8                                                  *         
*              CLEARANCE DAYS                                         *         
*              CLEARANCE DOLLARS                                      *         
*              CLEARANCE DAILY BALANCE                                *         
*              BILLING DAYS                                           *         
*              BILLING DOLLARS                                        *         
*              BILLING DAILY BALANCE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NIAG     DS    0H                                                               
*                                                                               
         LA    R0,6                SIX PACKED FIELDS                            
         LR    RF,R3               START OF OUTPUT                              
*                                                                               
         ZAP   0(8,RF),=P'0'       INIT OUTPUT FIELDS                           
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ICM   R4,15,NDCIDTBL      POINT TO BHBLOCK                             
         BZ    NIAGX               SKIP IF NO POINTER GIVEN                     
         USING BHBLOCK,R4          ESTABLISH BHBLOCK                            
*                                                                               
*        CLEARANCE DATA                                                         
*                                                                               
NIAGCLR  DS    0H                                                               
*                                                                               
         ICM   R1,15,CHQDAYD       RETURN NUMBER OF CLEARANCE DAYS              
         CVD   R1,DUB                                                           
         ZAP   0(8,R3),DUB                                                      
******   SRP   0(8,R3),1,0         ADD DECIMAL PLACE                            
*                                                                               
         CLI   GLARGS+1,C'G'       IF GROSS DOLLARS                             
         BNE   NIAGCGN                                                          
*                                                                               
         ICM   R1,15,CHQGRS                                                     
         CVD   R1,DUB                                                           
         ZAP   8(8,R3),DUB            RETURN GROSS CLEARED                      
*                                                                               
         OC    CHQDALI,CHQDALI        SKIP IF NO DATA                           
         BZ    *+10                                                             
         ZAP   16(8,R3),CHQDALI       RETURN GROSS DAILY BALANCE                
*                                                                               
         B     NIAGCLRX                                                         
*                                                                               
NIAGCGN  DS    0H                  ELSE                                         
*                                                                               
         ICM   R1,15,CHQNET                                                     
         CVD   R1,DUB                                                           
         ZAP   8(8,R3),DUB            RETURN NET CLEARED                        
*                                                                               
         OC    CHQDALIN,CHQDALIN      SKIP IF NO DATA                           
         BZ    *+10                                                             
         ZAP   16(8,R3),CHQDALIN      RETURN  NET DAILY BALANCE                 
*                                                                               
NIAGCLRX DS    0H                                                               
*                                                                               
*        BILLING DATA                                                           
*                                                                               
NIAGBLL  DS    0H                                                               
*                                                                               
         ICM   R1,15,CSHDAYD       RETURN NUMBER OF BILLING DAYS                
         CVD   R1,DUB                                                           
         ZAP   24(8,R3),DUB                                                     
*******  SRP   24(8,R3),1,0        ADD DECIMAL PLACE                            
*                                                                               
         CLI   GLARGS+1,C'G'       IF GROSS DOLLARS                             
         BNE   NIAGBGN                                                          
*                                                                               
         OC    CSHGRS,CSHGRS          SKIP IF NO DATA AVAILABLE                 
         BZ    *+10                                                             
         ZAP   32(8,R3),CSHGRS        RETURN GROSS BILLED                       
*                                                                               
         OC    CSHDALI,CSHDALI        SKIP IF NO DATA                           
         BZ    *+10                                                             
         ZAP   40(8,R3),CSHDALI       RETURN GROSS DAILY BALANCE                
*                                                                               
         B     NIAGBLLX                                                         
*                                                                               
NIAGBGN  DS    0H                  ELSE                                         
*                                                                               
         OC    CSHNET,CSHNET          SKIP IF NO DATA AVAILABLE                 
         BZ    *+10                                                             
         ZAP   32(8,R3),CSHNET        RETURN NET   BILLED                       
*                                                                               
         OC    CSHDALIN,CSHDALIN      SKIP IF NO DATA                           
         BZ    *+10                                                             
         ZAP   40(8,R3),CSHDALIN      RETURN  NET DAILY BALANCE                 
*                                                                               
NIAGBLLX DS    0H                                                               
*                                                                               
NIAGX    DS    0H                                                               
*                                                                               
         B     XIT3                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  - AGING DAYS TO DISBURSEMENT - OUTPUT                              *         
*                                                                     *         
*NTRY    R2==>  6PL8                                                  *         
*              CLEARANCE DAYS                                         *         
*              CLEARANCE DOLLARS                                      *         
*              CLEARANCE DAILY BALANCE                                *         
*              BILLING DAYS                                           *         
*              BILLING DOLLARS                                        *         
*              BILLING DAILY BALANCE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NOAG     DS    0H                                                               
*                                                                               
         OC    0(8,R2),0(R2)       SKIP IF NO DATA TO PRINT                     
         BZ    NOAGX                                                            
*                                                                               
         CLI   GLARGS,C'$'         IF DAILY BALANCE                             
         BE    NOAGDB                                                           
*                                                                               
         CLI   GLARGS,C'D'         IF AVERAGE DAYS TO DISBURSEMENT              
         BE    NOAGAD                                                           
*                                                                               
         B     NOAGX               UNRECOGNIZED OPTION                          
*                                                                               
*        DAILY BALANCE                                                          
*                                                                               
NOAGDB   DS    0H                                                               
*                                                                               
*        ACCUMULATE DATA FOR TOTALS                                             
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF DOING TOTALS                         
         BO    NOAGDBTN                                                         
*                                                                               
         LA    RF,8(R2)            POINT TO CLEARED DOLLARS                     
*                                                                               
         LA    RE,TBKCLRDB         POINT TO CLEARED DEBIT BUCKETS               
         CP    0(8,RF),=P'0'       IF CLEARED AMOUNT IS NEGATIVE                
         BNM   *+8                                                              
         LA    RE,TBKCLRCR            POINT TO CLEARED CREDIT BUCKETS           
*                                                                               
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RE),0(8,RF)  INCREMENT ALL LVL BUCKETS                
         LA    RE,L'TOTBKS(RE)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         LA    RF,16(R2)           POINT TO CLEARED DISBURSEMENT                
*                                                                               
         LA    RE,TBKCLDDB         POINT TO CLEARED DEBIT BUCKETS               
         CP    0(8,RF),=P'0'       IF CLEARED AMOUNT IS NEGATICE                
         BNM   *+8                                                              
         LA    RE,TBKCLDCR            POINT TO CLEARED CREDIT BUCKETS           
*                                                                               
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RE),0(8,RF)  INCREMENT ALL LVL BUCKETS                
         LA    RE,L'TOTBKS(RE)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         LA    RF,32(R2)           POINT TO BILLED  DOLLARS                     
*                                                                               
         LA    RE,TBKBLLDB         POINT TO BILLED  DEBIT BUCKETS               
         CP    0(8,RF),=P'0'       IF BILLED  AMOUNT IS NEGATIVE                
         BNM   *+8                                                              
         LA    RE,TBKBLLCR            POINT TO CLEARED CREDIT BUCKETS           
*                                                                               
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RE),0(8,RF)  INCREMENT ALL LVL BUCKETS                
         LA    RE,L'TOTBKS(RE)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         LA    RF,40(R2)           POINT TO BILLED  DISBURSEMENT                
*                                                                               
         LA    RE,TBKBLDDB         POINT TO BILLED  DEBIT BUCKETS               
         CP    0(8,RF),=P'0'       IF CLEARED AMOUNT IS NEGATIVE                
         BNM   *+8                                                              
         LA    RE,TBKBLDCR            POINT TO BILLED  CREDIT BUCKETS           
*                                                                               
         LA    R0,TOTNMLVQ         NUMBER OF TOTALS LEVELS                      
*                                                                               
         AP    0(L'TOTBKS,RE),0(8,RF)  INCREMENT ALL LVL BUCKETS                
         LA    RE,L'TOTBKS(RE)                                                  
         BCT   R0,*-10                                                          
*                                                                               
NOAGDBTN DS    0H                                                               
*                                                                               
*        PRINT DISBURSEMENT AMOUNT                                              
*                                                                               
         ZAP   WORK+25(8),16(8,R2)    ADD IN CLEARED DAILY BALANCE              
         AP    WORK+25(8),40(8,R2)    ADD IN BILLED  DAILY BALANCE              
*                                                                               
         ZAP   0(8,R2),WORK+25(8)  RETURN RESULT TO DRIVER                      
         MVI   GLHOOK,GLEDIT       HAVE DRIVER PRINT IT                         
*                                                                               
NOAGDBX  DS    0H                                                               
         B     NOAGX                                                            
*                                                                               
*        PRINT AVERAGE DAYS TO DISBURSEMENT                                     
*                                                                               
NOAGAD   DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     SKIP IF NOT TOTAL                            
         BNO   NOAGADTN                                                         
*                                                                               
         CLI   GLLEVEL,0           SKIP IF GRAND TOTAL                          
         BE    NOAGX                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GLLEVEL          GET CURRENT TOTAL LEVEL                      
         BCTR  RF,0                DECREMENT FOR INDEXING                       
*                                                                               
         LA    RE,L'TOTBKS         BUCKET LENGTH                                
         MR    RE,RE               RF = DISP INTO BUCKETS OF THIS LEVEL         
*                                                                               
         LA    RE,TBKBLDDB         ==> TO BILLING DEBIT TOTAL BUCKETS           
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   TBDOLDB,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                    
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKBLDCR         ==> TO BILLING CREDIT TOTAL BUCKETS          
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   TBDOLCR,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                    
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKCLDDB         ==> TO CLEARED TOTAL BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   TCDOLDB,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                    
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKCLDCR         ==> TO CLEARED TOTAL BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   TCDOLCR,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                    
         ZAP   0(L'TOTBKS,R1),=P'0'    CLEAR THIS LEVEL TOTAL                   
*                                                                               
         LA    RE,TBKCLRDB         ==> TO CLEARED TOTAL BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   CLRTOTDB,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                   
         ZAP   0(L'TOTBKS,R1),=P'0'     CLEAR THIS LEVEL TOTAL                  
*                                                                               
         LA    RE,TBKCLRCR         ==> TO CLEARED TOTAL BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   CLRTOTCR,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                   
         ZAP   0(L'TOTBKS,R1),=P'0'     CLEAR THIS LEVEL TOTAL                  
*                                                                               
         LA    RE,TBKBLLDB         ==> TO CLEARED TOTAL BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   BLLTOTDB,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                   
         ZAP   0(L'TOTBKS,R1),=P'0'     CLEAR THIS LEVEL TOTAL                  
*                                                                               
         LA    RE,TBKBLLCR         ==> TO CLEARED TOTAL BUCKETS                 
*                                                                               
         LA    R1,0(RF,RE)         POINT TO TOTAL FOR LEVEL                     
*                                                                               
         ZAP   BLLTOTCR,0(L'TOTBKS,R1)  SAVE THIS LEVEL TOTAL                   
         ZAP   0(L'TOTBKS,R1),=P'0'     CLEAR THIS LEVEL TOTAL                  
*                                                                               
         ZAP   WORK+25(8),=P'0'    INIT ACCUMULATOR                             
*                                                                               
         ZAP   MYREMAIN,TCDOLDB    GET CHECKING-DOLLAR DEBIT DAYS               
         AP    MYREMAIN,TBDOLCR    ADD IN BILL DOLLAR CREDIT DAYS               
*                                                                               
         ZAP   MYQUOTE,=P'0'       INIT QUOTIENT                                
*                                                                               
         ZAP   MYDUB,CLRTOTDB      SUM CHK DEBIT                                
         SP    MYDUB,BLLTOTCR      AND BLL CREDIT                               
*                                                                               
         CP    MYDUB,=P'0'                                                      
         BE    *+22                                                             
         XC    MYQUOTE,MYQUOTE                                                  
         SRP   MYQUOTE(16),2,0     *100                                         
         DP    MYQUOTE(16),MYDUB                                                
*                                                                               
         ZAP   DUB,MYQUOTE         SAVE ADD                                     
*                                                                               
         ZAP   MYREMAIN,TCDOLCR    GET CHECKING-DOLLAR CREDIT DAYS              
         AP    MYREMAIN,TBDOLDB    ADD IN BILL DOLLAR  DEBIT  DAYS              
*                                                                               
         ZAP   MYQUOTE,=P'0'       INIT QUOTIENT                                
*                                                                               
         ZAP   MYDUB,BLLTOTDB      SUM BLL DEBIT                                
         SP    MYDUB,CLRTOTCR      AND CHK CREDIT                               
*                                                                               
         CP    MYDUB,=P'0'                                                      
         BE    *+22                                                             
         XC    MYQUOTE,MYQUOTE                                                  
         SRP   MYQUOTE(16),2,0     *100                                         
         DP    MYQUOTE(16),MYDUB                                                
*                                                                               
         AP    DUB,MYQUOTE         SAVE ADD                                     
*                                                                               
         ZAP   MYQUOTE,DUB         AVERAGE CHK DAYS PLUS AVG BILL DAYS          
*                                                                               
         SRP   MYQUOTE,64-1,5      ROUND TO ONE DECIMAL                         
*                                                                               
NOAGADTX DS    0H                                                               
*                                                                               
         MVI   GLHOOK,GLDONT       DON'T PRINT ANYTHING                         
         ZAP   NOAGADD,MYQUOTE     SAVE AVG. DAYS TO DISBURSE                   
*                                                                               
         B     NOAGX                                                            
*                                                                               
NOAGADTN DS    0H                                                               
*                                                                               
*        PRINT DAYS TO DISBURSE                                                 
*                                                                               
         ZAP   WORK+25(8),0(8,R2)   ACCUMMULATE CLEARANCE DAYS                  
         AP    WORK+25(8),24(8,R2)  AND BILLING DAYS                            
*                                                                               
         ZAP   0(8,R2),WORK+25(8)  RETURN RESULT TO DRIVER                      
         MVI   GLHOOK,GLEDIT       HAVE DRIVER PRINT IT                         
*                                                                               
         CP    8(8,R2),=P'0'       IF NOTHING CLEARED                           
         BNE   *+10                                                             
         CP    32(8,R2),=P'0'      AND IF NOTHING BILLED                        
         BNE   *+10                                                             
         ZAP   0(8,R2),=P'0'           DON'T PRINT ANYTHING                     
*                                                                               
         B     NOAGX                                                            
*                                                                               
NOAGX    DS    0H                                                               
*                                                                               
         B     XIT3                                                             
*                                                                               
NOAGTXTL DC    C'AVG. DAYS TO DISBURSEMENT = '                                  
NOAGTXTS DC    C'ADD = '                                                        
*                                                                               
NOAGADD  DC    XL8'0'              AVG DAYS TO DISBURSE                         
DOLTOT   DS    PL8                 DOLLAR DAYS TOTAL                            
CLRTOTDB DS    PL8                 CLEARED TOTAL DOLLARS - DEBIT                
CLRTOTCR DS    PL8                 CLEARED TOTAL DOLLARS - CREDIT               
BLLTOTDB DS    PL8                 BILLED  TOTAL DOLLARS - DEBIT                
BLLTOTCR DS    PL8                 BILLED  TOTAL DOLLARS - CREDIT               
TBDOLDB  DS    PL8                 BILLING  DOLLAR DAYS TOTAL - DEBIT           
TBDOLCR  DS    PL8                 BILLING  DOLLAR DAYS TOTAL - CREDIT          
TCDOLDB  DS    PL8                 CHECKING DOLLAR DAYS TOTAL - DEBIT           
TCDOLCR  DS    PL8                 CHECKING DOLLAR DAYS TOTAL - CREDIT          
DAYSTOT  DS    F                   DAYS TOTAL                                   
BDAYSTOT DS    F                   BILLING/CASH DAYS TOTAL                      
CDAYSTOT DS    F                   CHECKING     DAYS TOTAL                      
*                                                                               
MYQUOTE  DS    D                   DIVISION WORKAREAS                           
MYREMAIN DS    D                                                                
MYDUB    DS    D                                                                
*                                                                               
*        BUFFERS TO HANDLE TOTALING ON OUTPUT - MAX 20 LEVELS OF TOTALS         
*                                                                               
TOTBKS   DS    0PL8                TOTALS BUCKETS                               
TBKBLLDB DS    20PL8               BILL         TOTAL     BUCKETS-DEBIT         
TOTNMLVQ EQU   (*-TOTBKS)/L'TOTBKS NUMBER OF LEVEL BUCKETS                      
TBKBLLCR DS    20PL8               BILL         TOTAL     BUCKETS-CREDT         
TBKCLRDB DS    20PL8               CLEARANCE    TOTAL     BUCKETS-DEBIT         
TBKCLRCR DS    20PL8               CLEARANCE    TOTAL     BUCKETS-CREDT         
TBKCSHDB DS    20PL8               CASH         TOTAL     BUCKETS-DEBIT         
TBKCSHCR DS    20PL8               CASH         TOTAL     BUCKETS-CREDT         
TBKBLDDB DS    20PL8               DISBURSEMENT BILL      BUCKETS-DEBIT         
TBKBLDCR DS    20PL8               DISBURSEMENT BILL      BUCKETS-CREDT         
TBKCLDDB DS    20PL8               DISBURSEMENT CLEARANCE BUCKETS-DEBIT         
TBKCLDCR DS    20PL8               DISBURSEMENT CLEARANCE BUCKETS-CREDT         
TOTNMBKQ EQU   (*-TOTBKS)/L'TOTBKS TOTAL NUMBER OF BUCKETS                      
*                                                                               
         EJECT                                                                  
*              INPUT               WORK=2 X (8 BYTE COMML NUMBERS)              
         SPACE 1                                                                
GETCNAMS NTR1                                                                   
         MVC   LASTCOMK,WORK                                                    
         MVC   COMMNMSV,NDSPACES                                                
         OC    0(8,R2),WORK                                                     
         BZ    XIT3                                                             
         SPACE 1                                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY             COMMERCIAL NAME                              
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),NBACTAM   (AGENCY/MEDIA/CLIENT)                        
         MVC   CMLKCML,LASTCOMK                                                 
         CLC   CMLKCML,=C'REASSIGN'                                             
         BE    COMERR3                                                          
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'TRFDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   CMLKCML,LASTCOMK                                                 
         BNE   COMERR3                                                          
         MVC   FILENAME,=C'TRFFILE '                                            
         GOTO1 GETREC                                                           
         LA    R3,COMMNMSV                                                      
         L     R4,AIO                                                           
         MVI   ELCODE,X'30'                                                     
******   BAS   RE,GETEL *****************************                           
         BNE   RESUNT3                                                          
         USING CMLDSCEL,R4                                                      
         MVC   0(24,R3),CMLDSC                                                  
*****    BAS   RE,NEXTEL3 ***************************                           
         BNE   *+10                                                             
         MVC   24(24,R3),CMLDSC                                                 
         OC    0(48,R3),NDSPACES                                                
         B     RESUNT3                                                          
         SPACE 1                                                                
COMERR3  MVC   COMMNAME(8),=C'REASSIGN'                                         
         B     RESUNT3                                                          
*                                                                               
LASTCOMK DS    CL8                                                              
COMMNMSV DS    CL48                                                             
               EJECT                                                            
* HANDLES MULTIPLE AND REGULAR 2 PRODUCT UNITS                                  
NIMPROD  DS    0H                                                               
         TM    NBSPLOPT,X'80'                                                   
         BNO   NIMPRD2                                                          
         MVC   BYTE,NBSPLPRN       SPLITTING PRODUCTS                           
         BAS   RE,LUPPROD3                                                      
         MVC   0(3,R3),NBACTAM     AGY/MED/CLT                                  
         MVC   3(3,R3),WORK                                                     
         CLI   GLARGS,C'N'         NAME ONLY                                    
         BNE   NIMPRDX             NO                                           
         BAS   RE,GETPROD          YES                                          
         MVC   0(20,R3),NDPRDNAM                                                
         B     XIT3                                                             
         SPACE 1                                                                
NIMPRD2  DS    0H                  NOT SPLITTING PRODUCTS                       
         CLI   NBPRDNO,0           MULTIPLE PRODUCTS                            
         BNE   NIMPRD5             YES                                          
         MVC   0(3,R3),NBACTAM     NO                                           
         MVC   BYTE,NBPRD                                                       
         BAS   RE,LUPPROD3                                                      
         MVC   3(3,R3),WORK                                                     
         CLC   WORK(3),=C'999'                                                  
         BE    NIMPRD4                                                          
         MVC   BYTE,NBPRD2                                                      
         CLI   BYTE,0                                                           
         BE    NIMPRD4                                                          
         BAS   RE,LUPPROD3                                                      
         MVI   6(R3),C'/'                                                       
         MVC   7(3,R3),WORK                                                     
NIMPRD4  CLI   GLARGS,C'N'            NAME ONLY                                 
         BNE   XIT3                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(3),3(R3)          YES                                       
         BAS   RE,GETPROD                                                       
         MVC   WORK+10(20),NDPRDNAM                                             
         CLI   NBPRD2,0                                                         
         BE    NIMPRD4D                                                         
         MVC   WORK(3),7(R3)       SECOND PRODUCT                               
         BAS   RE,GETPROD                                                       
         MVI   WORK+31,C'/'                                                     
         MVC   WORK+33(20),NDPRDNAM                                             
NIMPRD4D GOTO1 SQUASHER,DMCB,WORK+10,40                                         
         MVC   0(40,R3),WORK+10                                                 
         B     XIT3                                                             
         SPACE 1                                                                
NIMPRD5  DS    0H                  MULTIPLE PRODUCTS                            
         LR    R5,R3               SAVE R3                                      
         ZIC   R1,NBPRDNO          NUMBER OF PRODUCTS                           
         LA    R4,NBPRDLST         PRODUCT LIST                                 
         MVC   0(3,R3),NBACTAM                                                  
         LA    R3,3(R3)                                                         
NIMPRD6  MVC   BYTE,0(R4)                                                       
         BAS   RE,LUPPROD3                                                      
         MVC   0(3,R3),WORK                                                     
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),0             IF MORE PRODS                                
         BE    NIMPRDX                                                          
         MVI   0(R3),C'/'          SET A SLASH MARKER                           
         LA    R3,1(R3)                                                         
         BCT   R1,NIMPRD6                                                       
NIMPRDX  CLI   GLARGS,C'N'         NAME ONLY                                    
         BNE   XIT3                                                             
         MVC   WORK(24),3(R5)      R5 = SAVED R3/MOVE UP NAMES                  
         XC    0(27,R5),0(R5)                                                   
         MVC   0(24,R5),WORK                                                    
         B     XIT3                                                             
                                                                                
         EJECT                                                                  
                                                                                
NOMPROD  DS    0H                  OUT ROUTINE FOR MULTIPLE PRODS               
*                                                                               
         CLI   GLARGS,C'N'              NAME ONLY                               
         BNE   NOMPRD0                                                          
         MVC   0(40,R3),0(R2)            YES                                    
         B     XIT3                                                             
*                                                                               
NOMPRD0  CLI   GLARGS,C'O'         PUT ALL PRODS ON ONE LINE                    
         BNE   NOMPROD1                                                         
NOMPRD00 MVC   0(24,R3),3(R2)      YES                                          
         CLC   =C'999',0(R3)                                                    
         BNE   *+10                                                             
         MVC   0(3,R3),=C'UNA'                                                  
         CLC   =C'999',4(R3)                                                    
         BNE   *+10                                                             
         MVC   4(3,R3),=C'UNA'                                                  
         B     XIT3                                                             
*                                                                               
NOMPROD1 CLI   MYLTYP,C'M'         IF MIDLINE                                   
         BE    NOMPRD00            PUT ALL ON ONE LINE                          
         LA    R1,6                MAX NUMBER OF PRODS                          
         LR    R4,R2               SAVE START OF INPUT                          
         LA    R2,3(R2)            BUMP OVER AGY/MED/CLT                        
NOMPRD2  MVC   0(7,R3),0(R2)                                                    
         CLC   =C'999',0(R3)                                                    
         BNE   *+10                                                             
         MVC   0(3,R3),=C'UNA'                                                  
         CLC   =C'999',4(R3)                                                    
         BNE   *+10                                                             
         MVC   4(3,R3),=C'UNA'                                                  
         CLI   MYLTYP,C'H'         IF HEADLINES                                 
         BE    NOMPRD5             ONLY PASS FIRST TWO PRODUCTS                 
         CLI   7(R2),C'/'          ELSE IF MORE PRODUCTS                        
         BNE   NOMPRDX                                                          
         S     R1,=F'2'            DEAL WITH THEM                               
         C     R1,=F'0'                                                         
         BNH   NOMPRDX                                                          
         LA    R3,198(R3)          BUMP TO NEXT PRINT LINE                      
         LA    R2,8(R2)            ANY MORE PRODUCTS                            
         CLI   0(R2),X'40'                                                      
         BH    NOMPRD2                                                          
         B     NOMPRDX                                                          
*                                                                               
NOMPRD5  MVC   LABLAREA(7),=C'PRODUCT'                                          
         MVC   CODEAREA(7),0(R3)                                                
         CLI   7(R2),C'/'          ARE THERE MORE THAN TWO PRODUCTS             
         BNE   NOMPRD6             YES                                          
         MVC   NAMEAREA(24),0(R2)     THEN PASS CODES IN NAMEAREA               
         XC    CODEAREA(7),CODEAREA                                             
         MVC   CODEAREA+2(3),=C'***'   THEN PASS *** IN CODE AREA               
         B     GENOUTZ                                                          
NOMPRD6  XC    WORK,WORK           NO/GET PROD NAMES                            
         LR    R2,R3               SAVE R3                                      
         LR    R3,R4               PASS SAVED INPUT START WITH AGY/MED          
         MVC   WORK(3),3(R3)                                                    
         BAS   RE,GETPROD                                                       
         MVC   WORK+10(20),NDPRDNAM                                             
         CLI   6(R3),C'/'          IS THERE A SECOND PRODUCT                    
         BNE   NOMPRD7                                                          
         MVC   WORK(3),7(R3)        YES                                         
         BAS   RE,GETPROD                                                       
         MVI   WORK+30,C'/'                                                     
         MVC   WORK+33(20),NDPRDNAM                                             
NOMPRD7  GOTO1 SQUASHER,DMCB,WORK+10,43                                         
         MVC   NAMEAREA,WORK+10                                                 
         LR    R3,R2               RESET R3                                     
         B     GENOUTZ                                                          
*                                                                               
NOMPRDX  B     XIT3                                                             
                                                                                
         EJECT                                                                  
                                                                                
*              ROUTINE TO LOOK UP PRODUCT CODE                                  
         SPACE 3                                                                
*              INPUT               BYTE HAS PRODUCT NUMBER                      
*              OUTPUT              RETURN 3 BYTE CODE IN WORK                   
         SPACE 1                                                                
LUPPROD3 NTR1                                                                   
         MVC   WORK(3),=C'999'     MAKE SURE UNALLOCATED COMES LAST             
         CLI   BYTE,0                                                           
         BE    XIT3                                                             
         CLI   BYTE,X'FF'                                                       
         BE    XIT3                                                             
         CLI   NDSPLOPT,0          IS SPLIT BILLING ON?                         
         BE    LUP31                                                            
         L     R1,NDASPLBL                                                      
         USING SPLTBLKD,R1                                                      
         MVC   WORK(3),SPLIPRD                                                  
         B     XIT3                                                             
         SPACE 1                                                                
LUP31    L     R4,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R4                                                        
         LA    R2,CLIST                                                         
         LA    R5,220                                                           
         SPACE 1                                                                
LUP32    CLC   BYTE,3(R2)                                                       
         BE    LUP34                                                            
         LA    R2,4(R2)                                                         
         BCT   R5,LUP32                                                         
         B     XIT3                                                             
         SPACE 1                                                                
LUP34    MVC   WORK(3),0(R2)       3-BYTE PRODUCT                               
         B     XIT3                                                             
         SPACE 1                                                                
         DROP  R4                                                               
*                                                                               
GENOUTZ  DS    0H                                                               
         GOTO1 =A(XGENOUT),DMCB,(RA),0                                          
         B     XIT3                                                             
         EJECT                                                                  
*                                                                               
*              ROUTINE TO FILL IN PRODUCT DETAILS                               
         SPACE 3                                                                
*              INPUT               R3 = A(AGY/MED/CLIENT)                       
*              INPUT               WORK = PROD CODE                             
         SPACE 1                                                                
GETPROD  NTR1                                                                   
         MVC   WORK+3(3),WORK      PRODUCT                                      
         MVC   WORK(3),0(R3)       AGY/MED/CLT                                  
         LA    R2,WORK                                                          
         CLC   3(3,R2),=C'UNA'     SPECIAL FOR UNALLOCATED                      
         BE    GETPRD01                                                         
         CLC   3(3,R2),=C'999'     SPECIAL FOR UNALLOCATED                      
         BNE   GETPRD02                                                         
         SPACE 1                                                                
GETPRD01 MVC   NDPRDKEY,=C'UNA'                                                 
         MVC   NDPRDNAM,=CL20'UNALLOCATED'                                      
         MVI   NDPRDCOD,255                                                     
         MVC   NDPRDLST,0(R2)                                                   
         B     XIT3                                                             
         SPACE 1                                                                
GETPRD02 CLC   NDPRDLST,0(R2)      HAVE WE DONE THIS ONE BEFORE?                
         BE    XIT3                                                             
         MVC   NDPRDLST,0(R2)                                                   
         USING PRDHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   PKEYAM(6),NDPRDLST  (AM/CLI/PRODUCT)                             
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   NDPRDNAM,NDSPACES                                                
         MVI   NDPRDCOD,255                                                     
         CLC   KEYSAVE(13),KEY                                                  
         BNE   RESUNT3                                                          
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   NDPRDNAM,PNAME      GET OTHER STUFF                              
         OC    NDPRDNAM,NDSPACES                                                
         MVC   NDPRDCOD,PCODE+1                                                 
         MVC   NDPRDINT,PACCT                                                   
         L     R1,NDUDEFD          GET USER DEFINITION                          
         USING SBLOCK,R1                                                        
         MVC   SBUP1FLD,PUSER1                                                  
         MVC   SBUP2FLD,PUSER2                                                  
         B     RESUNT3                                                          
*                                                                               
RESUNT3  XC    FILENAME,FILENAME   RESET TO READ UNTFIL                         
         NETGO NVSETUNT,DMCB                                                    
         L     R5,NDAGBLOK                                                      
         USING NETGOALD,R5                                                      
         CLI   NGOALIOS,1          IF NETGOAL IS ACTIVE                         
         BNE   *+8                                                              
         MVI   NGOALIOS,2             TELL NETGOAL TO REESTABLISH SEQ           
         B     XIT3                                                             
         DROP  R5,R4,R1                                                         
         EJECT                                                                  
*                                                                               
NIBFRML  DS    0H                  BILL FORMULA                                 
         NETGO NVSETSPT                                                         
         XC    KEY,KEY                                                          
* - TRY FOR ESTIMATE SPECIFIC BILL FORMULA FIRST                                
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   BYTE,NBSPLPRN                                                    
         BAS   RE,LUPPROD3                                                      
         MVC   KEY+4(3),WORK                                                    
         MVC   KEY+7(1),NBACTEST                                                
         BAS   R5,GTSPTHI                                                       
         CLC   KEY(8),KEYSAVE                                                   
         BNE   NIBF20                                                           
         CLC   KEY+8(5),=6X'00'    MAKE SURE ITS ESTIMATE HEADER                
         BNE   NIBF20                                                           
         BAS   R5,GTSPTREC                                                      
         L     R5,AIO                                                           
         USING ESTHDR,R5                                                        
         CLC   EBILLCOM,=6X'00'    IF NO BFORMULA                               
         BE    NIBF20              TRY PRODUCT HEADER                           
         MVC   0(1,R3),EBILLBAS                                                 
         MVC   1(4,R3),EBILLCOM                                                 
         B     NIBFX                                                            
* - TRY FOR PRODUCT SPECIFIC BILL FORMULA                                       
NIBF20   XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   BYTE,NBSPLPRN                                                    
         CLC   =C'AAA',WORK                                                     
         BE    *+8                                                              
         BAS   RE,LUPPROD3                                                      
         MVC   KEY+4(3),WORK                                                    
         BAS   R5,GTSPTHI                                                       
         CLC   KEY(7),KEYSAVE                                                   
         BNE   NIBF30                                                           
         CLC   KEY+7(5),=6X'00'    MAKE SURE ITS PRODUCT RECORD                 
         BNE   NIBF30                                                           
         BAS   R5,GTSPTREC                                                      
         L     R5,AIO                                                           
         USING PRDHDR,R5                                                        
         CLC   PBILLCOM,=6X'00'    IF NO BFORMULA TRY PROD=AAA                  
         BE    NIBF30                                                           
         MVC   0(1,R3),PBILLBAS                                                 
         MVC   1(4,R3),PBILLCOM                                                 
         B     NIBFX                                                            
NIBF30   CLC   =C'AAA',WORK        HAVE WE TRIED FOR PROD AAA                   
         BE    NIBFX               YES                                          
         MVC   WORK(3),=C'AAA'     NO/TRY FOR PRODUCT AAA                       
         B     NIBF20                                                           
*                                                                               
NIBFX    NETGO NVSETUNT                                                         
         XC    FILENAME,FILENAME                                                
         MVC   KEY,NBKEY           RESET NETIO SEQUENCE                         
         GOTO1 HIGH                                                             
         B     XIT3                                                             
*                                                                               
GTSPTHI  MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         BR    R5                                                               
GTSPTREC MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         BR    R5                                                               
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
NOBFRML  DS    0H                                                               
         XC    WORK,WORK                                                        
         CLI   GLARGS,C'A'         IS IT ABBREVIATED FORMULA                    
         BE    NOBLF                                                            
**       GOTO1 =V(PRNTBL),DMCB,=C'0BF',0(R2),C'DUMP',5,=C'1D'                   
         CLC   1(4,R2),=6X'00'                                                  
         BE    NOBFX                                                            
         MVC   WORK(3),=C'NET'                                                  
         TM    0(R2),X'10'         IS IT NET BASED                              
         BO    *+10                                                             
         MVC   WORK(5),=C'GROSS'                                                
         MVC   WORK+22(2),=C'OF'                                                
         MVC   WORK+25(5),WORK                                                  
         MVC   WORK+6(4),=C'PLUS'                                               
         TM    1(R2),X'80'         IS IT NEGATIVE                               
         BNO   *+10                                                             
         MVC   WORK+6(5),=C'MINUS'                                              
         EDIT  (B4,1(R2)),(8,WORK+12),WRK=WORK+40,TRAIL=C'%'                    
         LA    R1,WORK+19                TO 4 DECIMAL PLACES                    
DECLOOP  CLI   0(R1),C'%'                                                       
         BE    GOTDEC                                                           
         BCTR  R1,0                                                             
         B     DECLOOP                                                          
GOTDEC   S     R1,=F'4'                                                         
         MVC   WORK+35(5),0(R1)    SAVE LAST 4 DIGITS + %                       
         MVI   0(R1),C'.'          SET DECIMAL                                  
         MVC   1(5,R1),WORK+35     RETURN LAST 4 DIGITS                         
         DS    0H                                                               
         GOTO1 SQUASHER,DMCB,WORK,30                                            
         MVC   1(30,R3),WORK                                                    
NOBFX    B     XIT3                                                             
*                                                                               
NOBLF    DS    0H                   ABBREVIATED BILL FORMULA                    
         OC    WORK(30),NDSPACES                                                
         CLC   1(4,R2),=6X'00'                                                  
         BE    NOBLFX                                                           
         MVI   WORK,C'N'                                                        
         TM    0(R2),X'10'         IS IT NET BASED                              
         BO    *+8                                                              
         MVI   WORK,C'G'                                                        
         MVI   WORK+1,C'+'                                                      
         TM    1(R2),X'80'         IS IT NEGATIVE                               
         BNO   *+8                                                              
         MVI   WORK+1,C'-'                                                      
         EDIT  (B4,1(R2)),(8,WORK+2),WRK=WORK+40,ALIGN=LEFT                     
         LA    R1,WORK+9                TO 4 DECIMAL PLACES                     
DECLOP   CLI   0(R1),X'40'                                                      
         BNE   GOTEND                                                           
         BCTR  R1,0                                                             
         B     DECLOP                                                           
GOTEND   S     R1,=F'3'                                                         
         MVC   WORK+35(4),0(R1)    SAVE LAST 4 DIGITS                           
         LA    RE,WORK+38                                                       
         LA    RF,4                                                             
CLRZERO  CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         CLI   0(RE),C'0'          SET ZEROS = BLANK                            
         BNE   SETPCT                                                           
         MVI   0(RE),C' '                                                       
         BCTR  RE,0                                                             
         BCT   RF,CLRZERO                                                       
SETPCT   MVI   1(RE),C'%'                                                       
*                                                                               
         MVI   0(R1),C'.'          SET DECIMAL                                  
         MVC   1(4,R1),WORK+35     RETURN LAST 4 DIGITS                         
         CLI   1(R1),C'%'          IF PERCENT AFTER DECIMAL                     
         BNE   *+12                                                             
         MVI   0(R1),C'%'          DROP DECIMAL                                 
         MVI   1(R1),C' '                                                       
         DS    0H                                                               
         GOTO1 SQUASHER,DMCB,WORK,30                                            
         MVC   0(12,R3),WORK                                                    
         LR    R1,R3                                                            
NOBLF20  CLI   0(R1),X'40'                                                      
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         B     NOBLF20                                                          
         MVC   0(1,R1),WORK                                                     
NOBLFX   B     XIT3                                                             
         EJECT                                                                  
         SPACE 1                                                                
* - LOCAL WORKING STORAGE                                                       
CLIINTSV DS    CL11                CLIENT INTERFACE CODE SAVE AREA              
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              OTHER AREAS                                                      
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*GOALBK*'                                                      
GOALBLOK DC    100X'00'                                                         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'NETNAMES'                                                      
NETNAMES DC    4800X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'GETPESTB'                                                      
GETPESTB DC    F'10000'                                                         
         DC    10000X'00'                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*NETCOM*'                                                      
COMMAREA DC    XL50'00'                                                         
COMMIO   DS    600C                                                             
         SPACE 1                                                                
       ++INCLUDE NEASTDAT                                                       
         DC    CL7'       ',X'FF'                                               
         SPACE 1                                                                
         EJECT                                                                  
*              INCLUDES                                                         
         SPACE 3                                                                
*              NETINCLS            INCLUDES HERE                                
*              NETDEMOT                                                         
*              NENETRAWEX                                                       
*              SPGENCLT                                                         
*              SPGENPRD                                                         
*              SPGENPRG                                                         
*              SPGENEST                                                         
*              SPGENSTA                                                         
*              SPGENMKT                                                         
*              SPTRCMML                                                         
*              NEGENPACK                                                        
*              NEACCTBLK                                                        
*              NEGENCOSTS                                                       
*              NEGENNBUFF                                                       
*              NENETGOALD                                                       
*              NEGENUNIT                                                        
*              NEDATELSTD                                                       
*              DRGLOBAL                                                         
*              DRIVETABLE                                                       
*              DRINTRECD2                                                       
*              NEGETPESTD                                                       
*              NECOMBLOK                                                        
*              NEGENCOM                                                         
*              SPLTBLKD                                                         
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE NENETRAWEX                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPTRNFEED                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEACCTBLK                                                      
       ++INCLUDE NEGENCOSTS                                                     
       ++INCLUDE NEGENNBUFF                                                     
       ++INCLUDE NENETGOALD                                                     
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEDATELSTD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE NEGETPESTD                                                     
       ++INCLUDE NECOMBLOK                                                      
       ++INCLUDE NEGENCOM                                                       
       ++INCLUDE SPLTBLKD                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPGENCLG                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE BHBLOCKBOB                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE NEGENIND                                                       
       ++INCLUDE NEWRIUDEFD                                                     
       ++INCLUDE SPGENREP                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063NEWRIDRBB 05/01/02'                                      
         END                                                                    
