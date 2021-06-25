*          DATA SET T41302     AT LEVEL 075 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044157.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T41302A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41302- INSERTION ORDERS- BUY READ'                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* SMYE 06/26/08 AT RD20 SKIP BUYS WITH JOB CODES BEGINNING WITH !               
*                (EXCLAMATION) - THESE ARE "GROUP" CODES HANDLED IN EIO         
*                                                                               
* SMYE 02/03    ADD CALL FROM ADBUYER LOGIC                                     
*                                                                               
* KWAN 05/15/02 INCLUDE ADDITIONAL CHARGES IN GETINS IF PROFILE IS ON           
*                                                                               
* KWAN 08/01/01 SKIP NO TRAFFIC BUYS (PBDSTAT IS X'20')                         
*                                                                               
* KWAN 05/18/01 BEFORE CHANGING REC, NEED TO CHECK FOR LOCKS                    
*                                                                               
* KWAN 03/28/01 MODIFY GETINS CALL TO EXCLUDE CHARGE AMOUNTS                    
*                                                                               
* BPLA 06/00    NOW LIVE SOURCE (WAS T41302B)                                   
*               PHASE AND ++INCLUDE T413WKA RESTORED                            
*                                                                               
* BPLA 02/10/00 COPY OF T41302 LEVEL 64 MADE 2//10/00                           
*               T413WKAB FOR T413WKA                                            
*                                                                               
* BPLA 12/12/99 CHANGE FOR EXPANDED PUB ADDRESS RECORDS                         
*               USING PPGETADR AND PPGETADRD                                    
*                                                                               
* SMYE 03/31/99 CHECK FOR PGADFAX GREATER THAN SPACES                           
*               AS WELL AS LENGTH OF PGADELEM BEFORE MOVING                     
*               PGADLIN3 AND PGADFAX INTO REP AREAS (AT RD27M..)                
*                                                                               
* BPLA 01/99    TREAT OUTDOOR LIKE THE OTHER MEDIA                              
*               WHEN SEARCHING FOR TRAFFIC ADDRESS                              
*               (USED TO USE CONTRACT ADD/REPS)                                 
*                                                                               
* SMYE 10/97    GETINS MADE CORE-RESIDENT                                       
*               SKIP BUYS FLAGGED WITH X'08' IN PBDSTAT (IN RD20 PROC)          
*                                                                               
* SMYE 04/97    USE CALL TO PPGETADR TO GET ADDRESSES (IN RD26..)               
*                                                                               
* BPLA 03/96    CHECK PUBAOVEL LENGTH BEFORE MOVING                             
*               PUBAOLN3 AND PUBAOFAX  INTO REP AREAS                           
*                                                                               
* BPLA 10/94    NEW VERSION FOR FRENCH CHANGES                                  
*                                                                               
* BPLA 10/24/90 FAX NUMBER SAVED IN MYFAX                                       
*               NEEDED FOR FAXING I/O'S                                         
*                                                                               
* SWON 01/24/90 INCLUDE FAX # IN INSERTION ORDER                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
T41302   CSECT                                                                  
         NMOD1 0,T41302,RR=R2                                                   
*                                                                               
         ST    R2,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         USING IOWORKD,R8                                                       
         LA    R7,1(R9)                                                         
         LA    R7,4095(R7)                                                      
         USING POLFILE,R9,R7                                                    
         USING T413FFD,RA                                                       
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   RD6                                                              
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PRQMED                                                  
         MVI   KEY+3,X'20'                                                      
         CLC   PRQCLT,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+4(3),PRQCLT                                                  
*                                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+7(3),PRQPRD                                                  
*                                                                               
         CLC   PRQEST,=C'ALL'                                                   
         BE    RD4                                                              
         PACK  DUB,PRQEST                                                       
         CVB   R0,DUB                                                           
         STH   R0,BEST                                                          
         MVC   KEY+19(2),BEST                                                   
*                                                                               
RD4      DS    0H                                                               
         CLC   PRQPUB(3),=C'ALL'                                                
         BE    *+10                                                             
         MVC   KEY+10(6),BPUB                                                   
*                                                                               
         MVC   KEY+16(3),BSTART                                                 
*                                                                               
         BAS   RE,HIGH                                                          
         B     RD6H                                                             
*                                                                               
RD6      DS    0H                                                               
         BAS   RE,SEQ                                                           
*                                                                               
RD6H     CLC   KEY(4),KEYSAVE      A/M                                          
         BNE   RD40                                                             
         CLI   KEY+25,X'FF'                                                     
         BE    RD6                                                              
         CLC   PRQCLT,=C'ALL'                                                   
         BE    RD8                                                              
         CLC   KEY+4(3),PRQCLT                                                  
         BNE   RD40                                                             
*                                                                               
RD8      DS    0H                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    RD10                                                             
         CLC   KEY+7(3),PRQPRD                                                  
         BE    RD12                                                             
RD8B     CLC   PRQCLT,=C'ALL'                                                   
         BE    RD6                                                              
         B     RD40                                                             
*                                                                               
RD10     DS    0H                                                               
         CLC   KEY+7(3),=C'ZZZ'    BYPASS POL IF PRD=ALL                        
         BE    RD8B                                                             
*                                                                               
RD12     DS    0H                                                               
         OC    KEY+21(3),KEY+21    BYPASS PASSIVE                               
         BNZ   RD6                                                              
*                                                                               
         OC    BEST,BEST                                                        
         BZ    RD14                                                             
         CLC   KEY+19(2),BEST                                                   
         BNE   RD6                                                              
*                                                                               
RD14     DS    0H                                                               
         CLC   PRQPUB(3),=C'ALL'                                                
         BE    RD16                                                             
         CLC   KEY+10(6),BPUB                                                   
         BE    RD16                                                             
         CLC   QPUB+8(3),=C'ZZZ'   TEST MULTI ZONES/EDITS                       
         BNE   RD14B                                                            
         CLC   KEY+10(4),BPUB                                                   
         BE    RD16                                                             
*                                                                               
RD14B    DS    0H                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    RD6                                                              
         B     RD8B                                                             
*                                                                               
RD16     DS    0H                                                               
         CLC   KEY+16(3),BSTART                                                 
         BL    RD6                                                              
         CLC   KEY+16(3),BEND                                                   
         BNH   RD18                                                             
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   IF ALL PUBS                                  
         BE    RD6                 CONTINUE                                     
         CLC   QPUB+8(3),=C'ZZZ'   OR IF MULTI Z/E                              
         BE    RD6                                                              
         B     RD14B                                                            
*                                                                               
RD18     DS    0H                                                               
         CLC   QBUYLIN,SPACES                                                   
         BE    RD20                                                             
         PACK  DUB,QBUYLIN                                                      
         CVB   R0,DUB                                                           
         STC   R0,BYTE                                                          
         CLC   BYTE,KEY+24         TEST LINE NUMBER                             
         BNE   RD6                                                              
*                                                                               
RD20     DS    0H                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETPRT                                                        
*                                                                               
         CLI   PBDJOB,C'!'          GROUP JOB CODE ? (HANDLED IN EIO)           
         BE    RD6                  YES, SKIP - NO INSTRUCTION RECORD           
*                                                                               
         CLI   PBDBFD,C'T'          TEST BUYS?                                  
         BE    RD6                  YES, SKIP                                   
*                                                                               
         TM    PBDSTAT,X'08'        HELD - NOT ON I/O OR CONTRACT?              
         BO    RD6                  YES, SKIP                                   
*                                                                               
         TM    PBDSTAT,X'20'        NO TRAFFIC?                                 
         BO    RD6                  YES, SKIP                                   
*                                                                               
         XC    FULL,FULL                                                        
         CLI   SHWACHGR,C'Y'        SHOW ADDITIONAL CHARGES?                    
         BE    *+8                                                              
         MVI   FULL,C'X'            TO EXCLUDE ADDITIONAL CHARGES               
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD,FULL                          
*                                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZ    RD6                                                              
         CLC   PRQJOB,=CL6'ALL'                                                 
         BE    RD21                                                             
         CLC   PBDJOB,PRQJOB                                                    
         BNE   RD6                                                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
RD21     DS    0H                  CHECK IF DOING CALL FROM ADBUYER *           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
         CLI   ADBSW,C'Y'          ADBUYER ?                                    
         BNE   RD22                NO - CONTINUE                                
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'99'        SERIAL NUMBER ELEMENT                        
         BAS   RE,NXTELEM                                                       
         BNE   RD6                 SKIP BUY - NO SERIAL NO ELEM IN REC          
         L     R3,ASERTBL          POINT TO SERIAL# TABLE                       
RD21LUP  CLI   0(R3),X'FF'         END OF TABLE ?                               
         BE    RD6                 YES - SKIP BUY - NOTHING FOUND               
         USING PSERELMD,R2                                                      
         CP    1(5,R3),PSERNUM     SERIAL NUMBER MATCH ?                        
         BNE   RD21BMP             NO - TRY NEXT TABLE ENTRY                    
         MVI   0(R3),C'Y'          INDICATE BUY REC "USED" HERE                 
         B     RD22                CONTINUE                                     
RD21BMP  LA    R3,6(R3)            BUMP TO NEXT TABLE ENTRY                     
         B     RD21LUP                                                          
         DROP  R2                                                               
RD22     DS    0H                                                               
         CLI   RCWRITE,C'Y'        UPDATING RECORDS?                            
         BNE   RD22H               NO, THEN NO NEED TO CHECK LOCKS              
         CLI   DATALKSW,X'FE'      LOCK ALREADY ISSUED?                         
         BE    RD22H                                                            
         BAS   RE,ADDLOCK          CHK DATA LOCK AND ISSUE ONE                  
         BE    RD22C                                                            
         MVI   ERRAREA,X'FE'       DATA IS ALREADY LOCKED!                      
         LHI   R3,D#REQID          FOR USE IN T41300 ERROR ROUTINE              
         STH   R3,SVERRFLD           WHEN DOING ADBUYER CALL                    
         B     EXXMOD                                                           
*                                                                               
RD22C    MVI   DATALKSW,X'FE'      LOCK HAS BEEN ADDED                          
         BAS   RE,HIGH             NEED TO RESTORE SEQUENCES                    
         CLC   KEY(25),KEYSAVE     SAME RECORD RETURNED?                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RD22H    DS    0H                                                               
         XC    DUB,DUB             GET PUB                                      
         MVC   DUB(6),PBUYKPUB                                                  
         CLC   QPUB+8(3),=C'ZZZ'   IF MULTI ZONES/EDITS                         
         BE    RD23                                                             
         BAS   RE,FNDPUB                                                        
         B     RD23F                                                            
*                                                                               
RD23     DS    0H                  MULTI Z/E                                    
         CLC   SVZON(2),PBUYKPUB+4 TEST NEW Z/E                                 
         BE    RD23B                                                            
         BAS   RE,FNDPUB                                                        
         MVC   SVZON(2),PBUYKPUB+4 SAVE Z/E                                     
         MVC   SVZONNM,PUBZNAME    AND ZONE NAME                                
*                                                                               
RD23B    DS    0H                                                               
         MVC   DUB(6),BPUB         NOW READ BASE PUB                            
         BAS   RE,FNDPUB                                                        
*                                                                               
RD23F    DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
*                                  MAKE SURE HAVE PROPER HEADERS                
         XC    WORK(32),WORK                                                    
*                                  CLIENT                                       
         MVC   WORK(7),PBUYKEY                                                  
         MVI   WORK+3,2                                                         
         CLC   WORK(25),PCLTKEY                                                 
         BE    RD24                                                             
         MVC   KEY,WORK                                                         
         BAS   RE,READ                                                          
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETPRT                                                        
*                                  PRODCUT                                      
RD24     DS    0H                                                               
         MVI   WORK+3,6                                                         
         MVC   WORK+7(3),PBUYKPRD                                               
         CLC   WORK(25),PPRDKEY                                                 
         BE    RD25                                                             
         MVC   KEY,WORK                                                         
         BAS   RE,READ                                                          
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETPRT                                                        
*                                                                               
RD25     DS    0H             MUST READ EST TO SEE IF TEST                      
         B     RD26           NO LONGER NEEDED - OUT 3/8/88                     
*                                                                               
RD26     DS    0H                                                               
*                                  TRY FOR ADDR ELEM                            
         MVC   SVLANG,LANG         SAVE LANGUAGE FOR LAST PRINTING              
         MVC   LANG,PUBLANG                                                     
*                                                                               
         MVC   SCPSCOM1,CPSCOM1    SAVE LAST FOR PRINTING                       
         MVC   SCPSCOM2,CPSCOM2    SAVE LAST FOR PRINTING                       
         XC    CPSCOM1,CPSCOM1     CLEAR                                        
         XC    CPSCOM2,CPSCOM2     CLEAR                                        
         MVI   ADRTYP,C'T'         TRA                                          
         CLI   ADDROPT,C'1'        SHIPPING ONLY                                
         BE    RD26A                                                            
         CLI   ADDROPT,C'2'        OR SHIPPING + PUB                            
         BNE   RD26A5                                                           
*                                                                               
RD26A    MVI   ADRTYP,C'S'                                                      
*                                                                               
RD26A5   DS    0H                                                               
         XC    PREPREC(PUBREC-PREPREC),PREPREC                                  
         XC    DUB,DUB                                                          
*                                    FILL IN 7 BYTES OF CLTDATA                 
RD26B    DS    0H                                                               
         MVC   CLTAGY,PUBKAGY        AGY                                        
         MVC   CLTMED,PRQMED         MED                                        
         MVC   CLTCODE,PCLTKCLT      CLIENT CODE                                
         MVC   CLTOFF,PCLTOFF        CLIENT OFFICE                              
*                                                                               
         GOTO1 APPGETAD,DMCB,(ADRTYP,CLTDATA),PUBREC,VDATAMGR                   
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),0             ADDRESS REC FOUND ?                          
         BNE   RD26C               YES                                          
*                                  NO ADDRESS REC FOUND                         
         JIF   ADRTYP,NE,C'S',AND,ADRTYP,NE,C'T',RD26X,JUMP=N                   
*                                  DIDN'T FIND SHIPPING OR TRAFFIC              
         CLI   QMEDIA,C'O'                                                      
         BNE   RD26X               TRY FOR REP                                  
         MVI   ADRTYP,C'C'         USE CON FOR OUTDOOR                          
         B     RD26A5              TRY FOR CONTRACT ADDRESS                     
*                                                                               
RD26C    MVC   DUB(3),1(R1)        ADDREC LEVEL(3)                              
         MVC   ELCODE,0(R1)        CODE FOUND                                   
         L     R2,4(R1)            A(ADDRESS INFO FROM CALL)                    
         ST    R2,FULL             SAVE A(ADDRESS)                              
*                                                                               
RD26D    DS    0H                                                               
         CLI   ELCODE,X'0B'        SEE IF I WAS LOOKING FOR SHIPPING            
         BE    RD27M               AND FOUND IT - THEN SKIP REP                 
*                                                                               
RD26X    DS    0H                  TRY FOR REP                                  
         MVI   ELCODE,X'14'                                                     
         LA    R2,PUBREC+33                                                     
*                                                                               
RD27B    DS    0H                                                               
         BRAS  RE,NXTELEM                                                       
         BNE   RD27M                                                            
         USING PUBREPEL,R2                                                      
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    RD27D                                                            
         CLC   PUBRPOFF,PCLTKCLT                                                
         BE    RD27D                                                            
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   RD27B                                                            
         CLC   PUBRPOFF+1(1),PCLTOFF                                            
         BNE   RD27B                                                            
*                                                                               
RD27D    DS    0H                                                               
         CLI   PUBREPEL+1,X'2D'    SEE IF BIG REP ELEM                          
         BL    RD27G                                                            
         TM    PUBCSCC1,X'80'      SEE IF ON INSERTION ORDERS                   
         BZ    RD27E                                                            
         MVC   CPSCOM1,PUBCSC1                                                  
RD27E    TM    PUBCSCC2,X'80'                                                   
         BZ    RD27G                                                            
         MVC   CPSCOM2,PUBCSC2                                                  
*                                                                               
RD27G    OC    PUBPAREP(12),PUBPAREP                                            
         BZ    RD27B                                                            
         LA    RF,PUBTRREP                                                      
         CLI   QMEDIA,C'O'                                                      
         BNE   *+8                                                              
         LA    RF,PUBCNREP                                                      
         OC    0(4,RF),0(RF)                                                    
         BZ    RD27M                                                            
         MVC   DUB+3(3),PUBRPOFF                                                
*                                                                               
         OC    DUB(3),DUB                                                       
         BZ    *+14                                                             
         CLC   DUB(3),DUB+3        SELECT MOST SPECIFIC                         
         BNH   RD27M                                                            
         XC    WORK(32),WORK                                                    
         MVC   WORK(2),PUBKAGY                                                  
         MVC   WORK+2(1),PRQMED                                                 
         MVI   WORK+3,X'11'                                                     
         MVC   WORK+4(4),0(RF)                                                  
         CLC   WORK(25),PREPKEY                                                 
         BE    RD30                                                             
         MVC   KEY,WORK                                                         
         BAS   RE,READ                                                          
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETPRT                                                        
         B     RD30                                                             
*                                                                               
RD27M    DS    0H                                                               
         OC    DUB(3),DUB                                                       
         BZ    RD30                                                             
         L     R2,FULL                                                          
         USING PGETADRD,R2                                                      
         MVC   PREPELEM(2),PGADELEM                                             
*                                                                               
* NOW MOVE FIELDS ONE AT A TIME                                                 
*                                                                               
         MVC   PREPNAME,PGADNAME                                                
         MVC   PREPLIN1,PGADLIN1                                                
         MVC   PREPLIN2,PGADLIN2                                                
         MVC   PREPATTN,PGADATTN                                                
         MVC   PREPTEL,PGADTEL                                                  
*                                                                               
         XC    PREPLIN3,PREPLIN3   MUST CLEAR                                   
         XC    PREPFAX,PREPFAX                                                  
*                                                                               
* ELEMENTS SHOULD ALL BE THE SAME LENGTH NOW                                    
*                                                                               
         OC    PGADFAX,SPACES                                                   
         CLC   PGADFAX,SPACES      ANY FAX ?                                    
         BNH   RD27S               NO - MUST HAVE EMAIL ONLY                    
         MVC   PREPLIN3(26),PGADLIN3                                            
         MVC   PREPFAX(12),PGADFAX                                              
*                                                                               
RD27S    MVC   PREPKEY(3),PCLTKEY                                               
         DROP  R2                                                               
*                                                                               
RD30     DS    0H                                                               
         XC    MYFAX,MYFAX                                                      
         CLI   PREPKEY,0                                                        
         BE    *+14                                                             
         MVC   MYFAX,PREPFAX                                                    
         B     RD30D                                                            
*                                                                               
         LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'11'                                                      
         BE    RD30B                                                            
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   RD30D                                                            
         USING PUBSADEL,R2                                                      
RD30B    MVC   MYFAX,PUBSFAXN                                                   
         DROP  R2                                                               
*                                                                               
RD30D    OC    MYFAX,SPACES                                                     
*                                                                               
RD30S    DS    0H                                                               
         CLC   PPGKEY(25),KEY                                                   
         BE    RD32                                                             
         MVC   KEY(25),PPGKEY                                                   
         BAS   RE,HIGH                                                          
*                                                                               
RD32     DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
RD40     DS    0H                  END                                          
         MVI   PBUYKEY,X'FF'                                                    
         B     EXXMOD                                                           
*                                                                               
ADRTYP   DS    CL1                 TYPE OF ADDR REC-PAY,TRAFFIC,ETC.            
CLTDATA  DS    0CL7                KEY INFO TO PPGETADR MODULE                  
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
FNDPUB   NTR1                      GET PUB RECORD- PUB CODE IN DUB              
*                                                                               
         XC    FULL,FULL                                                        
         MVI   PUBREC,0                                                         
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(1),PRQMED                                                    
         MVC   KEY+1(6),DUB                                                     
         MVC   KEY+7(2),AGYALPHA                                                
         BAS   RE,HIGHPUB                                                       
         B     *+8                                                              
FP2      BAS   RE,SEQPUB                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BE    FP3                                                              
         CLC   QPUB+8(3),=C'ZZZ'   TEST MULTI Z/E                               
         BNE   FP8                                                              
         CLC   KEY(5),KEYSAVE      TEST THRU Z/E                                
         BNE   FP8                                                              
*                                                                               
FP3      DS    0H                                                               
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   FP2                                                              
         CLI   KEY+9,X'81'                                                      
         BNE   FP2                                                              
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         BAS   RE,GETPUB                                                        
*                                                                               
         CLI   FROPT,C'Y'          SEE IF USING PUB'S LANGUAGE                  
         BE    *+8                 IF NOT - ALWAYS IN ENGLISH                   
         MVI   PUBLANG,0           THIS IS SAVE SINCE I NEVER                   
*                                  WILL BE WRITING THE PUB BACK                 
FP8      DS    0H                                                               
         MVC   KEY(64),WORK                                                     
         CLI   PUBREC,0                                                         
         BE    FPNO                                                             
*                                                                               
JUMPXIT1 XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FPNO     DS    0H                                                               
         DC    H'0'                PUB MISSING                                  
*                                                                               
NXTELEM  ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R2),0                                                          
         JNE   NXTELEM                                                          
         LTR   R2,R2               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADDLOCK  NTR1                                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,KEY+2                                                  
         MVC   L.LOCKCLT,KEY+4                                                  
*                                                                               
ADDLK2   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK2              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         BE    ADDLKERR            ALREADY LOCKED?                              
         CLI   4(R1),0                                                          
         BNE   ADDLKERR            ERROR OCCURED                                
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   PUB IS ALL?                                  
         BE    ADDLK4              YES, GO AHEAD AND ISSUE CLIENT LOCK          
         OC    KEY+10(4),KEY+10    NOTHING IN BASE PUB NUMBER?                  
         BZ    ADDLK4              YES, GO AHEAD AND ISSUE CLIENT LOCK          
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,KEY+02                                                 
         MVC   L.LOCKCLT,KEY+04                                                 
         MVC   L.LOCKPUB,KEY+10    BAS PUB NUMBER                               
*                                                                               
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
ADDLK3   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK3              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         BE    ADDLKERR            ALREADY LOCKED?                              
         CLI   4(R1),0                                                          
         BNE   ADDLKERR            ERROR OCCURED                                
*                                                                               
ADDLK4   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKLOCKQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK4              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         BE    ADDLKERR            ALREADY LOCKED                               
         CLI   4(R1),0                                                          
         BNE   ADDLKERR            ERROR OCCURED                                
         B     ADDLKOK                                                          
*                                                                               
ADDLKOK  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
ADDLKERR LTR   RB,RB               NOT EQUAL                                    
         J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         DROP  L                                                                
*                                                                               
DATALOCK EQU   158                 DATA LOCKED ERROR MSG                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE POLDMGR                                                        
*                                                                               
       ++INCLUDE POLEXTS                                                        
*                                                                               
       ++INCLUDE T413WKA                                                        
*                                                                               
       ++INCLUDE POLFILE                                                        
*                                                                               
       ++INCLUDE T413WKB                                                        
*                                                                               
PUBRPELD DSECT                                                                  
*                                                                               
       ++INCLUDE PUBREPEL                                                       
*                                                                               
PUBAOELD DSECT                                                                  
       ++INCLUDE PUBAOVEL                                                       
*                                                                               
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
*                                                                               
PSERELMD DSECT                                                                  
       ++INCLUDE PSERELEM                                                       
*                                                                               
       ++INCLUDE PPMAPEQUS                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
*                                                                               
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKPUB  DS    XL4                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075T41302    07/09/14'                                      
         END                                                                    
