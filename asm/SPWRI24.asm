*          DATA SET SPWRI24    AT LEVEL 057 AS OF 05/01/02                      
*PHASE T20424A,*                                                                
         TITLE 'T20424 - DMB&&B GOAL FILE TRANSFER PROGRAM'                     
T20424   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20424,RA,RR=R2,CLEAR=YES                                  
         LR    R8,RC                                                            
         USING WORKD,R8                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
*                                                                               
         CLI   MODE,VALREC         VALIDATE REQUEST                             
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PRINT THE REPORT                             
         BNE   *+12                                                             
         BAS   RE,PREP                                                          
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VREC     NTR1  ,                                                                
         LA    R2,FTPMEDH          VALIDATE MEDIA                               
         GOTO1 VALMED                                                           
         CLI   SBQMED,C'T'         MEDIA T AND R ONLY                           
         BE    *+12                                                             
         CLI   SBQMED,C'R'                                                      
         BNE   EINV                                                             
*                                                                               
         LA    R2,FTPCLTH          CLIENT                                       
         GOTO1 VALCLT                                                           
         CLI   SBQCGRD,0           NO CLIENT GROUPS                             
         BNE   EINV                                                             
         OC    SBBCLT,SBBCLT       IF SINGLE CLIENT,                            
         BZ    VREC2                                                            
         L     RE,AIO1             SAVE THE CLIENT RECORD TO IOAREA3            
         LA    RF,CLTHDRL                                                       
         L     R0,AIO3                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VREC2    LA    R2,FTPPRDH          PRODUCT                                      
         GOTO1 VALPRD                                                           
         CLI   SBQPGRD,C' '        NO PRODUCT GROUPS                            
         BH    EINV                                                             
         CLI   SBQBPRD2,0          NO PIGGYBACK                                 
         BNE   EINV                                                             
         CLC   SBQPRD,=C'POL'      TEST POL REQUEST                             
         BNE   *+8                                                              
         MVI   SBQBPRD,0           YES-MAKE SURE WE GET ALL PRODUCTS            
         CLI   WHEN,X'40'          IF NOW REQUEST,                              
         BNE   VREC4                                                            
         CLI   SBQBPRD,0           SINGLE PRODUCT ONLY                          
         BE    EINV                                                             
*                                                                               
VREC4    CLI   SBQBPRD,0           TEST SINGLE PROUCT REQUEST                   
         BE    *+14                                                             
         L     R1,AIO1             YES-SAVE THE PRODUCT NAME                    
         MVC   SBPRDNM,PNAME-PRDHDRD(R1)                                        
*                                                                               
         LA    R2,FTPESTH          ESTIMATE                                     
         GOTO1 VALEST                                                           
         CLI   WHEN,X'40'          IF NOW REQUEST,                              
         BNE   *+14                                                             
         CLC   SBQEST,SBQESTND     SINGLE ESTIMATE ONLY                         
         BNE   EINV                                                             
         MVI   SBQSEPES,C'Y'       TREAT ESTIMATES SEPARATELY                   
*                                                                               
         MVI   OV,C'N'             OPTIONS                                      
         LA    R2,FTPOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VREC6                                                            
         CLC   8(2,R2),=C'OV'      OVERNIGHT OPTION                             
         BNE   EINV                                                             
         CLI   OFFLINE,C'Y'        INVALID ONLINE                               
         BNE   EINV                                                             
         MVI   OV,C'Y'                                                          
         B     VRECX                                                            
*                                                                               
VREC6    OC    SBBCLT,SBBCLT       TEST SINGLE CLIENT REQUEST                   
         BNZ   VRECX                                                            
         LA    R2,FTPCLTH          NO-ERROR                                     
         B     EINV                                                             
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EINV     MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
MYCURSOR MVI   ERROR,X'FE'         OWN ERROR MESSAGE (CONHEAD HAS MSG)          
         GOTO1 CURSERR                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
         SPACE 1                                                                
PREP     NTR1  ,                                                                
         MVC   SVQBCLT,SBQBCLT     SAVE REQUESTED CLT/PRD                       
         MVC   SVQBPRD,SBQBPRD                                                  
*                                                                               
         MVC   SBCOMFAC,ACOMFACS   INITIALIZE SPOT BLOCK FOR SPOTIO             
         GOTO1 DATCON,DMCB,(3,BTODAY),SBQTODAY                                  
         LA    R1,IOHOOK                                                        
         ST    R1,SBIOHOOK                                                      
         LA    R1,CLTREC                                                        
         ST    R1,SBACLTRC                                                      
         MVC   SBAIO1(12),AIO1     PASS 3 IO AREAS                              
         L     R1,ASPOOLD                                                       
         MVC   SBPRINT,VPRINT-SPOOLD(R1)                                        
         OI    SBQSKIP,SBQSKMED    ACCOUNTING ONLY                              
         OI    SBQSKIP,SBQSKBUY+SBQSKBIL  SKIP BUYS AND BILLS                   
*                                                                               
         L     RE,AIO3             SAVE THE CLIENT RECORD FROM AIO3             
         LA    RF,CLTHDRL                                                       
         LA    R0,CLTREC                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LA    R1,CLTREC                                                        
         MVC   SBCOFF,COFFICE-CLTHDRD(R1)   SAVE CLIENT OFFICE                  
*                                                                               
         MVI   FIRST,C'Y'                                                       
         CLI   OV,C'Y'             TEST OVERNIGHT OPTION                        
         BNE   *+12                                                             
         BAS   RE,RDRECV           YES-READ RECOVERY FILE                       
         B     PREP20                                                           
*                                                                               
         BAS   RE,RDPROF           GET 00 PROFILE                               
         LA    R4,1                                                             
         CLI   SBQBPRD,0           TEST SINGLE PRODUCT REQUEST                  
         BNE   PREP4                                                            
         LA    R3,CLTREC           NO-LOOP THROUGH ALL THE PRODUCTS             
         LA    R3,CLIST-CLTHDRD(R3)                                             
         LA    R4,220                                                           
*                                                                               
PREP2    CLI   0(R3),C' '                                                       
         BNH   PREP20                                                           
         CLI   3(R3),X'FF'         AVOID POL                                    
         BE    PREP18                                                           
         MVC   SBQPRD,0(R3)                                                     
         MVC   SBPRD,0(R3)                                                      
         MVC   SBQBPRD,3(R3)                                                    
         BAS   RE,GETPRD           GET THE PRODUCT RECORD                       
*                                                                               
PREP4    OI    SBQSKIP,SBQSKGL     DON'T READ GOALS YET                         
         XC    SBASVETB,SBASVETB                                                
         OI    SBINDS,SBIONLIN     FORCE ONLINE PROCESSING FOR SPOTIO           
         GOTO1 SPOTIO,DMCB,SBLOCK  CALL SPOTIO TO GET ESTIMATE TABLE            
         L     R1,SBASVETB                                                      
         MVC   ESTTAB,0(R1)        CAPTURE THE ESTIMATE TABLE                   
         OC    ESTTAB,ESTTAB       TEST ANY ACTIVE ESTIMATES                    
         BZ    PREP18              NO-SKIP TO NEXT PRODUCT                      
         XC    ESTTAB2,ESTTAB2                                                  
         NI    SBQSKIP,255-SBQSKGL READ GOALS                                   
         LA    R5,ESTTAB                                                        
         LA    R6,ESTTAB2                                                       
         ST    R6,SBASVETB                                                      
         LA    R7,256                                                           
*                                                                               
PREP6    CLI   0(R5),0             CALL SPOTIO FOR EACH ESTIMATE                
         BE    PREP8                                                            
         MVC   0(1,R6),0(R5)                                                    
         LNR   R1,R7                                                            
         LA    R1,256(R1)                                                       
         STC   R1,SBBEST                                                        
         BAS   RE,GETEST           GET ESTIMATE RECORD                          
         MVI   NEWEST,C'Y'                                                      
         MVI   BLDWKTAB,C'Y'                                                    
         OI    SBINDS,SBIONLIN     FORCE ONLINE PROCESSING FOR SPOTIO           
         GOTO1 SPOTIO,DMCB,SBLOCK  GET GOALS FOR THIS ESTIMATE                  
         MVI   0(R6),0                                                          
*                                                                               
PREP8    LA    R5,1(R5)            NEXT ESTIMATE                                
         LA    R6,1(R6)                                                         
         BCT   R7,PREP6                                                         
*                                                                               
PREP18   LA    R3,4(R3)            NEXT PRODUCT                                 
         BCT   R4,PREP2                                                         
*                                                                               
PREP20   B     PREPX                                                            
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ RECOVERY FILE                                                  *         
***********************************************************************         
         SPACE 1                                                                
RDRECV   NTR1  ,                                                                
         L     R3,LBUF             GET BUFFER FOR BINSRCH                       
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
******** LA    RE,BUFFER           **TEST                                       
         ST    RE,ABUF                                                          
******** LA    R3,L'BUFFER         **TEST                                       
         LR    RF,R3                                                            
         XCEFL ,                   CLEAR                                        
         LA    R1,L'RKEY                                                        
         ST    R1,KEYLEN           SET L'KEY                                    
         SR    R2,R2                                                            
         LA    R1,L'REC            SET L'RECORD                                 
         ST    R1,RECLEN                                                        
         DR    R2,R1                                                            
         ST    R3,MAXRECS          SET MAX N'RECORDS ALLOWED                    
         XC    NRECS,NRECS                                                      
         XC    LASTKEY,LASTKEY                                                  
         OPEN  (RECVIN,(INPUT))                                                 
*                                                                               
RDRECV1  LA    R0,RECVHDR-4        READ RECOVERY FILE                           
         GET   RECVIN,(0)                                                       
         CLI   RFILTY,X'21'        TEST SPTFIL                                  
         BNE   RDRECV1                                                          
         CLI   RRECTY,2            TEST CHANGE OR ADD                           
         BE    *+12                                                             
         CLI   RRECTY,3                                                         
         BNE   RDRECV1                                                          
         LA    R2,RECKEY                                                        
         USING GOALRECD,R2                                                      
         CLC   GAGYALPH,=C'DB'     TEST DMB&B                                   
         BNE   RDRECV1                                                          
         CLI   GKEYTYPE,2          TEST GOAL RECORD                             
         BNE   RDRECV1                                                          
         CLC   GKEYAM,SBBAGYMD     CHECK AGENCY/MEDIA                           
         BNE   RDRECV1                                                          
         CLC   GKEY(GKEYDPT-GKEY),LASTKEY  TEST MARKET SAME AS LAST             
         BE    RDRECV1             YES-ALREADY PROCESSED                        
         MVC   LASTKEY,GKEY                                                     
         MVC   RCLT,GKEYCLT        SET BINSRCH KEY                              
         MVC   RPRD,GKEYPRD                                                     
         MVC   REST,GKEYEST                                                     
         MVC   RMKT,GKEYMKT                                                     
         MVI   RFLAG,0                                                          
         MVC   RBUYNAME,BLANKS                                                  
         LA    R1,GDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
RDRECV2  CLI   0(R1),0             SEARCH FOR ELEMENTS                          
         BE    RDRECV3                                                          
         CLI   0(R1),X'20'         TEST DESCRIPTION ELEMENT                     
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RDRECV2                                                          
         MVC   RBUYNAME,GBUYNAME-GDELEM(R1) YES-PICK UP BUYER NAME              
*                                                                               
RDRECV3  L     R3,NRECS                                                         
         L     R4,RECLEN                                                        
         L     R5,KEYLEN                                                        
         L     R6,MAXRECS                                                       
         GOTO1 BINSRCH,DMCB,(1,REC),ABUF,(R3),(R4),(0,(R5)),(R6)                
         CLI   0(R1),0             TEST RECORD FOUND                            
         BE    *+12                                                             
         LA    R3,1(R3)            NO-AUGMENT N'RECORDS SO FAR                  
         ST    R3,NRECS                                                         
         B     RDRECV1                                                          
*                                                                               
RDRECV4  DS    0H                                                               
         CLOSE (RECVIN,)                                                        
         ICM   R3,15,NRECS         TEST ANY RECORDS                             
         BZ    RDRECVX                                                          
         L     R2,ABUF             YES-READ RECORDS SEQUENTIALLY                
         XC    SBBCLT,SBBCLT                                                    
         XC    SBBPRD,SBBPRD                                                    
         XC    SBBEST,SBBEST                                                    
         XC    LASTREC,LASTREC                                                  
*                                                                               
RDRECV5  MVC   REC,0(R2)                                                        
         CLC   REC(RPRD-REC),LASTREC  TEST NEW CLIENT                           
         BE    RDRECV6                                                          
         MVC   SBBCLT,RCLT         GET NEW CLIENT RECORD                        
         BAS   RE,GETCLT                                                        
         BAS   RE,RDPROF           GET 00 PROFILE                               
*                                                                               
RDRECV6  CLC   REC(REST-REC),LASTREC  TEST NEW PRODUCT                          
         BE    RDRECV8                                                          
         MVC   SBBPRD,RPRD         YES-                                         
         MVC   SBQBPRD,RPRD                                                     
         LA    R1,CLTREC           GET ALPHA PROUCT CODE                        
         LA    R1,CLIST-CLTHDRD(R1)                                             
         LA    RF,220                                                           
*                                                                               
RDRECV7  CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   SBBPRD,3(R1)                                                     
         BE    *+14                                                             
         LA    R1,4(R1)                                                         
         BCT   RF,RDRECV7                                                       
         DC    H'0'                                                             
         MVC   SBQPRD,0(R1)                                                     
         MVC   SBPRD,0(R1)                                                      
         BAS   RE,GETPRD           GET PRODUCT RECORD                           
*                                                                               
RDRECV8  CLC   REC(RMKT-REC),LASTREC  TEST NEW ESTIMATE                         
         BE    RDRECV10                                                         
         MVC   SBBEST,REST         YES-READ ESTIMATE RECORD                     
         BAS   RE,GETEST                                                        
         MVI   NEWEST,C'Y'         NO GOALS YET                                 
         XC    ESTTAB2,ESTTAB2     PREPARE ESTIMATE TABLE JUST                  
         LA    R1,ESTTAB2          WITH THIS ESTIMATE                           
         ST    R1,SBASVETB                                                      
         SR    RE,RE                                                            
         ICM   RE,1,REST                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R1                                                            
         MVI   0(RE),1                                                          
         MVI   BLDWKTAB,C'Y'                                                    
         XC    LASTKEY,LASTKEY                                                  
         OI    SBINDS,SBIONLIN     FORCE ONLINE PROCESSING FOR SPOTIO           
         GOTO1 SPOTIO,DMCB,SBLOCK                                               
         MVC   REC,0(R2)                                                        
*                                                                               
RDRECV10 MVC   LASTREC,REC         SAVE CURENT RECORD                           
         TM    RFLAG,X'80'         TEST GOALS FOUND FOR THIS MARKET             
         BO    *+8                                                              
         BAS   RE,MISSMKT          NO-GENERATE MARKET RECORD WITH               
         LA    R2,L'REC(R2)           DELETE INDICATOR                          
         BCT   R3,RDRECV5          NEXT RECORD                                  
*                                                                               
RDRECVX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET CLIENT RECORD                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETCLT   NTR1  ,                                                                
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         USING CLTHDRD,R2                                                       
         MVC   CKEYAM,SBBAGYMD                                                  
         MVC   CKEYCLT,SBBCLT                                                   
         GOTO1 HIGH                                                             
         CLC   CKEY,KEYSAVE                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CLTREC                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   SBCOFF,COFFICE                                                   
         GOTO1 CLUNPK,DMCB,SBBCLT,SBQCLT                                        
GETCLTX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET 00 PROFILE                                                      *         
***********************************************************************         
         SPACE 1                                                                
RDPROF   NTR1  ,                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),SBAGY                                                  
         MVC   WORK+6(1),SBQMED                                                 
         MVC   WORK+7(3),SBCLT                                                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,SBSPPROF,DATAMGR                               
         MVC   WEEKST,SBSPPROF+8   SAVE WEEK START DAY                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET PRODUCT RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETPRD   NTR1  ,                                                                
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         USING PRDHDRD,R2                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
         MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,SBPRD                                                    
         GOTO1 HIGH                                                             
         MVC   SBPRDNM,BLANKS                                                   
         MVC   SBPRDNM(8),=C'*UNKNOWN'                                          
         CLC   PKEY,KEYSAVE                                                     
         BNE   GETPRDX                                                          
         L     R2,SBAIO1                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   SBPRDNM,PNAME       SAVE PRODUCT NAME                            
GETPRDX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET ESTIMATE RECORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
GETEST   NTR1  ,                                                                
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         USING ESTHDRD,R2                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,SBPRD                                                    
         MVC   EKEYEST,SBBEST                                                   
         GOTO1 HIGH                                                             
         MVC   SBESTNM,BLANKS                                                   
         MVC   SBESTNM(8),=C'*UNKNOWN'                                          
         CLC   EKEY,KEYSAVE                                                     
         BNE   GETESTX                                                          
         L     R2,SBAIO1                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   SBESTNM,EDESC       SAVE ESTIMATE NAME                           
         MVC   SBESTDEM,EDEMOS     AND DEMO                                     
GETESTX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SPOTIO HOOK                                                         *         
***********************************************************************         
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   SBMODE,SBPROCGL     GOAL RECORD                                  
         BE    PROCGOAL                                                         
*                                                                               
HKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GOAL RECORD HOOK FROM SPOTIO                                        *         
***********************************************************************         
         SPACE 1                                                                
PROCGOAL L     R2,SBAIO1                                                        
         USING GOALRECD,R2                                                      
         CLI   OV,C'Y'             TEST OVERNIGHT REQUEST                       
         BNE   GOAL2                                                            
         CLC   GKEY(GKEYDPT-GKEY),LASTKEY  YES-TEST MARKET CHANGE               
         BE    GOAL1                                                            
         MVC   LASTKEY,GKEY        YES-SEARCH BINSRCH TABLE FOR MKT             
         MVC   RCLT2,GKEYCLT                                                    
         MVC   RPRD2,GKEYPRD                                                    
         MVC   REST2,GKEYEST                                                    
         MVC   RMKT2,GKEYMKT                                                    
         L     R3,NRECS                                                         
         L     R4,RECLEN                                                        
         L     R5,KEYLEN                                                        
         L     R6,MAXRECS                                                       
         GOTO1 BINSRCH,DMCB,(0,REC2),ABUF,(R3),(R4),(0,(R5)),(R6)               
         MVI   SKIPMKT,C'Y'                                                     
         CLI   0(R1),0             TEST RECORD FOUND                            
         BNE   GOAL1                                                            
         MVI   SKIPMKT,C'N'        YES-DON'T SKIP THIS MARKET                   
         L     R1,0(R1)                                                         
         OI    RFLAG-REC(R1),X'80'     MARK RECORD WITH GOALS FOUND             
*                                                                               
GOAL1    CLI   SKIPMKT,C'Y'                                                     
         BE    GOALX                                                            
*                                                                               
GOAL2    MVC   BUYNAME,BLANKS                                                   
         XC    COST,COST                                                        
         XC    GRPS,GRPS                                                        
         MVI   NEWGOAL,C'Y'                                                     
         LA    R3,GDELEM                                                        
*                                                                               
GOAL3    CLI   0(R3),0             SEARCH FOR ELEMENTS                          
         BE    GOAL16                                                           
         CLI   0(R3),X'20'         TEST DESCRIPTION ELEMENT                     
         BNE   GOAL4                                                            
         MVC   BUYNAME,GBUYNAME-GDELEM(R3)  YES-PICK UP BUYER NAME              
         B     GOAL14                                                           
*                                                                               
GOAL4    CLI   0(R3),X'21'         TEST WEEK ELEMENT                            
         BNE   GOAL14                                                           
         USING GLEMENT,R3                                                       
         ICM   R5,15,GLBUDGET      YES-PICK UP COST AND GRPS                    
         ICM   R6,15,GLGRP                                                      
         CLI   NEWGOAL,C'Y'        TEST FIRST WEEK                              
         BNE   *+12                                                             
         MVI   NEWGOAL,C'N'        YES                                          
         B     GOAL9                                                            
         C     R5,COST             NO-TEST COST OR GRPS HAVE CHANGED            
         BNE   GOAL8                                                            
         C     R6,GRPS                                                          
         BNE   GOAL8                                                            
         L     R7,AWEEK            NO-TEST CONTIGUOUS WEEKS                     
         CLC   GLWEEK,16(R7)                                                    
         BNE   GOAL8                                                            
         CLC   10(2,R7),26(R7)     YES-TEST BROADCAST YEAR CHANGE               
         BNE   GOAL8                                                            
         CP    WKCNT,=PL2'52'      NO-TEST ALREADY 52 WKS IN THE FLIGHT         
         BNL   GOAL8                                                            
         LA    R7,16(R7)           NO-THIS WK IS A FLIGHT CONTINUATION          
         ST    R7,AWEEK                                                         
         MVC   ENDATE,10(R7)                                                    
         AP    WKCNT,=P'1'                                                      
         B     GOAL14                                                           
*                                                                               
GOAL8    BAS   RE,WRTGOAL          FLIGHT END - WRITE GOALS TO PRTQUE           
*                                                                               
*                                  FLIGHT START-                                
GOAL9    CLI   BLDWKTAB,C'Y'       TEST WEEKS TABLE SET YET                     
         BNE   *+12                                                             
         BAS   RE,GETWKS           NO-GET THE WEEKS NOW                         
         MVI   BLDWKTAB,C'N'                                                    
         LA    R7,WEEKTAB                                                       
*                                                                               
GOAL10   CLI   0(R7),X'FF'         FIND THE WEEK                                
         BE    GOAL14              NO - FUCK IT                                 
         CLC   GLWEEK,0(R7)                                                     
         BL    *+14                                                             
         CLC   GLWEEK,2(R7)                                                     
         BNH   GOAL12                                                           
         LA    R7,16(R7)                                                        
         B     GOAL10                                                           
*                                                                               
GOAL12   MVC   STDATE,4(R7)                                                     
         MVC   ENDATE,10(R7)                                                    
         ST    R7,AWEEK            SAVE A(START WEEK POSITION)                  
         ST    R5,COST             SAVE THE GOALS                               
         ST    R6,GRPS                                                          
         ZAP   WKCNT,=P'1'                                                      
*                                                                               
GOAL14   ZIC   R0,1(R3)            GET NEXT ELEMENT                             
         AR    R3,R0                                                            
         B     GOAL3                                                            
*                                                                               
GOAL16   BAS   RE,WRTGOAL                                                       
*                                                                               
GOALX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET ESTIMATE WEEK                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETWKS   NTR1  ,                                                                
         LA    R4,KEY              GET THE ESTIMATE RECORD                      
         XC    KEY,KEY                                                          
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,SBPRD                                                    
         MVC   EKEYEST,SBBEST                                                   
         GOTO1 HIGH                                                             
         CLC   EKEY,KEYSAVE                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,SBAIO2                                                        
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R2,SBAIO3           GET ESTIMATE'S WEEKS                         
         XC    WORK,WORK                                                        
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         CLI   EOWSDAY,0           TEST OUT OF WEEK ROTATOR                     
         BE    *+10                                                             
         MVC   SBSPPROF+8(1),EOWSDAY                                            
         LA    R3,4                GET WEEKS                                    
         CLI   EDAILY,C'Y'         TEST DAILY ESTIMATE                          
         BNE   *+8                                                              
         LA    R3,9                YES-GET DAYS                                 
* NO FUCKING EQUATES IN GOTO1 FOR US !                                          
         GOTO1 MOBILE,DMCB,(63,ESTART),((R3),(R2)),WORK,SBSPPROF                
         CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),MAXWKS                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   SBSPPROF+8(1),WEEKST                                             
         ZIC   R5,0(R1)            BUILD WEEK TABLE                             
         LA    R3,WEEKTAB          ST(2)/EN(2)/ST(6)/EN(6)                      
*                                                                               
GETWKS2  CLI   0(R2),X'FF'                                                      
         BE    GETWKS4                                                          
         MVC   0(4,R3),0(R2)                                                    
         GOTO1 DATCON,DMCB,(2,0(R2)),4(R3)                                      
         GOTO1 (RF),(R1),(2,2(R2)),10(R3)                                       
         LA    R2,4(R2)                                                         
         LA    R3,16(R3)                                                        
         BCT   R5,GETWKS2                                                       
*                                                                               
GETWKS4  MVI   0(R3),X'FF'                                                      
*                                                                               
GETWKSX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WRITE GOAL RECORDS TO PRINT QUEUE                                   *         
***********************************************************************         
         SPACE 1                                                                
WRTGOAL  ST    RE,SAVERE                                                        
         OC    COST,COST           TEST ANY GOALS                               
         BNZ   *+14                                                             
         OC    GRPS,GRPS                                                        
         BZ    WRTGOALX                                                         
         BAS   RE,GENHDR                                                        
*                                                                               
WRTGOAL2 CLC   GKEYMKT,SVMKT       GENERATE HEADER FOR NEW MARKET               
         BE    *+18                                                             
         MVI   MKTDEL,C'N'                                                      
         BAS   RE,GENRPM                                                        
         MVC   SVMKT,GKEYMKT                                                    
         BAS   RE,GENRPD           GENERATE DETAIL RECORD                       
*                                                                               
WRTGOALX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GENERATE GOALS PLAN MARKET RECORD FOR MARKET WITH ALL GOALS DELETED *         
***********************************************************************         
         SPACE 1                                                                
MISSMKT  NTR1  ,                                                                
         BAS   RE,GENHDR           HEADERS IF NECESSARY                         
         MVI   MKTDEL,C'Y'         GENERATE MARKET HEADER WITH THE              
         SR    RE,RE               DELETE INDICATOR                             
         ICM   RE,3,RMKT                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
         MVC   BUYNAME,RBUYNAME                                                 
         BAS   RE,GENRPM                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GENERATE HEADERS                                                    *         
* EDICT HEADERS ARE GENERATED FIRST TIME, AND GOALS PLAN HEADER       *         
* RECORD FOR NEW ESTIMATE                                             *         
***********************************************************************         
         SPACE 1                                                                
GENHDR   LR    R0,RE                                                            
         CLI   FIRST,C'Y'          GENERATE DDS HEADERS FIRST TIME              
         BNE   *+12                                                             
         BAS   RE,GENEDICT                                                      
         MVI   FIRST,C'N'                                                       
         CLI   NEWEST,C'Y'         GENERATE HEADER FOR NEW ESTIMATE             
         BNE   GENHDRX                                                          
         BAS   RE,GENRPH                                                        
         MVI   NEWEST,C'N'                                                      
         XC    SVMKT,SVMKT                                                      
GENHDRX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GENERATE EDICT HEADER RECORDS                                       *         
***********************************************************************         
         SPACE 1                                                                
GENEDICT NTR1  ,                                                                
         L     R6,ASPOOLD                                                       
         L     R7,ATWA                                                          
         LA    R1,BOXAREA                                                       
         ST    R1,TWAVBOX                                                       
         ST    R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'165'    SET WIDE                                     
         LA    R4,WIDEAREA                                                      
         ST    R4,BOXAWIDE                                                      
         MVC   XSPACES-WIDED(L'XSPACES,R4),BLANKS                               
         LA    R4,XP-WIDED(R4)     R4=A(PRINT LINE)                             
         ST    R4,APRINT                                                        
         LA    R0,4                CLEAR PRINT LINES TO SPACES                  
         LR    R1,R4                                                            
         MVC   0(L'XP,R1),BLANKS                                                
         LA    R1,L'XP(R1)                                                      
         BCT   R0,*-10                                                          
******** OI    SPOOLIND,SPUINIT                                                 
******** XC    SPOOLQLK,SPOOLQLK                                                
******** XC    SPOOLKEY,SPOOLKEY                                                
******** LA    R1,SPOOLKEY                                                      
******** USING PQPLD,R1                                                         
******** MVI   PLCLASS,C'G'        SET CLASS=G FOR EDICT                        
******** DROP  R1                                                               
******** OI    GENSTAT3,NOCLRSPK                                                
*                                                                               
         MVC   4(5,R4),=C'*HDR*'                                                
         MVC   9(6,R4),=C'EDICT='                                               
         MVC   15(5,R4),=C'DMBDE'  COL 16 - DESTINATION ID                      
         MVI   34(R4),C'W'         COL 35 - REPORT IS WIDE                      
         MVC   38(5,R4),=C'DMBDE'  COL 39 - FORMATTED DEST ID                   
         MVC   54(1,R4),SBQMED     COL 55 - BILLING INFO                        
         OC    SVQBCLT,SVQBCLT                                                  
         BZ    *+10                                                             
         MVC   55(3,R4),SBQCLT                                                  
         GOTO1 SPOOL,DMCB,(R6)                                                  
*                                                                               
         MVC   0(165,R4),BLANKS                                                 
         MVC   0(14,R4),=C'++DDS SPGATTRN'                                      
         LA    R4,15(R4)           COL 16 - APPLICATION AREA                    
         USING SPEDICTD,R4                                                      
         MVI   SPGTTYPE,SPGTDATQ   TYPE = DMB&B AS/400 TRANSMISSION             
         MVC   SPGTMED,SBQMED      MEDIA                                        
         OC    SVQBCLT,SVQBCLT                                                  
         BZ    *+10                                                             
         MVC   SPGTCLT,SBQCLT      CLIENT                                       
         CLI   SVQBPRD,0                                                        
         BE    *+10                                                             
         MVC   SPGTPRD,SBQPRD      PRODUCT                                      
         CLC   SBQEST,SBQESTND                                                  
         BNE   GENEDT2                                                          
         ZIC   RE,SBQEST           ESTIMATE                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SPGTEST,DUB                                                      
*                                                                               
GENEDT2  GOTO1 SPOOL,DMCB,(R6)                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GENERATE GOALS PLAN HEADER RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
GENRPH   NTR1  ,                                                                
         L     R6,ASPOOLD                                                       
         L     R4,APRINT                                                        
         MVC   0(165,R4),BLANKS                                                 
         USING RPHRECD,R4                                                       
         MVC   RPHTYP,=C'RPH'                                                   
         MVC   RPHCLT,SBPRDNM                                                   
         MVC   RPHREF,SBESTNM                                                   
         MVC   RPHMED,=C'03'                                                    
         CLI   SBQMED,C'T'                                                      
         BE    *+10                                                             
         MVC   RPHMED,=C'01'                                                    
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,SBQMED                                                  
         GOTO1 DEMOCON,DMCB,(1,SBESTDEM),(11,RPHDEM1),(C'S',DBLOCK)             
*                                                                               
         GOTO1 SPOOL,DMCB,(R6)                                                  
         MVI   LINE,1                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GENERATE GOALS PLAN MARKET RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
GENRPM   NTR1  ,                                                                
         L     R6,ASPOOLD                                                       
         L     R4,APRINT                                                        
         MVC   0(165,R4),BLANKS                                                 
         USING RPMRECD,R4                                                       
         MVC   RPMTYP,=C'RPM'                                                   
         MVC   RPMCLT,SBPRDNM                                                   
         MVC   RPMREF,SBESTNM                                                   
         MVC   RPMMKT,SBMKT                                                     
         MVC   RPMBYR,BUYNAME                                                   
         CLI   MKTDEL,C'Y'         TEST ALL GOALS DELETED                       
         BNE   *+8                                                              
         MVI   RPMDEL,C'D'         YES-TURN ON DELETE INDICATOR                 
         GOTO1 SPOOL,DMCB,(R6)                                                  
         MVI   LINE,1                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GENERATE GOALS PLAN DETAIL RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
GENRPD   NTR1  ,                                                                
         L     R2,SBAIO1                                                        
         USING GOALRECD,R2                                                      
         L     R6,ASPOOLD                                                       
         L     R4,APRINT                                                        
         MVC   0(165,R4),BLANKS                                                 
         USING RPDRECD,R4                                                       
         MVC   RPDTYP,=C'RPD'                                                   
         MVC   RPDCLT,SBPRDNM                                                   
         MVC   RPDREF,SBESTNM                                                   
         MVC   RPDMKT,SBMKT                                                     
         MVC   RPDDPT,GKEYDPT                                                   
         ZIC   RE,GKEYSLN                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RPDSLN,DUB                                                       
         MVC   RPDSTDT,STDATE                                                   
         MVC   RPDENDT,ENDATE                                                   
         L     RE,COST                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RPDCOST,DUB                                                      
         L     RE,GRPS                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RPDGRPS,DUB                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R6)                                                  
         MVI   LINE,1                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS, VARIABLES, STORAGE AREAS, ETC.                           *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
MAXWKS   EQU   53                  MAX N'WEEKS PER ESTIMATE                     
*                                                                               
         DS    0D                                                               
KEYLEN   DS    F                                                                
RECLEN   DS    F                                                                
NRECS    DS    F                                                                
MAXRECS  DS    F                                                                
ABUF     DS    A                   A(BINSRCH BUFFER)                            
LBUF     DC    F'100000'           L'BUFFER                                     
*                                                                               
REC      DS    0CL10               BINSRCH RECORD                               
RKEY     DS    0CL6                                                             
RCLT     DS    XL2                                                              
RPRD     DS    XL1                                                              
REST     DS    XL1                                                              
RMKT     DS    XL2                                                              
RFLAG    DS    XL1                                                              
RBUYNAME DS    CL3                                                              
*                                                                               
REC2     DS    0CL10                                                            
RKEY2    DS    0CL6                                                             
RCLT2    DS    XL2                                                              
RPRD2    DS    XL1                                                              
REST2    DS    XL1                                                              
RMKT2    DS    XL2                                                              
RFLAG2   DS    XL1                                                              
RBUYNAM2 DS    CL3                                                              
*                                                                               
LASTREC  DS    CL(L'REC)                                                        
*                                                                               
OV       DS    CL1                                                              
SKIPMKT  DS    CL1                                                              
MKTDEL   DS    CL1                                                              
LASTKEY  DS    CL13                                                             
*                                                                               
BLANKS   DC    CL256' '                                                         
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        +        
               MACRF=GM,EODAD=RDRECV4                                           
*ECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=U,BLKSIZE=9500,                     
*              MACRF=GM,EODAD=RDRECV4                                           
*                                                                               
*                                                                               
RSPARE   DS    XL4                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 2                                                                
RECKEY   DS    0XL27               IOAREA FOR RECOVERY RECORD                   
         DS    4096X                                                            
*                                                                               
**BUFFER   DS    CL1000                                                         
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
APRINT   DS    A                                                                
AWEEK    DS    A                                                                
COST     DS    F                                                                
GRPS     DS    F                                                                
SAVERE   DS    F                                                                
*                                                                               
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
BUYNAME  DS    CL3                                                              
FIRST    DS    CL1                                                              
NEWEST   DS    CL1                                                              
NEWGOAL  DS    CL1                                                              
BLDWKTAB DS    CL1                                                              
WEEKST   DS    XL1                                                              
SVQBCLT  DS    XL2                                                              
SVQBPRD  DS    XL1                                                              
SVMKT    DS    XL2                                                              
WKCNT    DS    PL2                                                              
ESTTAB   DS    XL256               ESTIMATE TABLES                              
ESTTAB2  DS    XL256                                                            
CLTREC   DS    XL(CLTHDRL)         CLIENT RECORD                                
*                                                                               
WEEKTAB  DS    (MAXWKS)CL16                                                     
         DS    CL1                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*                                                                               
BOXAREA  DS    CL(BOXL)                                                         
WIDEAREA DS    CL(WIDEL)                                                        
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* GOALS PLAN HEADER RECORD DSECT                                      *         
***********************************************************************         
         SPACE 1                                                                
RPHRECD  DSECT                                                                  
RPHREC   DS    0CL165                                                           
RPHTYP   DS    CL3                                                              
RPHCLT   DS    CL8                                                              
RPHREF   DS    CL8                                                              
RPHMED   DS    CL2                                                              
RPHDEM1  DS    CL6                                                              
RPHDEM2  DS    CL6                                                              
RPHDEM3  DS    CL6                                                              
RPHDEM4  DS    CL6                                                              
         SPACE 2                                                                
***********************************************************************         
* GOALS PLAN MARKET RECORD DSECT                                      *         
***********************************************************************         
         SPACE 1                                                                
RPMRECD  DSECT                                                                  
RPMREC   DS    0CL165                                                           
RPMTYP   DS    CL3                                                              
RPMCLT   DS    CL8                                                              
RPMREF   DS    CL8                                                              
         DS    CL12                                                             
RPMMKT   DS    CL4                                                              
RPMBYR   DS    CL3                                                              
         DS    CL126                                                            
RPMDEL   DS    CL1                                                              
         SPACE 2                                                                
***********************************************************************         
* GOALS PLAN DETAIL RECORD DSECT                                      *         
***********************************************************************         
         SPACE 1                                                                
RPDRECD  DSECT                                                                  
RPDREC   DS    0CL165                                                           
RPDTYP   DS    CL3                                                              
RPDCLT   DS    CL8                                                              
RPDREF   DS    CL8                                                              
RPDMKT   DS    CL4                                                              
RPDDPT   DS    CL1                                                              
RPDSLN   DS    CL3                                                              
RPDSTDT  DS    CL6                                                              
RPDENDT  DS    CL6                                                              
RPDCOST  DS    CL8                                                              
RPDGRPS  DS    CL9                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBIGBOX                                                       
BOXL     EQU     *-BOXD                                                         
       ++INCLUDE DDWIDED                                                        
WIDEL    EQU     *-WIDED                                                        
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF4D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE SPWRIWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPWRI24   05/01/02'                                      
         END                                                                    
