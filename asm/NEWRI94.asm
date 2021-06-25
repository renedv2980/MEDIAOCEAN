*          DATA SET NEWRI94    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T32094B,+0                                                               
         TITLE 'T32094 - MODULE TO READ HISTORY RECORDS'                        
*******************************************************************             
*          ***    CALLED FROM NEWRI20 FOR NET WRITER  ***                       
*                                                                               
*                                                                               
*       READS HISTORY RECORDS AND PASSES TO DRIVER                              
*                                                                               
* - NOTE R6 RESERVED FOR GLOBALD                                                
**********************************************************************          
T32094   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKLEN,T32094**,CLEAR=YES                                        
         USING T32094,RB,RA,R8                                                  
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
         LR    R7,RC                                                            
         USING WRKAREA,R7                                                       
         L     RC,0(R1)            *CALLING PROGRAMS RC                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,NDGLOBAL                                                      
         USING GLOBALD,R6                                                       
         LA    RF,HOOK             SET IN NEWRI20 AND RESET HERE SINCE          
         ST    RF,GLAHOOK          DRIVER IS CALLED FROM BOTH MODULES           
         B     RDH00                                                            
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
RDH00    DS    0H                                                               
         L     R1,ANETWS1          SAVE CLIENT REC OF CALLING PROGRAM           
         MVC   CLTRECSV,0(R1)      ( RESTORE AT EXIT )                          
         MVC   ESTMSK,NBESTMSK     SAVE EST MASK                                
         MVC   PRDMSK,NBPRDMSK     SAVE PROD MSK                                
         MVC   AIOSV,AIO                                                        
         LA    R1,MYIO             USE MYIO                                     
         ST    R1,AIO                                                           
                                                                                
* - SET UP KEY FOR HISTORY RECORD READ                                          
RDH001   LA    R2,1                                                             
         LA    R4,KEY                                                           
         USING NHKKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'40'                                                        
         MVC   NHKPAM,NBACTAM                                                   
         CLC   NBSELCLI,=C'ALL'           CLIENT                                
         BE    RDH12                                                            
         MVC   NHKPCLT,NBSELCLI                                                 
         LA    R2,1(R2)                                                         
         CLI   NBSELNET,0                 NETWORK                               
         BE    RDH12                                                            
         MVC   NHKNET,NBSELNET                                                  
         LA    R2,3(R2)                                                         
RDH12    DS    0H                                                               
         STC   R2,COMPLEN          SAVE KEY LENGTH COMPARE                      
*                                                                               
RDH16    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RDH25                                                            
*                                                                               
RDH20    DS    0H                                                               
         LA    R4,KEY              RESET R4                                     
         GOTO1 SEQ                                                              
*                                                                               
RDH25    ZIC   R2,COMPLEN          SET LENGTH OF KEY COMPARE                    
         EXCLC R2,KEY,KEYSAVE                                                   
         BNE   RDH25B              EOF                                          
         BAS   RE,FILTERS          CHECK AGAINST FILTERS                        
         BNE   RDH20               NOT OK-GET NEXT KEY                          
         B     RDH25C              CONTINUE                                     
                                                                                
RDH25B   BAS   RE,RESTCLT          EOF-RESTORE ORIGINAL CLT REC                 
         MVC   AIO,AIOSV               RESET AIO                                
         B     EXIT                    AND EXIT                                 
                                                                                
* - CLIENT GROUP FILTERS                                                        
RDH25C   CLI   NBSELCGR,0                                                       
         BE    *+12                                                             
         BAS   RE,CGRPFILT                                                      
         BNE   RDH20                                                            
                                                                                
* - CLIENT OFFICE FILTERS                                                       
*->      CLI   NBSELOFF,0                                                       
*->      BE    *+12                                                             
* HAVE PROGRAM GO THROUGH COFFFILT EVEN IF NBSELOFF=0 - WILL SET                
* VARIOUS VALUES IN CASE CLIENT HAS CHANGED                                     
         BAS   RE,COFFFILT                                                      
         BNE   RDH20                                                            
                                                                                
         GOTO1 GETREC                                                           
                                                                                
* - FILL IN NETBLOCK TO FUDGE CLI/PROD/EST/ ETC ETC ETC                         
                                                                                
         L     R4,AIO              POINT R4 TO RECORD                           
*                                                                               
         BAS   RE,GETPRD           GETS 1 AND 3 BYTE PROD CODES                 
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    RDH40                                                            
         CLC   NBSELPRD,=C'POL'                                                 
         BE    RDH40                                                            
         BAS   RE,CHKPRD                                                        
         BE    RDH20               NO MATCH/GET NEXT RECORD                     
                                                                                
* - PRODUCT GROUP FILTERS                                                       
RDH40    CLI   NBSELPGR,0                                                       
         BE    RDH45                                                            
         BAS   RE,CHKPRD                                                        
         BE    RDH20                                                            
                                                                                
                                                                                
RDH45    MVC   NBACTAM(3),NHKPAM          AM/CLI                                
         MVC   NBCLICOD,CLTSAV            CLIENT                                
         MVC   NBACTNET,NHKNET            NETWORK                               
         MVC   NBACTPRG,NHKPROG           PROGRAM                               
         MVC   NBACTDAT,NHKDATE           AIR DATE                              
         MVC   NBACTEST,NHKEST            EST                                   
         MVC   NBACTSUB,NHKSUB            SUBLINE                               
         MVC   NBACTDP,NHKDP              DAYPART                               
*                                                                               
* PASS TO DRIVER                                                                
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         B     RDH20                GET NEXT RECORD                             
*                                                                               
         MVC   AIO,AIOSV          RESET UNT FILE/RD SEQ/AIO AREA                
         MVC   KEY,NBKEY           ..REREAD UNIT KEY                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
GBXX     B     EXIT                                                             
*                                                                               
* CALLED FROM BILLING RECORD READ TO RESTORE CLIENT RECORD                      
* TO ANETWS1                                                                    
RESTCLT  NTR1                                                                   
         L     R1,ANETWS1                                                       
         CLC   CLTRECSV,0(R1)                                                   
         BE    RCLTX                                                            
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEY,CLTRECSV                                                     
         GOTO1 HIGH                                                             
         MVC   AIO,ANETWS1                                                      
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
                                                                                
         XC    FILENAME,FILENAME  RESTORE UNIT FILE                             
         MVC   KEY,NBKEY                                                        
         GOTO1 HIGH                RESTORE SEQUENCE                             
RCLTX    B     EXIT                                                             
         EJECT                                                                  
* - EXPECTS HISTORY KEY IN KEY                                                  
CHKCLT   NTR1                                                                   
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       DO WE NEED NEW CLIENT HEADER                 
         BE    CHKCLTX                                                          
         MVC   SVKEY,KEY           YES/SAVE KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY+1        AGY/MED + CLIENT                         
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1             CLIENT REC SITS IN ANETWS1               
         GOTO1 GETREC                                                           
         LA    R1,MYIO                 RESET AIO AREA                           
         ST    R1,AIO                                                           
         MVC   KEY,SVKEY               RESET KEY                                
         XC    FILENAME,FILENAME                                                
         GOTO1 HIGH                                                             
CHKCLTX  B     EXIT                                                             
         EJECT                                                                  
* - EXPECTS R2 TO POINT TO EST/PRDMSK                                           
TESTMASK NTR1                                                                   
*        LA    R2,NBESTMSK                                                      
*        LA    R2,ESTMSK                                                        
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         B     EXIT                                                             
***      BE    NO                                                               
***      B     YES                                                              
                                                                                
SETMASK  NTR1                                                                   
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0               R1 HAS NUMBER                                
         SLDL  R0,29               GET BYTE NO. INTO R0                         
         SRL   R1,29                   BIT  NO. INTO R1                         
         AR    R2,R0               R2 HAS (MASK)                                
         LA    R1,BITLIST(R1)                                                   
         OC    0(1,R2),0(R1)                                                    
         B     EXIT                                                             
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
BITTEST  DS    CL1                                                              
HOLDEFLT DS    CL1                                                              
FFS      DC    32X'FF'                                                          
ESTMSK   DS    CL32                                                             
PRDMSK   DS    CL32                                                             
                                                                                
                                                                                
         EJECT                                                                  
* - CLIENT GROUP FILTERING FOR HISTORY RECORDS                                  
*   KEY HAS HISTORY RECORD                                                      
CGRPFILT NTR1                                                                   
         MVC   MYKEY,KEY           SAVE CURRENT HIST REC KEY                    
         MVI   BYTE,0              CLEAR NEW CLIENT FLAG                        
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       ...DO WE NEED NEW CLIENT HEADER              
         BE    VCL2                                                             
         MVI   BYTE,C'Y'                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MYKEY+1     AGY/MED + CLIENT                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1         CLIENT REC SITS IN ANETWS1                   
         GOTO1 GETREC                                                           
         LA    R1,MYIO         RESET AIO AREA FOR HIST REC READ                 
         ST    R1,AIO                                                           
VCL1     XC    KEY,KEY            RESET SEQ READ FOR HIST RECS                  
         MVC   KEY,MYKEY                                                        
         XC    FILENAME,FILENAME                                                
         GOTO1 HIGH                                                             
* - CHECK CLIENT GROUP FILTERING                                                
VCL2     LA    R0,5                                                             
         LA    RF,CGRP1                                                         
         CLC   NBSELCGR(1),0(RF)       CHECK SCHEME LETTER                      
         BE    VCL5                                                             
VCL4     LA    RF,3(RF)                                                         
         BCT   R0,*-14                                                          
         B     VCLNO                                                            
VCL5     UNPK  DUB(5),1(3,RF)      UNPK PWOS                                    
         LA    R3,DUB                                                           
         LA    RE,NBSELCGR+1                                                    
         LA    R1,4                                                             
VCL6     CLI   0(RE),X'C1'         IF LETTER OR NUMBER                          
         BL    VCL7                                                             
         CLC   0(1,RE),0(R3)       MUST MATCH                                   
         BNE   VCL4                IF NO MATCH,TEST AGAINST NXT CGRP            
VCL7     LA    RE,1(RE)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,VCL6                                                          
         MVC   NBACTCGR(2),1(RF)   SET CLIENT GROUP CODE  PWOS                  
*                                                                               
         CLI   BYTE,C'Y'           IS IT NEW CLIENT                             
         BNE   VCLYES                                                           
         BAS   RE,SETPRD           YES/SET PRODUCT                              
         BAS   RE,SETEST           YES/SET ESTIMATES                            
         XC    KEY,KEY            RESET SEQ READ FOR HISTORY RECS               
         MVC   KEY,MYKEY                                                        
         XC    FILENAME,FILENAME                                                
         GOTO1 HIGH                                                             
         LA    R1,MYIO                                                          
         ST    R1,AIO             AND RESET I/O AREA FOR HIST REC               
*                                                                               
VCLYES   SR    RE,RE               CLIENT PASSED TESTS                          
VCLNO    LTR   RE,RE                                                            
VCLX     B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* - CLIENT OFFICE FILTERING FOR HIST RECORDS                                    
*   KEY HAS HISTORY RECORD                                                      
COFFFILT NTR1                                                                   
         MVC   MYKEY,KEY           SAVE CURRENT HIST REC KEY                    
         MVI   BYTE,0              CLEAR NEW CLIENT FLAG                        
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       ...DO WE NEED NEW CLIENT HEADER              
         BE    COF2                                                             
         MVI   BYTE,C'Y'                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MYKEY+1     AGY/MED + CLIENT                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         LA    R1,MYIO2                                                         
         MVC   AIO,ANETWS1            CLIENT REC SITS IN ANETWS1                
         GOTO1 GETREC                                                           
         LA    R1,MYIO                RESET AIO AREA FOR HISTORY REC            
         ST    R1,AIO                                                           
         XC    KEY,KEY                RESET SEQ READ FOR HIST REC               
         MVC   KEY,MYKEY                                                        
         XC    FILENAME,FILENAME                                                
         GOTO1 HIGH                                                             
* - CHECK CLIENT OFFICE FILTERING                                               
COF2     DS    0H                                                               
         CLI   NBSELOFF,0          IF OFFICE FILTERING                          
         BE    COFOK                                                            
         TM    NBOFFTYP,X'80'      CHECK OFFICE LIST                            
         BO    OFFLIST                                                          
         TM    NBOFFTYP,X'40'      CHECK NEGATIVE FILTER                        
         BO    NEGOFF                                                           
         CLC   COFFICE,NBSELOFF                                                 
         BNE   OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
OFFLIST  XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 NBCALLOV,DMCB                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB(8),DUB                                                       
         MVI   OFCSYS,C'N'                                                      
         MVI   OFCAUTH,C'$'                                                     
         MVC   OFCAUTH+1(1),NBSELOFF                                            
         OI    OFCAUTH+1,X'C0'                                                  
         MVC   OFCAGY,NBEFFAGY                                                  
*        MVC   OFCOFC,NBEFFOFF                                                  
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
         SPACE 1                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,NBACOM                                             
         TM    NBOFFTYP,X'40'      MAY BE NEGATIVE LIST                         
         BO    NEGLIST                                                          
         CLI   0(R1),0             IS OFFICE IN LIST                            
         BNE   OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
NEGLIST  CLI   0(R1),0             CHECK FOR NEGATIVE LIST                      
         BE    OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
NEGOFF   CLC   NBEFFOFF,NBSELOFF                                                
         BE    OFFNO                                                            
*                                                                               
COFOK    CLI   BYTE,C'Y'           IS IT NEW CLIENT                             
         BNE   OFFYES                                                           
         MVC   NBEFFOFF,COFFICE                                                 
         BAS   RE,SETPRD           YES/SET PRODUCT                              
         BAS   RE,SETEST           YES/SET ESTIMATES                            
         XC    KEY,KEY            RESET SEQ READ FOR HISTORY RECS               
         MVC   KEY,MYKEY                                                        
         XC    FILENAME,FILENAME                                                
         GOTO1 HIGH                                                             
         LA    R1,MYIO                                                          
         ST    R1,AIO              AND RESET I/O AREA FOR HIST REC              
*                                                                               
OFFYES   SR    RE,RE               CLIENT PASSED TESTS                          
OFFNO    LTR   RE,RE                                                            
OFFX     B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* - CALLED WHEN CLIENT CHANGES IN READ HISTORY REC ROUTINE                      
* - RESETS ESTIMATE MASK FOR NEW CLIENT                                         
*          DATA SET NENETIO    AT LEVEL 048 AS OF 04/06/94                      
SETEST   NTR1                                                                   
         MVC   AIO,NBAIO           READ ESTIMATE REC INTO NABAIO                
         MVI   NBEFFEST,0          PRESET EFFECTIVE NUMBER                      
         XC    KEY,KEY             SET UP GENERAL KEY                           
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
         L     R1,ANETWS1          GET CLIENT RECORD                            
         MVC   EKEYAM(3),1(R1)     SET A/M AND CLIENT IN KEY                    
         MVC   EKEYPRD,NBSELPRD                                                 
         CLC   EKEYPRD,=C'ALL'     IF NBSELPRD=ALL USE POL IN KEY               
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NBSELEST                                                 
         CLI   NBSELESE,0          IF EST RANGE, GET FIRST                      
         BNE   QVEST2                                                           
         CLI   NBSELEST,0          IF ONE ESTIMATE SELECTED GOTO READ           
         BNE   GETEST                                                           
         MVI   EKEYEST,1           IF ALL, GET FIRST EST                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLI   EKEYEST+1,0         MAKE SURE THIS IS EST                        
         BNE   BADEST                                                           
         B     PROCEST                                                          
         SPACE 1                                                                
QVEST2   DS    0H                  READ FIRST IF ALL OR RANGE                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BE    PROCEST                                                          
         B     BADEST                                                           
         SPACE 1                                                                
GETEST   DS    0H                                                               
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         BE    PROCEST                                                          
         B     BADEST                                                           
         SPACE 1                                                                
PROCEST  MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         MVC   ESTMSK,FFS        SET UP ESTIMATE MASK                           
         CLI   NBSELEST,0          FOR NO/ALL USE FF'S                          
         BE    ESTFILT                                                          
         SPACE 1                                                                
         XC    ESTMSK,ESTMSK                                                    
         ZIC   R1,NBSELEST                                                      
         LA    R2,ESTMSK                                                        
         BAS   RE,SETMASK          FOR SINGLE ESTIMATE                          
         STC   R1,NBEFFEST                                                      
         ZIC   R3,NBSELESE                                                      
         LTR   R3,R3                                                            
         BZ    ESTFILT                                                          
         SPACE 1                                                                
QI38     LA    R1,1(R1)            SET MASK TO RANGE NBSELEST TO NBSELE         
         CR    R1,R3                                                            
         BH    ESTFILT                                                          
         BAS   RE,SETMASK          FOR ESTIMATE RANGE                           
         B     QI38                                                             
         SPACE 1                                                                
ESTFILT  L     R4,NBAIO                                                         
         OC    NBSELEFL,NBSELEFL   PROCESS FILTERS                              
         BZ    ESTOK                                                            
         SPACE 1                                                                
         XC    ESTMSK,ESTMSK   REDO ESTMSK                                      
         LA    R4,KEY                                                           
         MVI   EKEYEST,0                                                        
         SPACE 1                                                                
QI42     LA    R4,KEY                                                           
         CLI   EKEYEST,255                                                      
         BE    ESTOK               LOOP EXIT                                    
         AI    EKEYEST,1           SKIP TO NEXT ESTIMATE                        
         XC    EKEYEST+1(5),EKEYEST+1                                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ESTOK               LOOP EXIT                                    
         CLI   NBSELESE,0          MAKE SURE ITS WITHIN A RANGE                 
         BE    QI44                                                             
         CLC   EKEYEST,NBSELEST                                                 
         BL    QI42                                                             
         CLC   EKEYEST,NBSELESE                                                 
         BH    QI42                                                             
         SPACE 1                                                                
QI44     L     R4,NBAIO            MEETS FILTER                                 
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         LA    R3,NBSELEFL                                                      
         LA    R5,EPROF                                                         
         LA    R0,3                                                             
         SPACE 1                                                                
QI46     CLI   0(R3),C'*'          WILD                                         
         BE    QI48                                                             
         CLI   0(R3),0                                                          
         BE    QI48                                                             
         TM    0(R3),X'40'                                                      
         BZ    QI47                                                             
         CLC   0(1,R3),0(R5)       FILTER                                       
         BNE   QI42                                                             
         B     QI48                                                             
         SPACE 1                                                                
QI47     MVC   HOLDEFLT,0(R5)                                                   
         NI    HOLDEFLT,X'FF'-X'40'   TURN OFF X'40' BIT                        
         CLC   0(1,R3),HOLDEFLT    NEGATIVE FILTER                              
         BE    QI42                                                             
         SPACE 1                                                                
QI48     LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,QI46                                                          
         ZIC   R1,EKEYEST          PASSED FILTERS                               
         LA    R2,ESTMSK                                                        
         BAS   RE,SETMASK          SO SET MASK                                  
         CLI   NBEFFEST,0          SAVE NUMBER OF FIRST GOOD EST                
         BNE   QI42                                                             
         STC   R1,NBEFFEST                                                      
         B     QI42                                                             
         SPACE 1                                                                
BADEST   DC    H'0'                SHOULD NEVER GET HERE                        
         SPACE 1                                                                
ESTOK    B     EXIT                                                             
         EJECT                                                                  
* - FILL IN PRODMSK FOR PRODUCT GROUP FILTER                                    
*          DATA SET NENETIO    AT LEVEL 054 AS OF 09/27/94                      
SETPRD   NTR1                                                                   
         MVC   AIO,NBAIO                                                        
         LA    R4,KEY              NO PRODUCT NUMBER GIVEN                      
         XC    KEY,KEY                                                          
         USING PRDHDR,R4                                                        
         L     R1,ANETWS1          CLIENT RECORD                                
         MVC   PKEYAM(3),1(R1)        SET A/M AND CLIENT IN KEY                 
         CLC   =C'ALL',NBSELPRD                                                 
         BE    SETPGRP                                                          
*8       MVC   PKEYPRD,NBSELPRD        I DON'T NEED PRODUCT RECORD              
**       MVC   FILENAME,=C'SPTDIR  '                                            
**       GOTO1 HIGH                                                             
**       CLC   KEY(7),KEYSAVE                                                   
**       BE    *+6                                                              
**       DC    H'0'                                                             
**       MVC   FILENAME,=C'SPTFILE '                                            
**       GOTO1 GETREC                                                           
         B     SETPX                                                            
*                                                                               
SETPGRP  DS    0H                                                               
         CLI   NBSELPGR,0          IF NO PRODUCT GROUP                          
         BNE   QI21                                                             
         MVC   NBPRDMSK,FFS        SET PRDMSK AND EXIT                          
         B     EXIT                                                             
         SPACE 1                                                                
QI21     LA    R4,KEY                                                           
         XC    PKEYPRD,PKEYPRD                                                  
         SPACE 1                                                                
QI22     LA    R4,KEY              GET NEXT PRODUCT                             
         AI    PKEYPRD+2,1                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    QI23                                                             
         MVC   KEY,KEYSAVE         NEED TO RESORE POL                           
         CLC   NBSELPRD,=C'POL'                                                 
         BNE   SETPX                                                            
         MVC   PKEYPRD,=C'POL'                                                  
         GOTO1 HIGH                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         B     SETPX                                                            
         SPACE 1                                                                
QI23     MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         LA    R2,PGRP1            SELECT ASSIGNMENT (V,W,X)                    
         CLC   NBSELPGR(1),PGRP1                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23A                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP1                                                       
         BE    QI22                                                             
QI23A    LA    R2,PGRP2                                                         
         CLC   NBSELPGR(1),PGRP2                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23B                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP2                                                       
         BE    QI22                                                             
QI23B    LA    R2,PGRP3                                                         
         CLC   NBSELPGR(1),PGRP3                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23C                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP3                                                       
         BE    QI22                                                             
QI23C    LA    R2,PGRP4                                                         
         CLC   NBSELPGR(1),PGRP4                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23D                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP4                                                       
         BE    QI22                                                             
QI23D    LA    R2,PGRP5                                                         
         CLC   NBSELPGR(1),PGRP5                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BO    QI22                NO / GET NEXT REC                            
         MVC   BYTE,NBSELPGR       YES/CHECK IT                                 
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP5                                                       
         BE    QI22                                                             
         SPACE 1                                                                
QI24     UNPK  DUB(5),1(3,R2)      SEE IF THIS PRODUCT IS IN                    
         LA    RF,DUB                                                           
         LA    RE,NBSELPGR+1                                                    
         LA    R0,4                                                             
         SPACE 1                                                                
QI25     CLI   0(RE),C'*'          WILD CARD OK                                 
         BE    QI26                                                             
         CLI   0(RE),X'40'         BLANKS/ZERO OK                               
         BNH   QI26                                                             
         CLC   0(1,RE),0(RF)       ELSE IT MUST MATCH                           
         BE    QI26                                                             
         TM    0(RE),X'40'         IS IT NEGATIVE FILTER                        
         BO    QI22                NO                                           
         MVC   BYTE,0(RE)          YES/CHECK IT                                 
         OI    BYTE,X'40'                                                       
         CLC   BYTE,0(RF)                                                       
         BE    QI22                                                             
         SPACE 1                                                                
QI26     LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,QI25                                                          
         SPACE 1                                                                
         LH    R1,PCODE            PASSED THE TESTS SO...                       
         LA    R2,NBPRDMSK                                                      
         BAS   RE,SETMASK          SET THIS PRODUCT AS OK                       
         B     QI22                                                             
SETPX    B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
*********************************************                                   
* - FILTER HISTORY RECORD                                                       
FILTERS  NTR1                                                                   
         LA    R4,KEY                                                           
         USING NHKKEY,R4                                                        
                                                                                
         CLI   NBSELEST,0                                                       
         BE    FL8                                                              
         CLI   NBSELESE,0          IS IT ESTIMATE RANGE                         
         BE    FL7                 NO                                           
         CLC   NHKEST,NBSELEST    YES                                           
         BL    FLNO                                                             
         CLC   NHKEST,NBSELESE                                                  
         BH    FLNO                                                             
         B                                                                      
FL7      CLC   NHKEST,NBSELEST    NO RANGE/MUST MATCH ESTIMATE                  
         BNE   FLX                                                              
                                                                                
FL8      CLI   NBSELDP,0          DAYPART                                       
         BE    FL10                                                             
         CLC   NHKDP,NBSELDP                                                    
         BL    FBNO                                                             
                                                                                
FL10     CLI   NBCMPSTR,0         DATE FILTER?                                  
         BE    FLYES                                                            
         CLC   NHKDATE,NBCMPSTR                                                 
         BL    FLNO                                                             
         CLC   NHKDATE,NBCMPEND                                                 
         BH    FLNO                                                             
FLYES    SR    R4,R4                                                            
*                                                                               
FLNO     LTR   R4,R4                                                            
*                                                                               
FLX      B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
*********************************************************                       
GETPRD3  NTR1                  RETURNS 3 BYTE PROD IN WORK                      
         L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GP5      CLC   WORK(1),3(R1)                                                    
         BE    GOTPRD3                                                          
         LA    R1,4(R1)                                                         
         BCT   R2,GP5                                                           
         MVC   WORK(3),=C'***'                                                  
         B     *+10                                                             
GOTPRD3  MVC   WORK(3),0(R1)                                                    
         B     EXIT                                                             
                                                                                
********************************************************                        
GETPRD1  NTR1                  USES WORK(3) AND RETURNS                         
         L     R1,ANETWS1      1 BYTE PROD IN WORK                              
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GTP5     CLC   WORK(3),0(R1)                                                    
         BE    GOTPRD                                                           
         LA    R1,4(R1)                                                         
         BCT   R2,GTP5                                                          
         MVI   WORK,0                                                           
         B     *+10                                                             
GOTPRD   MVC   WORK(1),3(R1)                                                    
         B     EXIT                                                             
         DROP  R1,R3                                                            
                                                                                
**************************************************************                  
GETPRD   NTR1                  RETURNS PRODUCT FROM HISTORY REC                 
         XC    ACTVPRD,ACTVPRD     CLEAR PROD AREAS                             
         XC    ACTVPRD3,ACTVPRD3                                                
         LA    R4,MYIO                                                          
         USING NHKKEY,R4                                                        
         MVC   ACTVPRD,NNHOPRD     SET PRODS FROM 01 ELEM                       
         MVI   ELCODE,5            LOOK FOR CHANGE ELEMS                        
         BAS   RE,GETEL                                                         
         BNE   GTP20                                                            
         USING NCHAEL,R4                                                        
GTP10    CLI   NCHGFCOD,C'B'       BRAND?                                       
         BNE   *+10                                                             
         MVC   ACTVPRD,NCHGFLD     YES/SET PRODUCTS                             
         BAS   RE,NEXTEL                                                        
         BE    GTP10               MORE ELEMS                                   
         DS    0H                  NO MORE ELEMS                                
                                                                                
GTP20    LA    R1,ACTVPRD          GET 3 BYTE PRD CODES FOR PRODUCTS            
         LA    R2,6                MAX PRODS                                    
         LA    R3,ACTVPRD3                                                      
GTP22    CLI   0(R1),0                                                          
         BE    GTPX                                                             
         MVC   WORK(1),0(R1)                                                    
         BAS   RE,GETPRD3                                                       
         MVC   0(3,R3),WORK                                                     
         LA    R3,3(R3)                                                         
         LA    R1,1(R1)                                                         
         BCT   R2,GTP22                                                         
GTPX     B     EXIT                THAT'S ALL                                   
         DROP  R4                                                               
                                                                                
*******************************************************                         
CHKPRD   NTR1                  CHECK PRODS AGAINST MASK                         
         LA    R3,ACTVPRD                                                       
         LA    R4,6                MAX PRODS                                    
         ZIC   R1,0(R3)                                                         
         LA    R2,PRDMSK                                                        
TSTPRD   BAS   RE,TESTMASK                                                      
         BNE   PRDOK                                                            
         LA    R3,1(R3)                                                         
         CLI   0(R3),0                                                          
         BE    PRDX                REJECT / NO MORE PRODS                       
         BCT   R4,TSTPRD                                                        
         LTR   R4,R4               REJECT / NO MATCHES                          
PRDOK    DS    0H                                                               
PRDX     B     EXIT                                                             
                                                                                
         EJECT                                                                  
*          DATA SET NEWRI20    AT LEVEL 022 AS OF 03/17/95                      
HOOK     NTR1                                                                   
         CLI   GLHOOK,GLHEAD                                                    
         BNE   HOOK2                                                            
         GOTO1 NDGENHED                                                         
         B     EXIT                                                             
         SPACE 1                                                                
HOOK2    CLI   GLHOOK,GLPRINT                                                   
         BNE   EXIT                                                             
         TM    NDLININD,X'80'                                                   
         BNO   *+8                                                              
         MVI   GLHOOK,GLDONT                                                    
         MVI   NDLININD,0          RESET LINE INDICATORS                        
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
MYIO     DS    CL3000                                                           
MYIO2    DS    CL3000                                                           
         EJECT                                                                  
*                                                                               
                                                                                
* - WORK AREA FOR RDBELEM                                                       
WRKAREA  DSECT                                                                  
MYDM     DS    CL96                                                             
ADRIVHK  DS    F                  R5 SAVED FOR DRIVER IN CALLING MODULE         
AIOSV    DS    F                   ADDRESS PASSED BY CALLING MODULE             
         DS    0D                                                               
MYDUB    DS    CL8                                                              
CLTSAV   DS    CL3                                                              
ACTVPRD  DS    CL6                LATEST PRODS (6) ON HISTORY REC               
ACTVPRD3 DS    CL18               3 BYTE PROD CODE FOR HIST REC (3X6)           
                                                                                
MYKEY    DS    CL13                                                             
SVKEY    DS    CL20                                                             
CLTRECSV DS    CL13                                                             
COMPLEN  DS    CL1                                                              
*                                                                               
MYWORK   DS    CL100                                                            
MYWORK2  DS    CL100                                                            
WRKLEN   EQU   *-WRKAREA                                                        
*                                                                               
*                                                                               
* NEGENINCLS                                                                    
* NEDATELSTD                                                                    
* DRGLOBAL                                                                      
* SPGENPRD                                                                      
* NEGENCOM                                                                      
* NECOMBLOK                                                                     
* SPGENBILL                                                                     
* SPBVALD                                                                       
* BHBLOCKD                                                                      
* NEGBLOCKD                                                                     
* NEGENUNIT                                                                     
* NEGENPACK                                                                     
* SPGENCLT                                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENHIST                                                      
       ++INCLUDE NEGENCOM                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE0D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NEWRI94   05/01/02'                                      
         END                                                                    
