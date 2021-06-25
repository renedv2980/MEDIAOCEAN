*          DATA SET SPREPCE02  AT LEVEL 011 AS OF 03/31/04                      
*PHASE SPCE02A                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SPCE02  SEARCH RECOVERY FOR COKEAT FILE TRANSACTIONS'           
SPCE02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPCE02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPCE02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    CE10                                                             
         B     EXIT                                                             
                                                                                
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
                                                                                
CE10     DS    0H                                                               
         MVC   LOAM+1(1),BAGYMD    LOAM = 00B1                                  
         MVC   HIAM+1(1),BAGYMD    HIAM = 00B4                                  
         NI    HIAM+1,X'FF'-X'0F'                                               
         OI    HIAM+1,X'04'                                                     
         MVC   COPYCLT1(4),=C'NNNN'                                             
                                                                                
         OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
CE20     GET   RECVIN,RCVREC-4                                                  
         LA    RE,RCVREC-4                                                      
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)                                                    
                                                                                
         LA    R5,RKEY                                                          
         USING ESTHDR,R5                                                        
         CLC   EKEY(2),LOAM        MAKE SURE FOR COKEAT                         
         BL    CE20                                                             
         CLC   EKEY(2),HIAM                                                     
         BH    CE20                                                             
                                                                                
         OC    EKEYPRD(9),EKEYPRD  CHECK IF CLIENT RECORD                       
         BNZ   CE30                                                             
         CLI   RRECTY,3            IS THIS AN ADD?                              
         BNE   CE20                                                             
         B     CE40                                                             
                                                                                
CE30     OC    EKEYEST+1(5),EKEYEST+1  CHECK IF PRODUCT OR ESTIMATE             
         BNZ   CE20                                                             
         CLC   EKEYCLT,=X'885F'    CLIENT CC                                    
         BNE   CE20                                                             
                                                                                
CE40     LA    R0,TABLEMAX                                                      
         L     R2,NUMENTRY                                                      
         GOTO1 =V(BINSRCH),DMCB,(1,EKEYAM),TABLE,(R2),L'TABLE,         +        
               (0,L'TABLE),(R0)                                                 
         OC    DMCB,DMCB                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   NUMENTRY,DMCB+8     UPDATE NUMBER OF ENTRIES                     
         B     CE20                                                             
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
CE100    DS    0H                                                               
         CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R7,P                                                             
         USING PRNTLND,R7                                                       
         BAS   RE,PRNTAB                                                        
         L     R3,NUMENTRY                                                      
         LA    R4,TABLE                                                         
         USING TABLED,R4                                                        
                                                                                
CE110    CLI   0(R4),0                                                          
         BE    CE200                                                            
         CLI   0(R4),X'FF'                                                      
         BE    CE200                                                            
                                                                                
         MVC   BYTE,TABAM                                                       
         NI    BYTE,X'FF'-X'F0'                                                 
         ZICM  RE,BYTE,1                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,COPYCLT1-1                                                    
         AR    R1,RE                                                            
         MVC   COPYCLT,0(R1)                                                    
         ST    R1,ACOPYCLT                                                      
                                                                                
         OC    TABPRD(4),TABPRD    IS THIS FOR CLIENT HEADER?                   
         BZ    CPYALL                                                           
                                                                                
         CLI   TABEST,0            IS THIS FOR PRODUCT HEADER?                  
         BE    CPYPRD                                                           
         B     CPYEST                                                           
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  COPY ALL PRODUCTS & ESTIMATES TO THE NEW CLIENT                    *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CPYALL   DS    0H                                                               
                                                                                
         LA    R5,KEY                                                           
         USING ESTHDR,R5                                                        
                                                                                
         CLI   COPYCLT,C'Y'        COPIED PRODUCT LISTS ALREADY?                
         BE    CPYALL20                                                         
                                                                                
         XC    KEY,KEY             GET CLIENT COPYING FROM                      
         MVC   EKEYAM,TABAM        A/M(1)                                       
         MVC   EKEYCLT,=X'885F'    CLIENT CC                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
                                                                                
         XC    KEY,KEY             GET THE CLIENTS COPYING TO                   
         MVC   EKEYAM(3),TABAM     A/M(1),CLT(2)                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R6,IOAREA           COPY THE NEW PRODUCT LIST FOR CLIENT         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         LA    R0,CLIST-CKEY(R6)   SET 'TO' ADDRESS                             
         LA    R1,880              SET 'TO' LENTH                               
         L     RE,ADCLT            SET 'FROM' ADDR                              
         LA    RE,CLIST-CKEY(RE)                                                
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   CPYALL10                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,=C'SPTFILE',KEY+14,AREC                      
CPYALL10 MVC   PRNTACTN,=C'CHA'                                                 
         BAS   RE,PRNTLN                                                        
         L     R1,RECCHGCN                                                      
         LA    R1,1(R1)                                                         
         ST    R1,RECCHGCN                                                      
                                                                                
CPYALL20 DS    0H                  COPY THE CC PRODUCTS & ESTIMATES             
         XC    KEY,KEY                                                          
         MVC   EKEYAM,TABAM        A/M(1)                                       
         MVC   EKEYCLT,=X'885F'    CLIENT CC                                    
                                                                                
CPYALL30 GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
CPYALL40 GOTO1 SEQ                                                              
         CLC   EKEY(4),KEYSAVE     00(1),A/M(1),CLT(2)                          
         BNE   CPYX                                                             
         OC    EKEYEST+1(5),EKEYEST+1  CHECK IF PRODUCT OR ESTIMATE             
         BNZ   CPYALL40                                                         
         MVC   SAVEKEY,EKEY                                                     
                                                                                
         CLI   EKEYEST,0           IS IT A PRODUCT HEADER?                      
         BNE   CPYALL50            NO, IT'S AN ESTIMATE HEADER                  
         GOTO1 GETPRD                                                           
         B     CPYALL60                                                         
CPYALL50 GOTO1 GETEST                                                           
                                                                                
CPYALL60 XC    KEY,KEY                CHECK FOR PRODUCTS AND ESIMATES           
         MVC   EKEYAM(3),TABAM        A/M(1),CLT(2)                             
         MVC   EKEYPRD(4),SAVEKEY+4   PRD(3),EST(1)                             
         GOTO1 HIGH                   IF EXISTS, THEN IT'S BEEN COPIED          
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BE    CPYALL90                                                         
                                                                                
         MVC   KEY,KEYSAVE                                                      
         CLI   EKEYEST,0           IS IT A PRODUCT HEADER?                      
         BNE   *+12                                                             
         L     R5,ADPRD            YES                                          
         B     CPYALL70                                                         
                                                                                
         L     R5,ADEST            NO, IT'S AN ESTIMATE HEADER                  
         BRAS  RE,CLRBCKTS                                                      
*                                                                               
CPYALL70 ST    R5,AREC                                                          
         MVC   EKEYCLT,TABCLT      CHANGE THE CLIENT                            
         MVI   ECNTRL,0            CONTROL BYTE                                 
         CLI   RCWRITE,C'Y'                                                     
         BNE   CPYALL80                                                         
         GOTO1 DATAMGR,DMCB,DMADD,=C'SPTFILE',KEY+14,AREC                       
*                                                                               
CPYALL80 MVC   PRNTACTN,=C'ADD'                                                 
         BAS   RE,PRNTLN                                                        
         L     R1,RECADDCN                                                      
         LA    R1,1(R1)                                                         
         ST    R1,RECADDCN                                                      
                                                                                
CPYALL90 LA    R5,KEY                   GET THE NEXT PRODUCT/ESTIMATE           
         MVC   KEY(L'SAVEKEY),SAVEKEY   FOR CLIENT CC                           
         B     CPYALL30                                                         
         DROP  R5                                                               
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  COPY ESTIMATE TO ALL CLIENTS                                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CPYEST   DS    0H                                                               
         LA    R5,KEY                                                           
         USING ESTHDR,R5                                                        
                                                                                
CPYEST10 XC    KEY,KEY             GET ESTIMATE COPYING FROM                    
         MVC   EKEYAM(L'TABLE),TABAM                                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
                                                                                
         XC    KEY,KEY             FIND THE NEXT CLIENT                         
         MVC   EKEYAM,TABAM        A/M                                          
                                                                                
CPYEST20 GOTO1 HIGH                                                             
         CLC   KEY(2),KEYSAVE      00,A/M                                       
         BNE   CPYX                                                             
         OC    EKEYPRD(9),EKEYPRD  CLIENT HEADER?                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   EKEYCLT,TABCLT      CHECK THAT IT'S NOT THE FROM CLIENT          
         BE    CPYEST60            YES, SO SKIP IT                              
                                                                                
         MVC   EKEYPRD(4),TABPRD   CHECK IF ESTIMATE HEADER EXISTS              
         GOTO1 HIGH                PRD(3),EST(1)                                
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BNE   CPYEST40                                                         
                                                                                
         LA    R6,IOAREA                                                        
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         L     R5,ADEST                                                         
         ST    R5,AREC                                                          
         MVC   EKEYCLT,KEY+EKEYCLT-EKEY   CHANGE THE CLIENT                     
         MVI   ECNTRL,0                                                         
         BRAS  RE,CLRBCKTS                                                      
                                                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   CPYEST30                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,=C'SPTFILE',KEY+14,AREC                      
CPYEST30 MVC   PRNTACTN,=C'CHA'                                                 
         BAS   RE,PRNTLN                                                        
         L     R1,RECCHGCN                                                      
         LA    R1,1(R1)                                                         
         ST    R1,RECCHGCN                                                      
         B     CPYEST60                                                         
                                                                                
CPYEST40 MVC   KEY,KEYSAVE                                                      
         L     R5,ADEST                                                         
         ST    R5,AREC                                                          
                                                                                
         MVC   EKEYCLT,KEY+EKEYCLT-EKEY   CHANGE THE CLIENT                     
         MVI   ECNTRL,0                                                         
         BRAS  RE,CLRBCKTS                                                      
                                                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   CPYEST50                                                         
         GOTO1 DATAMGR,DMCB,DMADD,=C'SPTFILE',KEY+14,AREC                       
CPYEST50 MVC   PRNTACTN,=C'ADD'                                                 
         BAS   RE,PRNTLN                                                        
         L     R1,RECADDCN                                                      
         LA    R1,1(R1)                                                         
         ST    R1,RECADDCN                                                      
                                                                                
CPYEST60 LA    R5,KEY              GET THE NEXT CLIENT                          
         MVC   EKEYPRD(9),=X'FFFFFFFFFFFFFFFFFF'                                
         B     CPYEST20                                                         
         DROP  R5                                                               
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  COPY PRODUCT AND PRODUCT LIST TO ALL CLIENTS                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CPYPRD   DS    0H                                                               
         LA    R5,KEY                                                           
         USING PRDHDR,R5                                                        
                                                                                
         CLI   COPYCLT,C'Y'        COPIED PRODUCT LISTS ALREADY?                
         BE    CPYPRD10                                                         
         XC    KEY,KEY             GET CLIENT COPYING FROM                      
         MVC   PKEYAM(3),TABAM     A/M(1),CLT(2)                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
                                                                                
CPYPRD10 XC    KEY,KEY             GET PRODUCT COPYING FROM                     
         MVC   PKEYAM(L'TABLE),TABAM                                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPRD                                                           
                                                                                
         XC    KEY,KEY             GET THE CLIENTS COPYING TO                   
         MVC   PKEYAM,TABAM        A/M                                          
                                                                                
CPYPRD20 GOTO1 HIGH                                                             
         CLC   KEY(2),KEYSAVE      00,A/M                                       
         BNE   CPYPRDX                                                          
         OC    PKEYPRD(9),PKEYPRD  CLIENT HEADER?                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   PKEYCLT,TABCLT      CHECK THAT IT'S NOT THE FROM CLIENT          
         BE    CPYPRD80            YES, SO SKIP IT                              
                                                                                
         CLI   COPYCLT,C'Y'        COPIED PRODUCT LISTS ALREADY?                
         BE    CPYPRD40                                                         
                                                                                
         LA    R6,IOAREA           COPY THE NEW PRODUCT LIST FOR CLIENT         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         LA    R0,CLIST-CKEY(R6)   SET 'TO' ADDRESS                             
         LA    R1,880              SET 'TO' LEN                                 
         L     RE,ADCLT            SET 'FROM' ADDR                              
         LA    RE,CLIST-CKEY(RE)                                                
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   CPYPRD30                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,=C'SPTFILE',KEY+14,AREC                      
CPYPRD30 MVC   PRNTACTN,=C'CHA'                                                 
         BAS   RE,PRNTLN                                                        
         L     R1,RECCHGCN                                                      
         LA    R1,1(R1)                                                         
         ST    R1,RECCHGCN                                                      
                                                                                
CPYPRD40 DS    0H                  CHECK IF PRODUCT HEADER EXISTS               
         MVC   PKEYPRD,TABPRD                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BNE   CPYPRD60                                                         
                                                                                
         LA    R6,IOAREA                                                        
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         L     R5,ADPRD                                                         
         ST    R5,AREC                                                          
         MVC   PKEYCLT,KEY+PKEYCLT-PKEY   CHANGE THE CLIENT                     
         MVI   PCNTRL,0                                                         
                                                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   CPYPRD50                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,=C'SPTFILE',KEY+14,AREC                      
CPYPRD50 MVC   PRNTACTN,=C'CHA'                                                 
         BAS   RE,PRNTLN                                                        
         L     R1,RECCHGCN                                                      
         LA    R1,1(R1)                                                         
         ST    R1,RECCHGCN                                                      
         B     CPYPRD80                                                         
                                                                                
CPYPRD60 MVC   KEY,KEYSAVE                                                      
         L     R5,ADPRD                                                         
         ST    R5,AREC                                                          
         MVC   PKEYCLT,KEY+PKEYCLT-PKEY   CHANGE THE CLIENT                     
         MVI   PCNTRL,0                                                         
                                                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   CPYPRD70                                                         
         GOTO1 DATAMGR,DMCB,DMADD,=C'SPTFILE',KEY+14,AREC                       
CPYPRD70 MVC   PRNTACTN,=C'ADD'                                                 
         BAS   RE,PRNTLN                                                        
         L     R1,RECADDCN                                                      
         LA    R1,1(R1)                                                         
         ST    R1,RECADDCN                                                      
                                                                                
CPYPRD80 LA    R5,KEY              GET THE NEXT CLIENT                          
         MVC   PKEYPRD(9),=X'FFFFFFFFFFFFFFFFFF'                                
         B     CPYPRD20                                                         
CPYPRDX  L     R1,ACOPYCLT                                                      
         MVI   0(R1),C'Y'                                                       
         DROP  R5,R7                                                            
                                                                                
CPYX     LA    R4,L'TABLE(R4)                                                   
         BCT   R3,CE110                                                         
                                                                                
CE200    DS    0H                                                               
         MVC   P1+15(13),=C'RECORDS ADDED'                                      
         EDIT  RECADDCN,(10,P1),0,ZERO=NOBLANK                                  
                                                                                
         MVC   P2+15(15),=C'RECORDS CHANGED'                                    
         EDIT  RECCHGCN,(10,P2),0,ZERO=NOBLANK                                  
         GOTO1 REPORT                                                           
                                                                                
CE220    DS    0H                                                               
         GOTO1 AENDREQ                                                          
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  PRINT A COPY OF THE TABLE                                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRNTAB   NTR1                                                                   
         CLI   RCWRITE,C'Y'                                                     
         BE    EXIT                ***** DON'T PRINT REPORT *****               
         LA    R2,P                                                             
         USING PRNTLND,R2                                                       
         LA    R4,TABLE                                                         
         ZICM  R3,NUMENTRY,4                                                    
         BZ    EXIT                                                             
         MVC   P1(5),=C'TABLE'                                                  
         GOTO1 REPORT                                                           
PRNTAB10 GOTO1 HEXOUT,DMCB,(R4),P1,L'TABLE                                      
         GOTO1 CLUNPK,DMCB,TABCLT,PRNTCLT                                       
         MVC   PRNTPRD(3),TABPRD                                                
         EDIT  TABEST,PRNTEST                                                   
         GOTO1 REPORT                                                           
         LA    R4,L'TABLE(R4)                                                   
         BCT   R3,PRNTAB10                                                      
         B     EXIT                                                             
         DROP  R2                                                               
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  PRINT A LINE OF REPORT                                             *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRNTLN   NTR1                                                                   
         CLI   RCWRITE,C'Y'                                                     
         BE    EXIT                ***** DON'T PRINT REPORT *****               
         USING ESTHDR,R5                                                        
         LA    R2,P                                                             
         USING PRNTLND,R2                                                       
                                                                                
         MVC   PRNTMED,EKEYAM                                                   
         NI    PRNTMED,X'FF'-X'F0'                                              
         CLI   PRNTMED,X'01'                                                    
         BNE   *+12                                                             
         MVI   PRNTMED,C'T'                                                     
         B     *+8                                                              
         MVI   PRNTMED,C'R'                                                     
         GOTO1 CLUNPK,DMCB,EKEYCLT,PRNTCLT                                      
         MVC   PRNTPRD,EKEYPRD                                                  
         EDIT  EKEYEST,PRNTEST                                                  
         GOTO1 REPORT                                                           
         DROP  R2,R5                                                            
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* CLEAR ALL ESTIMATE HEADER BUCKETS                                             
*=============================================================                  
CLRBCKTS NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDR,R5                                                        
*                                                                               
         ZAP   ECURPDN,=P'0'                                                    
*                                                                               
         LA    R0,26                                                            
         LA    R1,EORD                                                          
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,26                                                            
         LA    R1,EPAID                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,13                                                            
         LA    R1,EAUTH                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=4024,             X        
               MACRF=GM,EODAD=CE100                                             
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=4024,            X        
               BLKSIZE=16000,MACRF=PM                                           
         EJECT                                                                  
RECADDCN DS    F                   COUNT OF RECORDS ADDED                       
RECCHGCN DS    F                   COUNT OF RECORDS CHANGED                     
NUMENTRY DS    F                                                                
ACOPYCLT DS    A                   ADDRESS OF CPYCLT USING                      
COPYCLT  DS    C                                                                
COPYCLT1 DS    C                   Y=COPIED CLIENT HEADER MEDIA 1               
COPYCLT2 DS    C                   Y=COPIED CLIENT HEADER MEDIA 2               
COPYCLT3 DS    C                   Y=COPIED CLIENT HEADER MEDIA 3               
COPYCLT4 DS    C                   Y=COPIED CLIENT HEADER MEDIA 4               
LOAM     DS    XL2                                                              
HIAM     DS    XL2                                                              
ELCODE   DS    X                                                                
SAVEKEY  DS    CL13                                                             
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVRECLN DC    H'0'                                                             
         DC    H'0'                                                             
RCVREC   DS    0F                                                               
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0CL13                                                            
IOAREA   DS    4000C                                                            
TABLE    DS    4095CL7             A/M(1),CLT(2),PRD(3),EST(1)                  
TABLEMAX EQU   (*-TABLE)/7         MAX ENTRIES                                  
         DC    X'FF'                                                            
TABLED   DSECT                                                                  
TABAM    DS    C                   A/M                                          
TABCLT   DS    CL2                 CLIENT                                       
TABPRD   DS    CL3                 PRODUCT                                      
TABEST   DS    C                   ESTIMATE                                     
PRNTLND  DSECT                                                                  
PRNTACTN DS    CL3                 ACTION ADD/CHA                               
         DS    CL8                                                              
PRNTMED  DS    C                   MEDIA                                        
         DS    CL9                                                              
PRNTCLT  DS    CL3                 CLIENT                                       
         DS    CL7                                                              
PRNTPRD  DS    CL3                 PRODUCT                                      
         DS    CL9                                                              
PRNTEST  DS    CL3                 ESTIMATE                                     
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPCE02 03/31/04'                                      
         END                                                                    
