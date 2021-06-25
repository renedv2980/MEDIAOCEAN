*          DATA SET PPREPPC02A AT LEVEL 027 AS OF 05/01/02                      
*PHASE PPPC02A,+0                                                               
         TITLE 'PPPC02 - PUBFILE COMPARISON'                                    
*   CHANGE LOG                                                                  
*                                                                               
*        OPTIONS   QOPT1 N= NAME SORT                                           
*                        P= PAY REP SORT                                        
*                        BLANK= CODE SORT                                       
*                                                                               
*                  QOPT2 P=PUBS                                                 
*                        R=REPS                                                 
*                        C=CLIENTS                                              
*                                                                               
*                  QOPT3 D=DUPLICATES ONLY  (USE FOR PUB CODE SORT              
*                          OR REP CODE SORT OR CLIENT CODE SORT)                
*                                                                               
*                  IN QPUB (COL26) ENTER AGENCIES TO COMPARE                    
*                  (MAX IS 5)                                                   
*                  ENTRIES ARE 3 BYTES (AGY-2,PRTFILE-1)                        
*                                                                               
PPPC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPPC02                                                         
*                                                                               
         SPACE 2                                                                
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING PPPC02+4096,R7                                                   
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPPCWRKD,R8                                                      
         SPACE 2                                                                
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
         L     RF,UTL                                                           
         MVC   SAVESE,4(RF)         SAVE MY SE                                  
*                                   I NEED TO RESTORE IT AT TLLAST              
         CLI   QOPT2,C'P'          SEE IF DOING PUBS                            
         BNE   TLF1                                                             
         XC    OPENSES,OPENSES      MUST CLEAR OPEN SES                         
         L     RF,UTL                                                           
         MVC   OPENSES(1),4(RF)      SET FIRST SE OPEN                          
*                                                                               
TLF1     DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   SAVEP,SPACES                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   ENDSW,C'N'                                                       
*        BUILD AGYTAB FROM AGENCY LIST IN QPUB                                  
         LA    R1,AGYTAB                                                        
         LA    R2,QPUB                                                          
TLF2     CLI   0(R2),C' '     END OF LIST                                       
         BNH   TLF4                                                             
         MVC   0(2,R1),0(R2)                                                    
         MVI   2(R1),X'81'           PUB RECORD TYPE                            
         LA    RE,PRINTSES                                                      
TLF2B    CLC   2(1,R2),0(RE)                                                    
         BE    TLF2C                                                            
         LA    RE,2(RE)                                                         
         B     TLF2B                                                            
*                                                                               
TLF2C    MVC   3(1,R1),1(RE)         PRINTFILE SE                               
*                                                                               
         ZAP   4(4,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         LA    R2,3(R2)                                                         
         B     TLF2                                                             
*                                                                               
TLF4     MVC   0(2,R1),=X'FFFF'       SET END OF TABLE                          
*                                                                               
TLF6     DS    0H                                                               
         MVI   RUNSW,0                                                          
         MVC   SAVPARS+04(2),=C'62'   SORT REC LEN                              
         MVC   SAVPARS+06(2),=C'53'   SORT KEY LEN                              
         XC    KEY,KEY                MUST CLEAR KEY                            
         XC    OLDKEY,OLDKEY                                                    
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
         L     RF,UTL                                                           
         MVC   4(1,RF),SAVESE     MUST RESTORE REQUEST SE                       
*                                                                               
         MVI   ENDSW,C'Y'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+2(13),=C'RECORD TOTALS'                                        
         MVI   SPACING,2                                                        
         BAS   RE,TLPRT                                                         
*                                                                               
         LA    R5,AGYTAB                                                        
*                                                                               
TLLAST5  CLI   0(R5),X'FF'        END OF LIST                                   
         BE    EXIT                                                             
         MVC   P+2(07),=C'   RECS'                                              
         MVC   P+2(02),0(R5)                                                    
         EDIT  (P4,4(R5)),(7,P+12),0,COMMAS=YES                                 
         BAS   RE,TLPRT                                                         
         LA    R5,8(R5)                                                         
         B     TLLAST5            DO NEXT AGY                                   
*                                                                               
EXIT     DS    0H                                                               
         MVC   DMCB+4(8),SAVPARS                                                
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                  INPUT                                        
TLIN     NTR1                                                                   
         SPACE 2                                                                
         CLI   QOPT2,C'R'              SEE IF DOING REPS                        
         BE    TLINR                                                            
*                                                                               
         CLI   QOPT2,C'C'              SEE IF DOING CLIENTS                     
         BE    TLINC                                                            
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   TLIN10                                                           
*                                       FIRST TIME                              
TLIN0    DS    0H                                                               
         MVC   KEY(1),QMEDIA                                                    
TLIN6    DS    0H                                                               
*                                                                               
TLIN8    DS    0H                                                               
         GOTO1 HIGHPUB                                                          
         B     TLIN10A                                                          
*                                                                               
TLIN10   DS    0H                                                               
         GOTO1 SEQPUB                                                           
TLIN10A  DS    0H                                                               
         CLI   KEY,X'FF'        END OF FILE                                     
         BE    TLIN90                                                           
         CLC   KEY(1),QMEDIA          END OF MEDIA                              
         BNE   TLIN90                                                           
*                                                                               
         LA    R5,AGYTAB                                                        
*                                                                               
TLIN10D  CLI   0(R5),X'FF'                                                      
         BE    TLIN10                 NOT IN TABLE SO SKIP                      
         CLC   KEY+7(3),0(R5)                                                   
         BE    TLIN19                                                           
         LA    R5,8(R5)                                                         
         B     TLIN10D                                                          
*                                                                               
TLIN19   DS    0H                                                               
*                                                                               
         AP    4(4,R5),=P'1'         ADD TO COUNTER                             
*                                                                               
         GOTO1 GETNAME                READ PUB                                  
         L     R3,SAVPARS+4                                                     
         USING SORTRECD,R3                                                      
         XC    SORTREC,SORTREC                                                  
         MVC   SORTREC+58(4),KEY+27                                             
         MVC   SORTREC+51(2),KEY+7     AGENCY                                   
*                                                                               
         XC    PAYREP,PAYREP                                                    
*                                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'                                                     
         USING PUBREPEL,R2                                                      
TLIN20   BAS   RE,NEXTEL                                                        
         BNE   TLIN20X                                                          
         CLC   2(3,R2),=X'FFFFFF'                                               
         BNE   TLIN20                                                           
         MVC   PAYREP(4),PUBPAREP                                               
         MVC   SORTREC+53(4),PAYREP      PAYING REP                             
*                                                                               
         DROP  R2                                                               
*                                                                               
TLIN20X  DS    0H                                                               
         CLI   QOPT1,C' '            SEE IF DOING NUMBER SORT                   
         BNE   TLIN22                                                           
         MVC   SORTREC+5(6),KEY+1      PUB NUMBER                               
         MVC   SORTREC+11(20),PUBNAME                                           
         MVC   SORTREC+31(20),PUBZNAME                                          
         CLI   QMEDIA,C'N'             FOR NEWSPAPERS USE CITY, STATE           
         BNE   TLINX                                                            
         MVC   SORTREC+31(20),SPACES                                            
         MVC   SORTREC+31(16),PUBCITY                                           
         MVC   SORTREC+49(2),PUBSTATE                                           
         B     TLINX                                                            
*                                                                               
TLIN22   DS    0H                                                               
         CLI   QOPT1,C'P'              SEE IF PAYREP SORT                       
         BNE   TLIN22B                 IF NOT MUST BE NAME                      
         MVC   SORTREC(4),PAYREP                                                
*                                                                               
TLIN22B  MVC   SORTREC+5(20),PUBNAME                                            
         MVC   SORTREC+25(20),PUBZNAME                                          
         MVC   SORTREC+45(6),KEY+1                                              
         CLI   QMEDIA,C'N'             FOR NEWSPAPERS USE CITY, STATE           
         BNE   TLINX                                                            
         MVC   SORTREC+25(20),SPACES                                            
         MVC   SORTREC+25(16),PUBCITY                                           
         MVC   SORTREC+43(2),PUBSTATE                                           
         B     TLINX                                                            
*                                                                               
TLIN90   DS    0H                                                               
         LA    R5,AGYTAB                                                        
TLIN90C  CLI   0(R5),X'FF'           SEE IF AT END OF TABLE                     
         BE    TLIN90X                                                          
*                                                                               
         LA    R2,OPENSES                                                       
TLIN90D  CLC   0(1,R2),3(R5)                                                    
         BE    TLIN90F                                                          
         CLI   0(R2),0                                                          
         BNE   TLIN90D5                                                         
         MVC   0(1,R2),3(R5)                                                    
         B     TLIN90E                                                          
*                                                                               
TLIN90D5 LA    R2,1(R2)                                                         
         B     TLIN90D                                                          
*                                                                               
TLIN90E  DS    0H                                                               
*                                                                               
*        OPEN THE PRINTFILE                                                     
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),3(R5)         SET SE IN UTL                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'PRINT',SRTFILES,PPBYOWRK                
         XC    KEY,KEY                                                          
         B     TLIN0                                                            
*                                                                               
TLIN90F  LA    R5,8(R5)            BUMP TO NEXT AGY                             
         B     TLIN90C                                                          
*                                                                               
TLIN90X  MVI   SAVPARS+3,8                                                      
         XC    OPENSES,OPENSES       FOR PUBS MUST CLEAR FOR NEXT REQ           
         B     TLINX                                                            
*                                                                               
TLINX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
TLINR    DS    0H                       REP ROUTINE                             
         SPACE 2                                                                
*                                       FIRST TIME                              
*                                                                               
         OC    KEY,KEY                                                          
         BZ    TLINRF                                                           
*                                      NOT FIRST TIME - MUST RESET R5           
         LA    R5,AGYTAB                                                        
TLINR0   CLI   0(R5),X'FF'             END OF TABLE                             
         BNE   *+6                                                              
         DC    H'0'                    SOMETHING SCREWY                         
         CLC   0(2,R5),KEY                                                      
         BE    TLINR10                 GO DO SEQ READ                           
         LA    R5,8(R5)                                                         
         B     TLINR0                                                           
*                                       FIRST TIME                              
TLINRF   DS    0H                                                               
         LA    R5,AGYTAB                                                        
*                                                                               
TLINR2   CLI   0(R5),X'FF'            END OF TABLE                              
         BE    TLINR90                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),0(R5)                                                     
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'11'                                                      
*                                                                               
         L     RF,UTL                                                           
         MVC   OPENSES(1),4(RF)     SINCE MY FILE IS ALREADY OPENED             
*                                                                               
TLINR8   DS    0H                                                               
         GOTO1 HIGH                                                             
         B     TLINR10A                                                         
*                                                                               
TLINR10  DS    0H                                                               
         GOTO1 SEQ                                                              
TLINR10A DS    0H                                                               
         CLI   KEY,X'FF'        END OF FILE                                     
         BE    TLINR90                                                          
         CLC   KEY(4),KEYSAVE         AGY/MEDIA/RECORD CODE                     
         BNE   TLINR60                                                          
*                                                                               
*                                                                               
TLINR19  DS    0H                                                               
*                                                                               
         AP    4(4,R5),=P'1'        BUMP REP COUNTER                            
*                                                                               
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         L     R3,SAVPARS+4                                                     
         USING SORTRECD,R3                                                      
         XC    SORTREC,SORTREC                                                  
         MVC   SORTREC+58(4),KEY+27                                             
         MVC   SORTREC+51(2),KEY       AGENCY                                   
         CLI   QOPT1,C'N'            NAME SORT                                  
         BE    TLINR22                                                          
         MVC   SORTREC(5),KEY+4      REP CODE                                   
         MVC   SORTREC+5(30),PREPNAME                                           
         B     TLINRX                                                           
*                                                                               
TLINR22  DS    0H                                                               
         MVC   SORTREC(30),PREPNAME                                             
         MVC   SORTREC+30(5),KEY+4     CODE                                     
         B     TLINRX                                                           
*                                                                               
TLINR60  DS    0H                                                               
         LA    R5,8(R5)                DO NEXT AGY                              
         CLI   0(R5),X'FF'             END OF AGENCIES                          
         BE    TLINR90                                                          
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),3(R5)           SET PRTFILE                              
         LA    R4,OPENSES              SEE IF ALREADY OPEN                      
TLINR60C CLC   0(1,R4),3(R5)                                                    
         BE    TLINR60F                                                         
         CLI   0(R4),0                                                          
         BNE   TLINR60D                                                         
         MVC   0(1,R4),3(R5)        SAVE IN TABLE OF OPEN SES                   
*                                                                               
*        OPEN THE PRINTFILE                                                     
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),3(R5)                                                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'PRINT',SRTFILES,PPBYOWRK                
         B     TLINR60H                                                         
*                                                                               
TLINR60D LA    R4,1(R4)                                                         
         B     TLINR60C                                                         
*                                                                               
TLINR60F L     RF,UTL                                                           
         MVC   4(1,RF),3(R5)        SWITCHES ME TO AGENCY FILE                  
TLINR60H DS    0H                                                               
         B     TLINR2                  IN AGYTAB                                
*                                                                               
TLINR90  DS    0H                                                               
         MVI   SAVPARS+3,8                                                      
         B     TLINRX                                                           
*                                                                               
TLINRX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
TLINC    DS    0H                       CLIENT ROUTINE                          
         SPACE 2                                                                
*                                       FIRST TIME                              
*                                                                               
         OC    KEY,KEY                                                          
         BZ    TLINCF                                                           
*                                      NOT FIRST TIME - MUST RESET R5           
         LA    R5,AGYTAB                                                        
TLINC0   CLI   0(R5),X'FF'             END OF TABLE                             
         BNE   *+6                                                              
         DC    H'0'                    SOMETHING SCREWY                         
         CLC   0(2,R5),KEY                                                      
         BE    TLINC10                 GO DO SEQ READ                           
         LA    R5,8(R5)                                                         
         B     TLINC0                                                           
*                                       FIRST TIME                              
TLINCF   DS    0H                                                               
         LA    R5,AGYTAB                                                        
*                                                                               
TLINC2   CLI   0(R5),X'FF'            END OF TABLE                              
         BE    TLINC90                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),0(R5)                                                     
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'02'                                                      
*                                                                               
         L     RF,UTL                                                           
         MVC   OPENSES(1),4(RF)     SINCE MY FILE IS ALREADY OPENED             
*                                                                               
TLINC8   DS    0H                                                               
         GOTO1 HIGH                                                             
         B     TLINC10A                                                         
*                                                                               
TLINC10  DS    0H                                                               
         GOTO1 SEQ                                                              
TLINC10A DS    0H                                                               
         CLI   KEY,X'FF'        END OF FILE                                     
         BE    TLINC90                                                          
         CLC   KEY(4),KEYSAVE         AGY/MEDIA/RECORD CODE                     
         BNE   TLINC60                                                          
*                                                                               
*                                                                               
TLINC19  DS    0H                                                               
*                                                                               
         AP    4(4,R5),=P'1'        BUMP REP COUNTER                            
*                                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         BAS   RE,ASTESUB                                                       
         L     R3,SAVPARS+4                                                     
         USING SORTRECD,R3                                                      
         XC    SORTREC,SORTREC                                                  
         MVC   SORTREC+58(4),KEY+27                                             
         MVC   SORTREC+51(2),KEY       AGENCY                                   
         CLI   QOPT1,C'N'            NAME SORT                                  
         BE    TLINC22                                                          
         MVC   SORTREC(4),KEY+4      CLIENT CODE                                
         MVC   SORTREC+4(20),PCLTNAME                                           
         B     TLINCX                                                           
*                                                                               
TLINC22  DS    0H                                                               
         MVC   SORTREC(20),PCLTNAME                                             
         MVC   SORTREC+20(4),KEY+4   CLIENT CODE                                
         B     TLINCX                                                           
*                                                                               
TLINC60  DS    0H                                                               
         LA    R5,8(R5)                DO NEXT AGY                              
         CLI   0(R5),X'FF'             END OF AGENCIES                          
         BE    TLINC90                                                          
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),3(R5)           SET PRTFILE                              
         LA    R4,OPENSES              SEE IF ALREADY OPEN                      
TLINC60C CLC   0(1,R4),3(R5)                                                    
         BE    TLINC60F                                                         
         CLI   0(R4),0                                                          
         BNE   TLINC60D                                                         
         MVC   0(1,R4),3(R5)        SAVE IN TABLE OF OPEN SES                   
*                                                                               
*        OPEN THE PRINTFILE                                                     
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),3(R5)                                                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'PRINT',SRTFILES,PPBYOWRK                
         B     TLINC60H                                                         
*                                                                               
TLINC60D LA    R4,1(R4)                                                         
         B     TLINC60C                                                         
*                                                                               
TLINC60F L     RF,UTL                                                           
         MVC   4(1,RF),3(R5)        SWITCHES ME TO AGENCY FILE                  
TLINC60H DS    0H                                                               
         B     TLINC2                  IN AGYTAB                                
*                                                                               
TLINC90  DS    0H                                                               
         MVI   SAVPARS+3,8                                                      
         B     TLINCX                                                           
*                                                                               
TLINCX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                  OUTPUT                                       
TLOUT    NTR1                                                                   
         SPACE 2                                                                
         CLI   QOPT2,C'R'           SEE IF DOING REPS                           
         BE    TLOUTR                                                           
*                                                                               
         CLI   QOPT2,C'C'           SEE IF DOING CLIENTS                        
         BE    TLOUTC                                                           
*                                                                               
         MVC   P+68(4),SORTREC+53   PAYING REP                                  
*                                                                               
         MVC   P+80(2),SORTREC+51   AGENCY                                      
         CLI   QOPT1,C'N'          SEE IF DOING NAME SORT                       
         BE    TLOUTN                                                           
         CLI   QOPT1,C'P'          OR PAYING REP SORT                           
         BE    TLOUTN                                                           
         MVC   P+24(20),SORTREC+11   PUB NAME                                   
         MVC   P+46(20),SORTREC+31   PUB ZONE NAME                              
         GOTO1 PUBEDIT,DMCB,(0,SORTREC+5),P+1                                   
         B     TLOUTX                                                           
*                                                                               
TLOUTN   DS    0H                                                               
         MVC   P+1(20),SORTREC+5                                                
         MVC   P+23(20),SORTREC+25                                              
         GOTO1 PUBEDIT,DMCB,(0,SORTREC+45),P+47                                 
TLOUTX   DS    0H                                                               
         CLI   QOPT3,C'D'         SEE IF PRINTING DUPLICATES ONLY               
         BNE   TLOUTXX                                                          
         CLI   QOPT1,C'N'        NAME SORTS                                     
         BE    TLOUTXX                                                          
         CLI   QOPT1,C'P'        OR PAYING REP SORT                             
         BE    TLOUTXX                                                          
         LA    R6,10              11 -1 FOR EXECUTE                             
*                                                                               
TLOUTX5  OC    OLDKEY,OLDKEY       FIRST TIME- MUST BE NOT EQUAL                
         BZ    TLOUTX9             TO LAST PUB                                  
         EX    R6,TLDUPCK                                                       
         BNE   TLOUTX9                                                          
         CLC   SAVEP(10),SPACES   CHECK FOR SAVED P LINE                        
         BE    TLOUTXX                                                          
         MVC   TEMPP,P                                                          
         MVC   P,SAVEP             PRINT SAVED P                                
         BAS   RE,TLPRT                                                         
         MVC   SAVEP,SPACES                                                     
         MVC   P,TEMPP                                                          
         B     TLOUTXX                                                          
*                                                                               
TLOUTX9  DS    0H                                                               
*                                                                               
TLOUTXB  MVC   SAVEP,P            SAVE THE PRINT LINE I JUST BUILT              
         MVC   P,SPACES                                                         
         EX    R6,TLSVKEY                                                       
         B     TLOUTXZ                                                          
*                                                                               
TLOUTXX  BAS   RE,TLPRT                                                         
TLOUTXZ  DS    0H                                                               
         B     XIT                                                              
*                                                                               
TLDUPCK  CLC   SORTREC(0),OLDKEY       EXECUTED                                 
*                                                                               
TLSVKEY  MVC   OLDKEY(0),SORTREC       EXECUTED                                 
*                                                                               
         EJECT                                                                  
*                                                                               
TLOUTR   DS    0H                  OUTPUT REPS                                  
         SPACE 2                                                                
*                                                                               
         MVC   P+49(2),SORTREC+51   AGENCY                                      
         CLI   QOPT1,C'N'          SEE IF DOING NAME SORT                       
         BE    TLOUTRN                                                          
         MVC   P+2(5),SORTREC                                                   
         MVC   P+14(30),SORTREC+5    REP NAME                                   
         B     TLOUTRX                                                          
*                                                                               
TLOUTRN  DS    0H                                                               
         MVC   P+1(30),SORTREC                                                  
         MVC   P+37(5),SORTREC+30                                               
TLOUTRX  DS    0H                                                               
         CLI   QOPT3,C'D'         SEE IF PRINTING DUPLICATES ONLY               
         BNE   TLOUTRXX                                                         
         CLI   QOPT1,C'N'        NAME SORTS                                     
         BE    TLOUTRXX                                                         
*                                                                               
         CLC   OLDKEY(5),SORTREC                                                
         BNE   TLOUTRX9                                                         
         CLC   SAVEP(10),SPACES   CHECK FOR SAVED P LINE                        
         BE    TLOUTRXX                                                         
         MVC   TEMPP,P                                                          
         MVC   P,SAVEP             PRINT SAVED P                                
         BAS   RE,TLPRT                                                         
         MVC   SAVEP,SPACES                                                     
         MVC   P,TEMPP                                                          
         B     TLOUTRXX                                                         
*                                                                               
TLOUTRX9 DS    0H                                                               
*                                                                               
TLOUTRXB MVC   SAVEP,P            SAVE THE PRINT LINE I JUST BUILT              
         MVC   P,SPACES                                                         
         MVC   OLDKEY(5),SORTREC                                                
         B     TLOUTRXZ                                                         
*                                                                               
TLOUTRXX BAS   RE,TLPRT                                                         
TLOUTRXZ DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
TLOUTC   DS    0H                  OUTPUT CLIENTS                               
         SPACE 2                                                                
*                                                                               
         MVC   P+51(2),SORTREC+51   AGENCY                                      
         CLI   QOPT1,C'N'          SEE IF DOING NAME SORT                       
         BE    TLOUTCN                                                          
         MVC   P+5(4),SORTREC                                                   
         MVC   P+16(20),SORTREC+4    CLIENT NAME                                
         B     TLOUTCX                                                          
*                                                                               
TLOUTCN  DS    0H                                                               
         MVC   P+1(20),SORTREC                                                  
         MVC   P+30(4),SORTREC+20                                               
TLOUTCX  DS    0H                                                               
         CLI   QOPT3,C'D'         SEE IF PRINTING DUPLICATES ONLY               
         BNE   TLOUTCXX                                                         
         CLI   QOPT1,C'N'        NAME SORTS                                     
         BE    TLOUTCXX                                                         
*                                                                               
         CLC   OLDKEY(4),SORTREC                                                
         BNE   TLOUTCX9                                                         
         CLC   SAVEP(10),SPACES   CHECK FOR SAVED P LINE                        
         BE    TLOUTCXX                                                         
         MVC   TEMPP,P                                                          
         MVC   P,SAVEP             PRINT SAVED P                                
         BAS   RE,TLPRT                                                         
         MVC   SAVEP,SPACES                                                     
         MVC   P,TEMPP                                                          
         B     TLOUTCXX                                                         
*                                                                               
TLOUTCX9 DS    0H                                                               
*                                                                               
TLOUTCXB MVC   SAVEP,P            SAVE THE PRINT LINE I JUST BUILT              
         MVC   P,SPACES                                                         
         MVC   OLDKEY(5),SORTREC                                                
         B     TLOUTCXZ                                                         
*                                                                               
TLOUTCXX BAS   RE,TLPRT                                                         
TLOUTCXZ DS    0H                                                               
         B     XIT                                                              
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
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
         B     XIT                                                              
         SPACE 3                                                                
TLHEAD   DS    0H                                                               
*                                                                               
TLHD1    DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   ENDSW,C'Y'                                                       
         BE    TLHDX                                                            
*                                                                               
         CLI   QOPT2,C'C'     SEE IF DOING CLIENTS                              
         BNE   TLHD2                                                            
         MVI   RCSUBPRG,30         FOR CLIENT CODE SEQUENCE                     
         CLI   QOPT1,C'N'  SEE IF DOING NAME SORT                               
         BNER  RE                                                               
         MVI   RCSUBPRG,31         FOR CLIENT NAME SEQUENCE                     
         BR    RE                                                               
*                                                                               
TLHD2    MVI   RCSUBPRG,4                                                       
         CLI   QOPT1,C'N'  SEE IF DOING NAME SORT                               
         BE    TLHDX                                                            
         CLI   QOPT1,C'P'  OR PAYING REP                                        
         BE    TLHDX                                                            
         MVI   RCSUBPRG,2                                                       
TLHDX    DS    0H                                                               
         CLI   QOPT2,C'C'     SEE IF DOING CLIENTS                              
         BNE   TLHDX1                                                           
         CLI   ENDSW,C'Y'                                                       
         BNER  RE                                                               
         MVI   RCSUBPRG,1                                                       
         BR    RE                                                               
TLHDX1   ZIC   R0,RCSUBPRG                                                      
         CLI   QOPT2,C'R'     SEE IF DOING REPS                                 
         BNER  RE                                                               
         AH    R0,=H'10'                                                        
         STC   R0,RCSUBPRG                                                      
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
         EJECT                                                                  
*                                                                               
FIRST    DS    0H                                                               
*                                  ADD LOGO REC TO LABEL FILE                   
         XC    OPENSES,OPENSES                                                  
         XC    KEY,KEY             JUST IN CASE                                 
**** OPENING DATASET FOR OUTPUT                                                 
         OPEN  (FILEOUT,OUTPUT)     OPEN DATASET                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                                                               
         CLOSE FILEOUT                                                          
         B     EXIT                                                             
         EJECT                                                                  
**** ASTESUB - MY SUBROUTINE TO CREATE NEW REC.'S AND WRITE TO FILE             
ASTESUB  NTR1                                                                   
         USING DCLIENT,R4                                                       
         LA    R4,CLTREC                                                        
*** STORE  CLIENT CODE                                                          
         MVC   DCLTC,PCLTKCLT                                                   
*** STORE MEDIA CODE                                                            
         MVC   DMEDC,PCLTKMED                                                   
*** STORE AGENCY CODE                                                           
         MVC   DAGYC,PCLTKAGY                                                   
*** STORE SYSTEM                                                                
         MVI   DSYS,DPRNTS                                                      
*** STORING CLIENT NAME                                                         
         MVC   DCLTNM,PCLTNAME                                                  
         OC    DCLTNM,SPACES                                                    
**** WRITE TO DATASET                                                           
         PUT   FILEOUT,CLTREC                                                   
         DROP  R4                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
SRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
*                                                                               
       ++INCLUDE PRINTSES                                                       
*                                                                               
*          DATA SET SPREPFX02W AT LEVEL 040 AS OF 06/30/97                      
CLTREC   DS    CL27           CLIENT RECORD TO WRT TO DATASET                   
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,LRECL=27,MACRF=PM,     +        
               BLKSIZE=13500                                                    
         SPACE 3                                                                
*                                                                               
PPPCWRKD DSECT                                                                  
RUNSW    DS    X                                                                
ENDSW    DS    CL1                                                              
SEQSW    DS    CL1                                                              
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
BPUB     DS    XL6                                                              
*                                                                               
*                                                                               
*                                                                               
SAVESE   DS    CL1                                                              
*                                                                               
PAYREP   DS    CL5                                                              
*                                                                               
TODAY    DS    CL6                                                              
*                                                                               
CONDAT   DS    XL3                 CONTROL DATE TODAY IF NOT SPEC               
SVTODAY  DS    XL3                 SAVED TODAY'S DATE                           
OLDKEY   DS    CL20                                                             
         DS    0F                                                               
SAVPARS  DS    0CL24                                                            
         DS    6F                                                               
ELCODE   DS    X                                                                
LINENEED DS    X                                                                
SVBUYADR DS    F                                                                
SAVKEYS  DS    CL64                                                             
         DS    0D                                                               
OPENSES  DS    XL10      FILES I HAVE OPENED                                    
*                                                                               
AGYTAB   DS    CL42     8 BYTES PER AGY +2 (FOR FINAL X'FFFF')                  
*                       ENTRIES ARE AGY(2),X'81',SE#,PL4                        
*                       LAST ENTRY IS X'FFFF'                                   
*                                                                               
SAVEP    DS    CL132                                                            
*                                                                               
TEMPP    DS    CL132                                                            
*                                                                               
PPBYOWRK DS    CL600                                                            
*                                                                               
         SPACE 3                                                                
DCLIENT  DSECT                DSECT FOR RECORD TO WRITE TO DATASET              
DCLTC    DS    CL3                                                              
DCLTNM   DS    CL20                                                             
DAGYC    DS    CL2                                                              
DSYS     DS    CL1                                                              
DSPOTS   EQU   C'S'           FOR SPOT SYSTEM                                   
DPRNTS   EQU   C'P'                                                             
DNETS    EQU   C'N'                                                             
DMEDC    DS    C                                                                
*                                                                               
         SPACE 3                                                                
SORTRECD DSECT                                                                  
SORTREC  DS    0CL62                                                            
SORTKEY  DS    CL58                                                             
SRECDA   DS    CL4                                                              
         SPACE 3                                                                
       ++INCLUDE DDLOGOD                                                        
*                                                                               
WRKBUFF  CSECT                                                                  
         DS    4096X                                                            
         SPACE 3                                                                
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
       ++INCLUDE PPREPWORK                                                      
*                                                                               
         PRINT ON                                                               
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027PPREPPC02A05/01/02'                                      
         END                                                                    
