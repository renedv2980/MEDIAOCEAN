*          DATA SET PPFIS01    AT LEVEL 105 AS OF 05/20/20                      
*PHASE T41B01A                                                                  
         TITLE 'PPFIS01 - PRINTPAK FIS REC PROCESS'                             
*                                                                               
*  SEE NOTE AT RBX5 IF USER NEVER CAN REACH "REQUEST PROCESSED"                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                            CHANGE LOG                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  BOBY 01/07    PLANNED COSTS REPORTING                                        
*                                                                               
*  SMYE 05/02    NEW LIMIT ACCESS SECURITY (INCLUDING TRAFFIC OFFICE)           
*                                                                               
*  KWAN 07/00    BILL AMTS ARE NOW IN PL6 INSTEAD OF PL5                        
*                                                                               
*  BPLA 11/97    ATTEMPT TO SPEED UP BUY READING BY SKIPPING                    
*                CLIENTS WITH NO BUYS AND SKIPPING                              
*                ESTIMATE READ WHEN READING BUYS                                
*                                                                               
*  BPLA 10/97    GETINS MADE CORE-RESIDENT                                      
*                                                                               
*  BPLA 10/95    IF STOPPING REQUEST, UNVALIDATE MEDIA                          
*                                                                               
*  BPLA 02/95    AT BUYSTOP - CLEAR LOWER SCREEN                                
*                                                                               
*  BPLA 01/95    CHANGES TO ALLOW FOR CONTINUED BUY READING                     
*                WHEN MAX I/O'S ARE RACHED                                      
*                                                                               
*  BPLA 08/94    FIX BUG IN GETNCLT                                             
*                                                                               
*  BPLA 3/1/94   CHANGES FOR BILLING GROUPS                                     
*                                                                               
*  BPLA 11/8/93  CHANGES FOR OFFICE LIST READING (SVCLT = $N)                   
*                                                                               
*  BPLA 10/6/92  USE PPBVAL TFOR "EFFECTIVE" VALUES FOR UFC BILLING             
*                GETINS WILL HANDLE THE BILL ELEMS NOW                          
*                                                                               
         TITLE 'PPFIS01 - PRINTPAK FIS REC PROCESS'                             
T41B01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41B01,RR=R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LA    R8,T41B01+4095                                                   
         LA    R8,1(R8)                                                         
         USING T41B01+4096,R8      ** NOTE USE OF SECOND BASE REG **            
*                                                                               
         USING T41BFFD,RA                                                       
*                                                                               
         OC    PREVKEY,PREVKEY     SEE IF CONTINUATION                          
         BNZ   PROCESS            NOTE-SHOULD ONLY HAPPEN IF READING            
*                                 BUYS OR CLIENTS (IN GETNCLT)                  
*                                                                               
         CLI   PVSW,1              SEE IF ALL HEADS PREV VAL                    
         BE    DISPLAY             HAVE ALREADY READ DATA                       
*                                  CAN GO TO DISPLAY LOGIC                      
PROCESS  LA    R2,ACCMLEN*6/8                                                   
         LA    R3,BILLG            CLEAR ACCUMS                                 
*                                                                               
PROC1    ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R2,PROC1                                                         
*                                                                               
PROC4    DS    0H                  BUILD RMLIST                                 
*                                                                               
         XC    RMLIST,RMLIST                                                    
         MVC   RMLIST(2),SVSTRTB   START OF LIST OF YM FOR DISPLAY              
*                                                                               
         LA    R3,12               MAX TWELVE MONTHS IN LIST                    
         SR    R2,R2                                                            
         LA    R4,RMLIST                                                        
*                                                                               
PROC5    IC    R2,1(R4)            GET PREVIOUS MONTH                           
         LA    R2,1(R2)            BUMP TO NEXT MONTH                           
*                                                                               
         CH    R2,=H'12'           IF SAME YEAR                                 
         BH    PROC6                                                            
*                                                                               
         STC   R2,3(R4)               ADD TO LIST                               
         MVC   2(1,R4),0(R4)          SAME YEAR                                 
         B     PROC8                                                            
*                                                                               
PROC6    LA    R2,1                NEW JAN                                      
         STC   R2,3(R4)                                                         
         IC    R2,0(R4)                                                         
         LA    R2,1(R2)            NEXT YEAR                                    
         STC   R2,2(R4)                                                         
*                                                                               
PROC8    CLC   2(2,R4),SVENDB      SKIP IF AFTER REQUEST RANGE                  
         BH    PROC10                                                           
*                                                                               
PROC9    DS    0H                                                               
*                                                                               
         LA    R4,2(R4)            BUMP TO NEXT POSITION IN LIST                
         BCT   R3,PROC5                                                         
         B     PROC20              END OF LIST                                  
*                                                                               
PROC10   DS    0H                                                               
*                                                                               
         MVI   2(R4),0             ELSE CLEAR                                   
         B     PROC20              DONE                                         
*                                                                               
PROC20   DS    0H                                                               
*                                                                               
         TITLE 'PPFIS01 - PRINTPAK FIS REC PROCESS - RBUCS'                     
*                                                                               
         CLC   SVPUB(3),=C'ALL'       SEE IF DOING ALL PUBS                     
         BNE   RBUYS               NO CAN'T USE BUCKETS                         
*                                                                               
         CLI   COS2SW,C'Y'         COS2 OR OPEN OPTION IS ON?                   
         JE    RBUYS                                                            
*                                                                               
RBUCS    DS    0H                  READ BUCKETS FOR ORDERED + PAID              
*                                                                               
*        READ BUCKET RECORDS                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PBKKEY,R4           ESTABLISH BUCKET RECORD KEY                  
*                                                                               
         MVC   PBKKAGY,AGYALPHA    SET AGENCY                                   
         MVC   PBKKMED,SVMED       SET MEDIA                                    
         MVI   PBKKRCD,X'09'       SET RECORD CODE                              
         MVC   PBKKCLT,SVCLT       SET CLIENT                                   
*                                                                               
         CLC   SVPRD,=C'ALL'       SET PRODUCT IF DOING 1 PRODUCT               
         BE    *+10                                                             
         MVC   PBKKPRD,SVPRD                                                    
*                                                                               
         CLC   SVEST,=C'ALL'       SET ESTIMATE IF DOING 1 ESTIMATE             
         BE    *+10                                                             
         MVC   PBKKEST,SVESTB                                                   
*                                                                               
         XC    WORKPRD,WORKPRD     INIT CURRENT PRODUCT CODE                    
*                                                                               
RBK2     BAS   RE,HIGH             READ FIRST RECORD                            
         B     RBK5                                                             
*                                                                               
RBK3     BAS   RE,SEQ              READ NEXT BUCKET RECORD                      
*                                                                               
RBK5     DS    0H                                                               
*                                                                               
         LA    R4,KEY              RE-POINT TO KEY AREA                         
*                                                                               
         CLC   KEY(PBKKPRD-PBKKEY),KEYSAVE      END OF CLT                      
         BNE   RBILLS              YES - GO READ BILLS                          
*                                                                               
         GOTO1 GETFACT,DMCB,0      MAKE SURE WE AREN'T OVER IO LIMIT            
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLC   FATIOCNT,SVMAXIO                                                 
         BNL   STOP                                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         CLI   PBKKPRD,C'*'        SKIP AOR PRDS                                
         BE    RBK3                                                             
*                                                                               
         CLC   SVPRD,=C'ALL'                                                    
         BE    RBK7                                                             
*                                                                               
         CLC   PBKKPRD,SVPRD       MUST MATCH A SINGLE PRODUCT                  
         BNE   RBILLS              DONE                                         
*                                                                               
RBK7     CLC   SVEST,=C'ALL'                                                    
         BE    RBK9                                                             
*                                                                               
         CLC   PBKKEST,SVESTB      FILTER ON ESTIMATE                           
         BE    RBK9                KEEP IF EQUAL                                
         BH    RBK8                                                             
         MVC   PBKKEST,SVESTB      RESET ESTIMATE IF LOW                        
         B     RBK2                READ HIGH FOR THIS EST                       
*                                                                               
RBK8     MVC   PBKKEST,=2X'FF'     HIGH - GET NEXT PRD                          
         B     RBK2                                                             
*                                                                               
RBK9     DS    0H                                                               
*                                                                               
         CLC   PBKKPRD,WORKPRD     SKIP IF NOT NEW PRD                          
         BE    RBKPRDX                                                          
*                                                                               
         XC    WORKPBLD,WORKPBLD   INIT PRODUCT BILLING EFFECTIVE DATE          
*                                                                               
*        MUST READ PRDREC TO GET BILLING EFFECTIVE DATE                         
*                                                                               
         MVC   ESAVKEY,KEY         SAVE CURRENT BUCKET KEY                      
*                                                                               
*                                  DIFFERS IN CODE ONLY FROM BUCKET KEY         
*                                  AND NO ESTIMATE NUMBER                       
*                                                                               
         MVI   PBKKRCD,X'06'       SET THE RECORD CODE                          
         XC    PBKKEST,PBKKEST     ELIMINATE ESTIMATE CODE                      
*                                                                               
         BAS   RE,HIGH             READ PRODUCT  RECORD                         
*                                                                               
         CLC   PBKKEY,KEYSAVE      MUST FIND PRODUCT                            
         BNE   RBKPRDNF            PRD NOT FOUND SKIP BUCKET                    
*                                                                               
         BAS   RE,GETREC           READ IN PRODUCT  RECORD                      
*                                                                               
         MVC   KEY,ESAVKEY         RESTORE CURRENT BUCKET KEY                   
         BAS   RE,HIGH             RESTORE SEQ READ                             
*                                                                               
         B     RBKPRDFD                                                         
*                                                                               
RBKPRDNF DS    0H                  GET HERE IF PRD NOT ON FILE                  
*                                                                               
         MVC   KEY,ESAVKEY         RESTORE CURRENT BUCKET KEY                   
         BAS   RE,HIGH             RESTORE SEQ READ                             
*                                                                               
         B     RBK3                READ NEXT BUCKET                             
*                                                                               
RBKPRDFD DS    0H                                                               
*                                                                               
         MVC   WORKPRD,PBKKPRD     SAVE PRODUCT CODE                            
*                                                                               
         MVI   ELCODE,PPBPCECQ     LOOK FOR BILLING EFD ELEMENT                 
         LA    R2,PPRDELEM         SEARCH PRODUCT  RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RBKPRDBX            NONE                                         
*                                                                               
         USING PPRDBPCE,R2         ESTABLISH BILLING EFD ELEMENT                
*                                                                               
         MVC   WORKPBLD,PPBPCEFF   SAVE PRODUCT BILLING EFD                     
*                                                                               
         DROP  R2                                                               
*                                                                               
RBKPRDBX DS    0H                                                               
*                                                                               
RBKPRDX  DS    0H                                                               
*                                                                               
         MVC   ESAVKEY,KEY                                                      
         MVI   PBKKRCD,X'07'       READ ESTIMATE TO SEE IF TEST                 
*                                  AND GET ACTUALIZATION DATE                   
*                                  CHANGE RECORD CODE TO ESTIMATE               
         CLC   PESTREC(L'PESTKEY),KEY IF I HAVE RECORD                          
         BNE   RBK9B                                                            
*                                                                               
         MVC   KEY,ESAVKEY               RESTORE BUCKET KEY                     
         B     RBK9E                     JUST CHECK ESTIMATE                    
*                                                                               
RBK9B    BAS   RE,HIGH             READ ESTIMATE                                
*                                                                               
         CLC   KEY(L'PESTKEY),KEYSAVE     CONTINUE IF ESTIMATE FOUND            
         BE    RBK9C                                                            
*                                                                               
         MVC   KEY,ESAVKEY         RESTORE BUCKET KEY                           
*                                                                               
         BAS   RE,HIGH             RESET FILE POINTERS                          
*                                                                               
         B     RBK3                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
RBK9C    DS    0H                                                               
*                                                                               
         LA    R0,ESTREC           READ ESTIMATE RECORD                         
         ST    R0,AREC             INTO ESTIMATE SAVEAREA                       
         BAS   RE,GETREC                                                        
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
*                                                                               
         MVC   KEY,ESAVKEY         RESTORE BUCKET KEY                           
         BAS   RE,HIGH             RESTORE FILE POINTERS                        
*                                                                               
RBK9E    DS    0H                                                               
*                                                                               
         CLI   TESTSW,C'Y'         SKIP IF DOING TEST ESTIMATES                 
         BE    RBK10                                                            
*                                                                               
         TM    PESTTEST,X'80'                                                   
         BNZ   RBK3                TEST EST SO SKIP                             
*                                                                               
RBK10    DS    0H                  PROCESS BUCKET                               
*                                                                               
         XC    WORKEBLD,WORKEBLD   INIT BILLING EFFECTIVE DATES                 
         XC    WORKEALD,WORKEALD   INIT ACTUALIZATION     DATES                 
*                                                                               
         MVI   ELCODE,PEACTECQ     LOOK FOR ACTUALIZATION ELEMENT               
         LA    R2,PESTELEM         SEARCH ESTIMATE RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RBK10Y              NONE                                         
*                                                                               
         USING PESTACTD,R2         ESTABLISH ACTUALIZATION ELEMENT              
*                                                                               
         MVC   WORKEALD,PEACTDAT   SAVE ACTUALIZATION DATE                      
*                                                                               
         DROP  R2                                                               
*                                                                               
RBK10Y   DS    0H                                                               
*                                                                               
         MVI   ELCODE,PEBPCECQ     LOOK FOR BILLING EFD ELEMENT                 
         LA    R2,PESTELEM         SEARCH ESTIMATE RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RBK10Z              NONE                                         
*                                                                               
         USING PESTBPCE,R2         ESTABLISH BILLING EFD ELEMENT                
*                                                                               
         MVC   WORKEBLD,PEBPCEFF   SAVE BILLING EFD                             
*                                                                               
         DROP  R2                                                               
*                                                                               
RBK10Z   DS    0H                                                               
*                                                                               
RBK11    DS    0H                                                               
*                                                                               
         BAS   RE,GETREC           READ IN BUCKET RECORD                        
*                                                                               
         MVI   ELCODE,X'22'        FIND BILL MONTH BUCKET ELEMENT               
         L     R2,AREC             POINT TO FIRST ELEMENT IN RECORD             
         LA    R2,33(R2)                                                        
         USING BKELEM,R2                                                        
*                                                                               
         CLI   0(R2),X'22'         SKIP IF FIRST IS FOR BILL MONTH              
         BE    RBK15                                                            
*                                                                               
RBK12    BAS   RE,NEXTEL           FIND NEXT BILL MONTH ELEMENT                 
         BNE   RBK25                                                            
*                                                                               
RBK15    LA    R3,BKYM             CHECK IF MONTH IN MONTH LIST                 
         BAS   RE,FINDISP          ALSO FINDS MONTH'S BUCKET                    
         BNE   RBK12               DISPLACEMENT                                 
*                                                                               
RBK20    L     R6,DISP                                                          
*                                                                               
*        UPDATE PAID BUCKETS                                                    
*                                                                               
         LA    R5,BILLG            START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) PAID BUCKET TO BE UPDATED                    
         AP    0(8,R5),BKPGRS      UPDATE PAID GROSS                            
*                                                                               
         LA    R5,BILLN            START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) BUCKET TO BE UPDATED                         
         AP    0(8,R5),BKPNET      UPDATE PAID NET                              
*                                                                               
         LA    R5,BILLCD           START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) BUCKET TO BE UPDATED                         
         AP    0(8,R5),BKPCD       UPDATE PAID CD                               
*                                                                               
*        UPDATE ORDERED BUCKETS                                                 
*                                                                               
         CLI   PLNCSTSW,C'P'       SKIP IF NOT DOING PLANNED COSTS              
         BNE   RBK23                                                            
*                                                                               
*        FIND BILLING EFFECTIVE DATE                                            
*                                                                               
         CLI   WORKPCYN,C'Y'       IF NOT DOING PC BILLING                      
         BNE   RBK23                  USE ORDERED DOLLARS                       
*                                                                               
         LA    RF,WORKEBLD         ASSUME ESTIMATE LEVEL DATE                   
*                                                                               
         OC    0(2,RF),0(RF)       IF NO ESTIMATE BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKPBLD            ASSUME PRODUCT  LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       IF NO PRODUCT  BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKCBLD            ASSUME CLIENT   LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       IF NO BILLING EFFECTIVE DATE                 
         BZ    RBK23                  USE ORDERED DOLLARS                       
*                                                                               
         CLC   BKYM,0(RF)          IF BUCKET MONTH IS BEFORE BEFD               
         BL    RBK23                  USE ORDERED DOLLARS                       
*                                                                               
         OC    WORKEALD,WORKEALD   IF NO ACTUALIZATION DATE                     
         BZ    RBK24                  SKIP ORDERED DOLLARS                      
*                                                                               
         CLC   BKYM,WORKEALD       IF AFTER ACTUALIZATION DATE                  
         BH    RBK24                  SKIP ORDERED DOLLARS                      
*                                                                               
RBK23    DS    0H                                                               
*                                                                               
         LA    R5,BILLG            START OF BUCKETS                             
         LA    R5,0(R6,R5)         BUCKET TO UPDATE                             
         AP    0(8,R5),BKOGRS      UPDATE GROSS ORDERED                         
*                                                                               
         LA    R5,BILLN                                                         
         LA    R5,0(R6,R5)         BUCKET TO UPDATE                             
         AP    0(8,R5),BKONET      UPDATE NET ORDERED                           
*                                                                               
         LA    R5,BILLCD                                                        
         LA    R5,0(R6,R5)                                                      
         AP    0(8,R5),BKOCD       CD ORDERED                                   
*                                                                               
RBK24    DS    0H                                                               
*                                                                               
         B     RBK12               GO DO NEXT ELEM                              
*                                                                               
         DROP  R2                                                               
RBK25    DS    0H                                                               
         L     R2,AREC             POINT TO FIRST ELEMENT IN RECORD             
         LA    R2,33(R2)                                                        
         USING BKELEM,R2                                                        
         MVI   ELCODE,X'23'                                                     
         CLI   0(R2),X'23'                                                      
         BE    RBK27                                                            
RBK26    BAS   RE,NEXTEL                                                        
         BNE   RBK30                                                            
RBK27    CLI   BKTYPE,C'I'         SEE IF INS MTH                               
         BNE   RBK26                                                            
         LA    R3,BKYM                                                          
         BAS   RE,FINDISP                                                       
         BNE   RBK26                                                            
         L     R6,DISP                                                          
*                                                                               
*        UPDATE PAID BUCKETS                                                    
*                                                                               
         LA    R5,INSG             START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) PAID BUCKET TO BE UPDATED                    
         AP    0(8,R5),BKPGRS      UPDATE PAID GROSS                            
*                                                                               
         LA    R5,INSN             START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) BUCKET TO BE UPDATED                         
         AP    0(8,R5),BKPNET      UPDATE PAID NET                              
*                                                                               
         LA    R5,INSCD            START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) BUCKET TO BE UPDATED                         
         AP    0(8,R5),BKPCD       UPDATE PAID CD                               
*                                                                               
*        UPDATE ORDERED BUCKETS                                                 
*                                                                               
         CLI   PLNCSTSW,C'P'       IF NOT DOING PLANNED COSTS                   
         BNE   RBK28                  USE ORDERED DOLLARS                       
*                                                                               
*        FIND BILLING EFFECTIVE DATE                                            
*                                                                               
         CLI   WORKPCYN,C'Y'       IF NOT USING PC BILLING                      
         BNE   RBK28                  USE ORDERED DOLLARS                       
*                                                                               
         LA    RF,WORKEBLD         ASSUME ESTIMATE LEVEL DATE                   
*                                                                               
         OC    0(2,RF),0(RF)       IF NO ESTIMATE BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKPBLD            ASSUME PRODUCT  LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       IF NO PRODUCT  BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKCBLD            ASSUME CLIENT   LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       IF NO BILLING EFFECTIVE DATE                 
         BZ    RBK28                  USE ORDERED DOLLARS                       
*                                                                               
         CLC   BKYM,0(RF)          IF BUCKET MONTH IS BEFORE BEFD               
         BL    RBK28                  USE ORDERED DOLLARS                       
*                                                                               
         OC    WORKEALD,WORKEALD   IF NO ACTUALIZATION DATE                     
         BZ    RBK29                  SKIP ORDERED DOLLARS                      
*                                                                               
         CLC   BKYM,WORKEALD       IF AFTER ACTUALIZATION DATE                  
         BH    RBK29                  SKIP ORDERED DOLLARS                      
*                                                                               
RBK28    DS    0H                                                               
*                                                                               
         LA    R5,INSG             START OF BUCKETS                             
         LA    R5,0(R6,R5)         BUCKET TO UPDATE                             
         AP    0(8,R5),BKOGRS      UPDATE GROSS ORDERED                         
*                                                                               
         LA    R5,INSN                                                          
         LA    R5,0(R6,R5)         BUCKET TO UPDATE                             
         AP    0(8,R5),BKONET      UPDATE NET ORDERED                           
*                                                                               
         LA    R5,INSCD                                                         
         LA    R5,0(R6,R5)                                                      
         AP    0(8,R5),BKOCD       CD ORDERED                                   
*                                                                               
RBK29    DS    0H                                                               
*                                                                               
         B     RBK26               GO DO NEXT 23 ELEM                           
*                                                                               
RBK30    DS    0H                                                               
*                                                                               
         CLI   PLNCSTSW,C'P'       DONE IF NOT DOING PLANNED COSTS              
         BNE   RBK40                                                            
*                                                                               
         CLI   WORKPCYN,C'Y'       DONE IF NOT DOING PC BILLING                 
         BNE   RBK40                                                            
*                                                                               
*        HANDLE PLANNED COSTS BUCKETS                                           
*                                                                               
         MVI   ELCODE,X'42'        FIND REGULAR PLANNED COST BUCKETS            
         L     R2,AREC             POINT TO FIRST ELEMENT IN RECORD             
         LA    R2,33(R2)                                                        
         USING BKELEM,R2                                                        
*                                                                               
         CLI   0(R2),X'42'                                                      
         BE    RPC15                                                            
*                                                                               
RPC12    BAS   RE,NEXTEL                                                        
*                                                                               
         BNE   RPC25               NO MORE ELEMENTS                             
*                                                                               
RPC15    LA    R3,BKYM                                                          
*                                                                               
         BAS   RE,FINDISP                                                       
         BNE   RPC12                                                            
*                                                                               
RPC20    DS    0H                                                               
*                                                                               
*        FIND BILLING EFFECTIVE DATE                                            
*                                                                               
         LA    RF,WORKEBLD         ASSUME ESTIMATE LEVEL DATE                   
*                                                                               
         OC    0(2,RF),0(RF)       IF NO ESTIMATE BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKPBLD            ASSUME PRODUCT  LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       IF NO PRODUCT  BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKCBLD            ASSUME CLIENT   LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       SKIP IF NO BILLING EFD                       
         BZ    RPC24                                                            
*                                                                               
         CLC   BKYM,0(RF)          IF BUCKET MONTH IS AFTER BEFD                
         BL    RPC24                                                            
         CLC   BKYM,WORKEALD       AND AFTER ACTUALIZATION DATE                 
         BNH   RPC24                  USE PLANNED COST DOLLARS                  
*                                                                               
         L     R6,DISP             DISPLACEMENT INTO INTERNAL BUCKETS           
*                                                                               
         LA    R5,BILLG            START OF BUCKETS                             
         LA    R5,0(R6,R5)         MONTH BUCKET TO BE UPDATED                   
         AP    0(8,R5),BKOGRS         UPDATE GROSS ORDERED                      
*                                                                               
         LA    R5,BILLN                                                         
         LA    R5,0(R6,R5)                                                      
         AP    0(8,R5),BKONET         UPDATE NET ORDERED                        
*                                                                               
         LA    R5,BILLCD                                                        
         LA    R5,0(R6,R5)                                                      
         AP    0(8,R5),BKOCD       CD ORDERED                                   
*                                                                               
RPC24    DS    0H                                                               
*                                                                               
         B     RPC12               GO DO NEXT ELEM                              
*                                                                               
         DROP  R2                                                               
*                                                                               
RPC25    DS    0H                                                               
*                                                                               
         L     R2,AREC             POINT TO FIRST ELEMENT IN RECORD             
         LA    R2,33(R2)                                                        
         USING BKELEM,R2                                                        
*                                                                               
         MVI   ELCODE,X'43'        PLANNED COST INSERTION MONTH BUCKETS         
         CLI   0(R2),X'43'                                                      
         BE    RPC27                                                            
*                                                                               
RPC26    BAS   RE,NEXTEL                                                        
         BNE   RPC30                                                            
*                                                                               
RPC27    CLI   BKTYPE,C'I'         SEE IF INS MTH                               
         BNE   RPC29                                                            
*                                                                               
         LA    R3,BKYM                                                          
         BAS   RE,FINDISP                                                       
         BNE   RPC29               NO MORE ELEMENTS                             
*                                                                               
*        FIND BILLING EFFECTIVE DATE                                            
*                                                                               
         LA    RF,WORKEBLD         ASSUME ESTIMATE LEVEL DATE                   
*                                                                               
         OC    0(2,RF),0(RF)       IF NO ESTIMATE BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKPBLD            ASSUME PRODUCT  LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       IF NO PRODUCT  BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKCBLD            ASSUME CLIENT   LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       SKIP IF NO BILLING EFD                       
         BZ    RPC29                                                            
*                                                                               
         CLC   BKYM,0(RF)          IF BUCKET MONTH IS AFTER BEFD                
         BL    RPC29                                                            
         CLC   BKYM,WORKEALD       AND AFTER ACTUALIZATION DATE                 
         BNH   RPC29                  USE PLANNED COST DOLLARS                  
*                                                                               
         L     R6,DISP                                                          
         LA    R5,INSG                                                          
         AR    R5,R6                                                            
*                                                                               
         AP    0(8,R5),BKOGRS      UPDATE GROSS ORDERED                         
*                                                                               
         LA    R5,INSN                                                          
         AR    R5,R6                                                            
         AP    0(8,R5),BKONET                                                   
*                                                                               
         LA    R5,INSCD                                                         
         AR    R5,R6                                                            
         AP    0(8,R5),BKOCD                                                    
*                                                                               
RPC29    DS    0H                                                               
*                                                                               
         B     RPC26               GO DO NEXT 43 ELEM                           
*                                                                               
RPC30    DS    0H                                                               
*                                                                               
RBK40    B     RBK3                GO DO NEXT BUCKET REC                        
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
RBILLS   DS    0H                  READ BILL RECS                               
*                                  POST TO BILLING MTH ACCUMS                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'08'                                                      
         MVC   KEY+4(3),SVCLT                                                   
         CLC   SVPRD,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   KEY+7(3),SVPRD                                                   
         CLC   SVEST,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   KEY+10(2),SVESTB                                                 
RBL2     BAS   RE,HIGH                                                          
         B     RBL5                                                             
*                                                                               
RBL3     BAS   RE,SEQ                                                           
RBL5     CLC   KEY(7),KEYSAVE                                                   
         BNE   WRBUCS              END OF CLIENT NOW WRBUCS                     
**NEW 4/5/91                                                                    
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLC   FATIOCNT,SVMAXIO                                                 
         BNL   STOP                                                             
         DROP  R1                                                               
*                                                                               
         CLC   SVPRD,=C'ALL'                                                    
         BE    RBL7                                                             
         CLC   KEY+7(3),SVPRD                                                   
         BNE   WRBUCS                                                           
*                                                                               
RBL7     CLC   SVEST,=C'ALL'                                                    
         BE    RBL10                                                            
         CLC   KEY+10(2),SVESTB                                                 
         BE    RBL10                                                            
         BH    RBL8                                                             
         MVC   KEY+10(2),SVESTB                                                 
         XC    KEY+12(13),KEY+12        MUST CLEAR MOS ETC                      
         B     RBL2                                                             
*                                                                               
RBL8     MVC   KEY+10(2),=2X'FF'        PRODUCT                                 
         B     RBL2                                                             
*                                                                               
RBL10    OC    KEY+10(2),KEY+10    SKIP BILLS FOR EST 000                       
         BZ    RBL3                                                             
*                                                                               
         LA    R3,KEY+12           MOS                                          
         BAS   RE,FINDISP                                                       
         BNE   RBL3                OUT OF PERIOD                                
***                                                                             
*** NO NEED TO CHK FOR TEST ESTS AS THEY CAN'T HAVE BILLS                       
***                                                                             
         BAS   RE,GETREC                                                        
         CLI   PBRETAIL,X'41'      SKIP CORP SUMMARY BILLS                      
         BE    RBL3                                                             
*                                                                               
*        USE PPBVAL TO GET "EFFECTIVE" VALUES AND SET THEM INTO RECORD          
*                                                                               
         GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
*                                                                               
         MVC   PBILLGRS,PPBVEBG     SET GROSS TO EFFECTIVE                      
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                                                               
         L     R6,DISP                                                          
         LA    R5,BILLG                                                         
         LA    R5,ACCMLEN/3*2(R6,R5)                                            
         AP    0(8,R5),PBILLGRS                                                 
         LA    R5,BILLCD                                                        
         LA    R5,ACCMLEN/3*2(R6,R5)                                            
         AP    0(8,R5),PPBVEBC     EFFECTIVE CD                                 
         AP    PBILLNET,PPBVEBC   +EFFECTIVE CD = NET                           
         LA    R5,BILLN                                                         
         LA    R5,ACCMLEN/3*2(R6,R5)                                            
         AP    0(8,R5),PBILLNET                                                 
         B     RBL3                GO DO NEXT BILL                              
         EJECT                                                                  
RBUYS    DS    0H                  DOING ONE PUB - SO READ BUYS                 
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         OC    PREVKEY,PREVKEY     SEE IF CONTINUATION                          
         BZ    RB0                 NO                                           
*                                                                               
         LA    R3,1            READ BUCKETS FOR TWA                             
         LA    R4,BILLG        BUFFER ADDR                                      
         BAS   RE,RDTEMP       READ FROM TEMPSTR                                
         MVC   WORKCLT,PREVKEY+4                                                
         CLI   PREVKEY+3,X'02'    SEE IF PREVKEY IS ON A CLIENT                 
         BE    RB04               COULD HAPPEN IF MAXIO IS REACHED              
*                                 IN GETNCLT                                    
*                                                                               
*                                                                               
         MVC   KEY,PREVKEY                                                      
         LA    R7,15       FOR KEYCOMP                                          
         CLC   SVPRD,=C'ALL'                                                    
         BNE   *+8                                                              
         LA    R7,12                                                            
         B     RB1                 IF NOT MUST BE A BUY                         
*                                  RESET R7 AND CONTINUE READING                
RB0      MVC   WORKCLT,SVCLT                                                    
         CLI   SVCLT,C'&&'         CHK FOR BILLING GROUP                        
         BE    RB03                                                             
         CLI   SVCLT,C'*'          CHK FOR OFFICE                               
         BE    RB03                                                             
         CLI   SVCLT,C'$'          CHK FOR OFFICE LIST                          
         BE    RB03                                                             
         CLC   SVCLT,=C'ALL'                                                    
         BNE   RB05                                                             
RB03     XC    WORKCLT,WORKCLT                                                  
*                                                                               
RB04     BAS   RE,GETNCLT          GET FIRST CLIENT                             
         CLC   WORKCLT,=X'FFFFFF'   MEANS MAX I/O'S WAS REACHED                 
         BE    BUYSTOP             IN GETNCLT - MUST STOP                       
*                                                                               
         OC    WORKCLT,WORKCLT                                                  
         BZ    RBXX                NONE FOUND                                   
*                                                                               
RB05     DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'20'         USE 20 POINTERS                              
         MVC   KEY+4(3),WORKCLT                                                 
         MVC   KEY+7(3),SVPRD                                                   
*                                                                               
         CLC   =C'ALL',SVPUB       ALL PUB?                                     
         JNE   *+12                                                             
         LA    R7,10               FOR KEY COMPARE                              
         J     RB07                                                             
*                                                                               
         MVC   DUB(6),SVPUB                                                     
*                                                                               
         CLI   SVPUB+4,X'FF'       SEE IF DOING ALL ZONES/EDTS                  
         BNE   *+10                                                             
         XC    DUB+4(2),DUB+4                                                   
*                                                                               
         MVC   KEY+10(6),DUB                                                    
*                                                                               
         LA    R7,15               FOR KEY COMP                                 
*                                                                               
RB07     CLC   SVPRD,=C'ALL'                                                    
         BNE   RB1                                                              
*                                                                               
         MVI   KEY+3,X'21'         USE 21 POINTERS FOR ALL PRDS                 
         MVC   KEY+7(6),DUB        PUB                                          
         XC    KEY+13(3),KEY+13                                                 
         LA    R7,12               FOR KEY COMP                                 
*                                                                               
RB1      DS    0H                                                               
*                                                                               
         CLI   SVPUB+4,X'FF'       CHK FOR ALL ZONES/EDTS                       
         BNE   *+8                                                              
         BCTR  R7,0                COMPARE ONLY 8 DIGIT PUB                     
         BCTR  R7,0                                                             
*                                                                               
         BAS   RE,HIGH                                                          
*                                                                               
         B     RB4                                                              
*                                                                               
RB3      BAS   RE,SEQ                                                           
*                                                                               
RB4      EX    R7,KEYCOMP                                                       
         BNE   RBX                 END OF PUB/PRD OR PRD/PUB                    
**NEW 4/5/91                                                                    
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLC   FATIOCNT,SVMAXIO                                                 
         BNL   BUYSTOP                                                          
         DROP  R1                                                               
*                                                                               
RB6      CLC   SVEST,=C'ALL'                                                    
         BE    RB7                                                              
*                                                                               
         CLC   KEY+19(2),SVESTB                                                 
         BE    RB7                                                              
*                                                                               
         B     RB3                 MUST USE SEQ READ                            
*                                                                               
KEYCOMP  CLC   KEYSAVE(0),KEY      EXECUTED                                     
*                                                                               
RB7      DS    0H                                                               
*                                                                               
         CLC   SVPRD,=C'ALL'   ONLY SKIP PASSIVE IF DOING ALL PRDS              
         BNE   RB8             SO ZZZ BUYS WILL SHOW UNDER THEIR                
*                              ALLOCATIONS                                      
         OC    KEY+21(3),KEY+21                                                 
         BNZ   RB3                 SKIP PASSIVE BUYS                            
*                                                                               
RB8      TM    KEY+25,X'C0'        SEE IF CLOSED OUT OR FF DELETE               
         BO    RB3                 YES SKIP                                     
*                                                                               
******   B     RB8B                SKIP READ OF ESTIMATE                        
*                                  I DON'T NEED TO KNOW ESTIMATE                
*                                  STATUS WHEN READING BUYS                     
*****    CLI   TESTSW,C'Y'                                                      
*****    BE    RB8B                                                             
*                                                                               
*        CHECK FOR NEW PRODUCT                                                  
*                                                                               
RBPRD    DS    0H                                                               
*                                                                               
         LA    RF,KEY+7            POINT TO PRODUCT CODE                        
*                                                                               
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    RF,KEY+13        SINCE 21 POINTERS HAVE PRD IN KEY+13            
*                                                                               
         CLC   0(3,RF),WORKPRD     SKIP IF NOT NEW PRD                          
         BE    RBPRDX                                                           
*                                                                               
         XC    WORKPBLD,WORKPBLD   INIT PRODUCT BILLING EFFECTIVE DATE          
*                                                                               
*        MUST READ PRDREC TO GET BILLING EFFECTIVE DATE                         
*                                                                               
         MVC   ESAVKEY,KEY         SAVE CURRENT BUY KEY                         
*                                                                               
         CLI   KEY+3,X'20'                                                      
         BE    *+10                                                             
         MVC   KEY+7(3),0(RF)   FILL IN PRODUCT FROM BUY KEY                    
*                                                                               
         MVI   KEY+3,X'06'         RECORD CODE                                  
         XC    KEY+10(22),KEY+10   CLEAR REST OF KEY                            
*                                                                               
         BAS   RE,HIGH             READ PRODUCT  RECORD                         
*                                                                               
         CLC   KEY(L'PPRDKEY),KEYSAVE MUST FIND PRODUCT                         
         BNE   RBPRDNF             PRD NOT FOUND SKIP BUCKET                    
*                                                                               
         BAS   RE,GETREC           READ IN PRODUCT  RECORD                      
*                                                                               
         MVC   KEY,ESAVKEY         RESTORE CURRENT BUY KEY                      
         BAS   RE,HIGH             RESTORE SEQ READ                             
*                                                                               
         B     RBPRDFD                                                          
*                                                                               
RBPRDNF DS     0H                  GET HERE IF PRD NOT ON FILE                  
*                                                                               
         MVC   KEY,ESAVKEY         RESTORE CURRENT BUCKET KEY                   
         BAS   RE,HIGH             RESTORE SEQ READ                             
*                                                                               
         B     RB3                 READ NEXT BUCKET                             
*                                                                               
RBPRDFD DS     0H                                                               
*                                                                               
         MVC   WORKPRD,PPRDKPRD    SAVE PRODUCT CODE                            
*                                                                               
         MVI   ELCODE,PPBPCECQ     LOOK FOR BILLING EFD ELEMENT                 
         LA    R2,PPRDELEM         SEARCH PRODUCT  RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RBPRDBX             NONE                                         
*                                                                               
         USING PPRDBPCE,R2         ESTABLISH BILLING EFD ELEMENT                
*                                                                               
         MVC   WORKPBLD,PPBPCEFF   SAVE PRODUCT BILLING EFD                     
*                                                                               
         DROP  R2                                                               
*                                                                               
RBPRDBX DS     0H                                                               
*                                                                               
RBPRDX   DS    0H                                                               
*                                                                               
*        RESD ESTIMATE RECORD                                                   
*                                                                               
         MVC   ESAVKEY,KEY                                                      
         MVC   KEY+10(2),KEY+19                                                 
*                                                                               
         CLI   KEY+3,X'20'                                                      
         BE    *+10                                                             
         MVC   KEY+7(3),KEY+13  SINCE 21 POINTERS HAVE PRD IN KEY+13            
*                                                                               
         MVI   KEY+3,X'07'                                                      
         XC    KEY+12(20),KEY+12                                                
*                                                                               
         CLC   PESTREC(12),KEY          SEE IF I ALREADY HAVE                   
         BNE   RB80                                                             
         MVC   KEY,ESAVKEY              RESTORE KEY                             
         B     RB8A5                    CAN SKIP READS                          
*                                                                               
RB80     BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(12),KEYSAVE                                                  
         BE    RB8A                                                             
*                                                                               
         TM    ESAVKEY+25,X'80'     SEE IF BUY IS DELETED                       
         BNZ   *+6                                                              
         DC    H'0'               LIVE BUY ON MISSING ESTIMATE                  
         MVC   KEY,ESAVKEY                                                      
         BAS   RE,HIGH                                                          
         B     RB3                SKIP THE BUY                                  
*                                                                               
RB8A     DS    0H                                                               
         LA    R0,ESTREC                                                        
         ST    R0,AREC                                                          
         BAS   RE,GETREC                                                        
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         MVC   KEY,ESAVKEY                                                      
         BAS   RE,HIGH                                                          
*                                                                               
RB8A5    TM    PESTTEST,X'80'     SEE IF TEST EST                               
         BO    RB3                 YES-SKIP                                     
*                                                                               
         XC    WORKEBLD,WORKEBLD   INIT BILLING EFFECTIVE DATES                 
         XC    WORKEALD,WORKEALD   INIT ACTUALIZATION     DATES                 
*                                                                               
         MVI   ELCODE,PEACTECQ     LOOK FOR ACTUALIZATION ELEMENT               
         LA    R2,PESTELEM         SEARCH ESTIMATE RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RB10Y              NONE                                          
*                                                                               
         USING PESTACTD,R2         ESTABLISH ACTUALIZATION ELEMENT              
*                                                                               
         MVC   WORKEALD,PEACTDAT   SAVE ACTUALIZATION DATE                      
*                                                                               
         DROP  R2                                                               
*                                                                               
RB10Y    DS    0H                                                               
*                                                                               
         MVI   ELCODE,PEBPCECQ     LOOK FOR BILLING EFD ELEMENT                 
         LA    R2,PESTELEM         SEARCH ESTIMATE RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RB10Z               NONE                                         
*                                                                               
         USING PESTBPCE,R2         ESTABLISH BILLING EFD ELEMENT                
*                                                                               
         MVC   WORKEBLD,PEBPCEFF   SAVE BILLING EFD                             
*                                                                               
         DROP  R2                                                               
*                                                                               
RB10Z    DS    0H                                                               
*                                                                               
RB8B     BAS   RE,GETREC           READ-IN BUY                                  
*                                                                               
**TESTBUY**                                                                     
         CLI   TESTSW,C'Y'         OK TO READ TEST BUYS                         
         BE    RB8B5                                                            
         CLI   PBDBFD,C'T'         SKIP TEST BUYS                               
         BE    RB3                                                              
**TESTBUY**                                                                     
RB8B5    OC    SVREP,SVREP         SEE IF DOING A SPECIAL REP                   
         BZ    RB8K                                                             
         MVI   ELCODE,X'80'        LOOK FOR SPECIAL REP ELEM                    
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'80'                                                      
         BE    RB8E                                                             
RB8C     BAS   RE,NEXTEL                                                        
         BNE   RB3                 SKIP THIS BUY                                
RB8E     CLC   2(4,R2),SVREP       SEE IF REPS MATCH                            
         BE    RB8K                YES - PROCESS                                
         B     RB3                 SKIP THIS BUY                                
*                                                                               
RB8K     LA    R3,KEY+7            POINT TO PRODUCT                             
         CLI   KEY+3,X'20'                                                      
         BE    RB9                                                              
         LA    R3,KEY+13                                                        
*                                                                               
RB9      DS    0X                                                               
*                                                                               
         SR    R0,R0               INIT TYPE PARM FOR GETINS                    
*                                                                               
         CLI   PLNCSTSW,C'P'       SKIP IF NOT PLANNED COSTS                    
         BNE   RB23                                                             
*                                                                               
         CLI   WORKPCYN,C'Y'       SKIP IF CLIENT NOT DOING PC BILLING          
         BNE   RB23                                                             
*                                                                               
*        FIND BILLING EFFECTIVE DATE                                            
*                                                                               
         LA    RF,WORKEBLD         ASSUME ESTIMATE LEVEL DATE                   
*                                                                               
         OC    0(2,RF),0(RF)       IF NO ESTIMATE BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKPBLD            ASSUME PRODUCT  LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       IF NO PRODUCT  BILLING EFFECTIVE DTE         
         BNZ   *+8                                                              
         LA    RF,WORKCBLD            ASSUME CLIENT   LEVEL DATE                
*                                                                               
         OC    0(2,RF),0(RF)       IF NO BILLING EFFECTIVE DATE                 
         BZ    RB23                  USE ORDERED DOLLARS                        
*                                                                               
         CLC   PBUYKDAT(2),0(RF)   IF BUCKET MONTH IS BEFORE BEFD               
         BL    RB23                  USE ORDERED DOLLARS                        
*                                                                               
         OC    WORKEALD,WORKEALD   IF NO ACTUALIZATION DATE                     
         BZ    RB24                  USE PLANNED COST DOLLARS                   
*                                                                               
         CLC   PBUYKDAT(2),WORKEALD IF AFTER ACTUALIZATION DATE                 
         BH    RB24                  USE PLANNED COST DOLLARS                   
*                                                                               
         B     RB23                ELSE USE ORDERED DOLLARS                     
*                                                                               
RB24     DS    0H                                                               
*                                                                               
         LA    R0,C'P'             SET FOR PLANNED COSTS                        
*                                                                               
         CLI   COS2SW,C'Y'         COS2 OR OPEN OPTION IS ON?                   
         BNE   *+8                                                              
         LA    R0,C'B'             SET FOR PLANNED COSTS WITH COS2              
*                                                                               
         B     RB25                                                             
*                                                                               
RB23     DS    0H                                                               
*                                                                               
         CLI   COS2SW,C'Y'         COS2 OR OPEN OPTION IS ON?                   
         BNE   *+8                                                              
         LA    R0,C'O'                                                          
*                                                                               
RB25     DS    0H                                                               
*                                                                               
         GOTO1 VGETINS,DMCB,PBUYREC,((R0),GROSS),(R3),0,0,0                     
*                                                                               
         CLI   DMCB+4,C'X'         IF THERE ARE NO PLANNED COSTS                
         BNE   *+10                                                             
         XC    GROSS(12),GROSS        ZERO GROSS/AGYCOM/CSHDSC                  
*                                                                               
RB9V     TM    PBUYCNTL,X'80'      SEE IF DELETED                               
         BZ    RB12                                                             
         OC    PGROSS(16),PGROSS       SEE IF PAID                              
         BNZ   RB11                                                             
         OC    BGROSS(16),BGROSS                                                
         BNZ   RB11                SEE IF BILLED                                
         B     RB3                 NOT BILLED OR PAID -  IGNORE                 
RB11     XC    GROSS(20),GROSS     CLEAR GROSS FOR DELETED BUYS                 
RB12     LA    R3,KEY+16           INS DATE                                     
         BAS   RE,FINDISP                                                       
         BNE   RB15                OUT OF PERIOD                                
         L     R6,DISP                                                          
         LA    R4,INSG                                                          
         LA    R3,GROSS                                                         
         BAS   RE,POSTINS          POST ORD                                     
         LA    R4,ACCMLEN/3(R4)                                                 
         LA    R3,PGROSS           PAID                                         
         BAS   RE,POSTINS                                                       
         LA    R4,ACCMLEN/3(R4)                                                 
         LA    R3,BGROSS           BILLED                                       
         BAS   RE,POSTINS                                                       
*                                                                               
RB15     LA    R3,BLBLDT         POST TO BILL MTH ACCUMS                        
         BAS   RE,FINDISP                                                       
         BNE   RB3                 OUT OF PERIOD SKIP                           
         L     R6,DISP                                                          
         LA    R4,BILLG                                                         
         LA    R3,GROSS            ORDERED                                      
         BAS   RE,POSTINS                                                       
         LA    R4,ACCMLEN/3(R4)                                                 
         LA    R3,PGROSS           PAID                                         
         BAS   RE,POSTINS                                                       
         LA    R4,ACCMLEN/3(R4)                                                 
         LA    R3,BGROSS                                                        
         BAS   RE,POSTINS                                                       
*                                                                               
         B     RB3                 GO DO NEXT BUYREC                            
*                                                                               
RBX      DS    0H                                                               
         MVI   DMINBTS,X'C0'       RESET DMINBTS                                
         CLI   SVCLT,C'*'          CHK FOR OFFICE                               
         BE    RBX5                                                             
         CLI   SVCLT,C'&&'         CHK FOR BILLING GROUP                        
         BE    RBX5                                                             
         CLI   SVCLT,C'$'          CHK FOR OFFICE LIST                          
         BE    RBX5                                                             
         CLC   SVCLT,=C'ALL'       SEE IF DOING ALL CLTS                        
         BNE   RBXX                                                             
RBX5     DS    0H                                                               
*                                                                               
*        NOTE - AT THIS POINT KEY SHOULD CONTAIN KEY OF                         
*        THE RECORD THAT CAUSED THE CLIENT BREAK                                
*                                                                               
         BAS   RE,GETNCLT                                                       
         CLC   WORKCLT(3),=X'FFFFFF'   MEANS MAX I/O'S REACHED                  
         BE    BUYSTOP             IN GETNCLT -                                 
*                           NOTE - THERE IS A POTENTIAL FOR LOOPS               
*                                  IF THE REQUEST IS FOR AN OFFICE              
*                                  OR GROUP AND MAXIO IS REACHED                
*                                  BEFORE ANY CLIENT TO BE                      
*                                  PROCESSED IS FOUND                           
*                                  SYMTOM WILL BE USER KEEPS                    
*                                  HITTING ENTER AND WILL                       
*                                  NEVER REACH REQUEST PROCESED                 
*                                                                               
         XC    PREVKEY,PREVKEY     MUST CLEAR                                   
         OC    WORKCLT,WORKCLT                                                  
         BZ    RBXX                END OF CLIENTS                               
         B     RB05                                                             
*                                  WRITE BUCKETS TO TWA                         
RBXX     DS    0H                                                               
WRBUCS   LA    R3,1                PAGE 1                                       
         LA    R4,BILLG            BUFFER ADDR                                  
         BAS   RE,WRTEMP           WRITE TO TEMPSTR                             
         B     DP5                 GO DISPLAY - BYPASS READ OF TWA              
         EJECT                                                                  
DISPLAY  DS    0H                  READ BUCKETS FOR TWA                         
         LA    R3,1                PAGE 1                                       
         LA    R4,BILLG            BUFFER ADDR                                  
         BAS   RE,RDTEMP           READ FROM TEMPSTR                            
*                                                                               
DP5      DS    0H                                                               
         MVC   FISHD1(4),=C'BILL'                                               
         CLI   MTHSW,C'B'                                                       
         BE    *+10                                                             
         MVC   FISHD1(4),=C' INS'                                               
         FOUT  FISHD1H                                                          
         MVC   FISHD2(80),=CL80'MONTH    %PAID      ORDERED         PAIX        
               D       UNPAID       BILLED    BILLABLE '                        
         CLI   PLNCSTSW,C'P'       IF DOING PLANNED COSTS                       
         BNE   *+10                                                             
         MVC   FISHD2+20(7),=CL7'PLN/ORD'                                       
*                                                                               
         CLI   TESTSW,C'Y'         SEE IF DOING A TEST ESTIMATE                 
         BNE   DP8                                                              
*                                  CLEAR PAID AND BILLED COLUMNS                
         XC    FISHD2+30(49),FISHD2+30                                          
         XC    FISHD2+9(5),FISHD2+9    CLEAR %PAID ALSO                         
         B     DP10                                                             
*                                                                               
DP8      CLC   SVPUB(3),=C'ALL'                                                 
         BNE   DP10                                                             
         CLI   MTHSW,C'I'                                                       
         BNE   DP10                                                             
*                                  INS MTH + BUCKETS                            
*                                  NO BILLING DATA AVAILABLE                    
         XC    FISHD2+60(19),FISHD2+60                                          
DP10     FOUT  FISHD2H                                                          
         ZAP   TOTORD,=P'0'                                                     
         ZAP   TOTPD,=P'0'                                                      
         ZAP   TOTBLD,=P'0'                                                     
         LA    R3,FISOT01H              SET UP OUTPUT SCREEN                    
         LA    R1,RMLIST                PUT REQUEST LIST IN REG 1               
LPOUT    SR    R4,R4                    CLEAR REG 4                             
         IC    R4,0(R1)                 LOAD YEAR                               
         LTR   R4,R4                                                            
         BZ    TOTLP                                                            
         CVD   R4,DUB                                                           
         UNPK  YR,DUB                   PUT YEAR IN OUTPUT LINE                 
         OI    YR+1,X'F0'                                                       
         MVC   SLSH(1),=X'61'           PUT / IN OUTPUT LINE                    
         SR    R4,R4                                                            
         IC    R4,1(R1)                 LOAD MONTH                              
         MH    R4,=H'3'                 SET TABLE SIZE                          
         LA    R4,MONTAB(R4)                                                    
         MVC   MN(3),0(R4)              PUT ALPHA MONTH INTO OUTPUT LN          
         MVC   PP,=C' '                                                         
         CLC   0(2,R1),SVSTRTB                                                  
         BNE   LPOUT1                                                           
         CLI   PRSW,0                                                           
         BE    LPOUT2                                                           
         MVI   PP,C'P'                                                          
         B     LPOUT2                                                           
*                                                                               
LPOUT1   CLC   0(2,R1),SVENDB                                                   
         BNE   LPOUT2                                                           
         CLI   SUBSW,0                                                          
         BE    LPOUT2                                                           
         MVI   PP,C'S'                                                          
LPOUT2   LA    R5,RMLIST                  PUT REQUEST LIST INTO REG5            
         LR    R6,R1                    PUT REQUEST MONTH INTO REG 6            
         SR    R6,R5                    GET DIFFERENCE FOR RIGHT BUCKET         
         SLL   R6,2                     MULTIPLY BY 4 TO GET BUCKET LOC         
         EJECT                                                                  
*        BUCKET LOOP                                                            
BLKSET   DS    0H                                                               
         LA    R7,BILLG                                                         
         CLI   MTHSW,C'B'                                                       
         BE    *+8                                                              
         LA    R7,INSG                                                          
         CLI   DOLSW,C'1'          GROSS                                        
         BE    BLKSX                                                            
         CLI   DOLSW,C'2'          GROSS - CD                                   
         BNE   BLKS10                                                           
         LA    R5,ACCMLEN*2(R7)                                                 
BLKS3    LR    R4,R7                                                            
         LTR   R6,R6               SEE IF FIRST MONTH                           
         BNZ   BLKSX               NO - DON'T SUBTRACT AGAIN                    
         LA    R2,ACCMLEN/8                                                     
BLKS5    SP    0(8,R4),0(8,R5)                                                  
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R2,BLKS5                                                         
         B     BLKSX                                                            
*                                                                               
BLKS10   LA    R7,ACCMLEN(R7)                                                   
         CLI   DOLSW,C'3'          NET                                          
         BE    BLKSX                                                            
         CLI   DOLSW,C'4'          NET-CD                                       
         BNE   BLKS20                                                           
         LA    R5,ACCMLEN(R7)                                                   
         B     BLKS3                                                            
*                                                                               
BLKS20   CLI   DOLSW,C'5'          CD                                           
         BNE   BLKS25                                                           
         LA    R7,ACCMLEN(R7)                                                   
         B     BLKSX                                                            
*                                                                               
BLKS25   DC    H'0'                                                             
*                                                                               
BLKSX    DS    0H                                                               
         LA    R2,3                                                             
         LA    R7,0(R6,R7)                                                      
         LA    R9,WORKSP1                                                       
TBLPROC  ZAP   0(8,R9),0(8,R7)          PUT AMT INTO WORK                       
         LA    R7,104(R7)               GET NEXT LEVEL -ORD-PD-BLLD             
         LA    R9,8(R9)                 GET NEXT WORK                           
         BCT   R2,TBLPROC                                                       
         AP    TOTORD,WORKSP1      ADD TO TOTAL LINE                            
         AP    TOTPD,WORKSP2                                                    
         AP    TOTBLD,WORKSP3                                                   
CALC5    ZAP   WORKSP4(8),WORKSP1(8)    MOVE ORD TO UNPD                        
         SP    WORKSP4(8),WORKSP2(8)    UNPD = ORD-PAID                         
         ZAP   WORKSP5(8),WORKSP1(8)    MOVE ORD TO UNBL                        
         SP    WORKSP5(8),WORKSP3(8)    UNBL = ORD- BILLED                      
         ZAP   WORKSP1(8),WORKSP1(8)                                            
         BNE   DUD                      IF NOT ZERO GO TO DIVIDE                
         ZAP   WORKSP6(8),WORKSP1(8)   ZERO DIVISOR                             
         B     MFLDS                    GO TO MOVE FIELDS                       
DUD      ZAP   WORKSP6(16),WORKSP2(8)   MOVE PAID TO DIVIDE                     
         MP    WORKSP6(16),=P'1000'     SET DECIMAL POSITION FOR 3 PLCS         
         DP    WORKSP6(16),WORKSP1      PERCENT PAID                            
MFLDS    LA    R9,5                     SET LOOP                                
         LA    R2,WORKSP1                 GET BEGINING OF WORK AREA             
MFLDS2   ZAP   DUB,0(8,R2)                                                      
         CP    DUB,=P'0'                                                        
         BL    MFLDS3                                                           
         AP    DUB,=P'50'          ROUND                                        
         B     MFLDS4                                                           
MFLDS3   SP    DUB,=P'50'               NEGATIVE ROUNDING                       
MFLDS4   DP    DUB,=P'100'                                                      
         ZAP   0(8,R2),DUB(6)                                                   
         LA    R2,8(R2)                 BUMP TO NEXT AMT                        
         BCT   R9,MFLDS2                                                        
*        EDIT  LINE                                                             
         CP    WORKSP6(8),=P'0'             CHK FOR ZERO PCT PAID               
         BE    EDLP3               YES - SKIP                                   
         EDIT  (P8,WORKSP6),(7,PDPC),1,MINUS=YES                                
         CP    WORKSP6,=P'0'                                                    
         BH    EDLP3                                                            
         MVI   PDPC+7,C'-'                                                      
*              SINCE EDIT DOESN'T DO MINUS FOR 1 DECIMAL                        
EDLP3    EDIT  (P8,WORKSP1),(13,ORD),0,COMMAS=YES,MINUS=YES                     
*                                                                               
         CLI   TESTSW,C'Y'     SEE IF DOING TEST ESTIMATE                       
         BE    EDLP10          SKIP PAID AND BILLED COLUMNS                     
*                                                                               
         EDIT  (P8,WORKSP2),(13,PD),0,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,WORKSP4),(13,UNPD),0,COMMAS=YES,MINUS=YES                    
         CLC   SVPUB(3),=C'ALL'                                                 
         BNE   EDLP5                                                            
         CLI   MTHSW,C'I'                                                       
         BNE   EDLP5                                                            
         B     EDLP10                                                           
*                                  INS MTH + BUCKETS                            
*                                  NO BILLING DATA AVAILABLE                    
*                                                                               
EDLP5    DS    0H                                                               
         EDIT  (P8,WORKSP3),(13,BILD),0,COMMAS=YES,MINUS=YES                    
         EDIT  (P8,WORKSP5),(12,BLBL),0,COMMAS=YES,MINUS=YES                    
EDLP10   LA    R1,2(R1)                 BUMP MONTH TBL                          
         LA    R9,IOAREA                                                        
         FOUT  (R3),OUTAREA,80                                                  
         LA    R3,88(R3)                                                        
         CLC   TOTDS,=C' TOTAL '                                                
         BE    ENDJOB                                                           
         XC    OUTAREA,OUTAREA                                                  
         B     LPOUT                                                            
         EJECT                                                                  
*        TOTAL LINE ROUTINE                                                     
TOTLP    MVC   TOTDS,=C' TOTAL '                                                
         MVC   WORKSP1,TOTORD      TOTAL ORDERED                                
         MVC   WORKSP2,TOTPD                                                    
         MVC   WORKSP3,TOTBLD                                                   
         B     CALC5                                                            
*                                                                               
ENDJOB   LA    R4,FISOT14H+88                                                   
EJ1      OC    8(80,R3),8(R3)                                                   
         BZ    ENJB                                                             
LOOPED   CR    R3,R4                                                            
         BE    ENJB                                                             
         XC    8(80,R3),8(R3)                                                   
         FOUT  (R3)                                                             
         LA    R3,88(R3)                                                        
         B     EJ1                                                              
ENJB     MVC   FISEMSG,=CL60'FIS - REQUEST PROCESSED'                           
         XC    PREVKEY,PREVKEY        JUST IN CASE                              
         LA    R2,FISMDIAH                                                      
         B     EXIT                                                             
         EJECT                                                                  
MONTAB   DC    C'   JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
SPACES   DC    C'                         '                                     
         EJECT                                                                  
FINDISP  NTR1                                                                   
         XC    DISP,DISP                                                        
*                                  R3 HAS ADDR OF BINARY POSTING YM             
         LA    R5,RMLIST                                                        
         SR    R6,R6                                                            
FD2      CLC   0(2,R3),0(R5)                                                    
         BE    FDX                                                              
         BH    FD4                                                              
         CLI   PRSW,1                                                           
         BE    FDX                 YES                                          
         B     FDERR                                                            
*                                                                               
FD4      CLI   0(R5),0                                                          
         BE    FD6                                                              
         LA    R6,8(R6)                                                         
         LA    R5,2(R5)                                                         
         B     FD2                                                              
FD6      CLI   SUBSW,1                                                          
         BNE   FDERR                                                            
         LTR   R6,R6               SEE IF ZERO                                  
         BZ    FDX                 LEAVE ALONE                                  
         SH    R6,=H'8'            ELSE BACK UP ONE MTH                         
FDX      SR    RE,RE                                                            
         ST    R6,DISP             STORE DISPLACEMENT                           
FDERR    LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO GET NEXT CLIENT                                   *         
*                                                                     *         
*        WHEN I GET HERE KEY SHOULD EITHER BE EMPTY OR                *         
*        WILL CONTAIN THE KEY OF THE RECORD THAT CAUSED               *         
*        A CLIENT BREAK                                               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
GETNCLT  NTR1                                                                   
*                                                                               
         OC    KEY,KEY                                                          
         BZ    GETNC0X                                                          
*                                                                               
         CLI   KEY+3,X'20'      SEE IF A BUY                                    
         BE    GETNC05                                                          
         CLI   KEY+3,X'21'                                                      
         BE    GETNC05                                                          
*                                                                               
         XC    WORKCLT,WORKCLT    END OF BUYS SO QUIT                           
         B     GETNCX                                                           
*                                                                               
GETNC05  DS    0H                                                               
*                                                                               
         CLC   WORKCLT,KEY+4   CKECK SAME CLIENT                                
         BE    GETNC0X         MEANS NO BUYS FOR THIS CLT/PUB                   
*                              IF NEW CLIENT- TRY IT                            
         MVC   WORKCLT,KEY+4   USE CLIENT OF KEY THAT CAUSED BREAK              
*                              SO I CAN SKIP CLIENTS WITH NO BUYS               
         ZIC   RE,WORKCLT+2                                                     
         BCTR  RE,0            SO GETNC0X WILL READ THIS CLIENT                 
         STC   RE,WORKCLT+2                                                     
*                                                                               
GETNC0X  DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTKEY,R4          ESTABLISH CLIENT KEY                         
*                                                                               
         MVC   PCLTKAGY,AGYALPHA   SET AGECNY                                   
         MVC   PCLTKMED,SVMED      SET MEDIA                                    
         MVI   PCLTKRCD,X'02'      SET RECORD ID                                
*                                                                               
GETNCLP  DS    0H                                                               
*                                                                               
         MVC   PCLTKCLT,WORKCLT    SET CURRENT CLIENT CODE                      
         MVI   PCLTKCLT+L'PCLTKCLT,X'FF'  FORCE NEXT CLIENT                     
*                                                                               
         BAS   RE,HIGH             READ POINTER FOR NEXT CLIENT                 
*                                                                               
         GOTO1 GETFACT,DMCB,0      MUST CHECK MAXIOS                            
*                                                                               
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
*                                                                               
         CLC   FATIOCNT,SVMAXIO                                                 
         BNL   GETNCER1                                                         
*                                                                               
         DROP  R1                                                               
*                                                                               
         XC    WORKCLT,WORKCLT     NOT FOUND CLEAR WORKCLT                      
*                                                                               
         CLC   PCLTKEY(PCLTKCLT-PCLTKEY),KEYSAVE                                
         BNE   GETNCX              DONE ON CHANGE IN REC TYPE/MED/AGY           
*                                                                               
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    GETNC9                                                           
*                                                                               
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    GETNC10                                                          
*                                                                               
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BE    GETNC2                                                           
*                                                                               
*        ALL OR SINGLE CLIENT                                                   
*                                                                               
         B     GETNCFD                                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
*        OFFICE OR RANGE OF OFFICES                                             
*                                                                               
GETNC2   DS    0H                                                               
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,CKTRA            TRAFFIC OFFICE REPLACEMENT RTN               
*                                                                               
         CLI   SVCLT+1,C'-'          SEE IF DOING ALL BUT                       
         BNE   GETNC3                                                           
*                                                                               
         CLC   PCLTOFF(1),SVCLT+2                                               
         BE    GETNCCN             BYPASS THIS CLIENT                           
*                                                                               
         B     GETNCFD             RETURN CLLIENT CODE                          
*                                                                               
GETNC3   CLI   SVCLT+2,0           SEE IF DOING RANGE                           
         BNH   GETNC5              NO                                           
*                                                                               
*        TRANSLATE PCLTOFF TO 2 CH OFFICE CODE                                  
*                                                                               
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF      CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT,6(RA)                                                     
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD          A("SECRET BLOCK")                            
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         BNE   GETNCCN             SKIP CLIENT IF INVALID OFFICE CODE           
*                                                                               
         LA    R1,WORK2            ESTABLISH OFFICER CONTROL BLOCK              
         USING OFFICED,R1                                                       
*                                                                               
         CLC   OFCOFC2,SVOFCST2    MAKE SURE OFFICE IN RANGE                    
         BL    GETNCCN             LOW SKIP                                     
         CLC   OFCOFC2,SVOFCEN2                                                 
         BH    GETNCCN             HIGH SKIP                                    
*                                                                               
         B     GETNCFD             PROCESS                                      
*                                                                               
         DROP  R1                                                               
*                                                                               
GETNC5   CLC   PCLTOFF(1),SVCLT+1  ONE OFFICE - MUST MATCH                      
         BNE   GETNCCN             NO SKIP                                      
         B     GETNCFD                                                          
*                                                                               
*        OFFICE LIST                                                            
*                                                                               
GETNC9   DS    0H                  HERE FOR OFFICE LIST CHECKING                
*                                                                               
         BAS   RE,GETREC           MUST READ CLT                                
*                                                                               
         BAS   RE,CKTRA            TRAFFIC OFFICE REPLACEMENT RTN               
*                                                                               
         MVI   LIMITSW,C' '        SET TO USE OFFICER TO "SELECT"               
         BAS   RE,PPCLIVER         CALL OFFICER                                 
         BE    GETNCFD             CHECK FOR SECURITY                           
*                                                                               
         B     GETNCCN             SKIP THIS CLT                                
*                                                                               
*        BILLING GROUP                                                          
*                                                                               
GETNC10  DS    0H                  BILLING GROUP (&) TESTING                    
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         CLC   PCLTBLGP,SVCLT+1                                                 
         BNE   GETNCCN             SKIP THIS CLT                                
*                                                                               
         B     GETNCFD             ACCEPT THIS CLT                              
*                                                                               
*                                                                               
*        CLIENT IS GOOD CANDIDATE                                               
*              CHECK THAT SECURITY ALLOWS USER TO VIEW CLIENT                   
*                                                                               
GETNCFD  DS    0H                                                               
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    GETNC90             OKAY - NO LIMIT ACCESS                       
*                                                                               
         MVI   LIMITSW,C'Y'        SET TO USE OFFICER FOR SECURITY              
*                                                                               
         BAS   RE,PPCLIVER         CALL OFFICER                                 
         BNE   GETNCCN             SKIP THIS CLT                                
*                                                                               
         B     GETNC90             CLIENT OKAY FOR DISPLAY                      
*                                                                               
GETNCCN  DS    0H                                                               
*                                                                               
         MVC   WORKCLT,KEY+4                                                    
         B     GETNCLP             GO CHK NEXT CLIENT                           
*                                                                               
GETNC90  MVC   WORKCLT,KEY+4       PROCESS THIS CLIENT                          
*                                                                               
*        IF DISPLAYING PLANNED COSTS                                            
*              NEED TO GET B2B PROFILE FOR BILLING EFFECTIVE DATE               
*                                                                               
         CLI   PLNCSTSW,C'P'       SKIP IF NOT DISPLAYING PLANNED COSTS         
         BNE   GETNC99                                                          
*                                                                               
*        READ B2B PROFILE FOR CLIENT                                            
*                                                                               
         XC    WORKCBLD,WORKCBLD   INIT BILLING EFFECTIVE DATES                 
         XC    WORKPBLD,WORKPBLD   INIT BILLING EFFECTIVE DATES                 
         XC    WORKEBLD,WORKEBLD   INIT BILLING EFFECTIVE DATES                 
         XC    WORKEALD,WORKEALD   INIT ACTUALIZATION     DATES                 
         XC    WORKPCYN,WORKPCYN   INIT PC BILLING OPTION                       
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PB2B'    REQUEST B2B PROFILE                          
         NI    WORK,X'FF'-X'40'    MAKE 'P' LOWERCASE                           
*                                  NEEDED FOR 3 CH PROF IDS                     
         MVC   WORK+4(2),PCLTKAGY  AGENCY                                       
         MVC   WORK+6(1),PCLTKMED  MEDIA                                        
         MVC   WORK+7(3),PCLTKCLT  CLIENT                                       
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF  CLIENT OFFICE                                
*                                                                               
         L     RF,ACOMFACS         COMFACS ADDRESS                              
         L     R0,CDATAMGR-COMFACSD(RF)    DATAMGR ADDRESS                      
         L     RF,CGETPROF-COMFACSD(RF)    GETPROF ADDRESS                      
*                                                                               
         GOTO1 (RF),DMCB,WORK,B2BPROF,(R0),0,0                                  
*                                                                               
         MVC   WORKPCYN,B2BPROF+12   SAVE CLIENT PC BILLING OPTION              
         MVC   WORKCBLD,B2BPROF+13   SAVE CLIENT BILLING EFF DATE               
*                                                                               
         OC    WORKCBLD,WORKCBLD   SKIP IF NONE                                 
         BZ    GETNC95                                                          
*                                                                               
         CLI   WORKCBLD,80         SET CENTURY                                  
         BNL   GETNC95                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WORKCBLD         SET CENTURY                                  
         AHI   RF,100                                                           
         STC   RF,WORKCBLD                                                      
*                                                                               
GETNC95  DS    0H                                                               
*                                                                               
         XC    WORKPRD,WORKPRD     INIT CURRENT PRODUCT CODE                    
*                                                                               
GETNC99  DS    0H                                                               
*                                                                               
         B     GETNCX                                                           
*                                                                               
GETNCER1 DS    0H                  TOO MANY IO'S                                
*                                                                               
         MVC   WORKCLT(3),=X'FFFFFF'  TOO MANY IO'S                             
*                                                                               
         B     GETNCX              EXIT                                         
*                                                                               
GETNCX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
CKTRA    NTR1                                                                   
         CLI   TRFAGSW,C'Y'        TRAFFIC AGENCY ID ?                          
         BNE   CKTRAX              NO                                           
*                                  SEE IF TRAFFIC OFFICE EXISTS                 
         MVI   ELCODE,X'50'                                                     
         LA    R2,PCLTREC+33                                                    
         BAS   RE,NEXTEL                                                        
         BNE   CKTRAX                                                           
         MVC   PCLTOFF,2(R2)    REPLACE CLT OFFICE WITH TRAFFIC OFFICE          
CKTRAX   XIT1                                                                   
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
         EJECT                                                                  
POSTINS  DS    0H                  POST INSERTION DATA                          
*                                  R6 HAS DISP                                  
*        R4 POINTS TO ACCUM                                                     
         LA    R5,0(R6,R4)                                                      
         L     R0,0(R3)                                                         
         CVD   R0,DUB                                                           
         AP    0(8,R5),DUB                                                      
         LA    R5,ACCMLEN(R5)      NET                                          
         S     R0,4(R3)            GROSS - AGY COM = NET                        
         CVD   R0,DUB                                                           
         AP    0(8,R5),DUB                                                      
         LA    R5,ACCMLEN(R5)      CD                                           
         L     R0,8(R3)                                                         
         CVD   R0,DUB                                                           
         AP    0(8,R5),DUB                                                      
         BR    RE                                                               
         EJECT                                                                  
*                   DATA MANAGER INTERFACE FOR TEMPSTR WRITE                    
         SPACE 2                                                                
WRTEMP   ST    RE,FULL                                                          
         XC    DMCB+8(2),DMCB+8                                                 
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERMINAL NUMBER                       
         STC   R3,DMCB+8           PAGE                                         
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R4),(TERMNAL,0)            
         SPACE 2                                                                
         L     RE,FULL                                                          
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                TEMPSTR WRITE ERROR                          
         SPACE 2                                                                
RDTEMP   ST    RE,FULL                                                          
         XC    DMCB+8(2),DMCB+8                                                 
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERMINAL NUMBER                       
         STC   R3,DMCB+8           PAGE                                         
         MVI   DMCB+9,X'FF'        FOR FULL PAGE READ                           
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,(R4),(TERMNAL,0)           
         L     RE,FULL                                                          
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                TEMPSTR WRITE ERROR                          
         EJECT                                                                  
*       *************************                                               
******  TEST OFFICE LIST CHECKING                                               
*       *************************                                               
*                                                                               
*        GO HERE WHEN SVCLT IS $N - OFFICE LIST PROCESSING                      
*                                                                               
         SPACE 2                                                                
*NOP*PPCLIVER NTR1 ***** NOTE- I/O AREA M/B IN AREC ******                      
         SPACE 2                                                                
*NOP*    XC    WORK,WORK                                                        
*NOP*    LA    R1,WORK                                                          
*NOP*    USING OFFICED,R1                                                       
*NOP*    MVI   OFCSYS,C'P'                                                      
*NOP*    MVC   OFCAUTH,SVCLT    SVCLT SHOULD HAVE $N - OFFICE LIST              
*NOP*    MVC   OFCAGY,AGYALPHA                                                  
*NOP*    MVC   OFCOFC,PCLTOFF                                                   
*NOP*    DROP  R1                                                               
*NOP*    GOTO1 VOFFICER,DMCB,WORK,ACOMFACS                                      
*NOP*    CLI   0(R1),0                                                          
*NOP*    XIT1                                                                   
*******************************************************                         
*******************************************************                         
         EJECT                                                                  
*                                                                               
PPCLIVER NTR1                   *****  LIMIT ACCESS TESTING   *****             
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         CLI   LIMITSW,C'Y'        DOING SECURITY ?                             
         BE    *+10                YES                                          
         MVC   OFCAUTH,SVCLT       MUST BE "SELECTING" FROM OFC LIST            
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF      CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         CLI   LIMITSW,C'Y'        DOING SECURITY ?                             
         BE    *+10                YES                                          
         MVC   OFCLMT(2),SVCLT     MUST BE "SELECTING" FROM OFC LIST            
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD          A("SECRET BLOCK")                            
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
BUYSTOP  DS    0H                                                               
*                              SAVE KEY OF BUY OR CLIENT                        
         MVC   PREVKEY,KEY                                                      
         MVC   FISEMSG,=CL60'** HIT ENTER TO CONTINUE PROCESSING **'            
*                                                                               
         LA    R2,FISEND1H                                                      
         OI    FISOPTH+1,X'01'  MODIFY OPTIONS FIELD                            
         FOUT  FISOPTH                                                          
*                                                                               
         LA    R3,1           WRITE ACCUMS TO TEMPSTR                           
         LA    R4,BILLG       PAGE 1                                            
         BAS   RE,WRTEMP                                                        
*                                                                               
         LA    R3,FISOT01H                                                      
         LA    R4,FISOT14H+88      FIELD PAST LAST                              
BSTOP5   OC    8(80,R3),8(R3)      SEE IF ALREADY CLEAR                         
         BZ    STOPX                                                            
         XC    8(80,R3),8(R3)                                                   
         FOUT  (R3)                                                             
         LA    R3,88(R3)                                                        
         CR    R3,R4           SEE IF AT END OF SCREEN                          
         BE    STOPX                                                            
         B     BSTOP5                                                           
*                                                                               
*                                                                               
STOP     DS    0H                                                               
         XC    FISEMSG,FISEMSG                                                  
         MVC   FISEMSG(49),=C'**MAXIMUM FILE READS EXCEEDED - REQUEST SX        
               TOPPED**'                                                        
         NI    FISMDIAH+4,X'DF'  UNVALIDATE MEDIA                               
*                                                                               
STOPX    DS    0H                                                               
         FOUT  FISEMSGH                                                         
         B     EXXMOD                                                           
*                                                                               
       ++INCLUDE PPGENEROL                                                      
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPBVALD           NEW DSECT FOR PPBVAL ROUTINE                 
*                                                                               
       ++INCLUDE PPFISWRK          CONTAINS NEW DESCT FOR PBILLREC              
