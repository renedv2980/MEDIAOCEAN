*          DATA SET ACREPIC02  AT LEVEL 039 AS OF 04/10/15                      
*PHASE ACIC02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'ACIC02 - COKE EXPENDITURE INTERFACE'                            
ACIC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACIC02,R9,RR=R2                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACICD,RC                                                         
         CLI   MODE,RUNFRST                                                     
         BNE   RQFST                                                            
         ST    R2,RELO                                                          
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
                                                                                
         L     RF,=A(SAVERC)                                                    
         A     RF,RELO                                                          
         ST    RC,0(RF)            SAVE REG C                                   
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     RF,=A(HOOK)                                                      
         A     RF,RELO                                                          
         ST    RF,HEADHOOK                                                      
                                                                                
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         B     XIT                                                              
         EJECT                                                                  
RQFST    CLI   MODE,REQFRST                                                     
         BNE   PRACC                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(20,FYRUNDT)                              
         CLI   QOPT6,C' '                                                       
         BE    RQFST5                                                           
         LA    R1,RSIZEBIG                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         UNPK  RECCARD+27(3),DUB+6(2)                                           
         LA    R1,RAGYCD-RECD+1                                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
                                                                                
         CLI   QOPT6,C'B'               BOTTLER/AGENCY SORT                     
         BNE   RQFST3                                                           
         UNPK  SORTCARD+27(3),DUB+6(2)                                          
         MVC   SORTCARD+31(1),=C'7'                                             
         ZAP   CNTAGY,=P'0'                                                     
         B     RQFST5                                                           
                                                                                
RQFST3   CLI   QOPT6,C'A'               AGENCY/BOTTLER SORT                     
         BNE   RQFST5                                                           
         UNPK  SORTCARD+19(3),DUB+6(2)                                          
         MVC   SORTCARD+23(1),=C'7'                                             
         ZAP   CNTBOT,=P'0'                                                     
                                                                                
RQFST5   GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(40,ASORTC)                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT1,C'Y'       DIFFERENT HEADING FOR REPORT W/O TAPE           
         BE    *+8                                                              
         MVI   RCSUBPRG,5                                                       
         CLI   QOPT3,C'U'       DIFFERENT HEADING FOR UNAPPROVED ITEMS          
         BNE   *+8                                                              
         MVI   RCSUBPRG,3                                                       
         MVC   PAGE,=H'1'                                                       
         ZAP   RECCNT,=P'0'                                                     
         MVI   STATUS,0                                                         
         CLI   QOPT1,C'Y'                                                       
         BNE   RQFST7                                                           
         LA    R7,ACCOKE                                                        
         GOTO1 DYNALLOC,DMCB,DDPARM,DSPARM                                      
         OPEN  ((R7),(OUTPUT))                                                  
RQFST7   CLC   QSTART,SPACES                                                    
         BE    RQFST9                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STRT1)                                 
         GOTO1 (RF),(R1),,(2,STRT2)                                             
         GOTO1 (RF),(R1),,(8,STRT8)                                             
         GOTO1 (RF),(R1),,(20,FYQSTDT)                                          
         GOTO1 (RF),(R1),(0,QEND),(1,END1)                                      
         GOTO1 (RF),(R1),,(2,END2)                                              
         GOTO1 (RF),(R1),,(8,END8)                                              
         GOTO1 (RF),(R1),,(20,FYQENDT)                                          
*                                                                               
RQFST9   LA    R3,RECORD                                                        
         USING RECD,R3                                                          
         MVI   RECORD,C' '                                                      
         LA    R2,RECORD+1                                                      
         LA    R3,RSIZEBIG-1                                                    
         LA    R4,RECORD                                                        
         LR    R5,R3                                                            
         MVCL  R2,R4                                                            
         LA    R3,RECORD                                                        
                                                                                
         MVC   RAGY,=C'01'       IPG IS AGENCY 01                               
         MVC   YEAR,STRT1                                                       
         CLC   QSELECT,SPACES                                                   
         BE    XIT                                                              
         MVC   WORK(2),QSELECT+3   YY CHAR FORM                                 
         MVC   WORK+2(2),QSELECT   MM CHAR FORM                                 
         MVC   WORK+4(2),=C'01'    DD SET  FOR DATCON CALL                      
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   RTMNTH,WORK+10      MM   TRANSMISSION MONTH AND YEAR             
         MVC   RTYR,WORK+6         YYYY CHAR    FORM                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   YEAR,WORK+6         YY   PACKED FORM                             
         B     XIT                                                              
         EJECT                                                                  
                                                                                
PRACC    CLI   MODE,PROCACC                                                     
         BNE   PTRNS                                                            
         LA    R3,RECORD                                                        
         USING RECD,R3                                                          
         L     R2,ADHEIRA                                                       
         MVC   RACN,3(R2)     BOTTLER ACN NUMBER                                
         L     R4,ADLVANAM                                                      
         USING ACNAMED,R4                                                       
         MVC   RACNNM,SPACES       FILL WITH SPACES                             
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RACNNM(0),ACNMNAME  ACN NAME TO RECORD                           
                                                                                
         CLI   QOPT6,C' '                                                       
         BE    XIT                                                              
         L     R2,ADACC                                                         
         MVC   RAGYCD,8(R2)     BOTTLER ACN NUMBER                              
         L     R4,ADACCNAM                                                      
         USING ACNAMED,R4                                                       
         MVC   RAGYNM,SPACES       FILL WITH SPACES                             
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   RAGYNM(0),ACNMNAME  ACN NAME TO RECORD                           
         EJECT                                                                  
                                                                                
PTRNS    CLI   MODE,PROCTRNS                                                    
         BNE   RQLST                                                            
         LA    R3,RECORD                                                        
         USING RECD,R3                                                          
         L     R2,ADACC                                                         
         USING ACKEYD,R2                                                        
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   TRNTYPE,X'81'       IF IT IS A CHECK, SHOULDN'T BE               
         BE    XIT                 PROCESSING THIS                              
         ZAP   RGRS,TRNAMNT                                                     
         ZAP   RNET,TRNAMNT        STILL MUST SUBTRACT CD                       
         ZAP   RCOMM,=P'0'                                                      
         ZAP   RCOMM1,=P'0'                                                     
                                                                                
         MVC   RBUYER,8(R2)                                                     
         MVC   RBTYP,=C'BB'                                                     
         CLC   8(3,R2),=C'100'                                                  
         BL    PTRN2               BOTTLER BOUGHT                               
         MVC   RBTYP,=C'BA'                                                     
         CLC   8(3,R2),=C'900'                                                  
         BL    PTRN2               AGENCY BOUGHT                                
         MVC   RBTYP,=C'CA'        OR MC CANN                                   
                                                                                
PTRN2    CLI   QOPT2,C' '                                                       
         BE    PTRN4               NO FILTER FOR BUY TYPE                       
         CLI   QOPT2,C'A'                                                       
         BNE   PTRN2B                                                           
         CLC   RBTYP,=C'BA'        FILTER ON AGENCY BOUGHT                      
         BNE   XIT                                                              
         B     PTRN4                                                            
PTRN2B   CLI   QOPT2,C'B'                                                       
         BNE   PTRN2M                                                           
         CLC   RBTYP,=C'BB'        FILTER ON BOTTLER BOUGHT                     
         BNE   XIT                                                              
         B     PTRN4                                                            
PTRN2M   CLI   QOPT2,C'M'                                                       
         BNE   XIT                                                              
         CLC   RBTYP,=C'CA'        FILTER ON MC CANN BOUGHT                     
         BNE   XIT                                                              
                                                                                
PTRN4    L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         GOTO1 GETL,DMCB,(X'60',(R5)),0                                         
         CLI   ELERR,0                                                          
         BNE   XIT                                                              
         L     R4,ELADDR                                                        
         USING TRSTATD,R4                                                       
         NI    STAT2,X'FF'-ST2HELD                                              
         CLI   QOPT3,C'U'          UN-APPROVED ITEMS ONLY                       
         BNE   PTRN4B                                                           
         OC    TRSTUPDT,TRSTUPDT                                                
         BNZ   *+8                 CHECK FOR HOLD                               
         OI    STAT2,ST2HELD       OLD WAY UNAPPROVED                           
         B     PTRN4D                                                           
PTRN4B   CLC   TRSTUPDT,STRT2                                                   
         BL    XIT                 NOT APPROVED IN PERIOD                       
         CLC   TRSTUPDT,END2                                                    
         BH    XIT                 NOT APPROVED IN PERIOD                       
                                                                                
PTRN4D   GOTO1 GETL,DMCB,(X'50',(R5)),0                                         
         CLI   ELERR,0                                                          
         BNE   PTRN4F                                                           
         L     R4,ELADDR                                                        
         USING TRCASHD,R4                                                       
         ZAP   RGRS,TRCSGRS                                                     
         ZAP   RCOMM,TRCSGRS                                                    
         SP    RCOMM,TRCSNET       COMMISSION                                   
         ZAP   RCOMM1,RCOMM                                                     
         CLI   QOPT4,C'Y'          ONLY NET = GROSS ITEMS                       
         BNE   PTRN4E                                                           
         CP    RCOMM,=P'0'         IF IT HAS COMMISSION EXCLUDE                 
         BNE   XIT                                                              
PTRN4E   ZAP   DUB,TRCSGRS                                                      
                                                                                
         ZAP   RNET,TRCSNET        STILL MUST SUBTRACT CD                       
PTRN4F   GOTO1 GETL,DMCB,(X'46',(R5)),0                                         
         CLI   ELERR,0                                                          
         BNE   PTRN4H                                                           
         L     R4,ELADDR                                                        
         USING XPYELD,R4           EXTRA PAY ELEM                               
         SP    RNET,XPYCD          PACKED EXPENDITURE                           
         SP    DUB,XPYCD                                                        
         MP    DUB,=P'-1'                                                       
         UNPK  REXP,DUB            CHARACTER                                    
         MVC   RVND,XPYCLI                                                      
         SR    R1,R1                                                            
         ICM   R1,3,XPYEST         ESTIMATE NUMBER                              
         BZ    PTRN4G                                                           
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RESTN,DUB+6(2)                                                   
PTRN4G   MVC   RMKT,SPACES                                                      
         MVC   RCAT,SPACES                                                      
         CLI   XPYLN,XPYLN2Q                                                    
         BL    PTRN4H                                                           
         CLI   XPYMKT,C' '                                                      
         BNH   *+10                                                             
         MVC   RMKT,XPYMKT         MARKET NUMBER                                
         CLI   XPYCAT,C' '                                                      
         BNH   *+10                                                             
         MVC   RCAT,XPYCAT         SPOT EST CATEGORY                            
         SPACE 1                                                                
PTRN4H   GOTO1 GETL,DMCB,(X'44',(R5)),0                                         
         L     R4,ELADDR                                                        
         USING TRANSD,R4                                                        
         CLI   YEAR,0                                                           
         BE    *+14                                                             
         CLC   TRNSDATE(1),YEAR    TRANSACTION MUST MATCH YEAR                  
         BNE   XIT                                                              
         CLI   QOPT3,C'U'          UN-APPROVED ITEMS ONLY                       
         BNE   PTRN4I                                                           
         TM    STAT2,ST2HELD       MARKED UNAPPROVED OLD WAY                    
         BO    PTRN4J              THEN DON'T CHECK FOR NEW WAY                 
         TM    TRNSSTAT,X'04'      HELD                                         
         BNO   XIT                                                              
         B     PTRN4J                                                           
PTRN4I   TM    TRNSSTAT,X'04'      HELD = UNAPPROVED                            
         BO    XIT                                                              
         TM    STAT2,ST2HELD       MARKED UNAPPROVED OLD WAY                    
         BO    XIT                                                              
PTRN4J   GOTO1 DATCON,DMCB,(1,TRNSDATE),(20,WORK)                               
         MVC   RSMNTH,WORK+4       MOVE IN MM  , CHAR FORM                      
         MVC   RSYR,WORK           MOVE IN YYYY, CHAR FORM                      
                                                                                
         USING ACKEYD,R2                                                        
         LR    R2,R5                                                            
         MVC   RMED,ACKEYCON+5     MEDIA                                        
         CLC   RMED,=C'ST'                                                      
         BNE   PRNT4L                                                           
         CLI   ACKEYCON+7,C'0'                                                  
         BL    PRNT4L                                                           
         MVC   RMED,=C'SC'         MAKE CABLE                                   
*                                                                               
PRNT4L   BAS   RE,GETPRD           GET PRODUCT CODE                             
*        REMOVED OPTION 5 AS OF AUG13/1997                                      
*        BAS   RE,SEEPROD          SEE IF AGY/PROD SELECTION IS                 
*        BNE   PRTN9               WANTED.  NO GOOD GET OUT                     
         BAS   RE,GETVEH           GET VEHICLE NUMBER                           
                                                                                
         LA    R1,MEDTAB                                                        
PRNT5    CLI   0(R1),X'FF'                                                      
         BNE   *+6                 INVALID MEDIA CODE                           
         DC    H'0'                                                             
         CLC   2(2,R1),RMED                                                     
         BE    PRNT6                                                            
         LA    R1,L'MEDTAB(R1)                                                  
         B     PRNT5                                                            
                                                                                
PRNT6    MVC   RMED,0(R1)          CONVERTED MEDIA CODE                         
                                                                                
         GOTO1 SORTER,DMCB,=C'PUT',RECORD                                       
                                                                                
PRTN9    MVC   SAVKEY,0(R2)                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SAVKEY,IO                    
         CLI   DMCB+8,0                                                         
         BE    *+6                 CAN'T READ TRANSACTION                       
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
RQLST    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         ZAP   CNTPRD,=P'0'        ACTIVITY COUNTERS                            
         ZAP   CNTMED,=P'0'                                                     
         ZAP   CNTVEH,=P'0'                                                     
         ZAP   CNTMTH,=P'0'                                                     
         ZAP   CNTAGY,=P'0'                                                     
         ZAP   CNTBOT,=P'0'                                                     
         MVC   TOTBOT(24),PZEROS    TOTAL ACCUMULATORS                          
         MVC   TOTAGY(24),PZEROS    TOTAL ACCUMULATORS                          
         MVC   TOTPRD(24),PZEROS                                                
         MVC   TOTMED(24),PZEROS                                                
         MVC   TOTVEH(24),PZEROS                                                
         LA    RE,LASTREC                                                       
         LA    RF,RSIZEBIG                                                      
         XCEFL                                                                  
                                                                                
         OI    STATUS,PRD+MED+VEH  SET TO PRINT ALL NAMES                       
RQLST2   GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R4,15,DMCB+4                                                     
         BZ    RQLST3                                                           
         LA    R2,RECORD                                                        
         LA    R3,RSIZEBIG                                                      
         LR    R5,R3                                                            
         MVCL  R2,R4                                                            
*                                                                               
         BAS   RE,REPORT                                                        
         CLI   QOPT1,C'Y'                                                       
         BNE   RQLST2                                                           
         BAS   RE,TAPEPUT                                                       
         B     RQLST2                                                           
                                                                                
RQLST3   MVI   RECORD,C' '         FINISH UP REPORT                             
         LA    R2,RECORD+1                                                      
         LA    R3,RSIZEBIG-1                                                    
         LA    R4,RECORD                                                        
         LR    R5,R3                                                            
         MVCL  R2,R4                                                            
                                                                                
         OC    LASTREC(255),LASTREC     IF NO SAVED RECORD                      
         BZ    *+8                      THEN NO DATA GENERATED                  
         BAS   RE,CHABUY                                                        
         CLI   QOPT1,C'Y'                                                       
         BNE   RQLST5                                                           
         CLOSE (ACCOKE)                                                         
RQLST5   MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              PUT RECORD TO TAPE                                               
TAPEPUT  NTR1                                                                   
         USING RECD,R3                                                          
         LA    R3,RECORD           SORT RECORD                                  
         USING REC2D,R4                                                         
         LA    R4,RECORD2          NEW FORMAT TAPE REC                          
*                                                                               
         CLI   QOPT6,C'Y'          NEW FORMAT                                   
         BE    TPNEW                                                            
         CLI   YEAR,X'96'          PACKED YY                                    
         BL    TPOLD                                                            
*                                                                               
TPNEW    MVC   R2TYR(8),FYRUNDT    RUN   DATE FROM RCDATE                       
         MVC   R2SYR(8),FYQSTDT    START DATE FROM QSTART                       
         MVC   R2EYR(8),FYQENDT    END   DATE FROM QEND                         
*                                                                               
         MVC   R2ACN,RACN          BOTTLER ACN                                  
         MVC   R2PRD,RPRD          BRAND/PRODUCT                                
         MVC   R2MED,RMED          MEDIA CODE                                   
         MVC   R2BUYER,RBUYER      AGENCY CODE/BUYER                            
         MVC   R2MKT,RMKT          MARKET                                       
         MVC   R2CAT,RCAT          CATEGORY                                     
         MVC   R2ESTN,RESTN        ESTIMATE NUMBER                              
         MVC   R2VEH,RVEH          MEDIA VEHICLE NUMBER/STATION                 
         MVC   R2VND,RVND          MEDIA VEHICLE DESCRIPTION                    
         MVC   R2SPYR,RSYR         SPOT YEAR OF SERVICE                         
         MVC   R2SPMNTH,RSMNTH     SPOT MONTH OF SERVICE                        
         UNPK  R2GRS,RGRS          GROSS AMOUNT                                 
         UNPK  R2COM,RCOMM1        COMMISSION AMOUNT                            
         MVC   R2SPARE,SPACES                                                   
         PUT   ACCOKE,RECORD2                                                   
         B     TPX                                                              
TPOLD    MVC   RCAT,SPACES         SPACE OUT CATEGORY ON OLD FORMAT             
         MVC   RCOMM1,SPACES       SPACE OUT COMMISSION ON OLD FORMAT           
         PUT   ACCOKE,RECORD                                                    
TPX      AP    RECCNT,=P'1'                                                     
*        BAS   RE,DMPPUT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              GET PRODUCT CODE                                                 
GETPRD   NTR1                                                                   
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(5),ACKEYCON   C3PXX                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,IO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                 CAN'T READ PRODUCT CODE                      
         DC    H'0'                                                             
         GOTO1 GETL,DMCB,(X'23',IO),0                                           
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO PRODUCT CODE                              
         L     R4,ELADDR                                                        
         USING ACOTHERD,R4                                                      
         MVC   RPRD,ACOTNUM        PRODUCT CODE TO TAPE                         
         GOTO1 GETL,DMCB,(X'20',IO),0                                           
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO PRODUCT NAME                              
         L     R4,ELADDR                                                        
         USING ACNAMED,R4                                                       
         MVC   RPRDNM,SPACES       FILL WITH SPACES                             
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   RPRDNM(0),ACNMNAME  PRODUCT NAME TO RECORD                       
         EJECT                                                                  
*              GET VEHICLE CODE                                                 
GETVEH   NTR1                                                                   
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),ACKEYCON   COMPANY                                      
         MVC   MYKEY+1(2),=C'3M'                                                
         MVC   MYKEY+3(2),ACKEYCON+5   MEDIA                                    
         MVC   MYKEY+5(8),ACKEYCON+7   VEHICLE                                  
         CLC   MYKEY+3(2),=C'ST'                                                
         BNE   GETVEH10                                                         
         CLI   MYKEY+5,C'0'                                                     
         BL    GETVEH10                                                         
         MVI   MYKEY+4,C'C'        CHANGE TO MEDIA CABLE                        
         MVC   MYKEY+10(2),=C'CA'                                               
*                                                                               
GETVEH10 GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,IO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                 CAN'T READ VEHICLE RECORD                    
         DC    H'0'                                                             
         GOTO1 GETL,DMCB,(X'25',IO),0                                           
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO VEHICLE CODE                              
         L     R4,ELADDR                                                        
         USING ACNOD,R4                                                         
         MVC   RVEH,ACNO+3         VEHICLE CODE TO TAPE                         
*                                                                               
         CLC   RMKT,SPACES                                                      
         BH    XIT                 GOT MARKET FROM TRANSACTION                  
         GOTO1 GETL,DMCB,(X'A2',IO),0                                           
         CLI   ELERR,0                                                          
         BNE   XIT                                                              
         L     R4,ELADDR                                                        
         USING ACUFD,R4                                                         
         MVC   RMKT,ACUFDATA       MARKET CODE TO TAPE                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              CONTROL PRINTING OF REPORT                                       
                                                                                
         USING RECD,R3                                                          
         USING PRINTD,R5                                                        
REPORT   NTR1                                                                   
         LA    R3,RECORD                                                        
         LA    R4,LASTREC                                                       
         LA    R5,P                                                             
         OC    LASTREC(255),LASTREC     IS THIS FIRST TIME                      
         BZ    REPT2                    YES-SAVE THIS ONE AND GET OUT           
         CLC   RBTYP,RBTYP-RECD(R4)     BUY TYPE CHANGE                         
         BE    REPT4                                                            
         BAS   RE,CHABUY                                                        
REPT2    GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)                          
         B     REPT8                                                            
                                                                                
REPT4    DS    0H                                                               
         CLI   QOPT6,C'A'                                                       
         BNE   REPT5                                                            
         CLC   RAGYCD,RAGYCD-RECD(R4)   AGENCY CHANGE                           
         BE    *+12                                                             
         BAS   RE,CHAAGY                                                        
         B     REPT8                                                            
         CLC   RACN,RACN-RECD(R4)       BOTTLER CHANGE                          
         BE    REPT5A                                                           
         BAS   RE,CHABOT                                                        
         B     REPT8                                                            
                                                                                
REPT5    CLC   RACN,RACN-RECD(R4)       BOTTLER CHANGE                          
         BE    *+12                                                             
         BAS   RE,CHABOT                                                        
         B     REPT8                                                            
         CLC   RAGYCD,RAGYCD-RECD(R4)   AGENCY CHANGE                           
         BE    *+12                                                             
         BAS   RE,CHAAGY                                                        
         B     REPT8                                                            
                                                                                
REPT5A   CLC   RPRDNM,RPRDNM-RECD(R4)   PRODUCT CHANGE                          
         BE    *+12                                                             
         BAS   RE,CHAPRD                                                        
         B     REPT8                                                            
         CLC   RMED,RMED-RECD(R4)       MEDIA CHANGE                            
         BE    *+12                                                             
         BAS   RE,CHAMED                                                        
         B     REPT8                                                            
         CLC   RVEH,RVEH-RECD(R4)       VEHICLE CHANGE                          
         BE    *+12                                                             
         BAS   RE,CHAVEH                                                        
         B     REPT8                                                            
         CLC   RSMNTH(6),RSMNTH-RECD(R4) HAS MONTH CHANGED                      
         BNE   REPT6                                                            
         AP    TOTVEH(8),RNET      ADD TO HIGHER LEVEL                          
         AP    TOTVEH+8(8),RCOMM                                                
         AP    RNET,RNET-RECD(8,R4) SAME MONTH - ADD UP ACCUMS                  
         AP    RCOMM,RCOMM-RECD(8,R4)                                           
         B     REPTX                                                            
REPT6    BAS   RE,FORMAT           YES - FORMAT LAST LINE                       
         TM    STATUS,SKIPRNT                                                   
         BO    REPT8                                                            
         BAS   RE,PRNTIT           AND PRINT IT                                 
REPT8    AP    TOTVEH(8),RNET      ADD TO HIGHER LEVEL                          
         AP    TOTVEH+8(8),RCOMM                                                
*                                                                               
REPTX    DS    0H                                                               
         LA    R2,LASTREC                                                       
         LA    R3,RSIZEBIG                                                      
         LA    R4,RECORD                                                        
         LR    R5,R3                                                            
         MVCL  R2,R4                                                            
         B     XIT                                                              
         EJECT                                                                  
*              BUY TYPE CHANGE - PRINT INVOICE                                  
                                                                                
         USING PRINTD,R5                                                        
CHABUY   NTR1                                                                   
         LA    R5,P                                                             
         CLI   QOPT6,C'A'          IS IT AGENCY/BOTTLER SORT                    
         BNE   *+12                                                             
         BAS   RE,CHAAGY                                                        
         B     *+8                                                              
         BAS   RE,CHABOT           CHANGE LOWER LEVELS                          
         MVI   FORCEHED,C'Y'       START AT NEW PAGE                            
         MVI   RCSUBPRG,1          SET SPECIAL HEADLINE                         
         CLI   QOPT1,C'Y'                                                       
         BE    *+8                                                              
         MVI   RCSUBPRG,6          REQUEST W/O TAPE                             
         CLI   QOPT3,C'U'                                                       
         BNE   *+12                                                             
         MVI   RCSUBPRG,4                                                       
         B     *+10                                                             
         MVC   PAGE,=H'1'                                                       
                                                                                
         BAS   RE,INVOICE          PRINT AN INVOICE                             
                                                                                
         MVC   PMED(19),=C'* BUY TYPE TOTALS *'                                 
         LA    R2,TOTDUE                                                        
         BAS   RE,EDITAMT                                                       
         MVI   SPACING,3                                                        
         BAS   RE,PRNTIT                                                        
         CLI   QOPT3,C'U'          UNAPPROVED ITEMS-SKIP REMITTANCE             
         BE    CHABUY4                                                          
         MVC   P+54(13),=C'* TOTAL DUE *'                                       
         OI    STATUS,GRSONLY      PRINT GROSS AMOUNT ONLY                      
         LA    R2,TOTDUE                                                        
         BAS   RE,EDITAMT                                                       
         NI    STATUS,ALL-GRSONLY                                               
         BAS   RE,PRNTIT                                                        
         CLI   QOPT1,C'Y'          IF NOT GENERATING TAPE                       
         BNE   CHABUY4             THEN SKIP REMIT                              
         CLC   TOTDUE(24),PZEROS   IF NOTHING DUE                               
         BE    CHABUY4             SKIP REMIT                                   
         BAS   RE,REMIT                                                         
CHABUY4  BAS   RE,MEDSUM           PRINT MEDIA SUMMARY                          
         MVI   RCSUBPRG,0          RE-SET HEADLINE                              
         CLI   QOPT1,C'Y'                                                       
         BE    *+8                                                              
         MVI   RCSUBPRG,5                                                       
         CLI   QOPT3,C'U'                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              BOTTLER CHANGE                                                   
                                                                                
         USING PRINTD,R5                                                        
CHABOT   NTR1                                                                   
         CLI   QOPT6,C'B'                                                       
         BNE   CHABOT5                                                          
         BAS   RE,CHAAGY           CHANGE LOWER LEVELS                          
         MVI   FORCEHED,C'N'                                                    
         B     CHABOT6                                                          
CHABOT5  BAS   RE,CHAPRD           CHANGE LOWER LEVELS                          
         CP    CNTPRD,=P'1'        DO I NEED A TOTAL LINE                       
         BNH   CHABOT8                                                          
CHABOT6  LA    R2,TOTBOT                                                        
         BAS   RE,EDITAMT                                                       
         MVC   PPTOT(20),=C'***BOTTLER TOTALS***'                               
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         CLI   QOPT6,C'A'                                                       
         BNE   *+10                                                             
         AP    CNTBOT,=P'1'                                                     
                                                                                
CHABOT8  DS    0H                                                               
         CLI   QOPT6,C'B'                                                       
         BNE   *+10                                                             
         ZAP   CNTAGY,=P'0'                                                     
         CLI   QOPT6,C'B'                                                       
         BE    *+10                                                             
         ZAP   CNTPRD,=P'0'        CLEAR MEDIA COUNT                            
         MVC   TOTBOT(24),PZEROS                                                
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ACCOUNT AGENCY CHANGE                                            
                                                                                
         USING PRINTD,R5                                                        
CHAAGY   NTR1                                                                   
         CLI   QOPT6,C'A'                                                       
         BNE   CHAAGY5                                                          
         BAS   RE,CHABOT           CHANGE LOWER LEVELS                          
         MVI   FORCEHED,C'N'                                                    
         B     CHAAGY6                                                          
                                                                                
CHAAGY5  BAS   RE,CHAPRD           CHANGE LOWER LEVELS                          
         CP    CNTPRD,=P'1'        DO I NEED A TOTAL LINE                       
         BNH   CHAAGY8                                                          
CHAAGY6  LA    R2,TOTAGY                                                        
         BAS   RE,EDITAMT                                                       
         MVC   PPTOT(20),=C'***AGENCY  TOTALS***'                               
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         AP    CNTAGY,=P'1'                                                     
                                                                                
CHAAGY8  DS    0H                                                               
         CLI   QOPT6,C'A'                                                       
         BNE   *+10                                                             
         ZAP   CNTBOT,=P'0'        CLEAR MEDIA COUNT                            
         CLI   QOPT6,C'A'                                                       
         BE    *+10                                                             
         ZAP   CNTPRD,=P'0'        CLEAR MEDIA COUNT                            
         MVC   TOTAGY(24),PZEROS                                                
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              PRODUCT CHANGE                                                   
                                                                                
         USING PRINTD,R5                                                        
CHAPRD   NTR1                                                                   
         BAS   RE,CHAMED           CHANGE LOWER LEVELS                          
         AP    TOTBOT(8),TOTPRD(8)                                              
         AP    TOTBOT+8(8),TOTPRD+8(8)                                          
                                                                                
*        CLI   QOPT6,C' '                                                       
*        BE    CHAPRD5                                                          
         AP    TOTAGY(8),TOTPRD(8)                                              
         AP    TOTAGY+8(8),TOTPRD+8(8)                                          
         B     CHAPRD5                                                          
                                                                                
CHAPRD5  CP    CNTMED,=P'1'        DO I NEED A TOTAL LINE                       
         BL    CHAPRD8                                                          
         BE    CHAPRD6                                                          
         LA    R2,TOTPRD                                                        
         BAS   RE,EDITAMT                                                       
         MVC   PPTOT(20),=C'***PRODUCT TOTALS***'                               
         BAS   RE,PRNTIT                                                        
CHAPRD6  BAS   RE,PRNTIT                                                        
         AP    CNTPRD,=P'1'                                                     
CHAPRD8  ZAP   CNTMED,=P'0'        CLEAR MEDIA COUNT                            
         MVC   TOTPRD(24),PZEROS                                                
         OI    STATUS,PRD          SET TO PRINT NEW PRODUCT NAME                
         B     XIT                                                              
         EJECT                                                                  
*              MEDIA CHANGE                                                     
                                                                                
         USING PRINTD,R5                                                        
CHAMED   NTR1                                                                   
         BAS   RE,CHAVEH           CHANGE LOWER LEVEL                           
         AP    TOTPRD(8),TOTMED(8)                                              
         AP    TOTPRD+8(8),TOTMED+8(8)                                          
         CP    CNTVEH,=P'1'        DO I NEED A TOTAL LINE                       
         BE    CHAMED6                                                          
         BL    CHAMED8                                                          
         LA    R2,TOTMED                                                        
         BAS   RE,EDITAMT                                                       
         MVC   PMTOT(16),=C'**MEDIA TOTALS**'                                   
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
CHAMED6  AP    CNTMED,=P'1'        ADD TO MEDIA COUNT                           
CHAMED8  ZAP   CNTVEH,=P'0'        CLEAR VEHICLE COUNT                          
         MVC   TOTMED(24),PZEROS                                                
         OI    STATUS,MED          SET TO PRINT NEW MEDIA NAME                  
         B     XIT                                                              
         EJECT                                                                  
*              VEHICLE CHANGE                                                   
                                                                                
         USING PRINTD,R5                                                        
CHAVEH   NTR1                                                                   
         BAS   RE,FORMAT           FORMAT SAVED RECORD                          
         TM    STATUS,SKIPRNT                                                   
         BO    *+8                                                              
         BAS   RE,PRNTIT           AND PRINT IT                                 
         AP    TOTMED(8),TOTVEH(8)                                              
         AP    TOTMED+8(8),TOTVEH+8(8)                                          
         CP    CNTMTH,=P'1'        DO I NEED A TOTAL LINE                       
         BE    CHAVEH6                                                          
         BL    CHAVEH8                                                          
         LA    R2,TOTVEH                                                        
         BAS   RE,EDITAMT                                                       
         MVC   PVTOT(16),=C'*VEHICLE TOTALS*'                                   
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
CHAVEH6  AP    CNTVEH,=P'1'        ADD TO VEHICLE COUNT                         
CHAVEH8  ZAP   CNTMTH,=P'0'        CLEAR MONTH COUNT                            
         MVC   TOTVEH(24),PZEROS                                                
         OI    STATUS,VEH          SET TO PRINT NEW VEHICLE NAME                
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT A LINE                                                    
                                                                                
         USING RECD,R3                                                          
         USING PRINTD,R5                                                        
FORMAT   NTR1                                                                   
         LA    R3,LASTREC          PRINT FROM SAVED RECORD                      
         LA    R2,RNET                                                          
         BAS   RE,EDITAMT          MOVE AMOUNT FIELDS TO PRINT LINE             
         TM    STATUS,SKIPRNT                                                   
         BO    XIT                                                              
         LA    R5,P                                                             
         TM    STATUS,PRD          PRINT PRODUCT NAME IF NEEDED                 
         BZ    FORMAT1                                                          
         NI    STATUS,ALL-PRD                                                   
         LA    RF,L'PPROD                                                       
         GOTO1 CHOPPER,DMCB,(36,RPRDNM),((RF),PPROD),(C'P',4)                   
FORMAT1  TM    STATUS,MED          PRINT MEDIA NAME IF NEEDED                   
         BZ    FORMAT4                                                          
         NI    STATUS,ALL-MED                                                   
         LA    R1,MEDTAB           GET MEDIA NAME                               
         LA    R0,NMED                                                          
FORMAT2  CLC   RMED,0(R1)          MATCH ON COKE CODE                           
         BE    FORMAT3                                                          
         CLC   RMED,2(R1)          OR TRY TO MATCH ON DDS CODE                  
         BE    FORMAT3                                                          
         LA    R1,L'MEDTAB(R1)                                                  
         BCT   R0,FORMAT2                                                       
         DC    H'0'                UNKNOWN MEDIA CODE                           
FORMAT3  MVC   PMED,4(R1)                                                       
FORMAT4  TM    STATUS,VEH          PRINT VEHICLE NAME IF NEEDED                 
         BZ    FORMAT5                                                          
         NI    STATUS,ALL-VEH                                                   
         LA    RF,L'PVEH           VEHICLE NAME                                 
         GOTO1 CHOPPER,DMCB,(30,RVND),((RF),PVEH),(C'P',4)                      
         MVC   PMKT+1(4),RMKT                                                   
         MVC   PCAT,RCAT                                                        
         MVC   PESTN,RESTN                                                      
FORMAT5  MVC   WORK(2),RSYR+2      GET MONTH                                    
         MVC   WORK+2(2),RSMNTH                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,PMTH)                                    
         AP    CNTMTH,=P'1' ADD 1 TO MONTH COUNTER                              
         BAS   RE,PUTBUFF          PUT TO BUFFALO FOR INVOICE                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT AMOUNTS TO PRINT LINE                          
                                                                                
*                                  R2=A(ACCUMS)                                 
         USING PRINTD,R5                                                        
EDITAMT  NTR1                                                                   
         NI    STATUS,ALL-SKIPRNT                                               
         CP    0(8,R2),=P'0'                                                    
         BNE   EDIT1                                                            
         CP    8(8,R2),=P'0'                                                    
         BNE   EDIT1                                                            
         OI    STATUS,SKIPRNT      IF BOTH ZERO DO NOT PRINT LINE               
         B     XIT                                                              
EDIT1    LA    R5,P                                                             
         ZAP   16(8,R2),0(8,R2)    DERIVE GROSS                                 
         AP    16(8,R2),8(8,R2)                                                 
         LA    R3,3                                                             
         LA    R4,PNET                                                          
EDIT2    TM    STATUS,GRSONLY                                                   
         BZ    EDIT4                                                            
         CH    R3,=H'1'            LAST ACCUM IS GROSS AMOUNT                   
         BNE   EDIT6                                                            
EDIT4    EDIT  (P8,0(R2)),(13,0(R4)),2,MINUS=YES                                
EDIT6    LA    R2,8(R2)                                                         
         LA    R4,14(R4)                                                        
         BCT   R3,EDIT2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PRINT A LINE                                                     
                                                                                
         USING RECD,R3                                                          
PRNTIT   NTR1                                                                   
         LA    R3,LASTREC                                                       
         CLI   RCSUBPRG,0          ONLY IF PRINTING DETAIL                      
         BE    PRNT1                                                            
         CLI   RCSUBPRG,5                                                       
         BE    PRNT1                                                            
         CLI   RCSUBPRG,3                                                       
         BNE   PRNT1C                                                           
PRNT1    MVC   HEAD5+20(5),RACN    BOTTLER CODE                                 
         MVC   HEAD5+26(16),RACNNM BOTTLER NAME                                 
         MVC   HEAD5+1(7),=C'BOTTLER'                                           
                                                                                
         CLI   QOPT6,C'B'                                                       
         BNE   PRNT1A                                                           
         MVC   HEAD4+20(5),RACN    BOTTLER CODE                                 
         MVC   HEAD4+26(16),RACNNM BOTTLER NAME                                 
         MVC   HEAD4+1(7),=C'BOTTLER'                                           
         MVC   HEAD5,SPACES                                                     
         MVC   HEAD5+1(6),=C'AGENCY'                                            
         MVC   HEAD5+20(7),RAGYCD  AGENCY CODE                                  
         MVC   HEAD5+26(36),RAGYNM AGENCY NAME                                  
         B     PRNT1C                                                           
                                                                                
PRNT1A   CLI   QOPT6,C'A'                                                       
         BNE   PRNT1C                                                           
         MVC   HEAD4+1(6),=C'AGENCY'                                            
         MVC   HEAD4+20(7),RAGYCD  AGENCY CODE                                  
         MVC   HEAD4+26(36),RAGYNM AGENCY NAME                                  
         MVC   HEAD5,SPACES                                                     
         MVC   HEAD5+20(5),RACN    BOTTLER CODE                                 
         MVC   HEAD5+26(16),RACNNM BOTTLER NAME                                 
         MVC   HEAD5+1(7),=C'BOTTLER'                                           
                                                                                
PRNT1C   CLC   QSTART,SPACES                                                    
         BE    PRNT2                                                            
         MVC   HEAD4+86(8),STRT8   START DATE                                   
         MVI   HEAD4+94,C'-'                                                    
         MVC   HEAD4+95(8),END8    END DATE                                     
PRNT2    LA    R1,BTTAB            GET BUY TYPE                                 
         LA    R0,NBT                                                           
         CLC   RBTYP,0(R1)         MATCH ON CODE                                
         BE    *+14                                                             
         LA    R1,L'BTTAB(R1)                                                   
         BCT   R0,*-14                                                          
         DC    H'0'                UNKNOWN BUY TYPE                             
         MVC   HEAD5+81(16),2(R1)  BUY TYPE                                     
         CLI   RCSUBPRG,2                                                       
         BH    *+16                                                             
         MVC   HEAD3+20(4),18(R1)  INVOICE NUMBER                               
         MVC   HEAD4+20(5),QSELECT TRANSMISSION MONTH                           
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD BUFFALO RECORDS                                   
                                                                                
         USING BUFFD,R4                                                         
         USING RECD,R3                                                          
PUTBUFF  NTR1                                                                   
         LA    R4,BUFFREC                                                       
         LA    R3,LASTREC                                                       
         MVC   BUFFPRD,RPRDNM      PRODUCT NAME                                 
         MVC   BUFFMED,RMED        MEDIA CODE                                   
         ZAP   BUFFNET,RNET                                                     
         ZAP   BUFFCOMM,RCOMM                                                   
         ZAP   BUFFGRS,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFFREC                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT AN INVOICE FROM BUFFALO                         
                                                                                
         USING BUFFD,R4                                                         
         USING PRINTD,R5                                                        
INVOICE  NTR1                                                                   
         LA    R5,P                                                             
         MVC   TOTDUE(24),PZEROS                                                
         LA    R1,MEDTAB           CLEAR MEDIA TOTALS                           
         LA    R0,NMED                                                          
         MVC   24(24,R1),PZEROS                                                 
         LA    R1,L'MEDTAB(R1)                                                  
         BCT   R0,*-10                                                          
         XC    BUFFREC,BUFFREC                                                  
         XC    LASTPRD,LASTPRD                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFFREC,1                           
         TM    DMCB+8,X'80'                                                     
         BO    XIT                 NOTHING TO PRINT                             
         B     INV3                                                             
INV2     GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFFREC,1                            
         TM    DMCB+8,X'80'                                                     
         BO    INV2B                                                            
         LA    R4,BUFFREC                                                       
         CLC   BUFFPRD,LASTPRD     IF PRODUCT CHANGED                           
         BE    INV4                                                             
INV2B    CP    CNTMED,=P'1'        DO I NEED A TOTAL LINE                       
         BNH   INV3                                                             
         LA    R2,TOTPRD                                                        
         BAS   RE,EDITAMT                                                       
         MVC   PMED(13),=C'** TOTAL FOR '                                       
         MVC   PMED+13(30),LASTPRD                                              
         BAS   RE,PRNTIT                                                        
INV3     BAS   RE,PRNTIT                                                        
         ZAP   CNTMED,=P'0'        CLEAR MEDIA COUNT                            
         MVC   TOTPRD(24),PZEROS                                                
         TM    DMCB+8,X'80'        PRINT TOTALS IF FINISHED                     
         BO    XIT                                                              
         LA    R4,BUFFREC                                                       
         MVC   PPROD,BUFFPRD       PRODUCT NAME TO PRINT LINE                   
INV4     LA    R1,MEDTAB           GET MEDIA NAME                               
         LA    R0,NMED                                                          
INV6     CLC   BUFFMED,0(R1)       MATCH ON COKE CODE                           
         BE    INV8                                                             
         CLC   BUFFMED,2(R1)       OR TRY TO MATCH ON DDS CODE                  
         BE    INV8                                                             
         LA    R1,L'MEDTAB(R1)                                                  
         BCT   R0,INV6                                                          
         DC    H'0'                UNKNOWN MEDIA CODE                           
INV8     MVC   PMED,4(R1)                                                       
         AP    24(8,R1),BUFFNET    ADD TO MEDIA ACCUMS                          
         AP    32(8,R1),BUFFCOMM                                                
         LA    R2,BUFFNET                                                       
         BAS   RE,EDITAMT          FORMAT AMOUNTS                               
         BAS   RE,PRNTIT                                                        
         MVC   LASTPRD,BUFFPRD                                                  
         AP    TOTPRD(8),BUFFNET   ADD TO HIGHER LEVELS                         
         AP    TOTPRD+8(8),BUFFCOMM                                             
         AP    TOTDUE(8),BUFFNET                                                
         AP    TOTDUE+8(8),BUFFCOMM                                             
         AP    CNTMED,=P'1'                                                     
         B     INV2                                                             
         EJECT                                                                  
*              ROUTINE TO PRINT MEDIA SUMMARY                                   
                                                                                
         USING PRINTD,R5                                                        
MEDSUM   NTR1                                                                   
         LA    R5,P                                                             
         MVI   FORCEHED,C'Y'                                                    
         LA    R1,MEDTAB                                                        
         LA    RF,2                TOTAL OF 2 MEDIAS                            
         LA    R0,NPMED            FIRST DO MEDIA PRINT                         
MEDS2    MVC   PMED,4(R1)          MEDIA NAME                                   
         LA    R2,24(R1)                                                        
         CLC   0(24,R2),PZEROS     SKIP IF NOTHING TO PRINT                     
         BE    MEDS4                                                            
         AP    TOTPRD(8),0(8,R2)   ADD TO HIGHER LEVEL                          
         AP    TOTPRD+8(8),8(8,R2)                                              
         BAS   RE,EDITAMT          FORMAT AMOUNTS                               
         BAS   RE,PRNTIT                                                        
MEDS4    LA    R1,L'MEDTAB(R1)                                                  
         BCT   R0,MEDS2                                                         
         BCT   RF,*+8                                                           
         B     *+14                NOW PRINT MEDIA TOTALS                       
         MVC   PMED(19),=C'** TOTALS FOR PRINT'                                 
         B     *+10                                                             
         MVC   PMED(18),=C'** TOTALS FOR SPOT'                                  
         LA    R2,TOTPRD                                                        
         BAS   RE,EDITAMT          FORMAT TOTALS                                
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         MVC   TOTPRD(24),PZEROS                                                
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         LA    R0,NSMED            NOW DO SPOT MEDIAS                           
         B     MEDS2                                                            
         MVC   PMED(19),=C'* BUY TYPE TOTALS *'                                 
         LA    R2,TOTDUE                                                        
         BAS   RE,EDITAMT          FORMAT TOTALS                                
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT ADDRESS FOR INVOICE REMITTAL                    
                                                                                
REMIT    NTR1                                                                   
         BAS   RE,SWITCH           FORCE BOX BOTTOM (AND NEW TOP)               
         LA    R1,P+23                                                          
         LA    R0,4                                                             
         LA    RF,REMSG                                                         
REMIT2   MVC   0(L'REMSG,R1),0(RF)                                              
         LA    R1,132(R1)                                                       
         LA    RF,L'REMSG(RF)                                                   
         BCT   R0,REMIT2                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
                                                                                
         USING BOXD,R7                                                          
SWITCH   NTR1                      FORCE A BOTTOM AT END OF INVOICE             
         L     R7,ADBOX                                                         
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         ZIC   RF,LINE                                                          
         LA    RE,BOXROWS-1(RF)                                                 
         MVI   0(RE),C'B'                                                       
         MVI   BOXINIT,0                                                        
         BAS   RE,PRNTIT                                                        
         MVI   RCSUBPRG,2          SET SPECIAL BOX AND HEADS                    
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXINIT,0                                                        
         MVI   SPACING,3           SKIP A FEW LINES                             
         OI    STATUS,NOBOX                                                     
         BAS   RE,PRNTIT                                                        
         NI    STATUS,ALL-NOBOX                                                 
         CLI   LINE,50             DO WE HAVE ROOM                              
         BNH   *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RE,BOXROWS-1(RF)                                                 
         MVI   0(RE),C'T'                                                       
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS+22,C'L'                                                  
         MVI   BOXCOLS+23+L'REMSG,C'R'                                          
         MVI   BOXINIT,0                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
SEEPROD  NTR1                                                                   
         CLI   QOPT5,C'A'          ALL PRODUCTS?                                
         BE    YES                                                              
*                                  DEFAULT = ALL PRODUCTS EXCEPT NESTEA         
         LA    R1,PTAB                                                          
         LA    R0,NPRODS                                                        
         USING ACKEYD,R2                                                        
SPSRCH   CLC   QOPT5,0(R1)                                                      
         BNE   SPUP                                                             
         CLC   1(2,R1),ACKEYCON+3                                               
         BE    YES                                                              
SPUP     LA    R1,3(R1)                                                         
         BCT   R0,SPSRCH                                                        
         B     NO                                                               
                                                                                
         ANSR                                                                   
         EJECT                                                                  
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'PUT'                                                       
         LA    R3,RECORD                                                        
         LA    R8,150                                                           
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         AP    PDUMP,=P'1'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD AN ELEMENT                                        
                                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
                                                                                
ADDL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
                                                                                
         USING ACKEYD,R2                                                        
         MVC   HALF,ACLENGTH                                                    
         ZIC   R4,1(R3)                                                         
         AH    R4,HALF                                                          
         CH    R4,=H'999'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 HELLO,ELIST,(C'P',=C'ACCOUNT '),(R2),(R3)                        
         CLI   ELERR,0                                                          
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         EJECT                                                                  
*              ROUTINE TO DELETE AN ELEMENT                                     
                                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
                                                                                
DELL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
                                                                                
         CLI   ELERR,0                                                          
         BNE   XIT                                                              
         GOTO1 HELLO,ELIST,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
                                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
                                                                                
GETL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, TABLES, ETC.                                          
                                                                                
RELOTAB  DS    0A                                                               
         DC    V(SORTER)                                                        
         DC    V(HELLO)                                                         
         DC    V(PRNTBL)                                                        
         DC    A(SORTC)                                                         
         DC    A(BUFFALOC)                                                      
         DC    X'FF'                                                            
                                                                                
*              SORT SEQUENCE -                                                  
*        RAGY, RBTYP, RACN, RPRDNM, RMED, RVND                                  
                                                                                
*ORTCARD DC    CL80'SORT FIELDS=(1,4,A,11,5,A,187,36,A,46,2,A,16,30,A),         
*              FORMAT=BI,WORK=1'                                                
SORTCARD DC    CL80'SORT FIELDS=(1,4,A,011,5,A,011,5,A,187,36,A,46,2,A,X        
               16,30,A),FORMAT=BI,WORK=1'                                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(289,,289)'                            
                                                                                
DDPARM   DC    CL8'ACCOKE  '                                                    
DSPARM   DC    CL20'ACCTAPE.ACCOKE  '                                           
                                                                                
ACCOKE   DCB   DDNAME=ACCOKE,DSORG=PS,RECFM=FB,MACRF=PM,LRECL=150,     X        
               BLKSIZE=150                                                      
         EJECT                                                                  
PZEROS   DC    3PL8'0'                                                          
PDUMP    DC    PL3'0'                                                           
MAXDUMP  DC    PL3'50'                                                          
                                                                                
BTTAB    DS    0CL22                                                            
         DC    C'BA',CL16'AGENCY BOUGHT * ',C'8700'                             
         DC    C'BB',CL16'BOTTLER BOUGHT *',C'8701'                             
         DC    C'CA',CL16'MCCANN BOUGHT * ',C'8702'                             
NBT      EQU   (*-BTTAB)/L'BTTAB                                                
                                                                                
MEDTAB   DS    0CL48               COKE CODE/DDS CODE/ NAME                     
         DC    CL2'01',CL2'PN',CL20'NEWSPAPER',3PL8'0'                          
         DC    CL2'02',CL2'PO',CL20'OUTDOOR',3PL8'0'                            
         DC    CL2'05',CL2'PS',CL20'MISCELLANEOUS',3PL8'0'                      
         DC    CL2'07',CL2'PL',CL20'LOCAL EXTRA',3PL8'0'                        
NPMED    EQU   (*-MEDTAB)/L'MEDTAB                  N'PRINT MEDIAS              
SMED     DC    CL2'03',CL2'SR',CL20'RADIO',3PL8'0'                              
         DC    CL2'04',CL2'ST',CL20'TELEVISION',3PL8'0'                         
         DC    CL2'06',CL2'SM',CL20'MISCELLANEOUS TV',3PL8'0'                   
         DC    CL2'08',CL2'SC',CL20'CABLE',3PL8'0'                              
NSMED    EQU   (*-SMED)/L'MEDTAB                    N'SPOT MEDIAS               
NMED     EQU   (*-MEDTAB)/L'MEDTAB                                              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              AGENCY/PRODUCT TABLE                                   *         
***********************************************************************         
*                                                                               
*         NOTE:  IF YOU ADD A NEW PRODUCT CODE TO THE MCCANN, LINTAS            
*                OR LOWE MARSCHALK TABLE THEN YOU MUST ALSO ADD AN              
*                ENTRY FOR THAT PRODUCT TO THE 'ALL PRODUCTS EXCEPT             
*                NESTEA' TABLE!!!!!                                             
*                                                                               
*              CL1 = QOPT5 VALUE - AGENCY CODE M/S/L/N/C/U OR B                 
*              CL2 = PRODUCT CODE                                               
*                                                                               
***********************************************************************         
PTAB     DS    0CL3                                                             
         DC    C'BCO'              *** ANOTHER GROUP ***                        
         DC    C'BNC'                                                           
         DC    C'BBQ'                                                           
*                                                                               
         DC    C'MCC'              *** MCCANN PRODUCT TABLE ***                 
         DC    C'MCF'                                                           
         DC    C'MCL'                                                           
         DC    C'MCT'                                                           
         DC    C'MDY'                                                           
         DC    C'MPA'                                                           
         DC    C'MPC'                                                           
         DC    C'MPD'                                                           
         DC    C'MPP'                                                           
         DC    C'MPR'                                                           
         DC    C'MPS'                                                           
         DC    C'MNM'                                                           
*                                                                               
         DC    C'SCD'              *** LINTAS PRODUCT TABLE ***                 
         DC    C'STB'                                                           
*                                                                               
         DC    C'KOK'                                                           
*                                                                               
         DC    C'ZBQ'                                                           
*                                                                               
         DC    C'LDM'              *** LOWE MARSCHAL TABLE ***                  
         DC    C'LDC'                                                           
         DC    C'LDS'                                                           
         DC    C'LFN'                                                           
         DC    C'LFR'                                                           
         DC    C'LMJ'                                                           
         DC    C'LML'                                                           
         DC    C'LMM'                                                           
         DC    C'LMO'                                                           
         DC    C'LRB'                                                           
         DC    C'LSP'                                                           
*                                                                               
         DC    C'NNT'              *** NESTEA PRODUCT TABLE ***                 
*                                                                               
         DC    C'USG'              *** SURGE ***                                
*                                                                               
         DC    C'GTC'              *** BRAND GROUP G ***                        
*                                                                               
         DC    C'YMY'              *** MARTIN AGENCY ***                        
         DC    C'YPB'                                                           
*                                                                               
         DC    C'CCY'              *** CHIAT DAY AGENCY ***                     
         DC    C'CFT'                                                           
*                                                                               
         DC    C' BQ'              *** ALL PRODUCTS EXCEPT NESTEA ***           
         DC    C' CC'                                                           
         DC    C' CF'                                                           
         DC    C' CL'                                                           
         DC    C' CO'                                                           
         DC    C' CT'                                                           
         DC    C' CY'                                                           
         DC    C' DY'                                                           
         DC    C' CD'                                                           
         DC    C' DC'                                                           
         DC    C' TB'                                                           
         DC    C' TC'                                                           
         DC    C' DM'                                                           
         DC    C' DS'                                                           
         DC    C' FN'                                                           
         DC    C' FR'                                                           
         DC    C' FT'                                                           
         DC    C' MJ'                                                           
         DC    C' ML'                                                           
         DC    C' MM'                                                           
         DC    C' MO'                                                           
         DC    C' MY'                                                           
         DC    C' NC'                                                           
         DC    C' NM'                                                           
         DC    C' OK'                                                           
         DC    C' PA'                                                           
         DC    C' PB'                                                           
         DC    C' PC'                                                           
         DC    C' PD'                                                           
         DC    C' PP'                                                           
         DC    C' PR'                                                           
         DC    C' PS'                                                           
         DC    C' RB'                                                           
         DC    C' SP'                                                           
NPRODS   EQU   (*-PTAB)/L'PTAB                                                  
         EJECT                                                                  
***********************************************************************         
*              EQUATES TO COVER STATUS BYTE                           *         
***********************************************************************         
*                                                                               
ALL      EQU   X'FF'                                                            
PRD      EQU   X'80'               NEED PRODUCT NAME PRINTED                    
MED      EQU   X'40'               NEED MEDIA NAME PRINTED                      
VEH      EQU   X'20'               NEED VEHICLE NAME PRINTED                    
NOBOX    EQU   X'10'               TURN OFF BOXES                               
GRSONLY  EQU   X'08'               PRINT GROSS AMOUNT ONLY                      
SKIPRNT  EQU   X'04'               DON'T PRINT IF ACCUMS ZERO                   
*                                                                               
REMSG    DS    0CL41                                                            
         DC    C'PLEASE REMIT TO:   MCCANN ERICKSON, INC. '                     
         DC    C'                   P. O. BOX 8643        '                     
         DC    C'                   CHURCH STREET STATION '                     
         DC    C'                   NEW YORK, N. Y.  10249'                     
         EJECT                                                                  
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              BOX ROUTINE                                            *         
***********************************************************************         
*                                                                               
         USING PRINTD,R5                                                        
         USING BOXD,R7                                                          
         DS    0D                                                               
         ENTRY HOOK                                                             
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R7,ADBOX                                                         
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         TM    STATUS,NOBOX                                                     
         BO    HOOKX                                                            
         MVI   BOXROWS+5,C'T'       SET ROWS                                    
         MVI   BOXROWS+56,C'B'                                                  
         CLI   RCSUBPRG,2                                                       
         BE    HOOK2                                                            
         MVI   BOXROWS+7,C'M'                                                   
HOOK0    LA    R5,BOXCOLS           SET COLUMNS                                 
         MVI   BL,C'L'                                                          
         MVI   BR,C'R'                                                          
         MVI   BC1,C'C'                                                         
         CLI   RCSUBPRG,0          SKIP IF NOT PRINTING DETAIL                  
         BE    HOOK1                                                            
         CLI   RCSUBPRG,5                                                       
         BE    HOOK1                                                            
         CLI   RCSUBPRG,3                                                       
         BNE   *+12                                                             
HOOK1    MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC4A,C'C'                                                        
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         B     HOOKX                                                            
                                                                                
HOOK2    MVI   BOXCOLS+22,C'L'                                                  
         MVI   BOXCOLS+23+L'REMSG,C'R'                                          
                                                                                
HOOKX    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
                                                                                
SAVERC   DC    A(0)                                                             
         EJECT                                                                  
         ENTRY SORTC                                                            
SORTC    DS    0D                                                               
         DS    41000C                                                           
                                                                                
         BUFF  LINES=100,ROWS=1,COLUMNS=3,FLAVOR=PACKED,KEYLIST=(38,A)          
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
         SPACE 1                                                                
ACICD    DSECT                                                                  
RELO     DS    F                                                                
ADBOX    DS    A                                                                
ATYPES   DS    0A                                                               
SORTER   DS    V                                                                
HELLO    DS    V                                                                
PRNTBL   DS    V                                                                
ASORTC   DS    A                                                                
ADBUFC   DS    A                                                                
                                                                                
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
                                                                                
STATUS   DS    CL1                 SEE EQUATES                                  
STAT2    DS    CL1                                                              
ST2HELD  EQU   X'80'               TRANSACTION IS UNAPPROVED = HELD             
STRT1    DS    CL3                                                              
END1     DS    CL3                                                              
STRT2    DS    CL2                                                              
END2     DS    CL2                                                              
STRT8    DS    CL8                                                              
END8     DS    CL8                                                              
YEAR     DS    CL1                                                              
FYRUNDT  DS    CL8                 YYYYMMDD RUN DATE                            
FYQSTDT  DS    CL8                 YYYYMMDD QSTART DATE                         
FYQENDT  DS    CL8                 YYYYMMDD QSTART DATE                         
*                                                                               
CNTMTH   DS    PL3                                                              
CNTVEH   DS    PL3                                                              
CNTMED   DS    PL3                                                              
CNTPRD   DS    PL3                                                              
CNTAGY   DS    PL3                                                              
CNTBOT   DS    PL3                                                              
TOTVEH   DS    3PL8                                                             
TOTMED   DS    3PL8                                                             
TOTPRD   DS    3PL8                                                             
TOTAGY   DS    3PL8                                                             
TOTBOT   DS    3PL8                                                             
TOTDUE   DS    3PL8                                                             
RECCNT   DS    PL3                                                              
                                                                                
SAVKEY   DS    CL42                                                             
MYKEY    DS    CL42                                                             
                                                                                
RECORD   DS    CL(RSIZEBIG)        THIS SORT RECORD                             
LASTREC  DS    CL(RSIZEBIG)        LAST SORT RECORD                             
RECORD2  DS    CL(R2SIZE)          NEW FORMAT TAPE RECORD                       
BUFFREC  DS    CL(BUFFLNQ)         BUFFALO RECORD                               
LASTPRD  DS    CL36                                                             
IO       DS    CL1008                                                           
         EJECT                                                                  
***********************************************************************         
*              COKE RECORD DSECT                                      *         
***********************************************************************         
         SPACE 1                                                                
RECD     DSECT                                                                  
RAGY     DS    CL2                 AGENCY              01                       
RBTYP    DS    CL2                 BUY TAPE            BA/CA/BB                 
RTMNTH   DS    CL2                 TRANSMISSION MONTH                           
RTYR     DS    CL4                 TRANSMISSION YEAR                            
RACN     DS    CL5                 BOTTLER ACN NUMBER                           
RVND     DS    CL30                MEDIA VENDOR NAME/LOCATION                   
RMED     DS    CL2                 MEDIA TYPE                                   
RVEH     DS    CL5                 MEDIA VEHICLE CODE                           
RSMNTH   DS    CL2                 SPOT MONTH                                   
RSYR     DS    CL4                 SPOT YEAR                                    
RPRD     DS    CL2                 PRODUCT CODE                                 
RBUYER   DS    CL3                 PURCHASER(AGENCY) CODE                       
RESTN    DS    CL3                 ESTIMATE NUMBER                              
RFILLER  DS    CL1                 FILLER                                       
REXP     DS    CL11                EXPENDITURE AMOUNT                           
RMKT     DS    CL4                 MARKET CODE                                  
RSPARE   DS    CL58                SPACES                                       
RCOMM1   DS    PL8                 COMMISSION (NOT ACCUM)                       
RCAT     DS    CL2                 CARRY CATEGORY FOR NEW FORMAT                
RREPORT  EQU   *                   BEGINNING OF DATA NOT ON TAPE                
RACNNM   DS    CL36                BOTTLER NAME                                 
RPRDNM   DS    CL36                PRODUCT NAME                                 
RNET     DS    PL8                 PACKED EXPENDITURE AMOUNT                    
RCOMM    DS    PL8                 COMMISSION                                   
RGRS     DS    PL8                 GROSS                                        
RSIZE    EQU   *-RECD              SIZE OF SORT RECORD                          
RAGYCD   DS    CL7                 AGENCY CODE FROM ACCOUNT CODE                
RAGYNM   DS    CL36                PRODUCT NAME                                 
RSIZEBIG EQU   *-RECD              SIZE OF SORT RECORD                          
         EJECT                                                                  
***********************************************************************         
*        NEW COKE RECORD DSECT - 1996                                 *         
***********************************************************************         
         SPACE 1                                                                
REC2D    DSECT                                                                  
R2TYR    DS    CL4                 TRANSMISSION YEAR                            
R2TMNTH  DS    CL2                              MONTH                           
R2TDAY   DS    CL2                              DAY                             
R2SYR    DS    CL4                 REQUEST START YEAR                           
R2SMNTH  DS    CL2                               MONTH                          
R2SDAY   DS    CL2                               DAY                            
R2EYR    DS    CL4                 REQUEST END YEAR                             
R2EMNTH  DS    CL2                             MONTH                            
R2EDAY   DS    CL2                             DAY                              
R2ACN    DS    CL5                 BOTTLER ACN NUMBER                           
R2PRD    DS    CL2                 PRODUCT CODE                                 
R2MED    DS    CL2                 MEDIA TYPE                                   
R2BUYER  DS    CL3                 PURCHASER(AGENCY) CODE                       
R2MKT    DS    CL4                 MARKET CODE                                  
R2CAT    DS    CL2                 ESTIMATE CATEGORY                            
R2ESTN   DS    CL3                 ESTIMATE NUMBER                              
R2VEH    DS    CL5                 MEDIA VEHICLE CODE                           
R2VND    DS    CL30                MEDIA VENDOR NAME/LOCATION                   
R2SPYR   DS    CL4                 SPOT YEAR                                    
R2SPMNTH DS    CL2                 SPOT MONTH                                   
R2GRS    DS    CL11                GROSS AMOUNT                                 
R2COM    DS    CL11                COMMISSION AMOUNT                            
R2SPARE  DS    CL42                SPACES                                       
R2SIZE   EQU   *-REC2D             BEGINNING OF DATA NOT ON TAPE                
         EJECT                                                                  
***********************************************************************         
*              BUFFALO RECORD DSECT                                   *         
***********************************************************************         
         SPACE 1                                                                
BUFFD    DSECT                                                                  
BUFFKEY  DS    0CL38                                                            
BUFFPRD  DS    CL36                PRODUCT NAME                                 
BUFFMED  DS    CL2                 MEDIA CODE                                   
BUFFNET  DS    PL8                 PACKED EXPENDITURE AMOUNT                    
BUFFCOMM DS    PL8                 COMMISSION                                   
BUFFGRS  DS    PL8                 GROSS                                        
BUFFLNQ  EQU   *-BUFFD             SIZE OF BUFFALO RECORD                       
         EJECT                                                                  
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
         SPACE 1                                                                
PRINTD   DSECT                                                                  
BL       DS    CL1                                                              
PPROD    DS    CL21                PRODUCT NAME                                 
PPTOT    EQU   *-20                                                             
BC1      DS    CL1                                                              
PMED     DS    CL16                MEDIA NAME                                   
PMTOT    EQU   *-16                                                             
BC2      DS    CL1                                                              
PVEH     DS    CL20                VEHICLE NAME                                 
PVTOT    EQU   *-16                                                             
BC3      DS    CL1                                                              
PMKT     DS    CL6                                                              
BC4      DS    CL1                                                              
PCAT     DS    CL2                                                              
         DS    CL1                                                              
BC4A     DS    CL1                                                              
PMTH     DS    CL6                 MONTH                                        
BC5      DS    CL1                                                              
PESTN    DS    CL3                                                              
BC6      DS    CL1                                                              
PNET     DS    CL13                NET LESS CD                                  
BC7      DS    CL1                                                              
PCOMM    DS    CL13                COMMISSION                                   
BC8      DS    CL1                                                              
PGRS     DS    CL13                GROSS                                        
BR       DS    CL1                                                              
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=TA                                                 
                                                                                
* ACGENMODES                                                                    
* ACGENBOTH                                                                     
* ACGENFILE                                                                     
* ACREPWORKD                                                                    
* DDBUFFALOD                                                                    
* DDBIGBOX                                                                      
* DDREPXTRAD                                                                    
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039ACREPIC02 04/10/15'                                      
         END                                                                    
