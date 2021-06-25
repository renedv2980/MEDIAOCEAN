*          DATA SET NEWRI48    AT LEVEL 005 AS OF 02/23/15                      
*PHASE T32048A,*                                                                
*INCLUDE CLUNPK                                                                 
*INCLUDE NETNET                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'T32048 - TEXACO TAPE'                                           
T32048   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTEX**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)       RA=2ND BASE REGISTER                           
         LA    RA,1(RA)                                                         
         USING T32048,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
         LA    R1,HEADING          ANETWS2/ANETWS3=I/O AREAS                    
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANTWKTP                                                       
*                                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
LR       DS    0H                                                               
*                                                                               
         ZAP   RPTOTCOM,=P'0'                                                   
         ZAP   RPTOTDUE,=P'0'                                                   
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
         B     LR5                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,23,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=533'                                   
*                                                                               
DDNAME   DC    CL8'NTWKTP'                                                      
DSNAME   DC    CL20'NETTAPE.NE0GGAA1'                                           
DCBOPEN  DC    C'N'                                                             
*                                                                               
LR5      DS    0H                                                               
         CLI   TAPEOPT,C'Y'                                                     
         BNE   LR7                                                              
         L     RE,BOOKVAL                                                       
         CLC   =X'90EC',0(RE)                                                   
         BNE   LR7                                                              
         LA    RF,NTWKTP                                                        
         MVC   0(128,RE),0(RF)                                                  
         MVC   DSNAME+13(2),NBSELAGY                                            
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,DDNAME,DSNAME                                          
*****    GOTO1 =V(DYNALLOC),DMCB,DDNAME,DSNAME                                  
         L     R2,ANTWKTP                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         DROP  R4                                                               
*                                                                               
LR7      DS    0H                                                               
         MVI   NBDATA,C'U'                                                      
         NI    NBSPLOPT,X'FF'-X'C0'     TURN OFF SPLIT OPT                      
         MVI   NBUSER+13,0                                                      
*                                                                               
LR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALCLI                                                  
         BNE   LR12                                                             
         L     RF,ACLISTSV                                                      
         L     RE,NBAIO                                                         
         USING CKEY,RE                                                          
         LA    RE,CLIST                                                         
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         DROP  RE                                                               
LR12     CLI   NBMODE,NBPROCUN                                                  
         BE    LR20                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LR50                                                             
         B     LR10                                                             
*                                                                               
*                                                                               
LR20     DS    0H                                                               
         BC    0,LR30                                                           
         OI    LR20+1,X'F0'                                                     
         BAS   RE,DOBILREC                                                      
LR30     BAS   RE,DOUNIT                                                        
         B     LR10                                                             
         SPACE                                                                  
*                                                                               
*                                                                               
LR50     MVI   MYBYTE,0                  SET RECS FROM SORTER FLAG              
*                                                                               
LR51     GOTO1 SORTER,DMCB,=C'GET'       TAKE RECS FROM SORTER                  
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    LR70                                                             
         MVI   MYBYTE,1                                                         
         LA    R1,L'SORTREC                                                     
         MOVE  (SORTREC,(R1)),0(R3)                                             
         BAS   RE,PROCRECS         PRINTS RECS                                  
         B     LR51                                                             
                                                                                
* - EOF SORTER - NO MORE RECORDS                                                
LR70     DS    0H                                                               
         CLI   MYBYTE,1            ARE THERE ANY RECS                           
         BNE   EXIT                                                             
         BAS   RE,INVTOT           YES/PASS FINAL RECORD                        
* - DO PRINTED REPORT TOTAL                                                     
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   P+1(24),=C'*** COMMISSION TOTAL ***'                             
         MVC   P2+1(24),=C'*** AMOUNT DUE TOTAL ***'                            
         EDIT  (P10,RPTOTCOM),(14,P+25),2,ALIGN=LEFT,MINUS=YES                  
         EDIT  (P10,RPTOTDUE),(14,P2+25),2,ALIGN=LEFT,MINUS=YES                 
         BAS   RE,SPOOLIT                                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PUT UNIT BILLING ELEMENTS TO SORTER                                           
*                                                                               
DOUNIT   NTR1                                                                   
*        BAS   RE,GETPRD                                                        
*                                                                               
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   DOUX                                                             
         USING NUBILD,R6                                                        
                                                                                
* - KEY OF SORT RECORD                                                          
DOU10    MVI   SORTREC,X'40'                        SET REC TO BLANKS           
         LA    RF,SORTREC+1                                                     
         LA    R1,SORTLENE-1                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC   AMCLTSV,NBACTAM     SAVE AGY/MEDIA/CLT                           
         MVC   TXSCLT,NBCLICOD                        CLIENT                    
                                                                                
         MVC   BYTE,NUBILPRD                                                    
         BAS   RE,GETPRD                                                        
         MVC   TXSPROD,CURPROD                        3 CHARACTER PROD          
*                                                                               
         CLI   PRODUCT,0           IF FILTERING ON PRODUCT CODE                 
         BE    *+14                                                             
         CLC   CURPROD,PRODUCT     DO IT NOW                                    
         BNE   DOU50                                                            
                                                                                
         EDIT  (B1,NBACTEST),(3,TXSEST),FILL=0        EST                       
                                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(3,WORK)       ACT DATE                 
                                                                                
****     MVC   TXSYRMON,WORK                        YM OF SERVICE(N/A)          
                                                                                
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,TXSINVDT)       INV DATE             
         CLC   TXSINVDT,STRBDAT                                                 
         BL    DOU50                                                            
         CLC   TXSINVDT,ENDBDAT                                                 
         BH    DOU50                                                            
         MVC   TXSINVNO(2),TXSINVDT+2         SET 1ST 2DIGITS FROM DATE         
         MVC   TXSINVNO+2(4),NUBILNUM                      INV NUMBER           
***********************************************                                 
**       CLC   =C'031056',TXSINVNO  PXZ TEST                                    
**       BNE   DOU50                                                            
*************************************************                               
OKTEST   MVI   TXSTYPE,2                                                        
                                                                                
* - SORT RECORD DATA (DETAIL RECORD)                                            
         LA    R2,TXSDATA                                                       
         USING TXDATA,R2                                                        
         MVI   TXDD,C'D'                                                        
                                                                                
         ICM   R3,15,NUBILNET                    * NET COST                     
         CVD   R3,DUB                                                           
         UNPK  TXDNET,DUB+2(6)                                                  
                                                                                
         MVC   TXDCD,=11C'0'                      *CASH DISCOUNT                
                                                                                
*                                                                               
         MVC   DETPROD,TXSPROD                                                  
         MVC   DETEST,TXSEST                                                    
         BAS   RE,GETBFRML       RETURNS BILL FORMULA IN WORK                   
         ICM   R1,15,WORK+1        BILL FORMULA                                 
         BZ    DOU20                                                            
         SR    R0,R0                                                            
         ICM   RF,15,NUBILGRS                                                   
         TM    WORK,X'01'          ADJ BASIS IS GROSS                           
         BZ    DOU18                                                            
         ICM   RF,15,NUBILNET      OR NET                                       
DOU18    DS    0H                                                               
         LR    R4,RF               ****DO BILL FORMULA ON TOTAL                 
         B     DOU22               ****                                         
*****    MR    R0,RF                                                            
*****    L     RF,=F'1000000'                                                   
*****    DRNDR (R0),(RF)                                                        
*****    LR    R4,R1               PUT ADJUSTED COMMISSION INTO R4              
*****    B     DOU22               AND CONTINUE NORMAL PROCESSING               
                                                                                
DOU20    ICM   R4,15,NUBILGRS                    *COMMISSION                    
         SR    R4,R3                              (GROSS-NET = COMM)            
DOU22    CVD   R4,DUB                                                           
         UNPK  TXDCOM,DUB+2(6)                                                  
                                                                                
         MVC   TXDSUBNM(4),NBACTNET               *SUBCONTRACTOR                
                                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(X'20',WORK2)    *MOS YYMMDD            
         MVC   TXDMOS(2),WORK2+2                                                
         MVC   TXDMOS+2(2),=C'19'        ASSUME 19NN                            
         MVC   TXDMOS+4(2),WORK2                                                
         CLC   =C'80',TXDMOS+4                                                  
         BL    *+10                                                             
         MVC   TXDMOS+2(2),=C'20'                                               
                                                                                
* - PUT TO SORTER                                                               
         BAS   RE,PUTSORT                                                       
                                                                                
* - GET NEXT BILLING ELEMENT                                                    
DOU50    BAS   RE,NEXTEL                                                        
         BE    DOU10                                                            
*                                                                               
DOUX     B     EXIT                                                             
         EJECT                                                                  
*  READ BILL RECORDS *                                                          
DOBILREC NTR1                                                                   
         MVI   SORTSAVE,X'40'                                                   
         LA    RF,SORTSAVE+1                                                    
         LA    R1,L'SORTSAVE-1                                                  
         LA    RE,SORTSAVE                                                      
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         LA    R3,KEY                                                           
         USING BKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   BKEYAM,NBACTAM                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     DB20                                                             
DB17     MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
                                                                                
* - FILTER BILL RECORDS                                                         
DB20     DS    0H                                                               
         CLC   KEY(2),KEYSAVE         ID/AM                                     
         BNE   DBX                                                              
         OC    BKEYYSRV(5),BKEYYSRV   MAKE SURE ITS NOT EST REC                 
         BZ    DB17                                                             
         CLI   CLIENT,0                                                         
         BE    *+14                                                             
         CLC   BKEYCLT,CLIENT                                                   
         BNE   DB17                                                             
         CLI   PRODUCT,0                                                        
         BE    *+14                                                             
         CLC   BKEYPRD,PRODUCT                                                  
         BNE   DB17                                                             
         CLI   NBSELEST,0                                                       
         BE    DB30                                                             
         CLI   NBSELESE,0          IS IT RANGE                                  
         BE    DB25                                                             
         CLC   BKEYEST,NBSELEST    YES                                          
         BL    DB17                                                             
         CLC   BKEYEST,NBSELESE                                                 
         BH    DB17                                                             
         B     DB30                                                             
DB25     CLC   BKEYEST,NBSELEST                                                 
         BNE   DB17                                                             
                                                                                
DB30     DS    0H                                                               
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS2                                                      
         GOTO1 GETREC                                                           
         DROP  R3                                                               
         L     R6,AIO                                                           
         USING BILLREC,R6                                                       
         TM    BILSTAT,X'20'       CHK TRUE AOR BILL - SKIP                     
         BO    DB17                                                             
****     GOTO1 =V(PRNTBL),DMCB,=C'PUTSR2',SORTSAVE,C'DUMP',300,=C'1D'           
         CLC   BDATE(6),STRBDAT    CHK BILLING DATE                             
         BL    DB17                                                             
         CLC   BDATE(6),ENDBDAT    CHK BILLING DATE                             
         BH    DB17                                                             
                                                                                
* - BILL RECORD PASSES FILTERS                                                  
         BAS   RE,ESTHEAD                   GET ESTIMATE DESCRIPTION            
                                                                                
* - SET UP KEY FOR SORT RECORD                                                  
         MVI   SORTREC,X'40'                                                    
         LA    R1,SORTLENE-1                                                    
         MOVE  (SORTREC+1,(R1)),SORTREC                                         
         GOTO1 =V(CLUNPK),DMCB,BKEYCLT,TXSCLT                                   
         MVC   TXSPROD,BKEYPRD                                                  
         EDIT  (B1,BKEYEST),(3,TXSEST),FILL=0                                   
         MVC   TXSINVDT,BDATE                                                   
         MVC   TXSINVNO,BINVNO                                                  
*****************                                                               
**       CLC   =C'031056',TXSINVNO                                              
**       BNE   DB17                                                             
**       CLI   BKEYEST,1                                                        
**       BE    OKDB                                                             
**       CLC   =C'045633',TXSINVNO                                              
**       BNE   DB17                                                             
**       CLI   BKEYEST,11                                                       
**       BNE   DB17                                                             
*************************                                                       
OKDB     MVI   TXSTYPE,1                                                        
         EJECT                                                                  
* - DATA FOR SORT RECORD (HEADER RECORD)                                        
                                                                                
         LA    R2,TXSDATA                                                       
         USING TXDATA,R2                                                        
         MVI   TXHH,C'H'                                                        
*                                                                               
         MVC   TXHVENCD,=C'1331980750001'        DFCN AND DWCN                  
         CLC   NBSELAGY,=C'DF'                                                  
         BE    OKDB4                                                            
         CLC   NBSELAGY,=C'DW'                                                  
         BE    OKDB4                                                            
*                                                                               
         MVC   TXHVENCD,=C'0000500000460'        BBDO VENDOR CODE               
         CLC   NBSELAGY,=C'BD'                                                  
         BE    OKDB4                                                            
         CLC   NBSELAGY,=C'BN'                   AND BN?                        
         BE    OKDB4                                                            
*                                                                               
         MVC   TXHVENCD,=C'0000500050808'        OMNY VENDOR CODE               
         CLC   NBSELAGY,=C'OM'                                                  
         BE    OKDB4                                                            
*                                                                               
         MVC   TXHVENCD,=C'1329938710001'                                       
         CLC   NBSELAGY,=C'BT'     BATES                                        
         BE    OKDB4                                                            
         CLC   NBSELAGY,=C'TH'     AND ZENITH                                   
         BE    OKDB4                                                            
*                                                ELSE MUST BE CME               
         MVC   TXHVENCD,=C'7412825880001'        HOUSTON OFFICE = Z             
         CLI   CLTOFFC,C'Z'                                                     
         BE    *+10                                                             
         MVC   TXHVENCD,=C'4109856650002'                                       
*                                                                               
OKDB4    DS    0H                                                               
         MVC   TXHINUM(6),BINVNO                 INVOICE NUMBER                 
                                                                                
         MVC   TXHBUDYR,USER1                    BUDGET YEAR (USER1)            
         OC    TXHBUDYR,SPACES                                                  
                                                                                
         MVC   TXHMBFL,USER1+4                   MEDIA BUY FLAG                 
         OC    TXHMBFL,SPACES                                                   
                                                                                
         GOTO1 DATCON,DMCB,BDATE,(X'20',WORK2)                                  
         MVC   TXHDATE(4),WORK2+2    MOVE IN MMDD    INVOICE DATE               
         MVC   TXHDATE+4(2),=C'19'   ASSUME 19NN                                
         MVC   TXHDATE+6(2),WORK2     MOVE IN YY                                
         CLC   =C'80',WORK2                                                     
         BL    *+10                                                             
         MVC   TXHDATE+4(2),=C'20'                                              
                                                                                
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(X'20',WORK2)      DUE DATE             
         MVC   TXHDUED(4),WORK2+2                                               
         MVC   TXHDUED+4(2),=C'19'                                              
         MVC   TXHDUED+6(2),WORK2                                               
         CLC   =C'80',WORK2                                                     
         BL    *+10                                                             
         MVC   TXHDUED+4(2),=C'20'                                              
                                                                                
         MVC   TXHVENCN(3),TXSCLT                     VENDOR/CLT                
         CLI   TXHVENCN+2,X'40'                                                 
         BH    *+8                                                              
         MVI   TXHVENCN+2,C'0'                                                  
                                                                                
         MVC   TXHVENCN+3(3),TXSPROD                  VENDOR/PROD               
         CLI   TXHVENCN+5,X'40'                                                 
         BH    *+8                                                              
         MVI   TXHVENCN+5,C'0'                                                  
                                                                                
         MVC   TXHVENCN+6(3),TXSEST                   VENDOR/EST                
         OC    TXHVENCN,SPACES                                                  
                                                                                
*        EDIT  (B1,BKEYEST),(3,TXHSUPES),ALIGN=LEFT   SUPER ESTIMATE            
         MVC   TXHSUPES(3),USER1+5                     USER FIELD 6-8           
         OC    TXHSUPES(3),SPACES                                               
                                                                                
         MVC   TXHPRD,BKEYPRD+1                       PRODUCT                   
                                                                                
***            TXHORINV                                 ***DO LATER             
***            TXHMKT                                   ***DO LATER             
                                                                                
         MVC   TXHVEH,USER1+8                              (USER1)              
         OC    TXHVEH,SPACES                                                    
                                                                                
         MVC   TXHATYP,USER1+11                             (USER1)             
         OC    TXHATYP,SPACES                                                   
                                                                                
         MVC   TXHSVC,USER1+13                              (USER1)             
         OC    TXHSVC,SPACES                                                    
                                                                                
         MVC   TXHIDESC(20),ESTIDESC             ESTIMATE DESCRIPTION           
         OC    TXHIDESC,SPACES                                                  
                                                                                
         MVC   TXHTERMS,USER1+15                             (USER1)            
         OC    TXHTERMS,SPACES                                                  
                                                                                
         MVC   TXHPAYR,USER1+17                              (USER1)            
         OC    TXHPAYR,SPACES                                                   
                                                                                
* - CARRY BNET,BAMT,BACTUAL AT END OF INVOICE HEADER REC FOR LATER USE          
* - FIELDS NEEDED FOR PRINTING ON REPORT AND CHECKING UNIT$=INVOICE$            
* - THESE FIELDS ARE THEN CLEARED FROM RECORD                                   
* - (MEA CULPA)                                                                 
****     LA    R5,TXHSHPN+6                                                     
****     EDIT  (B4,BNET),(12,0(R5)),2,MINUS=YES     BNET PRINTABLE FORM         
         MVC   TXHSHPN+20(4),BNET                  BNET BINARY                  
         ICM   R1,15,BACTUAL                                                    
         STCM  R1,15,TXHSHPN+24        BACTUAL                                  
         PACK  DUB,BAMT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,15,TXHSHPN+28        BILLING GROSS                            
*                                                                               
         EJECT                                                                  
                                                                                
         CLI   SORTSAVE+2,X'40'                ...  FIRST TIME                  
         BNE   DB50                                                             
         LA    RF,SORTSAVE                                                      
         LA    R1,L'SORTSAVE                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)                 ...  YES/ STORE RECORD          
         B     DB17                                       GOTO SEQ              
*                                               ...  NO                         
DB50     CLC   SORTREC(TXSLENE),SORTSAVE   MULTIPLE INVOICE SAME DATE           
         BNE   DB60                                                             
*                                                                               
         LA    R2,SORTSAVE                                                      
         USING SORTREC,R2                                                       
         LA    R2,TXSDATA                                                       
         USING TXDATA,R2                                                        
***8     LA    R2,TXDATA                                                        
         ICM   R1,15,BNET                       COMBINE BILLS                   
         ICM   RE,15,TXHSHPN+20         SO ADD TO BILL $                        
         AR    RE,R1                     CARRIED OVER FOR LATER                 
         STCM  RE,15,TXHSHPN+20                                                 
***      LR    R4,RE                                                            
***      LA    R5,TXHSHPN+6                                                     
***      EDIT  ((R4)),(12,0(R5)),2,MINUS=YES PRINT NEW TOTAL                    
*                                                                               
         ICM   R1,15,BACTUAL           BILLING ACTUALS                          
         ICM   RE,15,TXHSHPN+24                                                 
         AR    RE,R1                                                            
         STCM  RE,15,TXHSHPN+24                                                 
*                                                                               
         PACK  DUB,BAMT                 BILLING GROSS                           
         CVB   R1,DUB                                                           
         ICM   RE,15,TXHSHPN+28                                                 
         AR    RE,R1                                                            
         STCM  RE,15,TXHSHPN+28                                                 
         B     DB17                  ARE THERE MORE MULTIPLE INVOICES           
*                                                                               
DB60     DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTSAVE                                     
         LA    RF,SORTSAVE                                                      
         LA    R1,L'SORTSAVE                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
******   GOTO1 =V(PRNTBL),DMCB,=C'PUTSR2',SORTSAVE,C'DUMP',300,=C'1D'           
         B     DB17                                                             
*                                                                               
DBX      CLI   SORTSAVE+2,X'40'                                                 
         BE    DBXX                                                             
         GOTO1 SORTER,DMCB,=C'PUT',SORTSAVE                                     
DBXX     NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         B     EXIT                                                             
         DROP  R6,R2                                                            
         EJECT                                                                  
         SPACE                                                                  
* - READ ESTIMATE HEADER FOR EST DESCRIPTION AND USER FIELD                     
* - CALLED FROM BILLING RECORD READ                                             
ESTHEAD  NTR1                                                                   
         L     R6,AIO                                                           
         USING BILLREC,R6                                                       
         MVC   KEYSV,KEY           SAVE BILL REC KEY                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BKEYAM                                                  
         MVC   KEY+2(6),BKEYCLT        CLT/PROD/EST                             
         CLC   CURESTDK,KEY            DO WE ALREADY HAVE IT                    
         BE    ESTHX                   YES                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),(KEYSAVE)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURESTDK,KEY                                                     
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING EKEY,R4                                                          
         MVC   ESTIDESC,EDESC             SAVE ESTIMATE DESCRIPTION             
         MVC   USER1,EUSER1               SAVE USER1 FIELD                      
         MVC   USER2,EUSER2               SAVE USER2 FIELD                      
*                                                                               
ESTHX    MVC   KEY,KEYSV                  RESTORE ...BILLREC KEYS               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                               ...READ SEQUENCE              
         MVC   AIO,ANETWS2                        ...I/O AREA                   
         B EXIT                                                                 
         DROP  R6                                                               
         EJECT                                                                  
************************                                                        
* - PROCESS SORT RECORDS: HEADER AND DETAIL RECORDS                             
*                         EACH HEADER SHOULD HAVE DETAIL RECORDS                
*                         AND HEADER $ MUST = TOTAL DETAIL $                    
PROCRECS NTR1                                                                   
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         LA    R2,TXSDATA          POINT TO TAPE DATA                           
         USING TXDATA,R2                                                        
***      GOTO1 =V(PRNTBL),DMCB,=C'PROCR',TXHH,C'DUMP',30,=C'1D'                 
         CLI   TXHH,C'H'           IS IT HEADER                                 
         BE    PRC10               YES                                          
* - DETAIL RECORD                                                               
         CLI   TXHEADSV,0      IS THERE HEADER WAITING TO BE PRINTED            
         BE    PRC50           NO/     GO TO DETAIL PROCESSING                  
         BAS   RE,WRTPHEAD     YES/    PRINT HEADER AND SEND TO TAPE            
         B     PRC50           NOW GO TO DETAIL PROCESSING                      
                                                                                
* - HEADER RECORD                                                               
PRC10    DS    0H                                                               
***      GOTO1 =V(PRNTBL),DMCB,=C'PRC10',TXHH,C'DUMP',30,=C'1D'                 
         CLI   PREVTYPE,0          FIRST TIME                                   
         BNE   PRC30                                                            
PRC25    ZAP   ALLNET,=P'0'        YES                                          
         ZAP   ALLCOM,=P'0'                                                     
         ZAP   HEADTOT,=P'0'                                                    
         ZAP   HEADNET,=P'0'                                                    
         ZAP   HEADACT,=P'0'                                                    
         ZAP   DETTOT,=P'0'                                                     
         ZAP   ALLADJ,=P'0'                                                     
         ZAP   ALLDUE,=P'0'                                                     
         MVI   PREVTYPE,C'H'                                                    
         MVI   PREVERR,0                                                        
         MVC   PREVKEY,SORTREC                                                  
         B     PRC40                                                            
*                                                                               
PRC30    CLI   PREVTYPE,C'D'       WAS PREVIOUS DETAIL                          
         BNE   PRC35                                                            
         CLI   PREVERR,C'Y'        WAS PREVIOUS IN ERROR CONDITION              
         BNE   PRC32                                                            
         MVI   PREVERR,0           YES/DONT CHK TOTS                            
         B     PRC25                                                            
*                                                                               
PRC32    CP    HEADTOT,DETTOT      ARE PREVIOUS HEADER/DETAIL TOTS OK           
         BNE   *+12                                                             
         BAS   RE,INVTOT                                                        
         B     PRC25                                                            
         BAS   RE,INVTOT                                                        
         MVI   P+1,C'*'                                                         
         MVC   P+2(10),P+1                                                      
         MVC   PUNDATE+8(34),=C'***  ERROR HEADER-DETAIL $$$   ***'             
         BAS   RE,SPOOLIT                                                       
         BAS   RE,SPOOLIT                                                       
         B     PRC25                                                            
                                                                                
* - PREVIOUS WAS HEADER                                                         
PRC35    CLI   PREVTYPE,C'H'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PREVKEY(TXSLENE),SORTREC   EQUAL KEYS                            
         BE    PRC40                      MULTIPLE INVOICES/OK                  
                                                                                
* - 2 HEADERS IN SUCCESSION AND KEYS NOT EQUAL - ERROR / NO DETAILS             
         BAS   RE,WRTPHEAD         WRITE PREVIOUS HEADER RECORD                 
         MVC   PUNDATE+8(34),=C'***  ERROR HEADER-DETAIL ERROR ***'             
         BAS   RE,SPOOLIT                                                       
         BAS   RE,SPOOLIT                                                       
         B     PRC25                                                            
*                                                                               
PRC40    DS    0H                                                               
         MVC   TXHH,=C'10'         HEADER IS 10                                 
         MVC   PTYPE,TXHH                                                       
         MVC   PINVDAT,TXHDATE                                                  
         MVC   PINVNO,TXHINUM                                                   
         MVC   PDESCR(3),TXSCLT                                                 
         MVC   PDESCR+4(3),TXSPROD                                              
         MVC   PDESCR+8(3),TXSEST                                               
         MVC   PDUEDAT,TXHDUED                                                  
         ICM   R1,15,TXHSHPN+20          . ADD TO HEADER TOTAL                  
         CVD   R1,DUB                                                           
         AP    HEADTOT,DUB                                                      
         AP    HEADNET,DUB                                                      
         MVC   TXHSHPN+20(4),=12X'40'    . AND CLEAR IT FROM TAPE               
         EDIT  (P10,HEADTOT),(12,PNETCOST),2,MINUS=YES                          
*****    MVC   PNETCOST(12),TXHSHPN+6    . MOVE NETCOST TO P LINE               
*****    MVC   TXHSHPN+6(12),=12X'40'    . AND CLEAR IT FROM TAPE               
         ICM   R1,15,TXHSHPN+24          . ADD BACTUAL TO ALLDUE                
         CVD   R1,DUB                                                           
         AP    ALLDUE,DUB                                                       
         AP    HEADACT,DUB                                                      
         ICM   R0,15,TXHSHPN+28          . BACT-BAMT = ALLADJ                   
         SR    R1,R0                                                            
         CVD   R1,DUB                                                           
*********AP    ALLADJ,R1           CORRECTED BY DEIS: 2/23/2015                 
         AP    ALLADJ,DUB                                                       
         MVC   TXHSHPN+24(4),=12X'40'    . CLEAR BACT FROM TAPE                 
         MVC   TXHSHPN+30(4),=12X'40'    . CLEAR BAMT FROM TAPE                 
         MVC   PLINESV,P           SAVE PRINT LINE                              
         LA    RF,TXHEADSV                                                      
         LA    R1,TXSDLENE                                                      
         LA    RE,TXSDATA                                                       
         MOVE  ((RF),(R1)),(RE)    SAVE DATA FOR TAPE                           
**       MVC   TXHEADSV,TXSDATA    SAVE DATA FOR TAPE                           
****     B     PRCX                                                             
***      GOTO1 =V(PRNTBL),DMCB,=C'PRC40',TXHEADSV,C'DUMP',30,=C'1D'             
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*                                                                               
* - WRITE HEADER RECORD TO PRINT LINE AND TAPE                                  
                                                                                
WRTPHEAD NTR1                                                                   
* - SET TEXACO ID TO PRINT LINE AND TAPE                                        
         MVI   P,X'40'                                                          
         MVC   P+1(L'P-1),P                                                     
         MVC   P(13),=C'*****TEX01810'   BEFORE EACH HEADER RECORD              
         CLI   TAPEOPT,C'Y'                                                     
         BNE   PHD10                                                            
         L     R1,ANTWKTP                                                       
         PUT   (R1),P                                                           
PHD10    MVC   P(2),=2X'40'                                                     
         MVC   P+1(13),=C'*****TEX01810'                                        
         BAS   RE,SPOOLIT                                                       
                                                                                
* - PRINT SAVED HEADER PRINT LINE                                               
         MVC   P,PLINESV                                                        
         BAS   RE,SPOOLIT                                                       
         XC    PLINESV,PLINESV     CLEAR SAVED HEADER PRINT LINE                
                                                                                
* - IF TAPE/ SET SAVED HEADER TAPE DATA TO TAPE                                 
         CLI   TAPEOPT,C'Y'                                                     
         BNE   PHD20                                                            
         L     R1,ANTWKTP                                                       
         PUT   (R1),TXHEADSV                                                    
                                                                                
* - CLEAR SAVED HEADER TAPE DATA                                                
PHD20    LA    RE,TXHEADSV                                                      
         LA    RF,TXSDLENE                                                      
         XCEF                                                                   
PHDX     B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
PRC50    CLI   TXDD,C'D'            *** DETAIL RECORD ***                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PREVTYPE,0          IF FIRST REC                                 
         BNE   PRC53                                                            
         ZAP   ALLNET,=P'0'        ERROR                                        
         ZAP   ALLCOM,=P'0'                                                     
         ZAP   ALLADJ,=P'0'                                                     
         ZAP   ALLDUE,=P'0'                                                     
         ZAP   HEADTOT,=P'0'                                                    
         ZAP   DETTOT,=P'0'                                                     
         MVI   PREVERR,C'Y'                                                     
         MVC   PREVKEY(TXSLENE),SORTREC                                         
         B     PRC55                                                            
*                                                                               
PRC53    DS    0H                            CHK SORTREC FIELDS                 
*                          DETAIL REC KEY SHOULD ALWAYS EQUAL PREV              
*                          RECORD KEY WHETHER IT IS HEADER OR DETAIL            
*                                                                               
         CLC   PREVKEY(TXSLENE),SORTREC                                         
         BE    PRC55                         NOT EQUAL MEANS ERROR              
         CLI   PREVTYPE,C'H'                     HEADER ERROR                   
         BNE   PRC54                                                            
         MVC   PREVKEY(TXSLENE),SORTREC                                         
         MVC   PUNDATE+9(34),=C'*** ERROR  HEADER-DETAIL ERROR ***'             
         BAS   RE,SPOOLIT                                                       
         BAS   RE,SPOOLIT                                                       
         MVI   PREVERR,C'Y'                                                     
         B     PRC55                                                            
*                                                                               
PRC54    DS    0H                                                               
         CLI   PREVTYPE,C'D'                     DETAIL ERROR                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PREVERR,C'Y'                                                     
         ZAP   DETTOT,=P'0'                                                     
         ZAP   HEADTOT,=P'0'                                                    
*                                                                               
*                                                                               
*                                                                               
PRC55    MVI   PREVTYPE,C'D'                                                    
         MVC   TXDD,=C'30'         DETAIL IS NOW 30                             
         MVC   PTYPE,TXDD                                                       
*                                                                               
         MVC   DETPROD,TXSPROD     SAVE PRODUCT FOR COMMISSION TOT CALL         
         MVC   DETEST,TXSEST       SAVE EST FOR COM CALL                        
         PACK  PKW,TXDNET                                                       
         AP    ALLNET,PKW          END OF REPORT TOTAL                          
         AP    DETTOT,PKW          HEADER-DETAIL CONTROL TOT                    
         TM    TXDNET+10,X'D0'     IF NEGATIVE                                  
         BNO   *+8                                                              
         MVI   TXDNET,C'-'                                                      
         OI    TXDNET+10,X'F0'                                                  
         EDIT  (P8,PKW),(12,PNETCOST),2,MINUS=YES                               
         PACK  PKW,TXDCOM                                                       
         AP    ALLCOM,PKW              END OF REPORT TOTAL                      
         MVC   TXDCOM,=11C'0'          CLEAR TXDCOM FORM RECORD                 
         MVC   PUNDATE(6),TXDMOS                                                
PRCX     DS    0H                                                               
         BAS   RE,SPOOLIT                                                       
         BAS   RE,WRITAPE                                                       
         B     EXIT                                                             
         EJECT                                                                  
WRITAPE  NTR1                                                                   
         B     SKIPHEX                                                          
         L     R1,CNTR                                                          
         C     R1,=F'10'                                                        
         BH    SKIPHEX                                                          
         LA    R1,1(R1)                                                         
         ST    R1,CNTR                                                          
         GOTO1 HEXOUT,DMCB,TXSDATA,P,300                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
SKIPHEX  CLI   TAPEOPT,C'Y'                                                     
         BNE   EXIT                                                             
         L     R1,ANTWKTP                                                       
         PUT   (R1),TXSDATA                   WRITE TAPE                        
         B     EXIT                                                             
         SPACE 2                                                                
*******************************************************                         
*   PUTS RECORD TO SORTER                                                       
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     EXIT                                                             
         SPACE 2                                                                
*******************************                                                 
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
**********************************                                              
*  TO GET PRD CODE FROM C LIST                                                  
*                                                                               
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   CURPROD,=C'***'    SET TO UNDEFINED                              
         B     GPX                                                              
GP12     CLC   BYTE,3(R2)                                                       
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   CURPROD,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
*                                                                               
GPX      B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
INVTOT   NTR1                       INVOICE TOTALS                              
         LA    RF,SORTSAVE                                                      
         LA    RE,SORTREC                                                       
         LA    R1,L'SORTSAVE                                                    
         MOVE  ((RF),(R1)),(RE)                                                 
***      MVC   SORTSAVE,SORTREC                                                 
         MVI   SORTREC,X'40'                                                    
         LA    RF,SORTREC+1                                                     
         LA    RE,SORTREC                                                       
         LA    R1,SORTLENE-1                                                    
         MOVE  ((RF),(R1)),(RE)                                                 
**       MVC   SORTREC+1(SORTLENE-1),SORTREC                                    
         LA    R2,SORTREC                                                       
         LA    R2,TXSDATA                                                       
         USING TXDATA,R2                                                        
         MVC   TXTT,=C'80'                                                      
         L     R4,ANTWKTP                                                       
         LA    R4,128(R4)                                                       
         USING RTOTD,R4                                                         
         UNPK  TXTNET,ALLNET+4(6)                                               
         TM    TXTNET+10,X'D0'                                                  
         BNO   *+8                                                              
         MVI   TXTNET,C'-'                                                      
         OI    TXTNET+10,X'F0'                                                  
*************************************                                           
* WORK BILL FORMULA MAGIC ON TOTAL                                              
*                                                                               
**       MVC   WORK(3),DETPROD   SET PRODUCT FROM DETAIL RECORDS                
*8       BAS   RE,GETBFRML       RETURNS BILL FORMULA IN WORK                   
*8       ICM   R1,15,WORK+1        BILL FORMULA                                 
**       BZ    BFM20                                                            
**       CVD   R1,DUB                                                           
*8       MP    ALLCOM,DUB+4(4)                                                  
*8       MVC   BYTE,ALLCOM+9       IF NEGATIVE                                  
*8       OI    BYTE,X'F0'                                                       
**       CLI   BYTE,X'FD'                                                       
**       BE    BFM19               DON'T ADD                                    
**       AP    ALLCOM,=P'500000'   ROUND UP                                     
**BFM19    DP    ALLCOM,=P'1000000'                                             
**         ZAP   WORK(10),ALLCOM(6)                                             
**         ZAP   ALLCOM,WORK(10)                                                
BFM20    EQU   *                                                                
         SP    HEADACT,HEADNET                                                  
         ZAP   ALLCOM,HEADACT                                                   
**************************************************************                  
         AP    RPTOTCOM,ALLCOM     PRINTED REPORT COMM TOTAL                    
         UNPK  TXTCOM,ALLCOM+4(6)                                               
         TM    TXTCOM+10,X'D0'                                                  
         BNO   *+8                                                              
         MVI   TXTCOM,C'-'                                                      
         OI    TXTCOM+10,X'F0'                                                  
         MVC   TXTCD,=11C'0'                                                    
         AP    RPTOTDUE,ALLDUE     PRINTED REPORT DUE TOTAL                     
         UNPK  TXTDUE,ALLDUE+4(6)                                               
         TM    TXTDUE+10,X'D0'                                                  
         BNO   *+8                                                              
         MVI   TXTDUE,C'-'                                                      
         OI    TXTDUE+10,X'F0'                                                  
         BAS   RE,WRITAPE                                                       
* SET UP PRINT LINE                                                             
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   PTYPE,=C'80'                                                     
         EDIT  (P10,ALLNET),(12,PNETCOST),2,MINUS=YES                           
         EDIT  (P10,ALLCOM),(12,PCOMMI),2,MINUS=YES                             
         BAS   RE,SPOOLIT                                                       
         LA    RF,SORTREC                                                       
         LA    R1,L'SORTREC                                                     
         LA    RE,SORTSAVE                                                      
         MOVE  ((RF),(R1)),(RE)                                                 
**       MVC   SORTREC,SORTSAVE                                                 
LASTRX   XIT1                                                                   
         DROP  R2,R3,R4                                                         
*                                                                               
         EJECT                                                                  
**************************************                                          
*                                                                               
* READ FOR ESTIMATE FIRST, THEN PROD THEN PROD=AAA                              
*                                                                               
* WORK HAS PRODUCT                                                              
GETBFRML NTR1                                                                   
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
**       MVC   KEY+1(3),NBKEY+1    AGY/CLI                                      
         MVC   KEY+1(3),AMCLTSV    AGY/CLI                                      
**       MVC   KEY+4(3),TXSPROD    PRODUCT                                      
         MVC   KEY+4(3),DETPROD    PRODUCT                                      
         XC    WORK(5),WORK                                                     
                                                                                
**       MVC   KEY+7(1),NBACTEST   ESTIMATE                                     
         MVC   WORK(3),DETEST                                                   
         OC    WORK(3),=X'F0F0F0'                                               
         PACK  DUB,WORK(3)                                                      
         CVB   R1,DUB                                                           
         STC   R1,KEY+7                                                         
         XC    WORK(5),WORK                                                     
**                                                                              
         BAS   RE,CHKTBL           CHECK BILL FRML TABLE                        
         BE    GB50                GOT IT                                       
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GB20                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS2                                                      
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING ESTHDR,R3                                                        
         OC    EBILLBAS(5),EBILLBAS BILL FORMULA                                
         BZ    GB20                                                             
         MVC   WORK(5),EBILLBAS                                                 
         BAS   RE,FILLTBL          PUT BILLFORMULA TO TABLE                     
         B     GB50                                                             
                                                                                
* - TRY PRODUCT HEADER                                                          
GB20     XC    KEY,KEY                                                          
**       MVC   KEY+1(3),NBKEY+1    AGY/CLI                                      
         MVC   KEY+1(3),AMCLTSV    AGY/CLI                                      
**       MVC   KEY+4(3),TXSPROD    PRODUCT                                      
         MVC   KEY+4(3),DETPROD    PRODUCT                                      
         BAS   RE,CHKTBL           CHECK BILL FRML TABLE                        
         BE    GB50                GOT IT                                       
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GB30                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,ANETWS2                                                      
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING PRDHDR,R3                                                        
         OC    PBILLBAS(5),PBILLBAS      BILL FORMULA                           
         BZ    GB30                                                             
         MVC   WORK(5),PBILLBAS                                                 
         BAS   RE,FILLTBL          PUT BILLFORMULA TO TABLE                     
         B     GB50                                                             
                                                                                
* TRY PROD - AAA                                                                
GB30     XC    KEY,KEY                                                          
**       MVC   KEY+1(3),NBKEY+1    AGY/CLI                                      
         MVC   KEY+1(3),AMCLTSV    AGY/CLI                                      
         MVC   KEY+4(3),=C'AAA'    PRODUCT                                      
         BAS   RE,CHKTBL           CHECK BILL FRML TABLE                        
         BE    GB50                                                             
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GB30                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,ANETWS2                                                      
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING PRDHDR,R3                                                        
         OC    PBILLBAS(5),PBILLBAS      BILL FORMULA                           
         BZ    GB50                                                             
         MVC   WORK(5),PBILLBAS                                                 
         BAS   RE,FILLTBL          PUT BILLFORMULA TO TABLE                     
         B     GB50                                                             
GB50     DS    0H                                                               
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI     RESET SEQ READ                               
         NETGO NVSETUNT,DMCB                                                    
GBX      XIT1                                                                   
                                                                                
*                                                                               
CHKTBL   NTR1                   DO I ALREADY HAVE FORMULA                       
         L     R1,=A(BILLTBL)                                                   
CKT10    CLC   KEY(8),0(R1)                                                     
         BE    CKT20                                                            
         LA    R1,13(R1)                                                        
         CLI   0(R1),X'FF'         EOF?                                         
         BNE   *+6                                                              
         DC    H'0'                EXPAND TABLE                                 
         OC    0(5,R1),0(R1)                                                    
         BNZ   CKT10                                                            
         B     CKTNO                                                            
CKT20    MVC   WORK(5),8(R1)                                                    
         SR    R1,R1                                                            
CKTNO    LTR   R1,R1                                                            
         XIT1                                                                   
                                                                                
*                                                                               
FILLTBL  NTR1                                                                   
         L     R1,=A(BILLTBL)                                                   
FILL10   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                EXPAND TABLE                                 
         OC    0(5,R1),0(R1)       OPEN SLOT                                    
         BZ    FILL20                                                           
         LA    R1,13(R1)                                                        
         B     FILL10                                                           
FILL20   MVC   0(8,R1),KEY                                                      
         MVC   8(5,R1),WORK                                                     
         OC    9(4,R1),9(R1)       IF ZERO                                      
         BNZ   FILLX                                                            
         MVC   9(4,R1),=F'150000'  ASSUME 15 PERCENT                            
         MVC   WORK+1(4),=F'150000'  ASSUME 15 PERCENT                          
FILLX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+15(20),SPLPRON                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(7),SPLEST                                                  
         MVC   H5+18(20),SPLESTN                                                
         MVC   H3+51(6),=C'PERIOD'                                              
         GOTO1 DATCON,DMCB,(0,STRBDAT),(5,H3+58)                                
         MVI   H3+67,C'-'                                                       
         GOTO1 DATCON,DMCB,(0,ENDBDAT),(5,H3+69)                                
         LA    R6,H8                                                            
         USING PLINED,R6                                                        
         MVC   PTYPE+132(2),=C'ID'                                              
         MVC   PINVDAT(7),=C'INVOICE'                                           
         MVC   PINVDAT+133(4),=C'DATE'                                          
         MVC   PINVNO(7),=C'INVOICE'                                            
         MVC   PINVNO+133(6),=C'NUMBER'                                         
         MVC   PDESCR+132(11),=C'CLT/PRD/EST'                                   
         MVC   PDUEDAT+1(3),=C'DUE'                                             
         MVC   PDUEDAT+133(4),=C'DATE'                                          
         MVC   PNETCOST+132(12),=C'NET UNITCOST'                                
         MVC   PCOMMI+132(12),=C' COMMISSION '                                  
***      MVC   PNET+132(4),=C'NTWK'                                             
         MVC   PUNDATE(7),=C'  UNIT '                                           
         MVC   PUNDATE+133(6),=C' DATE '                                        
         DROP  R6                                                               
*                                                                               
         DS    0H                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R4,BOXCOLS                                                       
         USING PLINED,R4                                                        
         MVI   PSTR,C'L'                                                        
         MVI   PINVDAT-1,C'C'                                                   
         MVI   PINVNO-1,C'C'                                                    
         MVI   PDBCRR-1,C'C'                                                    
         MVI   PDESCR-1,C'C'                                                    
         MVI   PDUEDAT-1,C'C'                                                   
         MVI   PNETCOST-1,C'C'                                                  
         MVI   PCOMMI-1,C'C'                                                    
***      MVI   PNET-1,C'C'                                                      
         MVI   PUNDATE-1,C'C'                                                   
         MVI   PUNDATE+8,C'R'                                                   
         SPACE                                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,6(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' TEXACO BILLING TAPE'                                    
         SSPEC H2,52,C' -------------------'                                    
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
NTWKTP   DCB   DDNAME=NTWKTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00503,                                            X        
               BLKSIZE=05030,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
**                                                                              
MYIO     DS    CL2000                                                           
*                                                                               
BILLTBL  DS    CL(13*300)          ROOM FOR 300 ENTRIES                         
         DC    X'FF'                                                            
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS4                         
*                                                                               
*                                                                               
MYWORK   DS    D                *  FROM EDIT MODULE *                           
RELO     DS    F                *                   *                           
ACLISTSV DS    F                *                   *                           
TAPEOPT  DS    CL1              *                   *                           
CLIENT   DS    CL2              *                   *                           
PRODUCT  DS    CL3              *                   *                           
STARTDAT DS    CL2              *                   *                           
ENDDAT   DS    CL2              *                   *                           
STRBDAT  DS    CL6              *                   *                           
ENDBDAT  DS    CL6              *                   *                           
CLTOFFC  DS    CL1              *  FROM EDIT MODULE *                           
*                                                                               
DETPROD  DS    CL3               SAVE PRODUCT FROM DETAIL RECORDS               
DETEST   DS    CL3               SAVE PRODUCT FROM DETAIL RECORDS               
*                                USED AT COMMISSION TOTAL CALCULATION           
*                                                                               
ANTWKTP  DS    F                                                                
DETCNT   DS    F                                                                
SVSPOOL  DS    F                                                                
CNTR     DS    F                                                                
NETRATES DS    CL8                                                              
CURPROD  DS    CL3                                                              
BOXSET   DS    CL1                                                              
MYBYTE   DS    CL1                                                              
KEYSV    DS    CL20                                                             
WORK2    DS    CL40                                                             
ESTIDESC DS    CL20                                                             
USER1    DS    CL32                                                             
USER2    DS    CL16                                                             
CURESTDK DS    CL13         CURRENT EST DESCRIPTION KEY                         
PREVKEY  DS    CL30                                                             
PREVTYPE DS    CL1                                                              
PREVERR  DS    CL1                                                              
*                                                                               
AMCLTSV  DS    CL3                 AGY/MEDIA/CLT SAVE                           
*                                                                               
HEADTOT  DS    PL10                                                             
DETTOT   DS    PL10                                                             
ALLNET   DS    PL10                REPORT NET AMOUNT                            
ALLCOM   DS    PL10                REPORT COMMISSION AMOUNT                     
ALLADJ   DS    PL10                REPORT ADJUSTMENT AMOUNT                     
ALLDUE   DS    PL10                REPORT DUE AMOUNT                            
PKW      DS    PL8                                                              
RPTOTCOM DS    PL10                COMMISSION TOTAL FOR REPORT                  
RPTOTDUE DS    PL10                AMOUNT DUE FOR REPORT                        
HEADNET  DS    PL10                ONE INVOICE NUMBER NET                       
HEADACT  DS    PL10                ONE INVOICE NUMBER ACTUAL                    
*                                                                               
         DS    0D                                                               
         DC    C'**SORT**'                                                      
SORTREC  DS    CL(SORTLENE)                                                     
         ORG   SORTREC                                                          
TXSYRMON DS    CL2                 YEAR/MONTH OF SERVICE (NOT USED)             
TXSINVDT DS    CL6                 INVOICE DATE                                 
TXSINVNO DS    CL6                 INVOICE NUMBER                               
TXSCLT   DS    CL3                                                              
TXSPROD  DS    CL3                                                              
TXSEST   DS    CL3                                                              
TXSLENE  EQU   *-TXSYRMON                                                       
TXSTYPE  DS    CL1                 TYPE HEADER=1,DETAIL=2                       
TXSDATA  DS    CL503                                                            
TXSDLENE EQU   *-TXSDATA                                                        
SORTLENE EQU   *-TXSYRMON                                                       
*                                                                               
SORTSAVE DS    CL(SORTLENE)                                                     
WORKLENE EQU   *-RELO                                                           
*                                                                               
PLINESV  DS    CL132                                                            
TXHEADSV DS    CL(TXSDLENE)                                                     
RTOTD    DSECT REPORT TOTAL DSECT                                               
TOTCNT   DS    F                   TOTAL UNITS FOR RUNLAST                      
TOTNET   DS    PL10                NET AMOUNT FOR RUNLAST                       
TOTCOM   DS    PL10                COMMISSION AMOUNT FOR RUNLAST                
         EJECT                                                                  
*                                                                               
PLINED   DSECT                    DSECT FOR PRINTED REPORT                      
PSTR     DS    CL1                                                              
PTYPE    DS    CL2                 C'10/20/30'                                  
         DS    CL2                                                              
PINVDAT  DS    CL8                 INVOICE DATE                                 
         DS    CL2                                                              
PINVNO   DS    CL6                 INVOICE NUMBER                               
         DS    CL2                                                              
PDBCRR   DS    CL2                 DEBIT/CREDIT                                 
         DS    CL1                                                              
PDESCR   DS    CL12                CLIENT/PROD/ESTIMATE                         
         DS    CL1                                                              
PDUEDAT  DS    CL8                 DUE DATE                                     
         DS    CL1                                                              
PNETCOST DS    CL12                NET UNIT COST                                
         DS    CL1                                                              
PCOMMI   DS    CL12                COMMISSION                                   
***      DS    CL1                                                              
***PNET     DS    CL4                 NETWORK                                   
         DS    CL1                                                              
PUNDATE  DS    CL8                 UNIT DATE                                    
         DS    CL1                                                              
PLENE    EQU   *-PSTR                                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDTEXACOD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID3D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
BILHD    DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEWRI48   02/23/15'                                      
         END                                                                    
