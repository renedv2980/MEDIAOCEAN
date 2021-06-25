*          DATA SET ACTRA07    AT LEVEL 064 AS OF 02/15/19                      
*PHASE T62207B                                                                  
*INCLUDE SPFMTINO                                                               
*INCLUDE PPFMTINO                                                               
         SPACE                                                                  
         TITLE '(T62207)  BILLING TRANSFER - BILL LIST OVERLAY'                 
T62207   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T62207**,R9,RR=RE                                              
*                                                                               
         USING TWAD,R5            R5=A(TWA)                                     
         USING SAVAREA,R6         R6=A(SAVE AREA)                               
         USING WORKD,R7           R7=A(GLOBAL WORKING STORAGE)                  
         L     RC,APALOCAL        RC=A(LOCAL WORKING STORAGE)                   
         USING LOCALD,RC                                                        
*                                                                               
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO             RELOCATION FACTOR                          
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   APMODE,APMVALP        VALIDATE KEY FOR LIST                      
         BE    VALPAR                                                           
         CLI   APMODE,APMFSCR        FIRST FOR LIST                             
         BE    GETFRST                                                          
         CLI   APMODE,APMGETS        GET RECORD FOR LIST                        
         BE    GETSEL                                                           
         CLI   APMODE,APMDISS        DISPLAY RECORD FOR LIST                    
         BE    DISSEL                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
YES      CR    RB,RB                                                            
         B     EXIT                                                             
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*========================*                                                      
* VALPAR  - VALIDATE KEY *                                                      
*========================*                                                      
*                                                                               
VALPAR   XC    APRECKEY,APRECKEY                                                
*                                 VALIDATE SYSTEM                               
         GOTO1 AVALSYS,BLISYSH                                                  
         BNE   VALX                                                             
         CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   VAL10                                                            
         BAS   RE,VALPKEY         VALIDATE MEDIA/CLT/PRD/DATES                  
         BNE   VALX                                                             
         B     VAL40                                                            
*                                                                               
VAL10    BAS   RE,VALSKEY         SPOT/NET VALIDATION                           
         BNE   VALX                                                             
*                                                                               
VAL40    GOTO1 ASETFILE           SET FILE & CHECK LIMIT ACCESS                 
         BE    VAL50                                                            
         LA    R2,BLISYSH                                                       
         ST    R2,APCURSOR                                                      
         B     VALX                                                             
*                                                                               
VAL50    MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   SELKEY,APRECKEY                                                  
         LA    RE,BLISL1H         SET APPARM FIELDS FOR CONTROLLER              
         ST    RE,APPARM          A(FIRST TWA SELECT FIELD HEADER)              
         LA    RE,14              # NUMBER OF LINES ON SCREEN                   
         STC   RE,APPARM+4                                                      
         LA    RE,BLISL2H-BLISL1H                                               
         STCM  RE,3,APPARM+6      LENGTH OF SELECT LINE                         
*                                                                               
VALX     B     EXIT                                                             
         SPACE                                                                  
MISSERR  STCM  R3,15,APCURSOR                                                   
         MVC   FVMSGNO,=AL2(FVIMISS)                                            
         LTR   RB,RB                                                            
         B     EXIT                                                             
         SPACE                                                                  
VALKNOTV MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------*                  
* VALSKEY - VALIDATE KEY FOR SPOT/NET                        *                  
*          APRECKEY = BKEY(13) + QOFF(2) + QMED(1) + QIDT(6) *                  
*------------------------------------------------------------*                  
*                                                                               
VALSKEY  NTR1                                                                   
         LA    R2,APRECKEY                                                      
         USING BILLRECD,R2        SPOT/NET VALIDATION                           
*                                                                               
         LA    R3,BLIMEDH                                                       
         GOTO1 AVALMED,(R3)                                                     
         BNE   VALSX              CC CODE SET                                   
         MVC   BKEYAM,BAGYMD      AGY/MED                                       
         OC    QMED,QMED          IF NO MEDIA CODE                              
         BNZ   VALS5                                                            
         GOTO1 AVALFLD,APPARM,BLIOFFH,4                                         
         CLI   APPARM,X'FF'       MAKE SURE NO OFF/CLT/PRD/EST                  
         BE    MISSERR            OTHERWISE MISSG INPUT FLD FOR MEDIA           
         BAS   RE,RDAGY           GET AGY REC INTO AIO2 & SET BAGYMD            
         MVC   BKEYAM,BAGYMD                                                    
VALS5    CLI   QSYS,C'N'          IF NETWORK                                    
         BNE   VALS10             BAGYMD IS SAME FOR DIFF MEDIAS                
         MVC   APRECKEY+15(1),QMED NEED QMED TO FORCE CHANGE IN KEY             
*                                                                               
VALS10   LA    R3,BLIOFFH         VALIDATE OFFICE                               
         MVI   OCFLAG,0                                                         
         GOTO1 AVALOFF,(R3)                                                     
         BNE   VALSX                                                            
         MVC   APRECKEY+13(2),QOFF                                              
         OC    QOFF,QOFF          IF OFFICE INPUT                               
         BZ    VALS20                                                           
         MVI   OCFLAG,C'O'        INDICATE SO                                   
*                                                                               
VALS20   LA    R3,BLICLTH         VALIDATE CLIENT                               
         GOTO1 AVALCLT,(R3)                                                     
         BNE   VALSX                                                            
         MVC   BKEYCLT,BCLT                                                     
         OC    BCLT,BCLT          IF NO CLIENT CODE                             
         BNZ   VALS25                                                           
         GOTO1 AVALFLD,APPARM,BLIPRDH,2                                         
         CLI   APPARM,X'FF'                                                     
         BE    MISSERR            CAN'T HAVE PRD/EST                            
         B     VALS30                                                           
*                                                                               
VALS25   CLI   OCFLAG,C'O'                                                      
         BE    VALKNOTV           CAN'T HAVE OFFICE & CLIENT INPUT              
         MVI   OCFLAG,C'C'        INDICATE CLIENT INPUT                         
*                                                                               
VALS30   LA    R3,BLIPRDH         VALIDATE PRODUCT                              
         GOTO1 AVALPRD,(R3)                                                     
         BNE   VALSX                                                            
         MVC   BKEYPRD,QPRD                                                     
         OC    QPRD,QPRD          IF NO PRODUCT CODE                            
         BNZ   VALS40                                                           
         GOTO1 AVALFLD,APPARM,BLIESTH,1                                         
         CLI   APPARM,X'FF'                                                     
         BE    MISSERR            CAN'T HAVE ESTIMATE INPUT                     
*                                                                               
VALS40   LA    R3,BLIESTH         VALIDATE ESTIMATE                             
         GOTO1 AVALEST,(R3)                                                     
         BNE   VALSX              CC CODE SET                                   
         MVC   BKEYEST,BEST+1                                                   
*                                                                               
         GOTO1 AVALRDT,BLIRDTH    VALIDATE RUN DATE                             
         BNE   VALSX              CC CODE SET                                   
         BAS   RE,SETBMN          SET BILL MONTH                                
         MVC   BKEYMBIL,BILMONTH  SET Y/M                                       
         GOTO1 AVALSRVM,BLISDTH   VALIDATE SERVICE MONTH                        
         BNE   VALSX              CC CODE SET                                   
         MVC   BKEYYSRV(2),BSRVM                                                
         GOTO1 AVALIDT,BLIIDTH    VALIDATE INV DATE                             
         BNE   VALSX              CC CODE SET                                   
         MVC   APRECKEY+16(6),QIDT FORCE INV DATE FOR CHANGE IN KEY             
         CR    RB,RB                                                            
VALSX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*=======================================*                                       
* RDAGY  - GET AGENCY RECORD INTO AIO2  *                                       
*          FOR DISSEL AND GET AGY NIBBLE*                                       
*=======================================*                                       
*                                                                               
RDAGY    NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,6                                                          
         MVC   IOKEY+1(2),TWAAGY                                                
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   IOKEY+1(2),QALPH   USE AGENCY OF MEDIA SPLIT FILES               
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AMYIO,APPARM,IOGET+IO2,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2                                                      
         USING AGYHDRD,R3                                                       
         LA    R1,AGYEL                                                         
RDAGY10  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),2                                                          
         BE    RDAGY20                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     RDAGY10                                                          
*                                                                               
RDAGY20  MVC   BAGYMD,3(R1)                                                     
         NI    BAGYMD,X'F0'       JUST GET AGENCY                               
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*=======================================*                                       
* SETBMN - SET BILL MONTH FROM RUN DATE *                                       
*=======================================*                                       
*                                                                               
SETBMN   NTR1                                                                   
         MVI   BILMONTH,0                                                       
         OC    BRDT,BRDT                                                        
         BZ    SETBMNX                                                          
         GOTO1 VDATCON,APPARM,(3,BRDT),(X'20',APWORK)                           
         PACK  APDUB,APWORK(2)    GET DECADE                                    
         SR    RE,RE                                                            
         CVB   RF,APDUB                                                         
         D     RE,=F'10'                                                        
         SLL   RE,4                                                             
         STC   RE,APBYTE                                                        
*                                                                               
         PACK  APDUB,APWORK+2(2)  GET MONTH                                     
         CVB   RF,APDUB                                                         
         STC   RF,APFULL                                                        
*                                                                               
         OC    APBYTE,APFULL                                                    
         MVC   BILMONTH,APBYTE                                                  
SETBMNX  XIT1                                                                   
         EJECT                                                                  
*----------------------------------------*                                      
* VALPKEY - VALIDATE KEY FOR PRINT       *                                      
*        APRECKEY = PBILLKEY(13) +QOFF(2)*                                      
*----------------------------------------*                                      
*                                                                               
VALPKEY  NTR1                                                                   
         LA    R2,APRECKEY                                                      
         XC    APRECKEY,APRECKEY                                                
         USING PBILLREC,R2                                                      
*                                                                               
         LA    R3,BLIMEDH         VALIDATE MEDIA                                
         GOTO1 AVALMED,(R3)                                                     
         BNE   VALPX                                                            
         MVC   PBILKMED,QMED                                                    
         OC    QMED,QMED          IF NO MEDIA CODE                              
         BNZ   VALP5                                                            
         GOTO1 AVALFLD,APPARM,BLIOFFH,4                                         
         CLI   APPARM,X'FF'                                                     
         BE    MISSERR            CAN'T BE ANY OFF/CLT/PRD/EST                  
*                                                                               
VALP5    LA    R3,BLIOFFH         VALIDATE OFFICE/OFFICE GROUP                  
         GOTO1 AVALOFF,(R3)                                                     
         BNE   VALPX                                                            
         MVC   APRECKEY+25(2),QOFF                                              
         OC    QOFF,QOFF          IF OFFICE INPUT                               
         BZ    VALP10                                                           
         MVI   OCFLAG,C'O'        INDICATE SO                                   
*                                                                               
VALP10   MVI   PBILKRCD,X'08'     RECORD TYPE                                   
         LA    R3,BLICLTH         VALIDATE CLIENT                               
         GOTO1 AVALCLT,(R3)                                                     
         BNE   VALPX              CC CODE SET                                   
         MVC   PBILKCLT,QCLT                                                    
         OC    QCLT,QCLT          IF NO CLIENT CODE                             
         BNZ   VALP15                                                           
         GOTO1 AVALFLD,APPARM,BLIPRDH,2                                         
         CLI   APPARM,X'FF'                                                     
         BE    MISSERR            CAN'T HAVE PRD/EST                            
         B     VALP20                                                           
*                                                                               
VALP15   CLI   OCFLAG,C'O'                                                      
         BE    VALKNOTV           CAN'T HAVE BOTH OFFICE & CLIENT               
         MVI   OCFLAG,C'C'        INDICATE CLIENT INPUT                         
*                                                                               
VALP20   LA    R3,BLIPRDH         VALIDATE PRODUCT                              
         GOTO1 AVALPRD,(R3)                                                     
         BNE   VALPX              CC CODE SET                                   
         MVC   PBILKPRD,QPRD                                                    
         OC    QPRD,QPRD          IF NO PRODUCT CODE                            
         BNZ   VALP30                                                           
         GOTO1 AVALFLD,APPARM,BLIESTH,1                                         
         CLI   APPARM,X'FF'                                                     
         BE    MISSERR            CAN'T HAVE ESTIMATE                           
*                                                                               
VALP30   LA    R3,BLIESTH         VALIDATE ESTIMATE                             
         GOTO1 AVALEST,BLIESTH                                                  
         BNE   VALPX              CC CODE SET                                   
         MVC   PBILKEST,BEST                                                    
         GOTO1 AVALRDT,BLIRDTH    VALIDATE RUN DATE                             
         BNE   VALPX              CC CODE SET                                   
         MVC   PBILKBMN,BRDT      SET Y/M                                       
         GOTO1 AVALSRVM,BLISDTH   VALIDATE SERVICE MONTH                        
         BNE   VALPX              CC CODE SET                                   
         MVC   PBILKMOS,BSRVM                                                   
         GOTO1 AVALIDT,BLIIDTH    VALIDATE INVOICE DATE                         
         BNE   VALPX              CC CODE SET                                   
         CR    RB,RB                                                            
VALPX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
GETFRST  DS    0H                                                               
         MVI   TOTLIN,C'N'        NOT A TOTAL LINE                              
         ZAP   TOTAMT,=P'0'                                                     
         XC    SVLCLT,SVLCLT                                                    
         MVI   SVLCPRF7,0                                                       
         B     EXIT                                                             
         SPACE                                                                  
*=======================================================*                       
* GETSEL - GET RECORD FOR LIST DISPLAY                  *                       
*        - S & R INVALID(AOR TRUE BILL)=> APRECID =ZERO *                       
*        - S VALID(ALWAYS) => APRECID =X'0800'          *                       
*        - R VALID(TRANSFERED BILL) => APRECID = X'8000'*                       
*        -                             REALLY = X'8800' *                       
*=======================================================*                       
*                                                                               
GETSEL   CLI   TOTLIN,C'Y'                                                      
         BE    GETSELX                                                          
         CLI   TOTLIN,C'T'                                                      
         BNE   *+12                                                             
         MVI   APMODE,APMEOFS     TELL GENERAL NO MORE LINES                    
         B     GETSELX                                                          
*                                                                               
         CLI   APINDS,APILFLST    FIRST LINE OF FIRST SCREEN                    
         BNE   GETSEL2                                                          
         MVI   FIRSTSCR,C'Y'                                                    
         OC    SVSTRTKY,SVSTRTKY  CONTINUE SEARCH THAT TIMED OUT                
         BZ    GETSEL2                                                          
         MVC   APRECKEY,SVSTRTKY                                                
         OI    APINDS,APILRERD                                                  
         XC    SVSTRTKY,SVSTRTKY                                                
*                                                                               
GETSEL2  MVC   APRECID,=X'0800'   DEFAULT SELECT CODE 'S' VALID                 
         CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   GETSEL5                                                          
         BAS   RE,GETPRT          GET PRINT RECORDS                             
         B     GETSELX                                                          
*                                                                               
GETSEL5  LA    R2,IOKEY                                                         
         USING BILLRECD,R2                                                      
         MVC   BKEY,APRECKEY                                                    
         CLI   BKEYTYPE,0         X'00' - TO BE BILL HEADER                     
         BNE   GETSEL10                                                         
         OC    BKEYYSRV(5),BKEYYSRV   MUST BE NON ZERO TO BE A                  
         BNZ   GETSEL20           BILL HEADER                                   
GETSEL10 XC    BKEY,BKEY                                                        
         MVC   BKEYAM,BAGYMD      AGENCY/MEDIA                                  
         MVC   BKEYCLT,BCLT       CLIENT                                        
         MVC   BKEYPRD,QPRD       PRODUCT                                       
         MVC   BKEYEST,BEST+1     ESTIMATE                                      
         MVC   BKEYMBIL,BILMONTH  BILL MONTH                                    
         MVC   BKEYYSRV(2),BSRVM  Y/M OF SERVICE                                
         B     GETSEL60                                                         
*                                                                               
GETSEL20 TM    APINDS,APILRERD    TEST RE-READ RECORD?                          
         BZ    GETSEL40                                                         
         GOTO1 AMYIO,APPARM,IOHI+IO1,=C'D'                                      
         CLI   MYIOERR,0                                                        
         BE    GETSEL70           ESTABLISH READ SEQUENCE                       
         B     GETSEL99                                                         
*                                                                               
GETSEL40 TM    APINDS,APILNSEQ    CONTINUE WITH SEQUENCIAL READ?                
         BO    GETSEL70                                                         
GETSEL60 LA    RE,IOHI+IO1                                                      
         ST    RE,APPARM                                                        
         GOTO1 AMYIO,APPARM,,=C'D'                                              
         CLI   MYIOERR,0                                                        
         BNE   GETSEL99                                                         
         B     GETSEL71                                                         
*                                                                               
GETSEL70 LA    RE,IOSEQ+IO1       GET NEXT RECORD                               
         ST    RE,APPARM                                                        
         GOTO1 AMYIO,APPARM,,=C'D'                                              
         CLI   MYIOERR,0                                                        
         BNE   GETSEL99                                                         
*                                                                               
GETSEL71 BAS   RE,CHKCNT          CHECK COUNT                                   
         BNE   GETSEL99                                                         
         LA    R2,IOKEY                                                         
         CLI   BKEY,0                 X'00'                                     
         BNE   GETSEL99                                                         
         OC    BKEYYSRV(5),BKEYYSRV   MAKE SURE BILL HEADER                     
         BZ    GETSEL70                                                         
*                                                                               
         MVC   APBYTE,BKEYAM      CHECK AGENCY                                  
         NI    APBYTE,X'F0'       GET JUST AGENCY OF BILL KEY                   
         MVC   APBYTE2,BAGYMD     GET JUST AGENCY OF REQ AGENCY                 
         NI    APBYTE2,X'F0'                                                    
         CLC   APBYTE,APBYTE2                                                   
         BNE   GETSEL99           FINISHED W/ THIS AGENCY'S BILL RECS           
*                                                                               
         OC    QMED,QMED                                                        
         BZ    GETSEL75                                                         
         CLI   QSYS,C'N'          IF NETWORK - CHECK MEDIA LATER                
         BE    GETSEL75                                                         
         CLC   BKEYAM,BAGYMD                                                    
         BNE   GETSEL99           NO MORE RECS FOR THIS MEDIA                   
*                                                                               
GETSEL75 OC    QOFF,QOFF          REQUESTED SPECIFIC OFFICE?                    
         BZ    GETSEL80                                                         
         BAS   RE,CHKCLT          CHECK IF BILL CLT BELONGS TO OFF              
         BE    GETSEL80                                                         
         MVC   IOKEY+BKEYPRD-BKEY(L'BKEYPRD),=3X'FF'                            
         B     GETSEL60           READ HIGH FOR NEXT CLIENT                     
*                                                                               
GETSEL80 OC    QCLT,QCLT          REQUESTED SPECIFIC CLIENT?                    
         BZ    GETSEL82           NO - ALL CLIENTS                              
         CLC   BKEYCLT,BCLT       YES - MUST MATCH                              
         BH    GETSEL99           NO MORE BILLS FOR THIS CLIENT                 
         BNE   GETSEL70                                                         
GETSEL82 OC    QPRD,QPRD          REQUESTED SPECIFIC PRODUCT?                   
         BZ    GETSEL84           NO - ALL PRODUCTS                             
         CLC   BKEYPRD,QPRD       YES - MUST MATCH                              
         BH    GETSEL99           NO MORE BILLS FOR THIS PRODUCT                
         BNE   GETSEL70                                                         
GETSEL84 OC    QEST,QEST          REQUESTED SPECIFIC ESTIMATE?                  
         BZ    GETSEL85           NO - ALL ESTIMATES                            
         CLC   BKEYEST,BEST+1     YES - MUST MATCH                              
         BH    GETSEL99           NO MORE BILLS FOR THIS ESTIMATE               
         BNE   GETSEL70                                                         
GETSEL85 OC    BRDT,BRDT          ANY RUN DATE REQUESTED?                       
         BZ    GETSEL86                                                         
         CLI   BILMONTH,X'5C'      IF 12TH MONTH IN 1995                        
         BNE   GETSL85A                                                         
         CLI   BKEYMBIL,X'5C'      THEN 12TH AND 13TH MONTH OKAY                
         BE    *+12                                                             
         CLI   BKEYMBIL,X'5D'                                                   
         BNE   GETSEL70                                                         
         B     GETSEL86                                                         
GETSL85A CLC   BKEYMBIL,BILMONTH                                                
         BNE   GETSEL70                                                         
GETSEL86 OC    BSRVM,BSRVM        Y/M OF SERIVCE REQUESTED?                     
         BZ    GETSEL92           NO - ALL MONTHS OF SERVICE                    
         CLC   BKEYYSRV(2),BSRVM  YES - MUST MATCH                              
         BNE   GETSEL70                                                         
*                                                                               
GETSEL92 MVC   APRECDA,BKEY+14    SAVE D/A                                      
         MVC   APRECKEY(L'BKEY),BKEY                                            
         GOTO1 AMYIO,APPARM,IOGET+IO1,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA1                                                      
         BAS   RE,CHKSTRA         CHECK TRANSFERRED                             
         BNE   GETSEL70                                                         
         CLI   BRETAIL,0          RETAIL BILL?                                  
         BE    *+12                                                             
         BAS   RE,CHKSPRET        YES -CHECK EXCLUDE SPECIAL RETAIL             
         BNE   GETSEL70                                                         
         BAS   RE,CHKTYPE         CHECK BILL TYPE                               
         BNE   GETSEL70                                                         
         BAS   RE,CHKSXDT         CHECK TRANFER DATE OPTION                     
         BNE   GETSEL70                                                         
*                                                                               
         OC    QIDT,QIDT          INV DATE REQUESTED?                           
         BZ    GETSEL93           NO - ALL BILL MONTHS                          
         GOTO1 VDATCON,APPARM,(0,BQDATE),(X'20',APWORK) DO THIS FOR Y2K         
         CLC   QIDT(2),APWORK     SAME YEAR?                                    
         BNE   GETSEL70                                                         
         CLC   QIDT+2(2),APWORK+2  SAME MONTH?                                  
         BNE   GETSEL70                                                         
         CLI   BIDT+2,0           ANY PARTICULAR DAY?                           
         BE    GETSEL93           NO                                            
         CLC   QIDT+4(2),APWORK+4 YES -MUST MATCH ON DAY                        
         BNE   GETSEL70                                                         
*                                                                               
GETSEL93 OC    QRDT,QRDT          RUN DATE REQUESTED?                           
         BZ    GETSEL94           NO -                                          
         GOTO1 VDATCON,APPARM,(0,BDATE),(X'20',APWORK)  DO THIS FOR Y2K         
         CLC   QRDT(2),APWORK      SAME YEAR                                    
         BNE   GETSEL70                                                         
         CLC   QRDT+2(2),APWORK+2  SAME MONTH?                                  
         BNE   GETSEL70                                                         
         CLI   BRDT+2,0           ANY PARTICULAR DAY?                           
         BE    GETSEL94           NO                                            
         CLC   QRDT+4(2),APWORK+4  YES -MUST MATCH ON DAY                       
         BNE   GETSEL70                                                         
*                                                                               
GETSEL94 MVI   APBYTE,0                                                         
         CLI   QSYS,C'N'          IF NET SYSTEM                                 
         BNE   GETSEL96                                                         
         OC    BLMED,SPACES                                                     
         MVI   APBYTE,C'N'                                                      
         CLI   BLMED,C' '         BLDMED = SPACES MEANS MEDIA=N                 
         BE    *+10                                                             
         MVC   APBYTE,BLMED       SET MEDIA                                     
         OC    QMED,QMED                                                        
         BZ    GETSEL96                                                         
         CLC   APBYTE,QMED                                                      
         BNE   GETSEL70                                                         
*                                                                               
GETSEL96 OC    BILPOST,BILPOST    IF PREVIOUSLY TRANSFERRED                     
         BZ    *+10                                                             
         MVC   APRECID(2),=X'8800' INDICATE S AND R BOTH VALID                  
*                                                                               
         TM    BILSTAT,X'20'      IF TRUE AOR BILL                              
         BO    GETSEL97                                                         
         CLI   BRETAIL,0                                                        
         BE    GETSEL98                                                         
         BAS   RE,CHKSAMTS        CHECK RETAIL BILL                             
         BE    GETSEL98                                                         
GETSEL97 XC    APRECID,APRECID    MAKE S & R BOTH INVALID                       
*                                                                               
GETSEL98 MVC   APRECKEY,BKEY                                                    
         B     GETSELX                                                          
*                                                                               
GETSEL99 MVI   TOTLIN,C'Y'        NO MORE LINES - TIME FOR TOTAL LINE           
         XC    APRECID,APRECID    WHICH ISN'T SELECTABLE                        
GETSELX  B     EXIT                                                             
         EJECT                                                                  
*====================================*                                          
* CHKCNT - CHECKS IO COUNT OKAY      *                                          
*====================================*                                          
*                                                                               
CHKCNT   NTR1                                                                   
         GOTO1 VGETFACT,APPARM,0  GET CURRENT IO COUNT                          
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO      MAX ALLOWABLE IOS                             
         MH    R3,=H'9'                                                         
         D     R2,=F'10'          90% OF MAX IOS                                
         CLM   R3,3,FATIOCNT      TEST RUNNING OUT                              
         BH    YES                OKAY                                          
         LA    R1,25              PRINT KEY LENGTH                              
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         LA    R1,13                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVSTRTKY(0),IOKEY   SAVE CURRENT KEY                             
         B     NO                                                               
         SPACE                                                                  
*=================================================*                             
* CHKSTRA-CHECK FOR TRANS, * TRANS - SETS CC CODE *                             
*=================================================*                             
*                                                                               
CHKSTRA  NTR1                                                                   
         TM    INOIND,INOTRA      WANT TRANSFERRED BILLED ONLY?                 
         BZ    CHKSTRA5                                                         
         TM    BCNTRL,X'01'       YES -                                         
         BO    YES                                                              
         OC    BILPOST,BILPOST                                                  
         BNZ   YES                                                              
         B     NO                 UNTRANSFERRED - SKIP IT                       
*                                                                               
CHKSTRA5 TM    INOIND,INOUTRA     WANT UNTRANS BILLED ONLY?                     
         BZ    YES                NO - WANT BOTH                                
         OC    BILPOST,BILPOST                                                  
         BZ    YES                                                              
         B     NO                 TRANSFERRED - SKIP IT                         
         EJECT                                                                  
*==============================================================*                
* CHKSPRET - (*SPRET) EXCLUDE UNTRANSFERABLE RETAIL FROM LIST  *                
*==============================================================*                
*                                                                               
CHKSPRET NTR1                                                                   
         TM    INOIND,INOSPRET    WANT TO EXLUDE SPEC RETAILS?                  
         BZ    YES                                                              
         BAS   RE,CHKSAMTS                                                      
         BE    YES                                                              
         B     NO                 SKIP THIS BILL                                
         SPACE                                                                  
*--------------------------------------------------------------*                
* CHKSAMTS - CHECKS IF RETAIL BILL SHOULD BE TRANSFERRED       *                
*--------------------------------------------------------------*                
*                                                                               
CHKSAMTS NTR1                                                                   
         CLI   BRETAIL,X'81'      RETAIL CORP SUMMS ARE NEVER TRANS             
         BE    NO                 SKIP BILL                                     
         ZAP   BGRSP,BGRSP                                                      
         BNZ   YES                                                              
         ZAP   BNETP,BNETP                                                      
         BNZ   YES                                                              
         ZAP   BACTP,BACTP                                                      
         BNZ   YES                                                              
         OC    BVATAMT,BVATAMT                                                  
         BNZ   YES                                                              
         B     NO                 SKIP THIS BILL                                
         EJECT                                                                  
*==============================================================*                
* CHKTYPE-CHECK FOR TY=AOR,REG,ETC.. OR *TY=AOR/REG/RET,ETC... *                
*    XIT - CC CODE SET EQUAL USE THIS RECORD                   *                
*==============================================================*                
*                                                                               
CHKTYPE  NTR1                                                                   
         OC    INOTYPE,INOTYPE    ANY PARTICULAR BILLING TYPES                  
         BZ    YES                                                              
         CLI   QSYS,C'P'                                                        
         BNE   *+12                                                             
         BAS   RE,GETPTYP         GET PRINT BILLING TYPE                        
         B     *+8                                                              
         BAS   RE,GETSTYP         GET BILLING TYPE                              
*                                                                               
         LA    R3,10                                                            
         LA    R2,INOTYPE                                                       
         MVC   APWORK(L'INOPTI),INOPTI                                          
         NC    APWORK(L'INOPTI),=A(OPTTYPB)                                     
         BZ    CHKTYP50           EXCLUDE TYPES                                 
*                                                                               
CHKTYP15 BAS   RE,SETLNGTH        RETURNED R1 = (L'COMPARE - 1)                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TYPBIL(0),0(R2)                                                  
         BE    YES                MATCH FOUND - INCLUDE                         
         LA    R2,L'TYPBIL(R2)                                                  
         BCT   R3,CHKTYP15                                                      
         B     NO                                                               
*                                                                               
CHKTYP50 BAS   RE,SETLNGTH        RETURNED R1 = (L'COMPARE - 1)                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TYPBIL(0),0(R2)                                                  
         BE    NO                 MATCH FOUND - EXCLUDE                         
         LA    R2,L'TYPBIL(R2)                                                  
         BCT   R3,CHKTYP50                                                      
         B     YES                                                              
         SPACE 2                                                                
SETLNGTH DS    0H                 RETURN R1 = LENGTH - 1                        
         LA    R1,1               COMPARE LENGTH OF 2                           
         CLI   2(R2),C' '                                                       
         BNH   SETLNGX                                                          
         LA    R1,1(R1)                                                         
         CLI   3(R2),C' '                                                       
         BNH   SETLNGX                                                          
         LA    R1,1(R1)                                                         
         CLI   4(R2),C' '                                                       
         BNH   SETLNGX                                                          
         LA    R1,1(R1)                                                         
SETLNGX  BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------*                
* GETSTYP- GET SPOT BILL TYPE - THE ORDER OF SETTING TYPES IS  *                
*          IMPORTANT        XIT - TYPBIL SET                   *                
*--------------------------------------------------------------*                
*                                                                               
GETSTYP  NTR1                                                                   
         TM    BILSTAT3,BSTREGQ    RETAIL REGIONAL BILLING                      
         BZ    *+14                                                             
         MVC   TYPBIL,=CL5'BPCR'                                                
         B     GETSTYPX                                                         
         TM    BILSTAT3,BSTLMGQ    RETAIL LMG (LOCAL MKT GRP) BILLING           
         BZ    *+14                                                             
         MVC   TYPBIL,=CL5'BPCL'                                                
         B     GETSTYPX                                                         
*                                                                               
         CLI   BRETAIL,0                                                        
         BE    GETSTYP5                                                         
         MVC   TYPBIL,=CL5'RET'   RETAIL                                        
         CLI   BRETAIL,X'41'                                                    
         BNE   *+12                                                             
         MVI   TYPBIL+3,C'S'      CORP SUMMARY                                  
         B     GETSTYPX                                                         
         TM    BRETAIL,X'01'                                                    
         BNO   *+12                                                             
         MVI   TYPBIL+3,C'C'      CORP RETAIL(CORP OUTLET,CORP CONTRL)          
         B     GETSTYPX                                                         
         TM    BRETAIL,X'02'                                                    
         BNO   GETSTYPX                                                         
         MVI   TYPBIL+3,C'P'      PARTICIPANT RETAIL                            
         B     GETSTYPX                                                         
*                                                                               
GETSTYP5 TM    BILSTAT,X'20'                                                    
         BNO   GETSTY5A                                                         
         MVC   TYPBIL,=CL5'AORA'  TRUE AOR                                      
         TM    BILSTAT3,BSTTRCNQ                                                
         BNO   *+10                                                             
         MVC   TYPBIL,=CL5'AORXA' TRUE AOR WITH TRADE                           
         B     GETSTYPX                                                         
*                                                                               
GETSTY5A TM    BILSTAT,X'10'                                                    
         BNO   GETSTYP7                                                         
         MVC   TYPBIL,=CL5'AORM'  CLIENT AOR MAIN ONE                           
         TM    BILSTAT3,BSTTRCNQ                                                
         BNO   *+10                                                             
         MVC   TYPBIL,=CL5'AORXM' CLIENT AOR MAIN ONE WITH TRADE                
         TM    BILSTAT,X'80'                                                    
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'AORJ'  CLIENT COMMISSION ADJUSTMENT                  
         TM    BILSTAT3,BSTTRCNQ                                                
         BNO   *+10                                                             
         MVC   TYPBIL,=CL5'AORXJ' CLIENT COMMISSION ADJ WITH TRADE              
         B     GETSTYPX                                                         
*                                                                               
         TM    BILSTAT,BSTSCOMQ                                                 
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'AORU'  CLIENT UPFRONT COMM                           
         B     GETSTYPX                                                         
         TM    BILSTAT,BSTSNETQ                                                 
         BNO   GETSTYPX                                                         
         MVC   TYPBIL,=CL5'AORN'  CLIENT UPFRONT NET                            
         B     GETSTYPX                                                         
*                                                                               
GETSTYP7 TM    BILSTAT,X'02'                                                    
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'COM'   COMMISSION ONLY BILL                          
         B     GETSTYPX                                                         
*                                                                               
         TM    BILSTAT,X'80'                                                    
         BNO   GETSTYP8                                                         
         MVC   TYPBIL,=CL5'ADJ'   COMMISSION ADJUSTMENT                         
         TM    BILSTAT3,BSTTRCNQ                                                
         BNO   *+10                                                             
         MVC   TYPBIL,=CL5'ADJX' COMMISSION ADJUSTMENT WITH TRADE               
         B     GETSTYPX                                                         
*                                                                               
GETSTYP8 TM    BILSTAT2,BSTAMAQ                                                 
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'DIFF'  ACTUAL-MINUS-ASSIGNED                         
         B     GETSTYPX                                                         
*                                                                               
         TM    BILSTAT,BSTSCOMQ                                                 
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'UFC'   REGULAR UPFRONT COMM                          
         B     GETSTYPX                                                         
*                                                                               
         TM    BILSTAT,BSTSNETQ                                                 
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'UFN'   REG UPFRONT NET                               
         B     GETSTYPX                                                         
*                                                                               
         TM    BILSTAT,BSTMANQ                                                  
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'MAN'   MANUAL BILL                                   
         B     GETSTYPX                                                         
*                                                                               
         MVC   TYPBIL,=CL5'REG'   REGULAR                                       
         TM    BILSTAT3,BSTTRCNQ                                                
         BNO   *+10                                                             
         MVC   TYPBIL,=CL5'REGX' REGULAR WITH TRADE                             
         TM    BILSTAT3,BSTMBARQ                                                
         BNO   *+10                                                             
         MVC   TYPBIL,=CL5'REGM' REGULAR WITH MIDAS TRADE (GROUPM)              
GETSTYPX B     EXIT                                                             
         EJECT                                                                  
*====================================================*                          
* CHKSXDT-CHECK FOR MXDTE=YYMMDD    XIT CC CODE SET  *                          
*====================================================*                          
*                                                                               
CHKSXDT  NTR1                                                                   
         OC    INOXDAT,INOXDAT    ANY PARTICULAR TRANSFER DATE                  
         BZ    YES                                                              
         OC    BILPOST,BILPOST    ANY TRANSFER DATE                             
         BZ    NO                                                               
         CLC   BILPOST,INOXDAT    TRANSFER DATES MUST MATCH                     
         BNE   NO                                                               
         B     YES                                                              
         SPACE                                                                  
*-----------------------------------------------------------*                   
* CHKCLT - CHECKS IF CLIENT BILL BELONGS TO OFF REQUESTED   *                   
*-----------------------------------------------------------*                   
*                                                                               
CHKCLT   NTR1                                                                   
**NO-OP**GOTO1 VCLUNPK,APPARM,(SVCPRF7,BKEYCLT),TEMPCLI                         
*                                                                               
         L     R3,ACLITAB                                                       
CHKCLT5  OC    0(3,R3),0(R3)       IF NOT END OF TABLE                          
         BZ    NO                                                               
         CLC   BKEYCLT,0(R3)       MATCH ON PACKED CLIENT CODE                  
         BE    YES                                                              
         LA    R3,3(R3)            BUMP TO NEXT ENTRY                           
         B     CHKCLT5             LOOP                                         
         DROP  R2                                                               
         EJECT                                                                  
*------------------------------------------*                                    
* GETPRT - GET PRINT BILL RECORDS          *                                    
*------------------------------------------*                                    
*                                                                               
GETPRT   NTR1                                                                   
         LA    R2,IOKEY                                                         
         USING PBILLREC,R2                                                      
         MVC   PBILLKEY,APRECKEY                                                
         OC    QALPH,QALPH                                                      
         BZ    GETP5                                                            
         CLC   PBILKAGY,QALPH     CORRECT AGENCY?                               
         B     GETP7                                                            
GETP5    CLC   PBILKAGY,TWAAGY    CORRECT AGENCY?                               
*                                                                               
GETP7    BNE   GETP10                                                           
         CLI   PBILKRCD,X'08'     CORRECT RECORD TYPE?                          
         BE    GETP20             YES                                           
GETP10   XC    PBILLKEY,PBILLKEY                                                
         MVC   PBILKAGY,TWAAGY    AGENCY                                        
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   PBILKAGY,QALPH     OVERRIDE AGENCY                               
         MVC   PBILKMED,QMED      MEDIA CODE                                    
         MVC   PBILKCLT,QCLT      CLIENT CODE                                   
         MVC   PBILKPRD,QPRD      PRODUCT CODE                                  
         MVC   PBILKEST,BEST      ESTIMATE                                      
         MVC   PBILKBMN,BRDT      MOVE IN Y/M                                   
         MVC   PBILKMOS,BSRVM     Y/M OF SERVICE                                
         B     GETP60                                                           
*                                                                               
GETP20   TM    APINDS,APILRERD    TEST RE-READ RECORD?                          
         BZ    GETP40                                                           
         GOTO1 AMYIO,APPARM,IORD+IO1,=C'F'                                      
         CLI   MYIOERR,0                                                        
         BE    GETP70           ESTABLISH READ SEQUENCE                         
         B     GETP99                                                           
*                                                                               
GETP40   TM    APINDS,APILNSEQ    CONTINUE WITH SEQUENCIAL READ?                
         BO    GETP70                                                           
GETP60   LA    RE,IOHI+IO1                                                      
         ST    RE,APPARM                                                        
         GOTO1 AMYIO,APPARM,,=C'D'                                              
         CLI   MYIOERR,0                                                        
         BNE   GETP99                                                           
         B     GETP71                                                           
*                                                                               
GETP70   LA    RE,IOSEQ+IO1       GET NEXT RECORD                               
         ST    RE,APPARM                                                        
         GOTO1 AMYIO,APPARM,,=C'D'                                              
         CLI   MYIOERR,0                                                        
         BNE   GETP99                                                           
*                                                                               
GETP71   BAS   RE,CHKCNT          CHECK COUNT                                   
         BNE   GETP99                                                           
         LA    R2,IOKEY           CHECK AGENCY                                  
         OC    QALPH,QALPH                                                      
         BZ    GETP73                                                           
         CLC   PBILKAGY,QALPH                                                   
         B     GETP74                                                           
GETP73   CLC   PBILKAGY,TWAAGY                                                  
GETP74   BNE   GETP99                                                           
*                                                                               
         OC    QMED,QMED          REQUESTED SPECIFIC MEDIA                      
         BZ    GETP75             NO - ALL MEDIAS                               
         CLC   PBILKMED,QMED                                                    
         BNE   GETP99             NO MORE RECS FOR THIS MEDIA                   
         CLI   PBILKRCD,X'08'     RECORD TYPE                                   
         BE    GETP78                                                           
         BL    GETP70             GET NEXT RECORD TYPE                          
         B     GETP99             NO MORE BILL RECORDS                          
*                                                                               
GETP75   CLI   PBILKRCD,X'08'     ALL MEDIAS - BILL REC?                        
         BE    GETP78                                                           
         BL    GETP70             GET NEXT RECORD                               
         MVI   PBILKRCD,X'FF'     SKIP TO NEXT MEDIA                            
         B     GETP70                                                           
*                                                                               
GETP78   OC    QOFF,QOFF          REQUESTED SPECIFIC OFFICE?                    
         BZ    GETP80                                                           
         BAS   RE,CHKPCLT         CHECK IF BILL CLT BELONGS TO OFF              
         BE    GETP80                                                           
         MVC   IOKEY+PBILKPRD-PBILLKEY(L'PBILKPRD),=3X'FF'                      
         B     GETP60              READ HIGH FOR NEXT CLIENT                    
*                                                                               
GETP80   OC    QCLT,QCLT          REQUESTED SPECIFIC CLIENT?                    
         BZ    GETP82             NO - ALL CLIENTS                              
         CLC   PBILKCLT,QCLT      YES - MUST MATCH                              
         BH    GETP99                                                           
         BNE   GETP70                                                           
GETP82   OC    QPRD,QPRD          REQUESTED SPECIFIC PRODUCT?                   
         BZ    GETP84             NO - ALL PRODUCTS                             
         CLC   PBILKPRD,QPRD      YES - MUST MATCH                              
         BH    GETP99                                                           
         BNE   GETP70                                                           
GETP84   CLC   =X'0000',PBILKEST                                                
         BE    GETP70             SKIP ZERO ESTIMATES                           
         OC    QEST,QEST          REQUESTED SPECIFIC ESTIMATE?                  
         BZ    GETP85             NO - ALL ESTIMATES                            
         CLC   PBILKEST,BEST      YES - MUST MATCH                              
         BNE   GETP70                                                           
GETP85   OC    BRDT,BRDT                                                        
         BZ    GETP86                                                           
         CLC   PBILKBMN,BRDT                                                    
         BNE   GETP70                                                           
GETP86   OC    BSRVM,BSRVM        Y/M OF SERIVCE REQUESTED?                     
         BZ    GETP92             NO - ALL MONTHS OF SERVICE                    
         CLC   PBILKMOS,BSRVM     YES - MUST MATCH                              
         BNE   GETP70                                                           
*                                                                               
GETP92   MVC   APRECDA,PBILLKEY+27    SAVE D/A                                  
         MVC   APRECKEY(L'PBILLKEY),PBILLKEY                                    
         GOTO1 AMYIO,APPARM,IOGET+IO1,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA1                                                      
         BAS   RE,CHKPTRA         CHECK TRANSFER OPTION                         
         BNE   GETP70                                                           
         CLI   PBRETAIL,0         IF RETAIL BILL                                
         BE    *+12                                                             
         BAS   RE,CHKPRET         CHECK EXCLUDE SPECIAL RETAIL BILLS            
         BNE   GETP70                                                           
         BAS   RE,CHKTYPE         CHECK TYPE OPTION                             
         BNE   GETP70             NO MATCH - SKIP IT                            
         BAS   RE,CHKPXDT         CHECK TRANFER DATE OPTION                     
         BNE   GETP70                                                           
*                                                                               
         OC    QIDT,QIDT          INV DATE REQUESTED?                           
         BZ    GETP93             NO - ALL BILL MONTHS                          
         GOTO1 VDATCON,APPARM,(3,PBILINVD),(X'20',APWORK)                       
         CLC   QIDT(2),APWORK     SAME YEAR?                                    
         BNE   GETP70                                                           
         CLC   QIDT+2(2),APWORK+2  SAME MONTH?                                  
         BNE   GETP70                                                           
         CLI   BIDT+2,0           ANY PARTICULAR DAY?                           
         BE    GETP93             NO                                            
         CLC   QIDT+4(2),APWORK+4 YES -MUST MATCH ON DAY                        
         BNE   GETP70                                                           
*                                                                               
GETP93   OC    QRDT,QRDT          RUN DATE REQUESTED?                           
         BZ    GETP94             NO -                                          
         GOTO1 VDATCON,APPARM,(0,PBILLDAT),(X'20',APWORK) FOR Y2K               
         CLC   QRDT(2),APWORK     SAME YEAR?                                    
         BNE   GETP70                                                           
         CLC   QRDT+2(2),APWORK+2  SAME MONTH?                                  
         BNE   GETP70                                                           
         CLI   BRDT+2,0           ANY PARTICULAR DAY?                           
         BE    GETP94             NO                                            
         CLC   QRDT+4(2),APWORK+4 YES -MUST MATCH ON DAY                        
         BNE   GETP70                                                           
*                                                                               
GETP94   OC    PBILPOST,PBILPOST   IF PREVIOUSLY TRANSFERRED                    
         BZ    *+10                                                             
         MVC   APRECID(2),=X'8800' INDICATE S AND R BOTH VALID                  
         TM    PBILCMSW,X'20'      IF TRUE AOR BILL                             
         BO    GETP96                                                           
         CLI   PBRETAIL,0                                                       
         BE    GETP98                                                           
         BAS   RE,CHKPAMTS        CHECK RETAIL BILL                             
         BE    GETP98                                                           
GETP96   XC    APRECID,APRECID    MAKE S & R BOTH INVALID                       
*                                                                               
GETP98   MVC   APRECKEY,PBILLKEY                                                
         B     GETPX                                                            
*                                                                               
GETP99   MVI   TOTLIN,C'Y'        NO MORE LINES - TIME FOR TOTAL                
         XC    APRECID,APRECID    WHICH ISN'T SELECTABLE                        
GETPX    B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------*                
* GETPTYP- GETS PRINT BILL TYPE- ORDER IS IMPORTANT            *                
*          EX. A FIN CD BILL WILL BE FIN      XIT=TYPBIL SET   *                
*--------------------------------------------------------------*                
*                                                                               
GETPTYP  NTR1                                                                   
         LA    RE,PBILLEL                                                       
         USING PBILOTH,RE                                                       
         SR    R0,R0                                                            
GETPTYP2 CLI   0(RE),0                                                          
         BE    GETPTYP6                                                         
         CLI   0(RE),X'09'                                                      
         BNE   GETPTYP4                                                         
         TM    PBILLIND,X'01'                                                   
         BNO   GETPTYP6                                                         
         MVC   TYPBIL,=CL5'FINM'   FINANCIAL MAIN ONE                           
         CLI   PBILLTYP,C'R'                                                    
         BNE   GETPTYPX                                                         
         MVI   TYPBIL+3,C'R'       FINANCIAL REBATE                             
         B     GETPTYPX                                                         
*                                                                               
GETPTYP4 ICM   R0,1,1(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R0                                                            
         B     GETPTYP2                                                         
*                                                                               
GETPTYP6 CLI   PBRETAIL,0                                                       
         BE    GETPTYP7                                                         
         MVC   TYPBIL,=CL5'RET'   RETAIL                                        
         CLI   PBRETAIL,X'41'                                                   
         BNE   *+12                                                             
         MVI   TYPBIL+3,C'S'      CORP SUMMARY                                  
         B     GETPTYPX                                                         
         TM    PBRETAIL,X'01'                                                   
         BNO   *+12                                                             
         MVI   TYPBIL+3,C'C'      CORP RETAIL(CORP OUTLET,CORP CONTRL)          
         B     GETPTYPX                                                         
         TM    PBRETAIL,X'02'                                                   
         BNO   GETPTYPX                                                         
         MVI   TYPBIL+3,C'P'      PARTICIPANT RETAIL                            
         B     GETPTYPX                                                         
*                                                                               
GETPTYP7 TM    PBILCMSW,X'20'                                                   
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'AORA' TRUE AOR                                       
         B     GETPTYPX                                                         
*                                                                               
         TM    PBILCMSW,X'10'                                                   
         BNO   GETPTYP8                                                         
         MVC   TYPBIL,=CL5'AORM' CLIENT AOR MAIN ONE                            
*                                                                               
         TM    PBILCMSW,X'01'                                                   
         BNO   GETPTY7                                                          
         MVI   TYPBIL+3,C'U'     CLIENT AOR UPFRONT COMMISSION                  
         CLI   PBILSEP,C'C'                                                     
         BNE   *+10                                                             
         MVC   TYPBIL+3(2),=C'CU'  CLIENT AOR CD W/ UPFRONT COMMISSION          
         B     GETPTYPX                                                         
GETPTY7  TM    PBILCMSW,X'08'                                                   
         BNO   GETPTY7X                                                         
         MVI   TYPBIL+3,C'N'     CLIENT AOR UPFRONT NET                         
         CLI   PBILSEP,C'C'                                                     
         BNE   *+10                                                             
         MVC   TYPBIL+3(2),=C'CN'  CLIENT AOR CD W/ UPFRONT NET                 
         B     GETPTYPX                                                         
GETPTY7X CLI   PBILSEP,C'A'                                                     
         BNE   *+12                                                             
         MVI   TYPBIL+3,C'J'     CLIENT ADJUSTMENT AOR                          
         B     GETPTYPX                                                         
         CLI   PBILSEP,C'C'                                                     
         BNE   GETPTYPX                                                         
         MVI   TYPBIL+3,C'C'     CLIENT CASH DISCOUNT AOR                       
         B     GETPTYPX                                                         
*                                                                               
GETPTYP8 TM    PBILCMSW,X'02'                                                   
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'COM'  COMMISSION ONLY                                
         B     GETPTYPX                                                         
*                                                                               
         CLI   PBILCDSW,C'S'                                                    
         BNE   GETPTYP9                                                         
         MVC   TYPBIL,=CL5'CDM'  MAIN BILL OF A CD PAIR                         
         CLI   PBILSEP,C'C'                                                     
         BNE   *+10                                                             
         MVC   TYPBIL,=CL5'CDC'  CD BILL OF PAIR                                
         TM    PBILCMSW,X'01'    CHECK UPFRONT WITH THESE TYPES TOO             
         BNO   *+8                                                              
         MVI   TYPBIL+3,C'U'                                                    
         TM    PBILCMSW,X'08'                                                   
         BNO   *+8                                                              
         MVI   TYPBIL+3,C'N'                                                    
         B     GETPTYPX                                                         
*                                                                               
GETPTYP9 CLI   PBILSEP,C'A'                                                     
         BNE   *+14                                                             
         MVC   TYPBIL,=CL5'ADJ'  COMMISSION ADJUSTMENT                          
         B     GETPTYPX                                                         
*                                                                               
         TM    PBILCMSW,X'01'                                                   
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'UFC'  CLIENT AOR UPFRONT COMMISSION                  
         B     GETPTYPX                                                         
         TM    PBILCMSW,X'08'                                                   
         BNO   *+14                                                             
         MVC   TYPBIL,=CL5'UFN'  CLIENT AOR UPFRONT NET                         
         B     GETPTYPX                                                         
*                                                                               
         CLI   PBILLTYP,C'M'     MANUAL BILL?                                   
         BNE   *+14                                                             
         MVC   TYPBIL,=CL5'MAN'                                                 
         B     GETPTYPX                                                         
*                                                                               
         MVC   TYPBIL,=CL5'REG'  REGULAR                                        
         TM    PBILSTAT,X'20'                                                   
         BNO   *+10                                                             
         MVC   TYPBIL,=CL5'REGM' REGULAR WITH MIDAS TRADE (GROUPM)              
GETPTYPX B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*=========================================================*                     
* CHKPTRA -CHECK FOR MATCH ON TRANS SETS CC CODE ON EXIT  *                     
*=========================================================*                     
*                                                                               
CHKPTRA  NTR1                                                                   
         TM    INOIND,INOTRA      WANT TRANSFERRED BILLED ONLY?                 
         BZ    CHKPTRA2                                                         
         TM    PBILLCTL+1,X'01'   YES                                           
         BO    YES                                                              
         OC    PBILPOST,PBILPOST                                                
         BNZ   YES                                                              
         B     NO                 UNTRANSFERRED - SKIP IT                       
*                                                                               
CHKPTRA2 TM    INOIND,INOUTRA     WANT UNTRANS BILLED ONLY?                     
         BZ    YES                NO - WANT BOTH                                
         OC    PBILPOST,PBILPOST                                                
         BZ    YES                                                              
         B     NO                 TRANSFERRED - SKIP IT                         
         SPACE                                                                  
*==============================================================*                
* CHKPRET - (*SPRET) EXCLUDE UNTRANSFERABLE RETAIL FROM LIST  *                 
*==============================================================*                
*                                                                               
CHKPRET  NTR1                                                                   
         TM    INOIND,INOSPRET    WANT TO EXLUDE SPEC RETAILS?                  
         BZ    YES                                                              
         BAS   RE,CHKPAMTS        CHECK RETAIL BILLS SHOULD BE EXCLUDED         
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*==============================================================*                
* CHKPAMTS - CHECKS IF RETAIL CORP SUM OR AMOUNTS = ZERO       *                
*==============================================================*                
*                                                                               
CHKPAMTS NTR1                                                                   
         CLI   PBRETAIL,X'81'     RETAIL CORP SUMMS ARE NEVER TRANS             
         BE    NO                                                               
         CP    PBILLGRS,=P'0'     SKIP ZERO BILLS                               
         BNE   YES                                                              
         CP    PBILLNET,=P'0'                                                   
         BNE   YES                                                              
         CP    PBILLBIL,=P'0'                                                   
         BNE   YES                                                              
         CP    PBILLRCV,=P'0'                                                   
         BNE   YES                                                              
*                                                                               
         LA    RE,PBILLEL                                                       
         SR    R0,R0                                                            
CHKPA10  CLI   0(RE),0                                                          
         BE    NO                 NO AMOUNTS                                    
         CLI   0(RE),X'0A'                                                      
         BE    CHKPA20                                                          
         CLI   0(RE),X'84'                                                      
         BE    CHKPA30                                                          
CHKPA15  ICM   R0,1,1(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R0                                                            
         B     CHKPA10                                                          
*                                                                               
         USING PBILVEL,RE                                                       
CHKPA20  OC    PBILLVAT,PBILLVAT                                                
         BNZ   YES                NOT ALL AMOUNTS ZERO                          
         B     CHKPA15            KEEP ON LOOKING                               
         DROP  RE                                                               
*                                                                               
         USING PBLPSTEL,RE                                                      
CHKPA30  OC    PBLPVAMT,PBLPVAMT                                                
         BNZ   YES                 VAT AMOUNT NOT ZERO                          
         OC    PBLPVBAS,PBLPVBAS   VAT BASIS AMOUNT NOT ZERO                    
         BNZ   YES                                                              
         B     CHKPA15                                                          
         DROP  RE                                                               
         SPACE                                                                  
*=================================================*                             
* CHKPXDT-CHECK FOR MXDTE=YYMMDD SETS CC CODE     *                             
*=================================================*                             
*                                                                               
CHKPXDT  NTR1                                                                   
         OC    INOXDAT,INOXDAT    ANY PARTICULAR TRANSFER DATE                  
         BZ    YES                                                              
         OC    PBILPOST,PBILPOST  ANY TRANSFER DATE                             
         BZ    NO                                                               
         CLC   PBILPOST,INOXDAT   TRANSFER DATES MUST MATCH                     
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
* CHKPCLT-CHECK IF BILL CLIENT IS IN OFFICE/OFFICE GROUP REQUESTED*             
*------------------------------------------------------------------*            
*                                                                               
CHKPCLT  NTR1                                                                   
         L     R3,ACLITAB          CHECK CLIENT VALID FOR REQUEST               
CHKPCLT5 OC    0(L'PBILKCLT,R3),0(R3)                                           
         BZ    NO                                                               
         CLC   PBILKCLT,0(R3)                                                   
         BE    YES                                                              
         LA    R3,L'PBILKCLT(R3)                                                
         B     CHKPCLT5                                                         
         DROP  R2                                                               
         EJECT                                                                  
*====================================*                                          
* DISSEL RECORD                      *                                          
* INPUT APPARM(4) =A (TWA DISP LINE) *                                          
*====================================*                                          
*                                                                               
DISSEL   L     R4,APPARM          R2=A(TWA DISPLAY LINE)                        
         USING LDISD,R4                                                         
         CLI   TOTLIN,C'Y'                                                      
         BNE   DISSEL2                                                          
*                                                                               
DISSEL1  MVC   LTYPE(11),=C'** TOTAL **'                                        
         EDIT  TOTAMT,(13,LAMOUNT),2,MINUS=YES,DUB=APDUB,WRK=APWORK             
         MVI   TOTLIN,C'T'        TELL GETSEL TOTALS ARE PRINTED                
         B     DISSELX                                                          
*                                                                               
DISSEL2  CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   DISSEL5                                                          
         BAS   RE,DISPRT          GO DISPLAY PRINT BILL                         
         B     DISSELX                                                          
*                                                                               
DISSEL5  L     R2,AIOAREA1                                                      
         USING BILLRECD,R2                                                      
         OC    BILPOST,BILPOST    TRANSFER DATE                                 
         BNZ   *+8                                                              
         MVI   LTRANS,C'*'        NO - INDICATE UNTRANSFERRED                   
*                                                                               
         BAS   RE,GETSTYP         GET BILL TYPE(REG/RET/AOR)                    
         MVC   LTYPE,TYPBIL                                                     
*                                                                               
         MVC   LMEDIA,QMED        MOVE MEDIA TO SCREEN                          
         OC    QMED,QMED                                                        
         BNZ   DISSEL20                                                         
         CLI   QSYS,C'N'          IF NETWORK                                    
         BNE   DISSEL15                                                         
         MVI   LMEDIA,C'N'                                                      
         OC    BLMED,SPACES                                                     
         CLI   BLMED,C' '                                                       
         BE    DISSEL20                                                         
         MVC   LMEDIA,BLMED       REAL MEDIA IS IN BLMED                        
         B     DISSEL20                                                         
DISSEL15 BAS   RE,CHKAGY          GET MEDIA FROM AGY RECORD                     
*                                                                               
DISSEL20 BAS   RE,DISSCLT         TRY TO DISPLAY CORRECT CLIENT CODE            
         MVC   LPRD,BKEYPRD       MOVE PRODUCT TO SCREEN                        
         ZIC   RE,BKEYEST                                                       
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LEST,APDUB         MOVE ESTIMATE TO SCREEN                       
         GOTO1 VDATCON,APPARM,(0,BQDATE),(5,LINVDT)                             
         GOTO1 VDATCON,APPARM,(0,BDATE),(5,LRUNDT)                              
         CLI   BKEYYSRV+1,X'0D'   IF SPECIAL 13TH MONTH                         
         BNE   DISSEL25                                                         
         MVC   LSRVMON(3),=C'13/'                                               
         ZIC   RE,BKEYYSRV                                                      
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LSRVMON+3(2),APDUB                                               
         B     DISSEL28                                                         
DISSEL25 MVC   APFULL(2),BKEYYSRV GET SERVICE MONTH                             
         MVI   APFULL+2,X'01'     FUDGE DAY                                     
         GOTO1 VDATCON,APPARM,(3,APFULL),(6,LSRVMON)                            
*                                                                               
DISSEL28 DS    0H                                                               
*        GOTO1 =V(SPFMTINO),APPARM,BDATE,(6,BINVNO),(QMED,B1PROF),     X        
               B1XPROF,RR=Y                                                     
         GOTO1 =V(SPFMTINO),APPARM,(C'B',0),(6,BINVNO),(QMED,B1PROF),  +        
               B1XPROF,BKEY,RR=Y                                                
         CLC   BINVMED,SPACES     BILL CONTAINS LONG INV # RULES?               
         BNH   DISSEL29                                                         
         L     R1,APPARM          YES - THEN SHOW LONG INV #                    
         MVC   LINVOICE,0(R1)                                                   
         B     DISSEL30                                                         
DISSEL29 L     R1,APPARM+4        ELSE - USE SHORT INV #                        
         MVC   LINVOICE(7),0(R1)                                                
*                                                                               
DISSEL30 BAS   RE,CALCAMT          CALCULATE BILL AMOUNT(BILL+GST+PST)          
         EDIT  PRTAMT,(13,LAMOUNT),2,MINUS=YES,DUB=APDUB,WRK=APWORK             
         AP    TOTAMT,PRTAMT                                                    
DISSELX  B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE GETS CLIENT RECORD INORDER TO DISPLAY CORRECT            
*              CLIENT CODE                                                      
         SPACE 1                                                                
DISSCLT  NTR1                                                                   
**NO-OP**GOTO1 VCLUNPK,APPARM,(SVCPRF7,BKEYCLT),LCLT                            
*                                                                               
         ZIC   R0,SVCPRF7          USE SAVED CLIENT CPROF+6                     
         OC    QCLT,QCLT           IF LIST IS FILTERED BY CLIENT                
         BNZ   DISSCLT9                                                         
*                                                                               
         ZIC   R0,SVLCPRF7         USE PREVIOUS SAVED CLIENT CPROF+6            
         CLC   BKEYCLT,SVLCLT      IF SAME CLIENT CODE                          
         BE    DISSCLT9                                                         
         MVC   SVLCLT,BKEYCLT      ELSE, SAVE NEW CLIENT CODE                   
         XC    IOKEY,IOKEY         BUILD CLIENT HEADER KEY                      
         MVC   IOKEY(4),BKEY       X'00',A/M,CLT                                
         GOTO1 AMYIO,APPARM,IOHI+IO3,=C'D'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(4),IOKEYSAV                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AMYIO,APPARM,IOGET+IO3,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    APINDS,APILRERD     SET TO RE-ESTABLISH DIRECTORY                
         L     RE,AIOAREA3                                                      
         USING CLTHDRD,RE                                                       
         ZIC   R0,CPROF+6                                                       
         STC   R0,SVLCPRF7         SAVE NEW CLIENTS CPROF+6                     
         DROP  RE                                                               
*                                                                               
DISSCLT9 GOTO1 VCLUNPK,APPARM,((R0),BKEYCLT),LCLT                               
         B     EXIT                                                             
         SPACE 2                                                                
*              ROUTINE TO CALCULATE SPOT/NET BILL AMOUNT                        
*                                  RETURNS AMOUNT IN PRTAMT                     
CALCAMT  NTR1                                                                   
         ZAP   PRTAMT,BACTP         BILL AMOUNT                                 
         CLC   BLEN,=H'90'                                                      
         BNH   CALCAMT2                                                         
         ICM   R0,15,BVATAMT       ADD GST AMOUNT                               
         CVD   R0,APDUB                                                         
         AP    PRTAMT,APDUB                                                     
*                                                                               
CALCAMT2 CLC   BLEN,=H'130'        ADD PST AMOUNT                               
         BNH   CALCAMTX                                                         
         SR    R0,R0                                                            
         ICM   R0,1,BILNPVTS                                                    
         BZ    CALCAMTX                                                         
         LA    RE,BILPVELD                                                      
         USING BILPVELD,RE                                                      
CALCAMT5 XC    APDUB,APDUB                                                      
         ICM   RF,15,BILPVAMT                                                   
         CVD   RF,APDUB                                                         
         AP    PRTAMT,APDUB        ADD PROVINCIAL PST TO BILL AMOUNT            
         LA    RE,BILPVLEN(RE)                                                  
         BCT   R0,CALCAMT5                                                      
*                                                                               
CALCAMTX B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*====================================*                                          
* CHKAGY - MATCHES BINARY A/M IN     *                                          
*          CURRENT KEY WITH AGY REC  *                                          
*          FOR MEDIA CODE AND MOVES  *                                          
*          IT TO PRINT LINE          *                                          
*====================================*                                          
*                                                                               
CHKAGY   NTR1                                                                   
         L     R3,AIOAREA2                                                      
         USING AGYHDRD,R3                                                       
         LA    R1,AGYEL                                                         
CHKAGY10 CLI   0(R1),0                                                          
         BE    CHKAGYX            END OF RECORD - NO MATCH                      
         CLI   0(R1),2                                                          
         BNE   CHKAGY20                                                         
         CLC   3(1,R1),BKEYAM     MATCH ON BINARY A/M?                          
         BNE   CHKAGY20                                                         
         MVC   LMEDIA,2(R1)       MOVE CHARACTER MEDIA TO PRINT LINE            
         B     CHKAGYX            GET OUT                                       
*                                                                               
CHKAGY20 SR    R0,R0                                                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     CHKAGY10                                                         
*                                                                               
CHKAGYX  B     EXIT                                                             
         DROP  R3,R2                                                            
         EJECT                                                                  
*----------------------------------------*                                      
* DISPRT - DISPLAY PRINT BILL            *                                      
*----------------------------------------*                                      
DISPRT   NTR1                                                                   
         L     R2,AIOAREA1                                                      
         USING PBILLREC,R2                                                      
         OC    PBILPOST,PBILPOST  TRANSFER DATE                                 
         BNZ   *+8                                                              
         MVI   LTRANS,C'*'        NO - INDICATE UNTRANSFERRED                   
         BAS   RE,GETPTYP         GET BILL TYPE (REG/RET/AOR/FIN)               
         MVC   LTYPE,TYPBIL                                                     
*                                                                               
DISP10   MVC   LMEDIA,PBILKMED       MOVE MEDIA TO SCREEN                       
         MVC   LCLT,PBILKCLT         MOVE CLIENT TO SCREEN                      
         MVC   LPRD,PBILKPRD         MOVE PRODUCT TO SCREEN                     
         SR    RE,RE                                                            
         ICM   RE,3,PBILKEST                                                    
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LEST,APDUB         MOVE ESTIMATE TO SCREEN                       
*                                                                               
         GOTO1 VDATCON,APPARM,(3,PBILINVD),(5,LINVDT)                           
         GOTO1 VDATCON,APPARM,(0,PBILLDAT),(5,LRUNDT)                           
         CLI   PBILKMOS+1,X'0D'   IF SPECIAL 13TH MONTH                         
         BNE   DISP15                                                           
         MVC   LSRVMON(3),=C'13/'                                               
         ZIC   RE,PBILKMOS                                                      
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LSRVMON+3(2),APDUB                                               
         B     DISP20                                                           
DISP15   MVC   APFULL(2),PBILKMOS GET SERVICE MONTH                             
         MVI   APFULL+2,1         FUDGE DAY                                     
         GOTO1 VDATCON,APPARM,(3,APFULL),(6,LSRVMON)                            
*                                                                               
*ISP20   BAS   RE,SETPINV         SET PRINT INVOICE NUMBER                      
DISP20   GOTO1 =V(PPFMTINO),APPARM,(C'B',PBILLDAT),(2,PBILKBNO),       +        
               (QMED,B1PROF),B1XPROF,PBILLREC,RR=Y                              
*ISP20   GOTO1 =V(PPFMTINO),APPARM,(C'B',0),(2,PBILKBNO),(QMED,B1PROF),+        
               B1XPROF,PBILLREC,RR=Y                                            
         CLC   PBILIMED,SPACES    BILL CONTAINS LONG INV # RULES?               
         BNH   DISP30                                                           
         L     R1,APPARM          YES - THEN SHOW LONG INV #                    
         MVC   LINVOICE,0(R1)                                                   
         B     DISP40                                                           
DISP30   L     R1,APPARM+4        ELSE - USE SHORT INV #                        
         MVC   LINVOICE(7),0(R1)                                                
*                                                                               
DISP40   BAS   RE,CALCPAMT         CALCULATE BILL AMT (BILL+GST+PST)            
         EDIT  PRTAMT,(13,LAMOUNT),2,MINUS=YES,DUB=APDUB,WRK=APWORK             
         AP    TOTAMT,PRTAMT                                                    
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CALCULATE PRINT BILL AMOUNT                           
*                                  RETURNS PRTAMT                               
CALCPAMT NTR1                                                                   
         ZAP   PRTAMT,PBILLRCV                                                  
         CLI   PBILCDSW,C'S'      IF CD ON SEP BILL                             
         BE    CALPAM5            YES - DON'T LOOK AT FORMULA                   
         TM    PBILBASA,X'04'     TEST LESS CD                                  
         BNO   CALPAM5                                                          
         AP    PRTAMT,PBILLGRS                                                  
         SP    PRTAMT,PBILLBIL                                                  
*                                                                               
CALPAM5  LA    RE,PBILLEL                                                       
         USING PBILVEL,RE                                                       
CALPAM8  CLI   0(RE),0                                                          
         BE    EXIT                                                             
         CLI   0(RE),X'0A'                                                      
         BNE   CALPAM10                                                         
         ICM   R0,15,PBILLVAT                                                   
         CVD   R0,APDUB                                                         
         AP    PRTAMT,APDUB        ADD GST AMOUNT                               
         B     CALPAM20                                                         
*                                                                               
CALPAM10 CLI   0(RE),X'84'                                                      
         BNE   CALPAM20                                                         
         USING PBLPSTEL,RE                                                      
         ICM   R0,15,PBLPVAMT      ADD PST AMOUNT                               
         CVD   R0,APDUB                                                         
         AP    PRTAMT,APDUB                                                     
*                                                                               
CALPAM20 SR    R0,R0               FIND NEXT ELEMENT                            
         ICM   R0,1,1(RE)                                                       
         AR    RE,R0                                                            
         B     CALPAM8                                                          
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
* SETPINV - SETS APWORK(6) TO BILL INVOICE NUMBER                               
*&&DO                                                                           
SETPINV  NTR1                                                                   
         MVC   APWORK(2),PBILLDAT+2                                             
*                                                                               
         OC    PBILPOST,PBILPOST  IF BILL TRANSFERRED                           
         BZ    SETPINV6                                                         
         CLC   PBILPOST,=X'BF01'  BEFORE 8/1/95                                 
         BNL   SETPINV6                                                         
         CLI   B1XPROF+4,0        CHECK B1X PROFILE                             
         BE    SETPINV5                                                         
         PACK  APDUB,PBILLDAT(2)  YEAR OF BILL                                  
         CVB   R0,APDUB                                                         
         ZIC   RF,B1XPROF+4       INVOICE BASE YEAR                             
         SR    R0,RF                                                            
         BNP   SETPINV5                                                         
         MH    R0,=H'12'                                                        
         PACK  APDUB,PBILLDAT+2(2)                                              
         CVB   RF,APDUB                                                         
         AR    R0,RF                                                            
*                                                                               
SETPINV2 CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(2),APDUB    SET NEW INVOICE MONTH                         
         B     SETPINV8                                                         
*                                                                               
SETPINV5 CLI   B1XPROF+5,0        SEE IF BUMPING INV MONTH                      
         BE    SETPINV8                                                         
         PACK  APDUB,PBILLDAT+2(2)                                              
         CVB   RF,APDUB                                                         
         ZIC   R0,B1XPROF+5                                                     
         AR    R0,RF                                                            
         CH    R0,=H'12'                                                        
         BNH   SETPINV2                                                         
         SH    R0,=H'12'                                                        
         B     SETPINV2                                                         
*                                                                               
SETPINV6 OC    PBILIMO,PBILIMO     CHECK NEW FIELD                              
         BZ    *+10                                                             
         MVC   APWORK(2),PBILIMO                                                
*                                                                               
SETPINV8 MVC   APHALF,PBILKBNO                                                  
         LH    R0,APHALF                                                        
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK+2(4),APDUB                                                
SETPIX   B     EXIT                                                             
         DROP  R4,R2                                                            
*&&                                                                             
         EJECT                                                                  
*=============*                                                                 
* LITERAL POOL*                                                                 
*=============*                                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
         EJECT                                                                  
*                                                                               
* ACTRAWRK                                                                      
*                                                                               
       ++INCLUDE ACTRAWRK                                                       
         EJECT                                                                  
LOCALD   DSECT                                                                  
OCFLAG   DS    CL1                O= OFFICE INPUTTED                            
APBYTE2  DS    CL1                                                              
RECHANG  DS    CL1                                                              
NEWREC   DS    CL1                                                              
BILMONTH DS    XL1                                                              
TYPBIL   DS    CL5                                                              
PRTAMT   DS    PL6                                                              
SELKEY   DS    CL(L'BKEY)                                                       
FIRSTSCR DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE ACTRA07D                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAEAD                                                       
         SPACE 2                                                                
         ORG                                                                    
*=====================*                                                         
* OTHER VARIOUS DSECTS*                                                         
*=====================*                                                         
*                                                                               
* SPGENBILL                                                                     
* SPGENAGY                                                                      
* PPNEWFILE                                                                     
*                                                                               
         PRINT OFF                                                              
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
*                                                                               
*PREFIX=P  =PPRELEM                                                             
       ++INCLUDE PPNEWFILE                                                      
*PREFIX=                                                                        
       ++INCLUDE PBLPSTEL                                                       
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064ACTRA07   02/15/19'                                      
         END                                                                    
