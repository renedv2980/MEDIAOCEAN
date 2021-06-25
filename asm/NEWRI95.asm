*          DATA SET NEWRI95    AT LEVEL 012 AS OF 05/06/04                      
*PHASE T32095A,+0                                                               
*INCLUDE DAYUNPK                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'T32095 - CUT-IN REPORT'                                         
T32095   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*NECUT*,RR=R2                                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T32095,RB,RA                                                     
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS2                                                       
         USING MYWORKD,R7                                                       
         L     R6,ANETWS4                                                       
         ST    R6,ACLIST                                                        
         ST    R2,RELO                                                          
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    REPMOD                                                           
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
REPMOD   DS    0H                                                               
*                                                                               
*                               * SET UP BINSRCH PARAMETERS                     
         SR    R0,R0               A OF REC TO BE ADDED                         
         L     R1,=A(BINTABLE)     A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,BRECLENE         LENGTH OF REC                                
         LA    R4,BKEYLENE         DISP OF KEY INTO REC                         
         L     R5,=F'500'          MAX RECS IN BINTBL                           
         STM   R0,R5,BINDMCB                                                    
*                                                                               
         L     RE,=A(BINTABLE)         CLEAR BINTABLE                           
         L     RF,=F'19000'        500 RECS X 38 REC LEN                        
         XCEF                                                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B     REP3                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,96,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=100'                                   
*                                                                               
REP3     DS    0H                                                               
*                                                                               
         MVI   NBDATA,C'U'         GET UNITS                                    
         OI    NBSPLOPT,X'C0'      ALWAYS SPLIT                                 
         MVI   NBSELUOP,C'A'       ACTUAL SCHEDULE                              
         MVI   PRDCNT,1                                                         
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST     LAST ONE                                     
         BE    REP500                                                           
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BNE   GETUNIT                                                          
*                                                                               
* ONLY LOOK AT UNITS WITH CUTINS/SECTIONALS                                     
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETUNIT                                                          
         USING NUSPRD,R2                                                        
REP03    CLI   NUSPRTYP,C'E'       SECTIONAL?                                   
         BE    REP05                                                            
         CLI   NUSPRTYP,C'U'       CUT IN ?                                     
         BNE   REP04                                                            
         CLI   NUSPRLEN,NUSPRLN3   CUT IN STATION ELEM?                         
         BE    REP05               YES                                          
REP04    BAS   RE,NEXTEL           NO-NEXT ELEM                                 
         BNE   GETUNIT                                                          
         B     REP03                                                            
*                                                                               
REP05    MVC   PRDNUM,NBPRDNO          NUMBER OF MULTI PRODS                    
         CLI   PRDNUM,0                                                         
         BNE   REP06                                                            
         MVI   PRDNUM,1                NO MULTIS/SET 1 OR 2                     
         CLI   NBPRD2,0                                                         
         BE    *+8                                                              
         MVI   PRDNUM,2                                                         
REP06    ZAP   PAKWRK2,=P'0'                                                    
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         CLI   PRDCNT,1              ARE WE ON FIRST TIME FOR UNIT?             
         BNE   REP10                 NO-DON'T DUPLICATE INFO ON P LINE          
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,PUNDATE)    DATE                     
         EDIT  (B1,NBACTEST),(3,PEST)                                           
         MVC   PVEND,NBACTNET                 NET                               
         MVC   PDAY,NBDAYNAM                 DAY                                
         GOTO1 UNTIME,DMCB,NBTIME,PSTEND             TIME                       
***      CLC   PREVPROG,NBACTPRG       SAME PROGRAM?                            
***      BE    REP10                   YES/SKIP PRINTING OF IT                  
***      MVC   PREVPROG,NBACTPRG                                                
***      MVC   PPROGRAM(6),NBACTPRG                                             
***      MVC   PPROGRAM+7(12),NBPROGNM                                          
* PUT OUT PRODUCTS                                                              
REP10    MVC   BYTE,NBSPLPRN                                                    
         BAS   RE,GETPRD3                                                       
         MVC   PPROD,WORK          PRODUCT                                      
         MVC   PRDSV,WORK          SAVE FOR BINSRCH BELOW                       
         BAS   RE,GETCML                                                        
         MVC   PCOMMLCD,WORK       COMML CODE                                   
         MVC   PCOMMLNM,WORK+8     COMML NAME                                   
REP55    EDIT  (B1,NBLEN),(3,PUNLEN)                                            
*                                                                               
         EDIT  (B4,NBACTUAL),(12,PCOST),2,COMMAS=YES  ACTUAL COST               
         L     R1,UNACTUAL             ADD TO UNIT ACTUAL $$ TOTAL              
         ICM   R2,15,NBACTUAL                                                   
         AR    R1,R2                                                            
         ST    R1,UNACTUAL                                                      
* PUT PERCENTAGE SHARE PER PRODUCT                                              
         CLI   NBPRDNO,0               IF MULTI PRODS                           
         BNE   PCTX                                                             
         CLI   PRDCNT,1                FIRST PRODUCT?                           
         BNE   PCT10                                                            
         CLC   NBP1SHR,=X'0000'        GET'S IT ALL?                            
         BNE   PCT08                                                            
         MVC   PCOST+15(9),=C'(100.00%)'                                        
         B     REP56                                                            
*                                                                               
PCT08    EDIT  (B2,NBP1SHR),(8,PCOST+15),2,BRACKET=YES,TRAIL=C'%'               
         B     REP56                   AND WRITE IT                             
PCT10    ICM   RE,15,=F'10000'                                                  
         SR    RF,RF                                                            
         ICM   RF,3,NBP1SHR                                                     
         SR    RE,RF                                                            
         EDIT  (RE),(8,PCOST+15),2,BRACKET=YES,TRAIL=C'%'                       
         B     REP56                                                            
PCTX     EQU   *                                                                
         L     R2,NBAIO                GET X'14' ELEMENT                        
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   REP56                                                            
         USING NUPRDD,R2                                                        
         ZIC   R4,NBPRDNO              SET NUMBER                               
         B     REP55AA                                                          
REP55A   LA    R4,1                    ELSE ASSUME 1 PROD                       
         CLI   NBPRD2,0                OR 2                                     
         BE    REP55AA                                                          
         LA    R4,1(R4)                                                         
*                                                                               
REP55AA  LA    R2,NUPRDPR              POINT R1-> PROD CODE                     
REP55B   CLC   NBSPLPRN,0(R2)                                                   
         BNE   REP55C                                                           
         EDIT  (B2,2(R2)),(8,PCOST+15),2,BRACKET=YES,TRAIL=C'%'                 
         B     REP56                                                            
REP55C   LA    R2,6(R2)                                                         
         BCT   R4,REP55B                                                        
REP56    BAS   RE,WRITIT                                                        
         DROP  R2                                                               
* SET ACTUAL COST TO BINSRCH                                                    
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING BINRECD,R2                                                       
         MVI   BREC1,1             REC=1 BY PRODUCT                             
         MVC   B1PRD,PRDSV         PROD CODE                                    
         ICM   R1,15,NBACTUAL                                                   
         CVD   R1,DUB                                                           
         ZAP   BUNTCOST,DUB                                                     
         ZAP   BCUTCOST,=P'0'       SET UP FOR FUTURE ADD PACK                  
         DROP  R2                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R2))                                     
         CLI   0(R1),X'01'         X'01'=REC FOUND                              
         BE    REP57                                                            
         L     RE,0(R1)                                                         
         USING BINRECD,RE                                                       
         AP    BUNTCOST,DUB                                                     
         DROP  RE                                                               
REP57    EQU   *                                                                
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING BINRECD,R2                                                       
         MVI   BREC2,2                  REC=2 BY NETWORK/DAYPART                
         MVC   B2NET,NBACTNET           NETWORK                                 
         MVC   B2NET+4(1),NBSTATYP                                              
         MVC   B2DPT,NBACTNDP           DAYPART                                 
         MVC   B2DPTNM(8),NBDPNAM       DPT NAME                                
         MVC   B2DPTNM+7(6),NBDPNAM2    DAYPART NAME CONTINUATION               
         ICM   R1,15,NBACTUAL                                                   
         CVD   R1,DUB                                                           
         ZAP   BUNTCOST,DUB                                                     
         ZAP   BCUTCOST,=P'0'      FOR FUTURE ADD PACK                          
         DROP  R2                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R2))                                     
         CLI   0(R1),X'01'         X'01'=REC FOUND                              
         BE    REP58                                                            
         L     RE,0(R1)                                                         
         USING BINRECD,RE                                                       
         AP    BUNTCOST,DUB                                                     
         DROP  RE                                                               
REP58    EQU   *                                                                
*                                                                               
* ARE THERE MORE PRODUCTS ON THIS UNIT                                          
         ZIC   R1,PRDCNT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,PRDCNT                                                        
         CLC   PRDCNT,PRDNUM           HAVE WE FINISHED PRODS?                  
         BNH   GETUNIT                NOT YET                                   
         MVI   PRDCNT,1               FINISHED/CLEAR PROD COUNTS                
         MVI   PRDNUM,0                                                         
*                                                                               
*                                     YES/DO SECTIONAL/CUTIN FOR UNIT           
REP60    DS    0H                                                               
         BAS   RE,WRITIT           SKIP A LINE                                  
         ZAP   PAKWRK1,=P'0'       PREPARE PACK WORK AREA                       
         ZAP   PAKSEC,=P'0'       PREPARE PACK WORK AREA                        
         XC    CUTINTBL(200),CUTINTBL  USE AS SAVE AREA                         
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   REP60D                  GO TO CUTINNS                            
         B     *+12                                                             
REP60A   MVI   ELCODE,X'03'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   REP60B                                                           
                                                                                
         USING NUSPRD,R2                                                        
         CLI   NUSPRTYP,C'E'       SECTIONAL?                                   
         BNE   REP60A                                                           
         ICM   R1,15,NUSPRAMT      GET DOLLARS                                  
         CVD   R1,DUB                                                           
         AP    PAKWRK1,DUB         ROLL THEM OVER                               
         CLI   CUTINTBL+1,0        ALREADY HAVE PRODUCT?                        
         BNE   REP60A                                                           
         MVC   BYTE,NUSPRBPR       BILLED PRODUCT                               
         CLI   BYTE,0              OK                                           
         BNE   REP60AA                                                          
         MVC   BYTE,NBPRD          NO/USE SCHEDULED PRODUCT                     
         CLI   BYTE,0                                                           
         BNE   REP60AA                                                          
         MVC   BYTE,NBPRDLST       ASSUME MULTIS                                
REP60AA  BAS   RE,GETPRD3                                                       
         MVC   CUTINTBL(3),WORK    SAVE PRODUCT IN CUTINTBL                     
         MVC   PRDSV2,WORK         SAVE PRODUCT FOR PROD EOF TOTALS             
         B     REP60A              GET NEXT SECTIONAL                           
REP60B   DS    0H                                                               
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   PSTEND(9),=C'SECTIONAL'                                          
         MVC   PPROD,CUTINTBL                      PRODUCT                      
         EDIT  (P8,PAKWRK1),(10,PCUCOST),2         $$$                          
         AP    PAKSEC,PAKWRK1                 PASS SEC$$ ALONG                  
         BAS   RE,SUMTOTS          END OF REPORT SUMMARY TOTALS                 
         ZAP   PAKWRK1,=P'0'                                                    
         MVI   CUTFLG,1                HAVE ONE                                 
         L     R2,NBAIO                       GET FEED ELEMENTS                 
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   REP60BF                 JUST GET OUT                             
*****    BE    *+6                                                              
*****    DC    H'0'                    MUST HAVE AT LEAST ONE                   
         USING NUFDCEL,R2                                                       
         MVC   PPROGRAM(4),NUFDCFED   FIRST FEED                                
         LA    R3,PPROGRAM         POINT R3->PROGRAM PRINT POSITION             
REP60BB  CLI   0(R3),X'40'         DROP TRAILING BLANKS                         
         BNH   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)            BUMP TO NEXT OPEN POSITION                   
         MVI   ELCODE,X'23'        RESTORE ELEMENT CODE                         
         BAS   RE,NEXTEL                                                        
         BE    REP60BC                                                          
         BCTR  R3,0                BACK UP TO C'/'                              
         MVI   0(R3),X'40'         CLEAR IT                                     
         B     REP60BF             THAT'S ALL                                   
REP60BC  MVC   0(4,R3),NUFDCFED    SET FEED                                     
         B     REP60BB                                                          
REP60BF  BAS   RE,WRITIT                                                        
         DROP  R2,R3                                                            
         EJECT                                                                  
* NOW DEAL WITH CUTINS                                                          
REP60D   DS    0H                                                               
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   PSTEND(5),=C'CUTIN:'                                             
*                                  SAVE CUTIN COMMLS IN CUTINTBL                
         XC    CUTINTBL(8),CUTINTBL CLEAR START OF TBL                          
         ZAP   PAKWRK1,=P'0'       PREPARE PACK WORK AREA                       
*                                                                               
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'17'        CUTIN COMML ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   REP62               NO CUT IN COMMERCIALS                        
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CUTINTBL(0),2(R2)   SAVE TABLE                                   
         MVI   CUTFLG,1                                                         
*                                                                               
REP62    L     R2,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   REP100                                                           
         USING NUSPRD,R2                                                        
REP65    CLI   NUSPRTYP,C'U'       CUT IN?                                      
         BE    REP70                                                            
REP67    MVI   ELCODE,X'03'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   REP100              NO MORE                                      
         B     REP65                                                            
REP70    CLI   NUSPRLEN,NUSPRLN3   CUT IN STATION NEW ELEM?                     
         BNE   REP67                                                            
         OC    NUSPRCIS,NUSPRCIS   STA/MKT PACKED?                              
         BZ    REP78                                                            
         GOTO1 MSUNPK,DMCB,NUSPRCIS,WORK,WORK+4                                 
         MVC   PAFFSTA,WORK+4             ****PASS STATION****                  
         MVC   MKTSAV,WORK         SAVE MARKET                                  
                                                                                
*                                     READ STATION REC FOR ADDRESS              
         CLI   SPOTSE,0            DO I HAVE SPOT SE NUMBER?                    
         BNE   *+8                                                              
         BAS   RE,OPENSTA          NO/OPEN SPOT STATION FILE                    
         L    R1,NBUTL             YES/SET TO READ SPOT STATION FILE            
         MVC   4(1,R1),SPOTSE                                                   
         NETGO NVSETSTA,DMCB                                                    
         MVI   USEIO,C'Y'                                                       
         LA    R1,MYIO                                                          
         ST    R1,AIO                                                           
         MVC   FILENAME,=C'STATION'                                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
******   USING ADDRREC,R4                                                       
******   MVI   ADDKTYPE,C'A'       ADDRESS                                      
******   MVI   ADDKMED,C'T'        SPOT FILE RECORD SO = T NOT N                
******   MVC   ADDKCALL,PAFFSTA    CALL LETTERS                                 
******   MVI   ADDKCALL+4,C'T'                                                  
******   MVC   ADDKAGY,NBSELAGY                                                 
******   MVC   ADDKFILL,=C'000000'                                              
         USING MKTREC,R4                                                        
         MVI   MKTKTYPE,C'M'       MARKET RECORD                                
         MVI   MKTKMED,C'T'        SPOT FILE RECORD SO = T NOT N                
         MVC   MKTKMKT,MKTSAV      MARKET SAVE FROM ABOVE                       
         MVC   MKTKAGY,NBSELAGY                                                 
         MVC   MKTKFILL,=C'0000000'                                             
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BE    REP74                                                            
**       DC    H'0'                                                             
         MVC   PPROGRAM(15),=C'*** UNKNOWN ***'                                 
         B     REP78                                                            
REP74    L     R4,AIO                                                           
         MVC   PPROGRAM,MKTNAME                                                 
         B     REP78               DON'T NEED ALL THAT BACKUP                   
*                                                                               
**       LA    R1,PPROGRAM+17                                                   
**       LA    RE,17                                                            
**REP75    CLI   0(R1),X'40'                                                    
**       BH    REP77                                                            
**       BCTR  R1,0             BACK UP ONE                                     
**       BCT   RE,REP75                                                         
**       B     REP78                                                            
**REP77    LA    R1,1(R1)            BACK UP                                    
**       MVI   0(R1),C','                                                       
**       MVC   1(3,R1),A3LINE   STATE                                           
REP78    EQU   *                                                                
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,C'N'                                                       
         MVC   AIO,NBAIO           RESET AIO AREA                               
         ICM   R1,15,NBUTL                                                      
         BZ    *+10                                                             
         MVC   4(1,R1),NETSE       RESET NETWORK SE NUMBER                      
         DROP  R4                                                               
*                                                                               
         MVI   CUTFLG,1                                                         
         MVC   BYTE,NUSPRBPR                                                    
         CLI   TRAFPRD,C'Y'        TRAFFIC PROD                                 
         BNE   REP79                                                            
         MVC   BYTE,NUSPRTPR       YES                                          
*                                                                               
REP79    BAS   RE,GETPRD3                                                       
         MVC   PPROD,WORK                                                       
         MVC   PRDSV2,WORK                                                      
*                                                                               
         ZIC   R1,NUSPRCMI         COMMERCIAL INDEX?                            
         LTR   R1,R1                                                            
         BZ    REP80                                                            
         BCTR  R1,0                                                             
         LA    RE,CUTINTBL ,                                                    
         MHI   R1,8                                                             
         AR    RE,R1                                                            
         MVC   PCOMMLCD,0(RE)      SET COMMERCIAL CODE                          
         MVC   WORK(8),PCOMMLCD                                                 
         BAS   RE,GTCMLNM                                                       
         MVC   PCOMMLNM,WORK+8                                                  
*                                                                               
REP80    EDIT  (B4,NUSPRAMT),(10,PCUCOST),2,COMMAS=YES   SET COST               
         BAS   RE,WRITIT                                 WRITE LINE             
         ICM   R1,15,NUSPRAMT                                                   
         CVD   R1,DUB                                                           
         ZAP   PAKWRK1,DUB         SAVE CUTINCOST IN PAKWRK1                    
*                                                                               
         BAS   RE,SUMTOTS          END OF REPORT SUMMARY TOTALS                 
         AP    PAKWRK2,PAKWRK1     ROLL OVER CUTIN COST TO PAKWRK2              
         B     REP67                                     GET NEXT ELEM          
                                                                                
*                                                                               
REP100   CLI   CUTFLG,0            DID WE HAVE ANY HITS?                        
         BE    REP100X                                                          
         MVI   CUTFLG,0                                                         
         MVC   PCOST+132(32),=C'--------------------------------'               
         BAS   RE,WRITIT                                                        
         MVC   PCOMMLCD(10),=C'UNIT TOTAL'                                      
         EDIT  (B4,UNACTUAL),(12,PCOST),2,COMMAS=YES                            
         AP    PAKWRK2,PAKSEC      ADD SECTIONAL $$$                            
         EDIT  (P8,PAKWRK2),(10,PCUCOST),2,COMMAS=YES                           
         L     R1,UNACTUAL             ADD ACTUAL COST TO CUTIN COSTS           
         XC    UNACTUAL,UNACTUAL       CLEAR UNIT ACT TOTAL $$$                 
         CVD   R1,DUB                                                           
         AP    PAKWRK2,DUB                                                      
         EDIT  (P8,PAKWRK2),(12,PTOTCST),2,COMMAS=YES                           
         ZAP   PAKWRK2,=P'0'       CLEAR PAK WORK AREA                          
         MVC   PCOST+132(32),=C'--------------------------------'               
         BAS   RE,WRITIT                                                        
         BAS   RE,WRITIT           AND SKIP A LINE                              
REP100X  DS    0H                                                               
         B     GETUNIT                                                          
         DROP  R3                                                               
                                                                                
*                                                                               
REP500   DS    0H                                                               
         L     R2,=A(BINTABLE)                                                  
         USING BINRECD,R2                                                       
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         ZAP   PAKWRK1,=P'0'                                                    
         ZAP   PAKWRK2,=P'0'                                                    
         ZAP   PAKWRK3,=P'0'                                                    
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         MVI   TOTRTN,C'Y'         SET FLAG FOR HOOK                            
         LA    R1,H10                                                           
         MVC   H10+20(6),=C'VENDOR'                                             
         MVC   H10+30(3),=C'DAY'                                                
         MVC   H10+162(4),=C'PART'                                              
         MVC   H10+37(7),=C'DAYPART'                                            
         MVC   H10+170(4),=C'DESC'                                              
         MVC   H10+63(4),=C'NATL'                                               
         MVC   H10+195(4),=C'COST'                                              
         MVC   H10+75(10),=C'FEED/CUTIN'                                        
         MVC   H10+210(4),=C'COST'                                              
         MVC   H10+94(5),=C'TOTAL'                                              
         MVC   H10+226(4),=C'COST'                                              
*                                                                               
REP510   CLI   0(R2),2             GET NETWORK BINRECS                          
         BE    REP520                                                           
REP512   LA    R2,BRECLENE(R2)     BUMP TO /GET 2'S                             
         CLI   0(R2),0             EOF?                                         
         BE    REP550              YES                                          
         B     REP510                                                           
REP520   LA    R3,P                                                             
         MVC   P+20(5),B2NET                                                    
         MVC   P+30(2),B2DPT                                                    
         MVC   P+35(14),B2DPTNM                                                 
         EDIT  (P8,BUNTCOST),(12,P+60),2,COMMAS=YES                             
         AP    PAKWRK1,BUNTCOST     ROLL OVER                                   
         EDIT  (P8,BCUTCOST),(12,P+75),2,COMMAS=YES                             
         AP    PAKWRK2,BCUTCOST  ROLL OVER                                      
         AP    BUNTCOST,BCUTCOST                                                
         AP    PAKWRK3,BUNTCOST   ROLL OVER                                     
         EDIT  (P8,BUNTCOST),(12,P+90),2,COMMAS=YES                             
         BAS   RE,WRITIT                                                        
         B     REP512                                                           
REP550   DS    0H                                                               
         MVC   P+60(42),=C'------------------------------------------'          
         BAS   RE,WRITIT                                                        
         MVC   P+20(14),=C'REQUEST TOTALS'                                      
         EDIT  (P8,PAKWRK1),(12,P+60),2,COMMAS=YES                              
         EDIT  (P8,PAKWRK2),(12,P+75),2,COMMAS=YES                              
         EDIT  (P8,PAKWRK3),(12,P+90),2,COMMAS=YES                              
         BAS   RE,WRITIT                                                        
         MVC   P+60(42),=C'------------------------------------------'          
         BAS   RE,WRITIT                                                        
         ZAP   PAKWRK1,=P'0'                                                    
         ZAP   PAKWRK2,=P'0'                                                    
         ZAP   PAKWRK3,=P'0'                                                    
*                                                                               
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         MVI   TOTRTN,C'Y'         SET FLAG FOR HOOK                            
         LA    R1,H10                                                           
         MVC   H10+30(5),=C'BRAND'                                              
         MVC   H10+63(4),=C'NATL'                                               
         MVC   H10+195(4),=C'COST'                                              
         MVC   H10+75(10),=C'FEED/CUTIN'                                        
         MVC   H10+210(4),=C'COST'                                              
         MVC   H10+94(5),=C'TOTAL'                                              
         MVC   H10+226(4),=C'COST'                                              
*                                                                               
         L     R2,=A(BINTABLE)         GET 1'S                                  
REP555   CLI   0(R2),1                                                          
         BE    REP570                                                           
         LA    R2,BRECLENE(R2)                                                  
         CLI   0(R2),0                                                          
         BE    REP600                                                           
         B     REP555                                                           
REP570   DS    0H                                                               
         LA    R3,P                                                             
         MVC   P+30(3),B1PRD                                                    
         EDIT  (P8,BUNTCOST),(12,P+60),2,COMMAS=YES                             
         AP    PAKWRK1,BUNTCOST                                                 
         EDIT  (P8,BCUTCOST),(12,P+75),2,COMMAS=YES                             
         AP    PAKWRK2,BCUTCOST                                                 
         AP    BUNTCOST,BCUTCOST                                                
         EDIT  (P8,BUNTCOST),(12,P+90),2,COMMAS=YES                             
         AP    PAKWRK3,BUNTCOST                                                 
         BAS   RE,WRITIT                                                        
         LA    R2,BRECLENE(R2)     BUMP TO NEXT BINREC                          
         B     REP555                                                           
REP600   DS    0H                                                               
         MVC   P+60(42),=C'------------------------------------------'          
         BAS   RE,WRITIT                                                        
         MVC   P+20(14),=C'REQUEST TOTALS'                                      
         EDIT  (P8,PAKWRK1),(12,P+60),2,COMMAS=YES                              
         EDIT  (P8,PAKWRK2),(12,P+75),2,COMMAS=YES                              
         EDIT  (P8,PAKWRK3),(12,P+90),2,COMMAS=YES                              
         BAS   RE,WRITIT                                                        
         MVC   P+60(42),=C'------------------------------------------'          
         BAS   RE,WRITIT                                                        
         B     XIT                                                              
         EJECT                                                                  
         DROP  R2                                                               
SUMTOTS  NTR1                                                                   
* SET CUT IN COST TO BINSRCH                                                    
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING BINRECD,R4                                                       
         MVI   BREC1,1             REC=1 BY PRODUCT                             
         MVC   B1PRD,PRDSV2        PROD CODE                                    
         ZAP   BCUTCOST,PAKWRK1                                                 
         ZAP   BUNTCOST,=P'0'                                                   
         DROP  R4                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R4))                                     
         CLI   0(R1),X'01'         X'01'=REC FOUND                              
         BE    SUM10                                                            
         L     RE,0(R1)                                                         
         USING BINRECD,RE                                                       
         AP    BCUTCOST,PAKWRK1                                                 
         DROP  RE                                                               
SUM10    EQU   *                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING BINRECD,R4                                                       
         MVI   BREC2,2                  REC=2 BY NETWORK/DAYPART                
         MVC   B2NET,NBACTNET           NETWORK                                 
         MVC   B2NET+4(1),NBSTATYP           NETWORK                            
         MVC   B2DPT,NBACTNDP           DAYPART                                 
         MVC   B2DPTNM(8),NBDPNAM       DPT NAME                                
         MVC   B2DPTNM+7(6),NBDPNAM2    DAYPART NAME CONTINUATION               
         ZAP   BCUTCOST,PAKWRK1                                                 
         ZAP   BUNTCOST,=P'0'                                                   
         DROP  R4                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R4))                                     
         CLI   0(R1),X'01'         X'01'=REC FOUND                              
         BE    SUMX                                                             
         L     RE,0(R1)                                                         
         USING BINRECD,RE                                                       
         AP    BCUTCOST,PAKWRK1                                                 
SUMX     XIT1                                                                   
         DROP  RE                                                               
                                                                                
*                                                                               
FILLIST  DC    CL8'SSTAFILE'                                                    
         DC    C'X'                                                             
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
GETCML   NTR1                                                                   
         XC    WORK,WORK                                                        
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETCMLXX                                                         
         USING NUCMLEL,R2                                                       
         CLC   NBPRD,BYTE          1ST COMMERCIAL?                              
         BE    GETCML10                                                         
         CLI   NBPRDLST,0          MULTI PRODS?                                 
         BE    GETCML20                                                         
         CLC   BYTE,NUCMLPRD       COPY SPLIT PROD?                             
         BNE   GETCML20                                                         
GETCML10 MVC   WORK(8),NUCML1                                                   
         B     GETCML80                                                         
GETCML20 CLC   NBPRD2,BYTE         2ND COMMERCIAL                               
         BNE   GETCML40            NO MATCH                                     
         MVC   WORK(8),NUCML2                                                   
         B     GETCML80                                                         
GETCML40 MVI   ELCODE,X'23'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GETCMLXX                                                         
         USING NUFDCEL,R2                                                       
         CLC   BYTE,NUFDCPRD                                                    
         BNE   GETCML40                                                         
         MVC   WORK(8),NUFDCML1                                                 
GETCML80 BAS   RE,GTCMLNM                                                       
         B     GETCMLXX                                                         
GETCMLXX B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* EXPECTS COMMERCIAL CODE IN WORK                                               
GTCMLNM  NTR1                                                                   
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),NBACTAM                                                
         MVC   CMLKCML,WORK        COMMERCIAL CODE                              
         MVC   CMLKSV,CMLKID                                                    
         NETGO NVSETSPT                                                         
         MVC   FILENAME,=C'TRFDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   CMLKSV,CMLKID                                                    
         BE    GTCMLNM5                                                         
         MVC   WORK+8(16),=CL16'***** NOT FOUND *****'                          
         B     GTCMLNMX                                                         
GTCMLNM5 MVC   FILENAME,=C'TRFFILE '                                            
         LA    R2,MYIO                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         USING CMLDSCEL,R2                                                      
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   GTCMLNMX                                                         
         MVC   WORK+8(16),CMLDSC                                                
GTCMLNMX NETGO NVSETUNT                                                         
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         MVC   AIO,NBAIO                                                        
         B     XIT                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
*  R3 POINTS TO OUTPUT AREA, WORK HAS 1 BYTE PROD CODE                          
*                                                                               
GETPRD3  NTR1                                                                   
         L     R2,ACLIST                                                        
PRD2     CLI   0(R2),0                                                          
         BNE   PRD4                                                             
         MVC   WORK(3),=C'UNA'     UNDEFINED                                    
         B     PRDX                                                             
PRD4     CLC   3(1,R2),BYTE                                                     
         BE    *+12                                                             
         LA    R2,4(R2)                                                         
         B     PRD2                                                             
         MVC   WORK(3),0(R2)       ***PRODUCT                                   
PRDX     B     XIT                                                              
*                                                                               
*********************************                                               
*                                                                               
         EJECT                                                                  
*                                                                               
SUBS     NTR1                  ** SUB TOTALS **                                 
*                                                                               
SUBX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
TOTS     NTR1                      GRANDTOTS                                    
         BAS   RE,WRITIT                                                        
         B     XIT                                                              
                                                                                
*                                                                               
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
*                                                                               
HOOK     NTR1                                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         MVC   H4(6),=C'CLIENT'                                                 
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H4+15(20),SPLCLIN                                                
****     MVC   H5(7),=C'PRODUCT'                                                
****     MVC   H5+10(6),SPLPRO                                                  
****     MVC   H5+18(20),SPLPRON                                                
****     CLC   SPLPRO(3),=C'ALL'                                                
****     BNE   *+16                                                             
****     MVC   H5+10(3),PRDSV                                                   
****     XC    H5+18(20),H5+18                                                  
         MVC   H5(7),=C'DAYPART'                                                
         CLI   NBSELNDP+1,X'40'                                                 
         BH    *+14                                                             
         MVC   H5+10(3),=C'ALL'                                                 
         B     *+10                                                             
         MVC   H5+10(2),NBSELNDP                                                
         DROP  R5                                                               
*                                                                               
*                                                                               
         CLI   QTITLE,C'Y'                                                      
         BNE   *+14                                                             
         MVC   WORK(40),TITLE                                                   
         B     *+10                                                             
         MVC   WORK(40),=CL40'CUTIN/SECTIONAL DETAIL REPORT'                    
         GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+43(40),WORK                                                
*                                                                               
HK1      DS    0H                                                               
*                                                                               
         CLI   TOTRTN,C'Y'         IF DOING TOTALS                              
         BE    XIT                 THAT'S ALL                                   
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PUNDATE(3),=C'AIR'                                               
         MVC   PUNDATE+132(4),=C'DATE'                                          
***      MVC   PUNDATE+132(8),=C'VEND/DAY'                                      
         MVC   PVEND+132(4),=C'VEND'                                            
         MVC   PDAY+132(3),=C'DAY'                                              
*                                                                               
         MVC   PEST+132(3),=C'EST'                                              
*                                                                               
*                                                                               
         MVC   PSTEND(9),=C'START/END'                                          
         MVC   PSTEND+134(4),=C'TIME'                                           
*                                                                               
         MVC   PPROGRAM(8),=C'PROGRAM/'                                         
         MVC   PPROGRAM+132(9),=C'GEOGRAPHY'                                    
*                                                                               
         MVC   PAFFSTA+132,=C'AFFL'                                             
*                                                                               
         MVC   PPROD+132(3),=C'BRD'                                             
*                                                                               
         MVC   PCOMMLCD(5),=C'COMML'                                            
         MVC   PCOMMLCD+132(4),=C'CODE'                                         
*                                                                               
         MVC   PCOMMLNM+3(10),=C'COMMERCIAL'                                    
         MVC   PCOMMLNM+138(4),=C'NAME'                                         
*                                                                               
         MVC   PUNLEN+132,=C'LEN'                                               
*                                                                               
         MVC   PCOST+4(4),=C'UNIT'                                              
         MVC   PCOST+136(4),=C'COST'                                            
*                                                                               
         MVC   PCUCOST,=C'FEED/CUTIN'                                           
         MVC   PCUCOST+136(4),=C'COST'                                          
*                                                                               
         MVC   PTOTCST+3(5),=C'TOTAL'                                           
         MVC   PTOTCST+136(4),=C'COST'                                          
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         SPACE                                                                  
WRITIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         GETEL  (R2),DATADISP,ELCODE                                            
                                                                                
OPENSTA  NTR1                                                                   
*   - READ ACCESS REC TO FIND SE NUMBER - PASS IN BYTE                          
         LA    R4,KEY                                                           
         USING CT5REC,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CT5KTYP,C'5'                                                     
         MVC   CT5KALPH,NBSELAGY  SET AGENCY ALPHA                              
         LA    R4,MYIO          AND USE MY OEN                                  
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         GOTO1 HIGH                                                             
         LA    R4,CT5DATA                                                       
         SR    R0,R0                                                            
OPENC    CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'21'                                                      
         BNE   *+12                                                             
         CLI   2(R4),X'02'                                                      
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     OPENC                                                            
         DROP  R4                                                               
*                                                                               
         MVC   SPOTSE,3(R4)          MOVE SPOT SYS NUMBER                       
         MVI   USEIO,C'N'                                                       
         MVC   AIO,NBAIO           RESET AIO                                    
* - GET UTL                                                                     
         L     R3,ATWA                                                          
         L     R3,TWAMASTC-TWATASK(R3)      POINT TO PASTC                      
         L     R3,MCUTL-MCBLOCK(R3)         POINT TO UTL                        
         ST    R3,NBUTL                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   NETSE,4(R3)         SAVE NET SE NUMBER                           
         MVC   4(1,R3),SPOTSE      SET SPOT SE                                  
         LA    R5,FILLIST                                                       
         GOTO1 NBDM,DMCB,=C'OPEN',=CL8'SPOT',(R5),MYIO                          
REP70X   XIT1                                                                   
         EJECT                                                                  
               LTORG                                                            
         SPACE 3                                                                
MYIO     DS    CL4000                                                           
*                                                                               
BINTABLE DS    CL19000                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
MYWORKD  DSECT                                                                  
QTITLE   DS    CL1                 *** FROM EDIT                                
TRAFPRD  DS    CL1                 *** FROM EDIT                                
TITLE    DS    CL40                ***                                          
ACLIST   DS    F                   ***                                          
*                                                                               
RELO     DS    F                                                                
DATPOINT DS    F                                                                
PRDPOINT DS    F                                                                
BINDMCB  DS    6F                                                               
PRDCNT   DS    F                                                                
PRDNUM   DS    F                                                                
UNACTUAL DS    F                                                                
CUTFLG   DS    CL1                                                              
SPOTSE   DS    CL1                                                              
NETSE    DS    CL1                                                              
CMLKSV   DS    CL13                                                             
PRDSV    DS    CL3                                                              
PRDSV2   DS    CL3                                                              
PPNAM    DS    CL1                                                              
MYWORK   DS    CL40                                                             
CUTINTBL DS    CL281               35X8 COMMERCIAL NUMBER                       
PAKWRK1  DS    PL8                                                              
PAKWRK2  DS    PL8                                                              
PAKWRK3  DS    PL8                                                              
PAKSEC   DS    PL8                                                              
TOTRTN   DS    CL1                                                              
MKTSAV   DS    CL4                                                              
PREVPROG DS    CL6                                                              
*                                                                               
                                                                                
*****---->     SORT KEY FOR UNIT RECORD                                         
                                                                                
*****---->  PRINT LINE                                                          
PLINED   DSECT                                                                  
PUNDATE  DS    CL8                 UNIT DATE                                    
         DS    CL1                                                              
PVEND    DS    CL5                 VENDOR                                       
         DS    CL1                                                              
PDAY     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                 ESTIMATE                                     
         DS    CL1                                                              
PSTEND   DS    CL11                START-END TIME                               
         DS    CL1                                                              
PPROGRAM DS    CL18                PROGBOTH                                     
         DS    CL1                                                              
PAFFSTA  DS    CL4                 AFFILLIATED STATION                          
         DS    CL1                                                              
PPROD    DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PCOMMLCD DS    CL8                 COMMERCIAL NAME                              
         DS    CL1                                                              
PCOMMLNM DS    CL16                COMMERCIAL NAME                              
         DS    CL1                                                              
PUNLEN   DS    CL3                 SPOT LENGTH                                  
         DS    CL1                                                              
PCOST    DS    CL12                ACTUAL UNIT COST                             
         DS    CL1                                                              
PCUCOST  DS    CL10                CUTIN COST                                   
         DS    CL1                                                              
PTOTCST  DS    CL12                TOTAL COST                                   
*                                                                               
BINRECD  DSECT                    TOTALS BY PROD/NETWORK                        
BREC1    DS    CL1                 =1                                           
B1PRD    DS    CL3                                                              
         DS    CL18                SPARE                                        
BKEYLENE EQU   *-BREC1                                                          
         ORG   BREC1                                                            
BREC2    DS    CL1                 =2                                           
B2NET    DS    CL5                 NETWORK                                      
B2DPT    DS    CL2                 DAYPART                                      
B2DPTNM  DS    CL14                DAYPART NAME                                 
*                                                                               
BUNTCOST DS    PL8                 UNIT COST                                    
BCUTCOST DS    PL8                 CUTIN/FEED COST                              
BRECLENE EQU   *-BREC1                                                          
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEDD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEWRI95   05/06/04'                                      
         END                                                                    
