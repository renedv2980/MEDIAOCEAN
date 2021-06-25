*          DATA SET SPREPPR02  AT LEVEL 013 AS OF 02/17/09                      
*PHASE SPPR02C                                                                  
*INCLUDE BUFFERIN                                                               
SPPR02   TITLE 'PARAMOUNT PICTURES OVERALL TELEVISION SUMMARY'                  
SPPR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPPR02,R8                                                    
         L     RA,0(R1)                                                         
         LR    RC,RA                                                            
         AHI   RC,4096                                                          
         LR    R9,RC                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,RC,R9                                                 
                                                                                
         CLI   MODE,PROCBUY                                                     
         BE    PROCB                                                            
         CLI   MODE,PROCGOAL                                                    
         BE    PROCG                                                            
         CLI   MODE,STAFRST                                                     
         BE    STAF                                                             
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
         CLI   MODE,MKTLAST                                                     
         BE    MKTL                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST PROCESSING                                            *         
***********************************************************************         
                                                                                
REQF     L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LHI   R0,MAXWEEKS                                                      
         STCM  R0,15,MEDNUMWK                                                   
         LHI   R0,128                                                           
         STCM  R0,15,MEDLCHNK                                                   
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         XC    MEDNUMPE,MEDNUMPE                                                
         MVI   MEDEXTDM,NUMDEMOS                                                
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   RQOPTS,RQOPTS_1DEC   FORCE 1 DECIMAL REPORTING                   
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ESTIMATE FIRST PROCESSING                                           *         
* NOTE THAT MEDDATE WILL CHANGE QSTART TO A MONDAY, SO IT NEEDS TO BE *         
* SAVED BEFORE THAT CALL !                                                      
***********************************************************************         
                                                                                
ESTF     GOTOR MEDPRDRD,DMCB,SPWORKD                                            
                                                                                
         MVC   SVQSTART,QSTART                                                  
                                                                                
         GOTOR MEDDATE,DMCB,SPWORKD                                             
         GOTOR GETDAY,DMCB,SVQSTART,FULL  GET START DATE DAY                    
                                                                                
         XC    WEEKTAB(WEEKTABL),WEEKTAB                                        
         LA    R4,WEEKTAB                                                       
         CLI   0(R1),1             TEST MONDAY START                            
         BNE   *+8                 NO                                           
         AHI   R4,L'WEEKTAB        IF MONDAY START, NO STUNT WEEK               
                                                                                
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVC   MEDEXTAX,SPOTPROF+12                                             
                                                                                
         L     RF,MEDAFRST                                                      
         MVC   BQSTARTP,0(RF)                                                   
ESTF02   MVC   0(L'WEEKTAB,R4),0(RF) SET START/END DATES                        
         AHI   R4,L'WEEKTAB                                                     
         AHI   RF,L'MEDDATES                                                    
         C     RF,MEDALAST                                                      
         BNH   ESTF02                                                           
         L     RF,MEDALAST                                                      
         MVC   BQENDP,2(RF)                                                     
         DROP  R3                                                               
                                                                                
         L     RE,ADEST                                                         
         MVC   SVDEMOS(SVDEMOSL),EDEMLST-ESTHDRD(RE)                            
                                                                                
         LA    R0,2                SET DEMO RERATE TYPE                         
         CLI   QRERATE,C' '                                                     
         BE    ESTF04                                                           
         LA    R0,5                                                             
         CLI   QRERATE,C'A'        ADJUST ONLY                                  
         BE    ESTF04                                                           
                                                                                
         LA    R0,3                SET FOR PURCHASED RERATED                    
         CLC   =C'NO',QHUT1                                                     
         BE    *+8                                                              
         AHI   R0,1                SET FOR ADJUSTMENT                           
         CLI   QRERATE,C'I'        RERATE BASED ON INVOICE                      
         BNE   *+8                                                              
         AHI   R0,3                                                             
                                                                                
ESTF04   ST    R0,SVRERATE                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MARKET FIRST                                                        *         
***********************************************************************         
                                                                                
MKTF     GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
                                                                                
         MVI   COMLINEN,0          SET NO COMMENTS FOUND                        
         MVI   COMLINEP,0          SET NO COMMENTS PRINTED                      
         L     R2,ADCOMREC                                                      
         USING COMRECD,R2          BUILD KEY FOR COMMENT RECORD LOOK-UP         
         XC    COMKEY,COMKEY                                                    
         MVI   BCMTYPE,C'P'                                                     
         MVC   BCMCLT,BCLT                                                      
         XC    BCMPGR,BCMPGR                                                    
         MVC   BCMPRD,BPRD                                                      
         MVC   BCMEST,BEST                                                      
         MVC   BCMMKT,BMKT                                                      
         XC    BCMSTA,BCMSTA                                                    
         GOTOR GETCOM                                                           
         OC    COMKEY,COMKEY       TEST COMMENT RECORD FOUND                    
         BZ    MKTFX                                                            
                                                                                
         LA    R0,COMLINES         CLEAR COMMENT BLOCK TO SPACES                
         LHI   R1,COMLINEL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE                                                            
                                                                                
         LA    RE,COMLINEM         BUILD COMMENT BLOCK FROM RECORD              
         LA    RF,COMLINES                                                      
         LA    R2,COMEL                                                         
         SR    R0,R0                                                            
MKTF02   CLI   0(R2),0                                                          
         BE    MKTFX                                                            
         CLI   0(R2),X'05'                                                      
         BE    *+12                                                             
         CLI   0(R2),X'15'                                                      
         BNE   MKTF04                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),2(R2)                                                    
         AHI   RF,L'COMLINES                                                    
         BCT   RE,*+8                                                           
         B     MKTFX                                                            
         IC    R0,COMLINEN         BUMP N'COMMENT LINES TO PRINT                
         AHI   R0,1                                                             
         STC   R0,COMLINEN                                                      
MKTF04   IC    R0,1(R2)            BUMP TO NEXT ELEMENT ON RECORD               
         AR    R2,R0                                                            
         B     MKTF02                                                           
         DROP  R2                                                               
                                                                                
MKTFX    B     EXIT                                                             
                                                                                
***********************************************************************         
* STATION FIRST                                                       *         
***********************************************************************         
                                                                                
STAF     L     RE,ADSTAT                                                        
         MVC   SVAFFL,SNETWRK-STARECD(RE)                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUY RECORD PROCESSING                                               *         
***********************************************************************         
                                                                                
PROCB    BAS   RE,FIXCOVRD                                                      
         XC    PSLIST,PSLIST                                                    
         GOTOR MEDPSL,DMCB,SPWORKD,PSLIST                                       
                                                                                
         LA    R2,PSLIST                                                        
PROCB02  CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLC   0(1,R2),BPRD                                                     
         BNE   PROCB06                                                          
                                                                                
PROCB04  L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         GOTOR MEDGETBY,DMCB,SPWORKD,SVRERATE                                   
                                                                                
         GOTOR BUYPUT              POST BUY VALUES TO BUFFERIN                  
                                                                                
PROCB06  LA    R2,2(R2)                                                         
         B     PROCB02                                                          
                                                                                
*====================================================================*          
* SCAN BUY RECORD FOR COST OVERRIDES THAT APPLY ONLY TO COST1/COST2  *          
* SINCE THIS PROGRAM DOES **NOT** WRITE RECORDS BACK TO THE FILE     *          
* WE WILL SIMPLY RESET THE COST OVERRIDE INDICATOR AS REQUIRED       *          
*====================================================================*          
FIXCOVRD NTR1                                                                   
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
FIXCOV2  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    FIXCOVX                                                          
         CLI   0(R6),X'0B'                                                      
         BL    FIXCOV2                                                          
         CLI   0(R6),X'0C'                                                      
         BH    FIXCOV2                                                          
         TM    6(R6),X'20'                                                      
         BZ    FIXCOV2                                                          
*                                                                               
         LR    R7,R6                                                            
FIXCOV4  IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'10'                                                      
         BL    FIXCOV2                                                          
         CLI   0(R7),X'13'                                                      
         BL    FIXCOV4                                                          
         BH    FIXCOV2                                                          
* X'13' ELEMENT FOUND                                                           
         TM    2(R7),X'40'         TEST COST2 OVERRIDE ONLY                     
         BZ    FIXCOV2             NO                                           
         CLI   QCOST2,C'Y'         TEST CLIENT VERSION                          
         BE    FIXCOV2             YES - KEEP OVERRIDE                          
         NI    6(R6),X'DF'         UNSET OVERRIDE IN ORIGINAL ELEMENT           
         XC    7(3,R6),7(R6)       AND CLEAR AMOUNT                             
         B     FIXCOV2                                                          
*                                                                               
FIXCOVX  B     EXIT                                                             
                                                                                
***********************************************************************         
* BUY RECORD POST ROUTINES                                            *         
***********************************************************************         
                                                                                
BUYPUT   NTR1  ,                                                                
         L     R5,MEDAFRST                                                      
                                                                                
BUYPUT02 CLI   0(R5),0             IF NO DATE, SKIP ENTRY                       
         BE    BUYPUT10                                                         
                                                                                
         L     R4,4(R5)            POINT TO DOLLARS                             
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    BUYPUT08                                                         
                                                                                
         GOTOR GETWKN              SET WEEKNUM VALUE                            
                                                                                
***********************************************************************         
* REPORT 1 - DEMO TOTALS                                              *         
***********************************************************************         
                                                                                
BUYPUT04 XC    BFREC(BFRECL),BFREC                                              
         MVI   BFTYPE,BFK1Q        TYPE 1 - REPORT TOTALS                       
         MVC   BFBDOL,MEDBYD                                                    
                                                                                
         LA    R6,SVDEMOS          DEMO LIST                                    
         LA    R7,MEDBY1           DEMO VALUES                                  
         LA    R0,NUMDEMOS                                                      
                                                                                
BUYPUT06 OC    0(L'SVDEMOS,R6),0(R6)                                            
         BZ    BUYPUT08            NO DEMO - DONE                               
         SR    RE,RE               SET DEMO POSITION NUMBER                     
         IC    RE,BFK1POS                                                       
         AHI   RE,1                                                             
         STC   RE,BFK1POS                                                       
         MVC   BFBDEM,0(R7)        POINTS                                       
         GOTOR PUTBUF              PUT DEMO TOTAL RECORD                        
                                                                                
         AHI   R6,L'SVDEMOS                                                     
         AHI   R7,L'MEDBY1+L'MEDBY1EQ                                           
         BCT   R0,BUYPUT06                                                      
                                                                                
***********************************************************************         
* REPORTS 2-4                                                         *         
***********************************************************************         
                                                                                
BUYPUT08 BAS   RE,GETEQSLN                                                      
*                                                                               
         XC    BFREC(BFRECL),BFREC                                              
         MVI   BFTYPE,BFK2Q        TYPE 2 - FLT/SLN SUMMARY                     
         MVC   BFK2SLN,EQSLN       SLN FROM MEDPSL ENTRY                        
         MVC   BFK2WEEK,WEEKNUM    START/END DATES                              
                                                                                
         MVC   BFSPOTS,MEDBYSPT                                                 
         MVC   BFBDOL,MEDBYD                                                    
         MVC   BFBEQDOL,MEDBYDEQ                                                
         MVC   BFBDEM,MEDBY1                                                    
         MVC   BFBEQDEM,MEDBY1EQ                                                
         GOTOR PUTBUF              PUT FLIGHT/SPOT LENGTH RECORD                
                                                                                
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFK3Q                                                     
                                                                                
         GOTOR PUTBUF              PUT ALL DAYPARTS ALL SPOT LENGTHS            
                                                                                
         MVC   BFK3SLN,EQSLN                                                    
         GOTOR PUTBUF              PUT ALL DAYPARTS BY SPOT LENGTH              
                                                                                
         MVC   BFK3DPT,MEDDPART                                                 
         GOTOR PUTBUF              PUT DAYPART BY SPOT LENGTH                   
                                                                                
         MVI   BFK3SLN,0                                                        
         GOTOR PUTBUF              PUT DAYPART ALL SPOT LENGTHS                 
                                                                                
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFK4Q                                                     
         GOTOR PUTBUF              PUT ALL STATIONS/AFFILIATES                  
                                                                                
         MVC   BFK4STA,STAPRINT                                                 
         MVC   BFK4AFF,SVAFFL                                                   
         GOTOR PUTBUF              PUT STATION/AFFILIATE                        
                                                                                
BUYPUT10 AHI   R5,L'MEDDATES       NEXT WEEK                                    
                                                                                
         C     R5,MEDALAST                                                      
         BH    EXIT                                                             
         LA    R0,MEDMON01                                                      
         CR    R5,R0                                                            
         BL    BUYPUT02                                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
GETEQSLN NTR1                                                                   
         MVI   BYTE,C'R'           FIND EQTAB                                   
         CLI   MED,C'R'                                                         
         BE    GETEQ2                                                           
         CLI   MED,C'X'                                                         
         BE    GETEQ2                                                           
         MVI   BYTE,C'T'                                                        
         CLI   MED,C'T'                                                         
         BE    GETEQ2                                                           
         CLI   MED,C'N'                                                         
         BE    GETEQ2                                                           
         CLI   MED,C'C'                                                         
         BE    GETEQ2                                                           
         DC    H'0'                                                             
*                                                                               
GETEQ2   L     R1,VSLNTAB                                                       
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            DSPL TO EOT                                  
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
GETEQ4   CLC   0(2,R1),=C'00'      TEST DEFAULT ENTRY                           
         BE    GETEQ10                                                          
         CLC   0(2,R1),AGY                                                      
         BNE   *+14                                                             
GETEQ6   CLC   2(1,R1),BYTE                                                     
         BE    GETEQ10                                                          
         BXLE  R1,RE,GETEQ4                                                     
         DC    H'0'                                                             
*                                                                               
GETEQ10  SR    RE,RE                                                            
         IC    RE,1(R2)            GET ACTUAL SLN                               
         AR    RE,RE                                                            
         LA    RE,4(R1,RE)         POINT TO TABLE ENTRY (HDR=4)                 
         MVC   EQSLN,1(RE)         AND USE THE LENGTH FOR EQUIV                 
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GOAL RECORD PROCESSING                                              *         
***********************************************************************         
                                                                                
         USING GOALREC,KEY                                                      
PROCG    L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         GOTOR MEDGETGL,DMCB,SPWORKD                                            
         GOTOR GOLPUT              POST GOAL DATA                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GOAL RECORD POSTING ROUTINES                                        *         
***********************************************************************         
                                                                                
GOLPUT   NTR1  ,                                                                
         L     R5,MEDAFRST                                                      
                                                                                
GOLPUT02 CLI   0(R5),0                                                          
         BE    GOLPUT04                                                         
                                                                                
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
                                                                                
         OC    MEDGLD(12),MEDGLD                                                
         BZ    GOLPUT04                                                         
                                                                                
         GOTOR GETWKN              SET WEEKNUM VALUE                            
                                                                                
***********************************************************************         
* REPORT 1 - DEMO TOTALS                                              *         
***********************************************************************         
                                                                                
         XC    BFREC(BFRECL),BFREC                                              
         MVI   BFTYPE,BFK1Q        TYPE 1 - REPORT TOTALS                       
         MVI   BFK1POS,1           SET DEMO POSITION 1                          
                                                                                
         MVC   BFGDOL,MEDGLD                                                    
         MVC   BFGEQDOL,MEDGLDEQ                                                
         MVC   BFGDEM,MEDGL1                                                    
         MVC   BFGEQDEM,MEDGL1EQ                                                
                                                                                
         GOTOR PUTBUF              PUT DEMO TOTALS RECORD                       
                                                                                
***********************************************************************         
* REPORT 3 - DAYPART/SPOT LENGTH SUMMARY                              *         
***********************************************************************         
                                                                                
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFK3Q        TYPE 3 - DPT/SLN SUMMARY                     
                                                                                
         GOTOR PUTBUF              PUT ALL DAYPARTS ALL SPOT LENGTHS            
                                                                                
         L     RE,ADGOAL                                                        
         MVC   BFK3SLN,GKEYSLN-GOALRECD(RE)                                     
         GOTOR PUTBUF              PUT ALL DAYPARTS BY SPOT LENGTH              
                                                                                
         MVC   BFK3DPT,MEDDPART                                                 
         GOTOR PUTBUF              PUT DAYPART BY SPOT LENGTH                   
                                                                                
         MVI   BFK3SLN,0           PUT DAYPART ALL SPOT LENGTHS                 
         GOTOR PUTBUF                                                           
                                                                                
GOLPUT04 AHI   R5,L'MEDDATES       NEXT WEEK                                    
                                                                                
         C     R5,MEDALAST                                                      
         BH    EXIT                                                             
         LA    R0,MEDMON01                                                      
         CR    R5,R0                                                            
         BL    GOLPUT02                                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* MATCH MEDWEEK ENTRY TO WEEKTAB - RETURNS WEEKNUM (1-5)              *         
***********************************************************************         
                                                                                
GETWKN   LA    R0,MAXWEEKS                                                      
         LA    R1,WEEKTAB                                                       
                                                                                
GETWKN02 CLC   2(2,R1),2(R5)       MATCH WEEK END DATE                          
         BE    GETWKN04                                                         
         AHI   R1,L'WEEKTAB                                                     
         BCT   R0,GETWKN02                                                      
         GOTOR REPORT                                                           
         MVC   P(33),=C'**Error** Request period too long'                      
         GOTOR REPORT                                                           
         GOTOR AENDREQ                                                          
                                                                                
GETWKN04 LHI   R1,MAXWEEKS+1                                                    
         SR    R1,R0               GIVES WEEK NUMBER                            
         STC   R1,WEEKNUM                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR MARKET - PRINT THE REPORT                                  *         
***********************************************************************         
                                                                                
MKTL     L     R2,ADBLOCK                                                       
         USING DBLOCKD,R2          GET ESTIMATE DEMO NAMES                      
         MVC   DBSELMED,QMED       SET MEDIA CODE IN DBLOCK                     
         L     R3,ADEST                                                         
         USING ESTHDRD,R3                                                       
         LA    RE,SVDEMOS                                                       
         LA    RF,NUMDEMOS                                                      
         SR    R0,R0                                                            
MKTL02   OC    0(L'SVDEMOS,RE),0(RE)                                            
         BZ    MKTL04                                                           
         AHI   RE,L'SVDEMOS                                                     
         AHI   R0,1                                                             
         BCT   RF,MKTL02                                                        
MKTL04   GOTOR DEMOCON,DMCB,((R0),EDEMLST),(13,SVDEMOSN),              *        
               (C'S',DBLOCKD),(SPOTPROF+9,EUSRNMS)                              
         DROP  R2,R3                                                            
                                                                                
         XC    DEMVALS(DEMVALSL),DEMVALS                                        
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFK1Q                                                     
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         B     MKTL08                                                           
                                                                                
MKTL06   GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
                                                                                
MKTL08   BE    *+8                                                              
         MVI   BFTYPE,BFKEOFQ                                                   
         CLI   BFTYPE,BFK1Q                                                     
         BE    *+8                                                              
         MVI   BFTYPE,BFKEOFQ                                                   
                                                                                
         CLI   BFTYPE,BFK1Q        TEST REPORT TOTALS VALUE                     
         BNE   MKTL12                                                           
         CLI   BFK1POS,1           TEST PRIMARY DEMO                            
         BNE   MKTL10              NO                                           
         MVC   DGDOL,BFGDOL        EXTRACT GOAL DOLLARS                         
         MVC   DBDOL,BFBDOL        EXTRACT ACTUAL DOLLARS                       
         MVC   D1GDEM,BFGDEM       EXTRACT GOAL DEMOS                           
                                                                                
MKTL10   SR    R1,R1               EXTRACT ACTUAL DEMO VALUE                    
         IC    R1,BFK1POS                                                       
         SLL   R1,2                                                             
         LA    R1,D1BDEM-L'D1BDEM(R1)                                           
         MVC   0(L'D1BDEM,R1),BFBDEM                                            
         B     MKTL06                                                           
                                                                                
MKTL12   LA    RE,BLDHED           SET A(HEADLINE BUILDING ROUTINE)             
         ST    RE,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
                                                                                
         GOTOR SLNSUM              PRINT SPOT LENGTH SUMMARY                    
         GOTOR DPTSUM              PRINT DAYPART SUMMARY                        
         GOTOR STASUM              PRINT STATION/AFFILIATE SUMMARY              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD HEADLINES                                          *         
***********************************************************************         
                                                                                
BLDHED   ST    RE,SAVERE                                                        
         MVC   HEAD4+9(L'QMED),QMED                                             
         EDIT  D1GDEM,(6,HEAD3+126),1,ZERO=BLANK                                
         OC    DGDOL,DGDOL                                                      
         BZ    BLDHED02                                                         
         EDIT  DGDOL,(11,HEAD4+121),2,FLOAT=$                                   
BLDHED02 EDIT  D1BDEM,(6,HEAD5+126),1,ZERO=BLANK                                
         OC    DBDOL,DBDOL                                                      
         BZ    BLDHED04                                                         
         EDIT  DBDOL,(11,HEAD6+121),2,FLOAT=$                                   
BLDHED04 ICM   RE,15,D1GDEM                                                     
         BZ    BLDHED06                                                         
         L     R1,D1BDEM                                                        
         M     R0,F1000                                                         
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,HEAD7+126),1                                             
BLDHED06 ICM   RE,15,DGDOL                                                      
         BZ    BLDHED08                                                         
         L     R1,DBDOL                                                         
         M     R0,F1000                                                         
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,HEAD8+126),1                                             
BLDHED08 LA    R2,HEAD5+49                                                      
         LA    R3,SVDEMOS                                                       
         LA    R4,SVDEMOSN                                                      
         LA    R5,DTITLES                                                       
         LA    R6,D1BDEM                                                        
         LA    R7,NUMDEMOS                                                      
BLDHED10 OC    0(L'SVDEMOS,R3),0(R3)                                            
         BZ    BLDHED14                                                         
         MVC   0(L'DTITLES,R2),0(R5)                                            
         MVC   9(L'SVDEMOSN,R2),0(R4)                                           
         ICM   R1,15,0(R6)                                                      
         BZ    BLDHED12                                                         
         EDIT  (R1),(6,20(R2)),1                                                
BLDHED12 AHI   R2,L'HEAD5                                                       
         AHI   R3,L'SVDEMOS                                                     
         AHI   R4,L'SVDEMOSN                                                    
         AHI   R5,L'DTITLES                                                     
         AHI   R6,L'D1BDEM                                                      
         BCT   R7,BLDHED10                                                      
BLDHED14 MVC   WORK(30),HEAD8+7    TIDY-UP MARKET                               
         MVI   HEAD8+7,C' '                                                     
         MVC   HEAD8+8(30),WORK                                                 
         CLC   QOPT2(2),SPACES     TEST REVISION NUMBER GIVEN                   
         BE    BLDHEDX                                                          
         MVC   0(L'REVLIT,R2),REVLIT                                            
         MVC   L'REVLIT+1(2,R2),QOPT2                                           
         CLI   QOPT2,C' '                                                       
         BE    *+12                                                             
         CLI   QOPT2,C'0'                                                       
         BNE   BLDHEDX                                                          
         MVC   L'REVLIT+1(1,R2),QOPT3                                           
         MVI   L'REVLIT+2(R2),C' '                                              
BLDHEDX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT SPOT LENGTH SUMMARY                                *         
***********************************************************************         
                                                                                
SLNSUM   NTR1  ,                                                                
         XC    BFKEYLST,BFKEYLST   CLEAR VALUES                                 
         XC    REPTOTS(REPTOTSL),REPTOTS                                        
                                                                                
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFK2Q                                                     
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         B     SLNSUM04                                                         
                                                                                
SLNSUM02 GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
                                                                                
SLNSUM04 BE    *+8                                                              
         MVI   BFTYPE,BFKEOFQ                                                   
         CLI   BFTYPE,BFK2Q                                                     
         BE    *+14                                                             
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFKEOFQ                                                   
                                                                                
         OC    BFKEYLST,BFKEYLST   TEST FIRST TIME                              
         BNZ   SLNSUM10                                                         
         CLI   BFTYPE,BFKEOFQ      TEST NO RECORDS FOUND                        
         BE    SLNSUMX                                                          
                                                                                
         L     R2,AR2BOX01         POINT TO REPORT BOX DEFINITIONS              
         MVC   P(R2LL),0(R2)                                                    
         GOTOR REPORT              PRINT TOP OF REPORT BOX                      
         AHI   R2,R2LL                                                          
                                                                                
         MVC   P1(R2LL),0(R2)      BUILD FLIGHT DATE TITLE LINE                 
         LA    R3,WEEKTAB+4        IGNORE STUNT WEEK                            
         LA    R4,R2LFL1                                                        
         LA    R0,MAXWEEKS-1                                                    
SLNSUM06 OC    0(4,R3),0(R3)       TEST END OF TABLE                            
         BZ    SLNSUM08                                                         
         GOTOR DATCON,DMCB,(X'42',(R3)),(8,1(R4))                               
         MVI   9(R4),C'-'                                                       
         GOTOR (RF),(R1),(X'42',2(R3)),(8,10(R4))                               
SLNSUM08 AHI   R3,L'WEEKTAB        BUMP TO NEXT WEEK IN LIST                    
         AHI   R4,L'R2LST1+L'R2LC01                                             
         BCT   R0,SLNSUM06                                                      
         GOTOR REPORT              PRINT FLIGHT DATE LINE                       
         AHI   R2,R2LL                                                          
                                                                                
         L     RF,REPORT           PRINT REMAINDER OF BOX HEADLINES             
         LA    R0,R2BOXHED                                                      
         MVC   P(R2LL),0(R2)                                                    
         GOTOR (RF)                                                             
         AHI   R2,R2LL                                                          
         BCT   R0,*-12                                                          
                                                                                
SLNSUM10 CLC   BFK2SLN,BFKEYLST+(BFK2SLN-BFKEY)                                 
         BE    SLNSUM16                                                         
         CLI   BFKEYLST+(BFK2SLN-BFKEY),0                                       
         BE    SLNSUM14                                                         
         OC    LINTOT01,LINTOT01                                                
         BZ    SLNSUM12                                                         
         EDIT  LINTOT01,(11,R2LFLTD),2,FLOAT=$                                  
                                                                                
SLNSUM12 EDIT  LINTOT02,(6,R2LFLTR),1,ZERO=BLANK                                
         GOTOR REPORT                                                           
                                                                                
SLNSUM14 CLI   BFTYPE,BFKEOFQ      TEST EOF ENCOUNTERED                         
         BE    SLNSUM20                                                         
         MVC   P1(R2LL),0(R2)                                                   
         XC    LINTOTS(LINTOTSL),LINTOTS                                        
         EDIT  BFK2SLN,(3,R2LTIT),ALIGN=LEFT                                    
         LA    R1,R2LTIT                                                        
         AR    R1,R0                                                            
         MVC   1(3,R1),=C'sec'                                                  
                                                                                
SLNSUM16 SR    R3,R3                                                            
         ICM   R3,1,BFK2WEEK                                                    
         MHI   R3,L'R2LST1+L'R2LC01                                             
         LA    R3,R2LST1-(L'R2LST1+L'R2LC01)(R3)                                
                                                                                
         OC    BFBDOL,BFBDOL                                                    
         BZ    SLNSUM18                                                         
         EDIT  BFBDOL,(11,0(R3)),2,FLOAT=$                                      
                                                                                
SLNSUM18 EDIT  BFBDEM,(6,12(R3)),1,ZERO=BLANK                                   
                                                                                
         ICM   R1,15,BFBDOL        ACCUMULATE ALL FLT DOLLARS                   
         A     R1,LINTOT01                                                      
         ST    R1,LINTOT01                                                      
         ICM   R1,15,BFBDEM        ACCUMULATE ALL FLT TRPS                      
         A     R1,LINTOT02                                                      
         ST    R1,LINTOT02                                                      
                                                                                
         SR    RE,RE                                                            
         IC    RE,BFK2WEEK                                                      
         MHI   RE,L'REPTOT01+L'REPTOT02                                         
         LA    RE,REPTOT01-(L'REPTOT01+L'REPTOT02)(RE)                          
         ICM   R1,15,BFBDOL                                                     
         A     R1,0(RE)                                                         
         ST    R1,0(RE)                                                         
         ICM   R1,15,BFBDEM                                                     
         A     R1,L'REPTOT01(RE)                                                
         ST    R1,L'REPTOT01(RE)                                                
                                                                                
         ICM   R1,15,BFBDOL                                                     
         A     R1,REPTOT11                                                      
         ST    R1,REPTOT11                                                      
         ICM   R1,15,BFBDEM                                                     
         A     R1,REPTOT12                                                      
         ST    R1,REPTOT12                                                      
         MVC   BFKEYLST,BFKEY                                                   
         B     SLNSUM02                                                         
                                                                                
SLNSUM20 AHI   R2,R2LL                                                          
                                                                                
         MVC   P1(R2LL),0(R2)                                                   
         GOTOR REPORT              PRINT BOX AT BOTTOM OF DATA                  
         AHI   R2,R2LL                                                          
                                                                                
         MVC   P1(R2LL),0(R2)      FORMAT TOTALS LINE                           
         LA    R3,R2LST1                                                        
         LA    R4,REPTOT01                                                      
         LA    R5,6                                                             
SLNSUM22 OC    0(L'REPTOT01,R4),0(R4)                                           
         BZ    SLNSUM24                                                         
         EDIT  (B4,0(R4)),(11,0(R3)),2,FLOAT=$                                  
                                                                                
SLNSUM24 EDIT  (B4,4(R4)),(6,12(R3)),1,ZERO=BLANK                               
         AHI   R3,L'R2LST1+L'R2LC01                                             
         AHI   R4,L'REPTOT01+L'REPTOT02                                         
         BCT   R5,SLNSUM22                                                      
         GOTOR REPORT              PRINT TOTALS LINE                            
         AHI   R2,R2LL                                                          
                                                                                
         MVC   P1(R2LL),0(R2)                                                   
         GOTOR REPORT              PRINT BOTTOM OF REPORT BOX                   
                                                                                
SLNSUMX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT DAYPART SUMMARY                                    *         
***********************************************************************         
                                                                                
DPTSUM   NTR1  ,                                                                
         XC    BFKEYLST,BFKEYLST   CLEAR VALUES                                 
         XC    REPTOTS(REPTOTSL),REPTOTS                                        
                                                                                
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFK3Q                                                     
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         B     DPTSUM04                                                         
                                                                                
DPTSUM02 GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
                                                                                
DPTSUM04 BE    *+8                                                              
         MVI   BFTYPE,BFKEOFQ                                                   
         CLI   BFTYPE,BFK3Q                                                     
         BE    *+14                                                             
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFKEOFQ                                                   
                                                                                
         OC    BFKEYLST,BFKEYLST   TEST FIRST TIME                              
         BNZ   DPTSUM06                                                         
         CLI   BFTYPE,BFKEOFQ      TEST ANY RECORDS ADDED                       
         BE    DPTSUMX                                                          
         L     R2,AR3BOX01         POINT TO REPORT BOX DEFINITIONS              
         L     RF,REPORT                                                        
         LA    R0,R3BOXHED         NUMBER OF BOX HEADLINES                      
                                                                                
         MVC   P1(R3LL),0(R2)                                                   
         GOTOR (RF)                                                             
         AHI   R2,R3LL                                                          
         BCT   R0,*-12                                                          
                                                                                
         CLI   QMED,RADMEDQ                                                     
         BNE   *+8                                                              
         AHI   R2,R3LL                                                          
         MVC   P1(R3LL),0(R2)                                                   
         GOTOR (RF)                                                             
         AHI   R2,R3LL                                                          
         CLI   QMED,RADMEDQ                                                     
         BE    *+8                                                              
         AHI   R2,R3LL                                                          
         MVC   P1(R3LL),0(R2)                                                   
         GOTOR (RF)                                                             
         AHI   R2,R3LL                                                          
                                                                                
         XC    LINTOTS(LINTOTSL),LINTOTS                                        
                                                                                
DPTSUM06 CLI   BFTYPE,BFKEOFQ                                                   
         BE    DPTSUM12                                                         
         OC    BFK3DPT,BFK3DPT     TEST ALL DAYPART TOTALS                      
         BNZ   DPTSUM12                                                         
         LA    R1,REPTOT01                                                      
         CLI   BFK3SLN,0           TEST ALL SPOT LENGTH TOTALS                  
         BE    DPTSUM08                                                         
         LA    R1,REPTOT05                                                      
         CLI   QMED,RADMEDQ        TEST RADIO                                   
         BNE   *+16                                                             
         CLI   BFK3SLN,SECS30                                                   
         BE    DPTSUM08                                                         
         B     *+12                                                             
         CLI   BFK3SLN,SECS15      TEST 15 SECOND TOTALS                        
         BE    DPTSUM08                                                         
         LA    R1,REPTOT09                                                      
         CLI   QMED,RADMEDQ        TEST RADIO                                   
         BNE   *+16                                                             
         CLI   BFK3SLN,SECS60                                                   
         BE    DPTSUM08                                                         
         B     DPTSUM10                                                         
         CLI   BFK3SLN,SECS30                                                   
         BNE   DPTSUM10                                                         
DPTSUM08 MVC   L'REPTOT01*0(L'REPTOT01,R1),BFBDEM                               
         MVC   L'REPTOT01*1(L'REPTOT02,R1),BFGDEM                               
         MVC   L'REPTOT01*2(L'REPTOT03,R1),BFBDOL                               
         MVC   L'REPTOT01*3(L'REPTOT04,R1),BFGDOL                               
DPTSUM10 MVC   BFKEYLST,BFKEY                                                   
         B     DPTSUM02                                                         
                                                                                
DPTSUM12 CLC   BFK3DPT,BFKEYLST+(BFK3DPT-BFKEY)                                 
         BE    DPTSUM30                                                         
         CLI   BFKEYLST+(BFK3DPT-BFKEY),0                                       
         BE    DPTSUM30                                                         
                                                                                
         MVC   P1(R3LL),0(R2)                                                   
         MVC   R3LTIT(L'BFK3DPT),BFKEYLST+(BFK3DPT-BFKEY)                       
                                                                                
         EDIT  LINTOT01,(6,R3LT#TB+1),1,ZERO=BLANK                              
         EDIT  LINTOT02,(6,R3LT#TG+1),1,ZERO=BLANK                              
                                                                                
         ICM   RE,15,LINTOT02                                                   
         BZ    DPTSUM14                                                         
         L     R1,LINTOT01                                                      
         M     R0,F1000                                                         
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(7,R3LT#NDX),1                                              
                                                                                
DPTSUM14 ICM   RE,15,REPTOT01                                                   
         BZ    DPTSUM16                                                         
         L     R1,LINTOT01         CALCULATE % OF ACTUAL TRPS                   
         M     R0,F10000                                                        
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,R3LTPTB+1),2,ZERO=BLANK                                  
                                                                                
DPTSUM16 ICM   RE,15,REPTOT02                                                   
         BZ    DPTSUM18                                                         
         L     R1,LINTOT02         CALCULATE % OF GOAL TRPS                     
         M     R0,F10000                                                        
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,R3LTPTG+1),2,ZERO=BLANK                                  
                                                                                
DPTSUM18 ICM   RE,15,LINTOT04      TEST 15 SECOND TRPS NON-ZERO                 
         BZ    DPTSUM20                                                         
         L     R1,LINTOT03                                                      
         M     R0,F10                                                           
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    DPTSUM20                                                         
         EDIT  (R1),(9,R3LC15A),2,FLOAT=$                                       
                                                                                
DPTSUM20 ICM   RE,15,LINTOT06      TEST 15 SECOND TRPS NON-ZERO                 
         BZ    DPTSUM22                                                         
         L     R1,LINTOT05                                                      
         M     R0,F10                                                           
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    DPTSUM22                                                         
         EDIT  (R1),(9,R3LC15G),2,FLOAT=$                                       
                                                                                
DPTSUM22 ICM   RE,15,LINTOT08      TEST 30 SECOND TRPS NON-ZERO                 
         BZ    DPTSUM24                                                         
         L     R1,LINTOT07                                                      
         M     R0,F10                                                           
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    DPTSUM24                                                         
         EDIT  (R1),(9,R3LC30A),2,FLOAT=$                                       
                                                                                
DPTSUM24 ICM   RE,15,LINTOT10      TEST 30 SECOND TRPS NON-ZERO                 
         BZ    DPTSUM26                                                         
         L     R1,LINTOT09                                                      
         M     R0,F10                                                           
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    DPTSUM26                                                         
         EDIT  (R1),(9,R3LC30G),2,FLOAT=$                                       
                                                                                
DPTSUM26 EDIT  LINTOT04,(6,R3L#15),1,ZERO=BLANK                                 
         EDIT  LINTOT08,(6,R3L#30),1,ZERO=BLANK                                 
                                                                                
         ICM   RE,15,LINTOT01      TEST ANY ACHIEVED TRPS                       
         BZ    DPTSUM28                                                         
         L     R1,LINTOT04         CALCULATE 15 SECOND % TRPS                   
         M     R0,F10000                                                        
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,R3LP15),2,ZERO=BLANK                                     
                                                                                
         L     R1,LINTOT08         CALCULATE 30 SECOND % TRPS                   
         M     R0,F10000                                                        
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,R3LP30),2,ZERO=BLANK                                     
                                                                                
DPTSUM28 GOTOR REPORT              PRINT DAYPART LINE                           
         XC    LINTOTS(LINTOTSL),LINTOTS                                        
                                                                                
DPTSUM30 CLI   BFTYPE,BFKEOFQ      TEST ALL DAYPARTS PRINTED                    
         BE    DPTSUM38                                                         
         CLI   BFK3SLN,0           TEST ALL SPOT LENGTHS FOR DAYPART            
         BNE   DPTSUM32                                                         
         MVC   LINTOT01,BFBDEM     YES - SET TOTAL BUY/GOAL TRPS                
         MVC   LINTOT02,BFGDEM                                                  
         B     DPTSUM36                                                         
                                                                                
DPTSUM32 LA    R1,LINTOT03         EXTRACT BUY/GOAL $/TRPS BY LENGTH            
         CLI   QMED,RADMEDQ        TEST RADIO                                   
         BNE   *+16                                                             
         CLI   BFK3SLN,SECS30                                                   
         BE    DPTSUM34                                                         
         B     *+12                                                             
         CLI   BFK3SLN,SECS15                                                   
         BE    DPTSUM34                                                         
         LA    R1,LINTOT07                                                      
         CLI   QMED,RADMEDQ        TEST RADIO                                   
         BNE   *+16                                                             
         CLI   BFK3SLN,SECS60                                                   
         BE    DPTSUM34                                                         
         B     DPTSUM36                                                         
         CLI   BFK3SLN,SECS30                                                   
         BNE   DPTSUM36                                                         
DPTSUM34 MVC   L'LINTOT03*0(L'LINTOT03,R1),BFBDOL                               
         MVC   L'LINTOT03*1(L'LINTOT04,R1),BFBDEM                               
         MVC   L'LINTOT03*2(L'LINTOT05,R1),BFGDOL                               
         MVC   L'LINTOT03*3(L'LINTOT06,R1),BFGDEM                               
DPTSUM36 MVC   BFKEYLST,BFKEY                                                   
         B     DPTSUM02                                                         
                                                                                
DPTSUM38 AHI   R2,R3LL                                                          
         MVC   P1(R3LL),0(R2)                                                   
         GOTOR REPORT              PRINT BOTTOM OF DATA BOX                     
         AHI   R2,R3LL                                                          
         MVC   P1(R3LL),0(R2)                                                   
                                                                                
         EDIT  REPTOT01,(6,R3LT#TB+1),1,ZERO=BLANK                              
         EDIT  REPTOT02,(6,R3LT#TG+1),1,ZERO=BLANK                              
                                                                                
         ICM   RE,15,REPTOT02                                                   
         BZ    DPTSUM40                                                         
         L     R1,REPTOT01                                                      
         M     R0,F1000                                                         
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(7,R3LT#NDX),1                                              
                                                                                
DPTSUM40 ICM   RE,15,REPTOT05      TEST 15 SECOND TRPS NON-ZERO                 
         BZ    DPTSUM42                                                         
         L     R1,REPTOT07                                                      
         M     R0,F10                                                           
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    DPTSUM42                                                         
         EDIT  (R1),(9,R3LC15A),2,FLOAT=$                                       
                                                                                
DPTSUM42 ICM   RE,15,REPTOT06      TEST 15 SECOND TRPS NON-ZERO                 
         BZ    DPTSUM44                                                         
         L     R1,REPTOT08                                                      
         M     R0,F10                                                           
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    DPTSUM44                                                         
         EDIT  (R1),(9,R3LC15G),2,FLOAT=$                                       
                                                                                
DPTSUM44 ICM   RE,15,REPTOT09      TEST 30 SECOND TRPS NON-ZERO                 
         BZ    DPTSUM46                                                         
         L     R1,REPTOT11                                                      
         M     R0,F10                                                           
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    DPTSUM46                                                         
         EDIT  (R1),(9,R3LC30A),2,FLOAT=$                                       
                                                                                
DPTSUM46 ICM   RE,15,REPTOT10      TEST 30 SECOND TRPS NON-ZERO                 
         BZ    DPTSUM48                                                         
         L     R1,REPTOT12                                                      
         M     R0,F10                                                           
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    DPTSUM48                                                         
         EDIT  (R1),(9,R3LC30G),2,FLOAT=$                                       
                                                                                
DPTSUM48 EDIT  REPTOT05,(6,R3L#15),1,ZERO=BLANK                                 
         EDIT  REPTOT09,(6,R3L#30),1,ZERO=BLANK                                 
                                                                                
         ICM   RE,15,REPTOT01      TEST ANY ACHIEVED TRPS                       
         BZ    DPTSUM50                                                         
         L     R1,REPTOT05         CALCULATE 15 SECOND % TRPS                   
         M     R0,F10000                                                        
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,R3LP15),2,ZERO=BLANK                                     
                                                                                
         L     R1,REPTOT09         CALCULATE 30 SECOND % TRPS                   
         M     R0,F10000                                                        
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,R3LP30),2,ZERO=BLANK                                     
                                                                                
DPTSUM50 GOTOR REPORT              PRINT DAYPART LINE                           
         AHI   R2,R3LL                                                          
         MVC   P1(R3LL),0(R2)                                                   
         GOTOR REPORT              PRINT BOTTOM OF REPORT BOX                   
                                                                                
DPTSUMX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT STATION/AFFILIATE SUMMARY                          *         
***********************************************************************         
                                                                                
STASUM   NTR1  ,                                                                
         XC    BFKEYLST,BFKEYLST   CLEAR VALUES                                 
         XC    REPTOTS(REPTOTSL),REPTOTS                                        
                                                                                
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFK4Q                                                     
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         B     STASUM04                                                         
                                                                                
STASUM02 GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
                                                                                
STASUM04 BE    *+8                                                              
         MVI   BFTYPE,BFKEOFQ                                                   
         CLI   BFTYPE,BFK4Q                                                     
         BE    *+14                                                             
         XC    BFKEY(BFKEYL),BFKEY                                              
         MVI   BFTYPE,BFKEOFQ                                                   
                                                                                
         OC    BFKEYLST,BFKEYLST   TEST FIRST TIME                              
         BNZ   STASUM08                                                         
         CLI   BFTYPE,BFKEOFQ      TEST NO RECORDS FOUND                        
         BE    STASUMX                                                          
                                                                                
         L     R2,AR4BOX01         POINT TO REPORT BOX DEFINITIONS              
         MVC   P1(R4LL),0(R2)                                                   
         MVC   P1+61(17),=C'Revision comments'                                  
         GOTOR REPORT                                                           
         AHI   R2,R4LL                                                          
         LA    R0,R4BOXHED         NUMBER OF BOX HEADLINES                      
                                                                                
STASUM06 MVC   P1(R4LL),0(R2)                                                   
         GOTOR NXTCOM                                                           
         GOTOR REPORT                                                           
         AHI   R2,R4LL                                                          
         BCT   R0,STASUM06                                                      
                                                                                
STASUM08 CLI   BFTYPE,BFKEOFQ      TEST END OF FILE                             
         BE    STASUM18                                                         
         OC    BFK4STA,BFK4STA     TEST ALL STATION TOTALS                      
         BNZ   STASUM10                                                         
         MVC   REPTOTS(BFDATAL),BFDATA                                          
         MVC   BFKEYLST,BFKEY                                                   
         B     STASUM02                                                         
                                                                                
STASUM10 MVC   P1(R4LL),0(R2)      BUILD STATION/AFFILIATE LINE                 
         MVC   R4LSTA(L'BFK4STA),BFK4STA                                        
         MVC   R4LAFF(L'BFK4AFF),BFK4AFF                                        
                                                                                
         OC    BFBDOL,BFBDOL                                                    
         BZ    STASUM12                                                         
         EDIT  BFBDOL,(11,R4LDOLS),2,FLOAT=$                                    
                                                                                
STASUM12 EDIT  BFSPOTS,(5,R4LSPOTS),ZERO=BLANK                                  
                                                                                
         EDIT  BFBDEM,(6,R4LTRPS),1,ZERO=BLANK                                  
                                                                                
         ICM   RE,15,REPTOTS+(BFBDOL-BFDATA)                                    
         BZ    STASUM14                                                         
         L     R1,BFBDOL                                                        
         M     R0,F10000                                                        
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,R4LPDOLS+1),2,ZERO=BLANK                                 
                                                                                
STASUM14 ICM   RE,15,REPTOTS+(BFBDEM-BFDATA)                                    
         BZ    STASUM16                                                         
         L     R1,BFBDEM                                                        
         M     R0,F10000                                                        
         SLDA  R0,1                                                             
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(6,R4LPTRPS+1),2,ZERO=BLANK                                 
                                                                                
STASUM16 GOTOR NXTCOM                                                           
         GOTOR REPORT                                                           
         MVC   BFKEYLST,BFKEY                                                   
         B     STASUM02                                                         
                                                                                
STASUM18 AHI   R2,R4LL                                                          
         MVC   P1(R4LL),0(R2)                                                   
         GOTOR NXTCOM                                                           
         GOTOR REPORT                                                           
         AHI   R2,R4LL                                                          
         MVC   P1(R4LL),0(R2)                                                   
         ICM   R1,15,REPTOTS+(BFBDOL-BFDATA)                                    
         BZ    STASUM20                                                         
         EDIT  (R1),(11,R4LDOLS),2,FLOAT=$                                      
                                                                                
STASUM20 ICM   R1,15,REPTOTS+(BFSPOTS-BFDATA)                                   
         BZ    STASUM22                                                         
         EDIT  (R1),(5,R4LSPOTS)                                                
                                                                                
STASUM22 ICM   R1,15,REPTOTS+(BFBDEM-BFDATA)                                    
         BZ    STASUM24                                                         
         EDIT  (R1),(6,R4LTRPS),1                                               
                                                                                
STASUM24 GOTOR NXTCOM                                                           
         GOTOR REPORT                                                           
         AHI   R2,R4LL                                                          
         MVC   P1(R4LL),0(R2)                                                   
         GOTOR NXTCOM                                                           
         GOTOR REPORT                                                           
                                                                                
STASUM26 CLC   COMLINEP,COMLINEN   PRINT REMAINDER OF COMMENTS                  
         BE    STASUMX                                                          
         GOTOR NXTCOM                                                           
         GOTOR REPORT                                                           
         B     STASUM26                                                         
                                                                                
STASUMX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT NEXT COMMENT LINE FROM COMMENT BUFFER                         *         
***********************************************************************         
                                                                                
NXTCOM   CLC   COMLINEP,COMLINEN                                                
         BER   RE                                                               
         SR    R1,R1                                                            
         IC    R1,COMLINEP                                                      
         AHI   R1,1                                                             
         STC   R1,COMLINEP                                                      
         MHI   R1,L'COMLINES                                                    
         LA    R1,COMLINES-L'COMLINES(R1)                                       
         MVC   P1+61(L'COMLINES),0(R1)                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PUT RECORD TO BUFFERIN AND DO TRACE IF NECESSARY                    *         
***********************************************************************         
                                                                                
PUTBUF   OC    BFDATA(BFDATAL),BFDATA                                           
         BZR   RE                                                               
         ST    RE,SAVERE                                                        
         CLI   QOPT1,C'Y'          TEST TRACE OPTION                            
         BNE   PUTBUF02                                                         
         GOTOR PRNTBL,DMCB,=C'PUTBUF',BFREC,C'DUMP',BFRECL,=C'1D00'             
                                                                                
PUTBUF02 GOTOR BUFFERIN,DMCB,('BUFFAPUT',BUFFET),BFREC,ACOMFACS                 
                                                                                
PUTBUFX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
                                                                                
BUFFERIN DC    V(BUFFERIN)                                                      
AR2BOX01 DC    A(R2BOX01)                                                       
AR3BOX01 DC    A(R3BOX01)                                                       
AR4BOX01 DC    A(R4BOX01)                                                       
                                                                                
F10      DC    F'10'                                                            
F1000    DC    F'1000'                                                          
F10000   DC    F'10000'                                                         
                                                                                
REVLIT   DC    C'Revision number'                                               
                                                                                
DTITLES  DS    0CL8                ** DEMO TITLES **                            
         DC    CL8'Primary:'                                                    
         DC    CL8'Demo2:'                                                      
         DC    CL8'Demo3:'                                                      
         DC    CL8'Demo4:'                                                      
                                                                                
RADMEDQ  EQU   C'R'                RADIO MEDIA LETTER                           
                                                                                
SECS15   EQU   15                                                               
SECS30   EQU   30                                                               
SECS60   EQU   60                                                               
                                                                                
BUFFET   BUFFD TYPE=B,KEYLEN=BFKEYL,COLUMNS=BFDATAN,FILE=BUFFWK,       *        
               BUFFERS=10                                                       
                                                                                
R2BOX01  DC    (R2LL)AL1(TOPFLAT)                                               
         ORG   R2BOX01+(R2LSTR-R2L)                                             
         DC    AL1(TOPL)                                                        
         ORG   R2BOX01+(R2LC01-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX01+(R2LC03-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX01+(R2LC05-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX01+(R2LC07-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX01+(R2LC09-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX01+(R2LC11-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX01+(R2LEND-R2L)                                             
         DC    AL1(TOPR)                                                        
         ORG   R2BOX01+R2LL                                                     
                                                                                
R2BOX02  DC    (R2LL)C' '                                                       
         ORG   R2BOX02+(R2LSTR-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'        Flt date'                                              
         ORG   R2BOX02+(R2LC01-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX02+(R2LC03-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX02+(R2LC05-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX02+(R2LC07-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX02+(R2LC09-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX02+(R2LC11-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX02+(R2LEND-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX02+R2LL                                                     
                                                                                
R2BOX03  DC    (R2LL)C' '                                                       
         ORG   R2BOX03+(R2LSTR-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX03+(R2LC01-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'       Stunt1'                                                 
         ORG   R2BOX03+(R2LC03-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'       Flt#1'                                                  
         ORG   R2BOX03+(R2LC05-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'       Flt#2'                                                  
         ORG   R2BOX03+(R2LC07-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'       Flt#3'                                                  
         ORG   R2BOX03+(R2LC09-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'       Flt#4'                                                  
         ORG   R2BOX03+(R2LC11-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  Total all Flts'                                              
         ORG   R2BOX03+(R2LEND-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX03+R2LL                                                     
                                                                                
R2BOX04  DC    (R2LL)AL1(TOPFLAT)                                               
         ORG   R2BOX04+(R2LSTR-R2L)                                             
         DC    AL1(LEFTT)                                                       
         ORG   R2BOX04+(R2LC01-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX04+(R2LC02-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX04+(R2LC03-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX04+(R2LC04-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX04+(R2LC05-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX04+(R2LC06-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX04+(R2LC07-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX04+(R2LC08-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX04+(R2LC09-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX04+(R2LC10-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX04+(R2LC11-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX04+(R2LC12-R2L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R2BOX04+(R2LEND-R2L)                                             
         DC    AL1(RIGHTT)                                                      
         ORG   R2BOX04+R2LL                                                     
                                                                                
R2BOX05  DC    (R2LL)C' '                                                       
         ORG   R2BOX05+(R2LSTR-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'Spot length'                                                   
         ORG   R2BOX05+(R2LC01-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'    Dollars'                                                   
         ORG   R2BOX05+(R2LC02-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  TRPs'                                                        
         ORG   R2BOX05+(R2LC03-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'    Dollars'                                                   
         ORG   R2BOX05+(R2LC04-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  TRPs'                                                        
         ORG   R2BOX05+(R2LC05-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'    Dollars'                                                   
         ORG   R2BOX05+(R2LC06-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  TRPs'                                                        
         ORG   R2BOX05+(R2LC07-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'    Dollars'                                                   
         ORG   R2BOX05+(R2LC08-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  TRPs'                                                        
         ORG   R2BOX05+(R2LC09-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'    Dollars'                                                   
         ORG   R2BOX05+(R2LC10-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  TRPs'                                                        
         ORG   R2BOX05+(R2LC11-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'    Dollars'                                                   
         ORG   R2BOX05+(R2LC12-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  TRPs'                                                        
         ORG   R2BOX05+(R2LEND-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX05+R2LL                                                     
                                                                                
R2BOX06  DC    (R2LL)AL1(TOPFLAT)                                               
         ORG   R2BOX06+(R2LSTR-R2L)                                             
         DC    AL1(LEFTT)                                                       
         ORG   R2BOX06+(R2LC01-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC02-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC03-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC04-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC05-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC06-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC07-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC08-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC09-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC10-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC11-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LC12-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX06+(R2LEND-R2L)                                             
         DC    AL1(RIGHTT)                                                      
         ORG   R2BOX06+R2LL                                                     
                                                                                
R2BOXHED EQU (*-R2BOX03)/R2LL                                                   
                                                                                
R2BOX07  DC    (R2LL)C' '                                                       
         ORG   R2BOX07+(R2LSTR-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC01-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC02-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC03-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC04-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC05-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC06-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC07-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC08-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC09-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC10-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC11-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LC12-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+(R2LEND-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX07+R2LL                                                     
                                                                                
R2BOX08  DC    (R2LL)AL1(TOPFLAT)                                               
         ORG   R2BOX08+(R2LSTR-R2L)                                             
         DC    AL1(LEFTT)                                                       
         ORG   R2BOX08+(R2LC01-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC02-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC03-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC04-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC05-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC06-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC07-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC08-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC09-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC10-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC11-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LC12-R2L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R2BOX08+(R2LEND-R2L)                                             
         DC    AL1(RIGHTT)                                                      
         ORG   R2BOX08+R2LL                                                     
                                                                                
R2BOX09  DC    (R2LL)C' '                                                       
         ORG   R2BOX09+(R2LSTR-R2L)                                             
         DC    AL1(VERT)                                                        
         DC    C'          Totals'                                              
         ORG   R2BOX09+(R2LC01-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC02-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC03-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC04-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC05-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC06-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC07-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC08-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC09-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC10-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC11-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LC12-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+(R2LEND-R2L)                                             
         DC    AL1(VERT)                                                        
         ORG   R2BOX09+R2LL                                                     
                                                                                
R2BOX10  DC    (R2LL)AL1(BOTF)                                                  
         ORG   R2BOX10+(R2LSTR-R2L)                                             
         DC    AL1(BOTL)                                                        
         ORG   R2BOX10+(R2LC01-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC02-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC03-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC04-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC05-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC06-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC07-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC08-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC09-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC10-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC11-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LC12-R2L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R2BOX10+(R2LEND-R2L)                                             
         DC    AL1(BOTR)                                                        
         ORG   R2BOX10+R2LL                                                     
                                                                                
R3BOX01  DC    (R3LL)AL1(TOPFLAT)                                               
         ORG   R3BOX01+(R3LSTR-R3L)                                             
         DC    AL1(TOPL)                                                        
         ORG   R3BOX01+(R3LC01-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX01+(R3LC04-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX01+(R3LC06-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX01+(R3LC10-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX01+(R3LC12-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX01+(R3LEND-R3L)                                             
         DC    AL1(TOPR)                                                        
         ORG   R3BOX01+R3LL                                                     
                                                                                
R3BOX02  DC    (R3LL)C' '                                                       
         ORG   R3BOX02+(R3LSTR-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX02+(R3LC01-R3L)                                             
         DC    AL1(VERT)                                                        
         DC    C'     Total # TRPs'                                             
         ORG   R3BOX02+(R3LC04-R3L)                                             
         DC    AL1(VERT)                                                        
         DC    C' Total % TRPs'                                                 
         ORG   R3BOX02+(R3LC06-R3L)                                             
         DC    AL1(VERT)                                                        
         DC    C'                  CPP'                                         
         ORG   R3BOX02+(R3LC10-R3L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  # of TRPs'                                                   
         ORG   R3BOX02+(R3LC12-R3L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  % of TRPs'                                                   
         ORG   R3BOX02+(R3LEND-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX02+R3LL                                                     
                                                                                
R3BOX03  DC    (R3LL)AL1(TOPFLAT)                                               
         ORG   R3BOX03+(R3LSTR-R3L)                                             
         DC    AL1(VERT)                                                        
         DC    (L'R3LTIT)C' '                                                   
         ORG   R3BOX03+(R3LC01-R3L)                                             
         DC    AL1(LEFTT)                                                       
         ORG   R3BOX03+(R3LC02-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX03+(R3LC03-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX03+(R3LC04-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX03+(R3LC05-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX03+(R3LC06-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX03+(R3LC07-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX03+(R3LC08-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX03+(R3LC09-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX03+(R3LC10-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX03+(R3LC11-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX03+(R3LC12-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX03+(R3LC13-R3L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R3BOX03+(R3LEND-R3L)                                             
         DC    AL1(RIGHTT)                                                      
         ORG   R3BOX03+R3LL                                                     
                                                                                
R3BOXHED EQU (*-R3BOX01)/R3LL                                                   
                                                                                
R3BOX04T DC    (R3LL)C' '          TELEVISION                                   
         ORG   R3BOX04T+(R3LSTR-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'Daypart'                                                       
         ORG   R3BOX04T+(R3LC01-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'   Goal'                                                       
         ORG   R3BOX04T+(R3LC02-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'    Ach'                                                       
         ORG   R3BOX04T+(R3LC03-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'  Index'                                                       
         ORG   R3BOX04T+(R3LC04-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'   Goal'                                                       
         ORG   R3BOX04T+(R3LC05-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'    Ach'                                                       
         ORG   R3BOX04T+(R3LC06-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'  Goal 15'                                                     
         ORG   R3BOX04T+(R3LC07-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'   Ach 15'                                                     
         ORG   R3BOX04T+(R3LC08-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'  Goal 30'                                                     
         ORG   R3BOX04T+(R3LC09-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'   Ach 30'                                                     
         ORG   R3BOX04T+(R3LC10-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'15 sec'                                                        
         ORG   R3BOX04T+(R3LC11-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'30 sec'                                                        
         ORG   R3BOX04T+(R3LC12-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'15 sec'                                                        
         ORG   R3BOX04T+(R3LC13-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'30 sec'                                                        
         ORG   R3BOX04T+(R3LEND-R3L)                                            
         DC    AL1(VERT)                                                        
         ORG   R3BOX04T+R3LL                                                    
                                                                                
R3BOX04R DC    (R3LL)C' '          RADIO                                        
         ORG   R3BOX04R+(R3LSTR-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'Daypart'                                                       
         ORG   R3BOX04R+(R3LC01-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'   Goal'                                                       
         ORG   R3BOX04R+(R3LC02-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'    Ach'                                                       
         ORG   R3BOX04R+(R3LC03-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'  Index'                                                       
         ORG   R3BOX04R+(R3LC04-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'   Goal'                                                       
         ORG   R3BOX04R+(R3LC05-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'    Ach'                                                       
         ORG   R3BOX04R+(R3LC06-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'  Goal 30'                                                     
         ORG   R3BOX04R+(R3LC07-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'   Ach 30'                                                     
         ORG   R3BOX04R+(R3LC08-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'  Goal 60'                                                     
         ORG   R3BOX04R+(R3LC09-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'   Ach 60'                                                     
         ORG   R3BOX04R+(R3LC10-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'30 sec'                                                        
         ORG   R3BOX04R+(R3LC11-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'60 sec'                                                        
         ORG   R3BOX04R+(R3LC12-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'30 sec'                                                        
         ORG   R3BOX04R+(R3LC13-R3L)                                            
         DC    AL1(VERT)                                                        
         DC    C'60 sec'                                                        
         ORG   R3BOX04R+(R3LEND-R3L)                                            
         DC    AL1(VERT)                                                        
         ORG   R3BOX04R+R3LL                                                    
                                                                                
R3BOX05  DC    (R3LL)AL1(TOPFLAT)                                               
         ORG   R3BOX05+(R3LSTR-R3L)                                             
         DC    AL1(LEFTT)                                                       
         ORG   R3BOX05+(R3LC01-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC02-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC03-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC04-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC05-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC06-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC07-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC08-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC09-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC10-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC11-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC12-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LC13-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX05+(R3LEND-R3L)                                             
         DC    AL1(RIGHTT)                                                      
         ORG   R3BOX05+R3LL                                                     
                                                                                
R3BOX06  DC    (R3LL)C' '                                                       
         ORG   R3BOX06+(R3LSTR-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC01-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC02-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC03-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC04-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC05-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC06-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC07-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC08-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC09-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC10-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC11-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC12-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LC13-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+(R3LEND-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX06+R3LL                                                     
                                                                                
R3BOX07  DC    (R3LL)AL1(TOPFLAT)                                               
         ORG   R3BOX07+(R3LSTR-R3L)                                             
         DC    AL1(LEFTT)                                                       
         ORG   R3BOX07+(R3LC01-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC02-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC03-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC04-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC05-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC06-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC07-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC08-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC09-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC10-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC11-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC12-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LC13-R3L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R3BOX07+(R3LEND-R3L)                                             
         DC    AL1(RIGHTT)                                                      
         ORG   R3BOX07+R3LL                                                     
                                                                                
R3BOX08  DC    (R3LL)C' '                                                       
         ORG   R3BOX08+(R3LSTR-R3L)                                             
         DC    AL1(VERT)                                                        
         DC    C'            Totals'                                            
         ORG   R3BOX08+(R3LC01-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC02-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC03-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC04-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC05-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC06-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC07-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC08-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC09-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC10-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC11-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC12-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LC13-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+(R3LEND-R3L)                                             
         DC    AL1(VERT)                                                        
         ORG   R3BOX08+R3LL                                                     
                                                                                
R3BOX09  DC    (R3LL)AL1(BOTF)                                                  
         ORG   R3BOX09+(R3LSTR-R3L)                                             
         DC    AL1(BOTL)                                                        
         ORG   R3BOX09+(R3LC01-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC02-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC03-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC04-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC05-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC06-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC07-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC08-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC09-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC10-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC11-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC12-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LC13-R3L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R3BOX09+(R3LEND-R3L)                                             
         DC    AL1(BOTR)                                                        
         ORG   R3BOX09+R3LL                                                     
                                                                                
R4BOX01  DC    (R4LL)AL1(TOPFLAT)                                               
         ORG   R4BOX01+(R4LSTR-R4L)                                             
         DC    AL1(TOPL)                                                        
         ORG   R4BOX01+(R4LC01-R4L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R4BOX01+(R4LC02-R4L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R4BOX01+(R4LC03-R4L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R4BOX01+(R4LC04-R4L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R4BOX01+(R4LC05-R4L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R4BOX01+(R4LC06-R4L)                                             
         DC    AL1(TOPT)                                                        
         ORG   R4BOX01+(R4LEND-R4L)                                             
         DC    AL1(TOPR)                                                        
         ORG   R4BOX01+R4LL                                                     
                                                                                
R4BOX02  DC    (R4LL)C' '                                                       
         ORG   R4BOX02+(R4LSTR-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX02+(R4LC01-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX02+(R4LC02-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX02+(R4LC03-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C' # of'                                                         
         ORG   R4BOX02+(R4LC04-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C' Total'                                                        
         ORG   R4BOX02+(R4LC05-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  %$$ '                                                        
         ORG   R4BOX02+(R4LC06-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C' %TRPs'                                                        
         ORG   R4BOX02+(R4LEND-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX02+R4LL                                                     
                                                                                
R4BOX03  DC    (R4LL)C' '                                                       
         ORG   R4BOX03+(R4LSTR-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'Station'                                                       
         ORG   R4BOX03+(R4LC01-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'Affiliate'                                                     
         ORG   R4BOX03+(R4LC02-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'    Dollars'                                                   
         ORG   R4BOX03+(R4LC03-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'Spots'                                                         
         ORG   R4BOX03+(R4LC04-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'  TRPs'                                                        
         ORG   R4BOX03+(R4LC05-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'Station'                                                       
         ORG   R4BOX03+(R4LC06-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'Station'                                                       
         ORG   R4BOX03+(R4LEND-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX03+R4LL                                                     
                                                                                
R4BOX04  DC    (R4LL)AL1(TOPFLAT)                                               
         ORG   R4BOX04+(R4LSTR-R4L)                                             
         DC    AL1(LEFTT)                                                       
         ORG   R4BOX04+(R4LC01-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX04+(R4LC02-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX04+(R4LC03-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX04+(R4LC04-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX04+(R4LC05-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX04+(R4LC06-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX04+(R4LEND-R4L)                                             
         DC    AL1(RIGHTT)                                                      
         ORG   R4BOX04+R4LL                                                     
                                                                                
R4BOXHED EQU (*-R4BOX02)/R4LL                                                   
                                                                                
R4BOX05  DC    (R4LL)C' '                                                       
         ORG   R4BOX05+(R4LSTR-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX05+(R4LC01-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX05+(R4LC02-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX05+(R4LC03-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX05+(R4LC04-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX05+(R4LC05-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX05+(R4LC06-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX05+(R4LEND-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX05+R4LL                                                     
                                                                                
R4BOX06  DC    (R4LL)AL1(TOPFLAT)                                               
         ORG   R4BOX06+(R4LSTR-R4L)                                             
         DC    AL1(LEFTT)                                                       
         ORG   R4BOX06+(R4LC01-R4L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R4BOX06+(R4LC02-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX06+(R4LC03-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX06+(R4LC04-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX06+(R4LC05-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX06+(R4LC06-R4L)                                             
         DC    AL1(CROSS)                                                       
         ORG   R4BOX06+(R4LEND-R4L)                                             
         DC    AL1(RIGHTT)                                                      
         ORG   R4BOX06+R4LL                                                     
                                                                                
R4BOX07  DC    (R4LL)C' '                                                       
         ORG   R4BOX07+(R4LSTR-R4L)                                             
         DC    AL1(VERT)                                                        
         DC    C'           Totals'                                             
         ORG   R4BOX07+(R4LC02-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX07+(R4LC03-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX07+(R4LC04-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX07+(R4LC05-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX07+(R4LC06-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX07+(R4LEND-R4L)                                             
         DC    AL1(VERT)                                                        
         ORG   R4BOX07+R4LL                                                     
                                                                                
R4BOX08  DC    (R4LL)AL1(TOPFLAT)                                               
         ORG   R4BOX08+(R4LSTR-R4L)                                             
         DC    AL1(BOTL)                                                        
         ORG   R4BOX08+(R4LC02-R4L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R4BOX08+(R4LC03-R4L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R4BOX08+(R4LC04-R4L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R4BOX08+(R4LC05-R4L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R4BOX08+(R4LC06-R4L)                                             
         DC    AL1(BOTT)                                                        
         ORG   R4BOX08+(R4LEND-R4L)                                             
         DC    AL1(BOTR)                                                        
         ORG   R4BOX08+R4LL                                                     
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
COMRECD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDBOXEQUS                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
                                                                                
         ORG   SPACEND                                                          
                                                                                
SAVERE   DS    A                   SAVED RE VALUE                               
SVRERATE DS    A                   RERATE TYPE                                  
NUMDEMOS EQU   4                                                                
SVDEMOS  DS    (NUMDEMOS)XL3       FIRST N ESTIMATE HEADER DEMOS                
SVDEMOSL EQU   *-SVDEMOS                                                        
SVDEMOSN DS    (NUMDEMOS)CL11      FIRST N ESTIMATE DEMO NAMES                  
SVAFFL   DS    CL(L'SNETWRK)                                                    
PSLIST   DS    XL64                                                             
WEEKNUM  DS    XL1                                                              
EQSLN    DS    XL1                                                              
MAXWEEKS EQU   5                                                                
WEEKTAB  DS    (MAXWEEKS)XL4                                                    
WEEKTABL EQU   *-WEEKTAB                                                        
SVQSTART DS    CL6                                                              
                                                                                
DEMVALS  DS    0F                  ** DEMO RELATED VALUES **                    
DGDOL    DS    F                   GOAL DOLLARS                                 
DBDOL    DS    F                   ACTUAL DOLLARS                               
D1GDEM   DS    F                   DEMO 1 GOAL TRPS                             
D1BDEM   DS    F                   DEMO 1 ACTUAL TRPS                           
D2BDEM   DS    F                   DEMO 2 ACTUAL TRPS                           
D3BDEM   DS    F                   DEMO 3 ACTUAL TRPS                           
D4BDEM   DS    F                   DEMO 4 ACTUAL TRPS                           
DEMVALSL EQU   *-DEMVALS                                                        
                                                                                
LINTOTS  DS    0F                  ** LINE TOTALS ACCUMULATORS **               
LINTOT01 DS    F                                                                
LINTOT02 DS    F                                                                
LINTOT03 DS    F                                                                
LINTOT04 DS    F                                                                
LINTOT05 DS    F                                                                
LINTOT06 DS    F                                                                
LINTOT07 DS    F                                                                
LINTOT08 DS    F                                                                
LINTOT09 DS    F                                                                
LINTOT10 DS    F                                                                
LINTOTSL EQU   *-LINTOTS                                                        
                                                                                
REPTOTS  DS    0F                  ** REPORT TOTALS ACCUMULATORS **             
REPTOT01 DS    F                                                                
REPTOT02 DS    F                                                                
REPTOT03 DS    F                                                                
REPTOT04 DS    F                                                                
REPTOT05 DS    F                                                                
REPTOT06 DS    F                                                                
REPTOT07 DS    F                                                                
REPTOT08 DS    F                                                                
REPTOT09 DS    F                                                                
REPTOT10 DS    F                                                                
REPTOT11 DS    F                                                                
REPTOT12 DS    F                                                                
REPTOTSL EQU   *-REPTOTS                                                        
                                                                                
BFKEYLST DS    XL(BFKEYL)          LAST BUFFERIN RECORD KEY                     
                                                                                
BFREC    DS    0X                  ** BUFFERIN RECORD **                        
                                                                                
BFKEY    DS    0X                  ** BUFFERIN RECORD KEY **                    
                                                                                
BFTYPE   DS    XL1                 ** REPORT TYPE **                            
BFKEOFQ  EQU   X'FF'               END OF FILE FLAG                             
                                                                                
BFKDATA  DS    0X                  ** KEY DATA **                               
                                                                                
BFK1Q    EQU   1                   REPORT TOTALS                                
BFK1POS  DS    XL1                 DEMO POSITION (1-N)                          
                                                                                
         ORG   BFKDATA                                                          
BFK2Q    EQU   2                   FLIGHT/SPOT LENGTH SUMMARY                   
BFK2SLN  DS    XL1                 SPOT LENGTH                                  
BFK2WEEK DS    XL1                 WEEK NUMBER (0=STUNT)                        
                                                                                
         ORG   BFKDATA                                                          
BFK3Q    EQU   3                   DAYPART/SPOT LENGTH SUMMARY                  
BFK3DPT  DS    CL3                 DAYPART CODE                                 
BFK3SLN  DS    XL1                 SPOT LENGTH                                  
                                                                                
         ORG   BFKDATA                                                          
BFK4Q    EQU   4                   STATION/AFFILIATE SUMMARY                    
BFK4STA  DS    CL7                 STATION CODE                                 
BFK4AFF  DS    CL3                 AFFILIATE CODE                               
         DS    XL1                 FILLER (TO MAKE KEY 12 BYTES LONG)           
                                                                                
BFKEYL   EQU   *-BFKEY                                                          
                                                                                
         ORG   BFREC+BFKEYL                                                     
BFDATA   DS    0XL4                ** BUFFERIN RECORD DATA **                   
BFSPOTS  DS    XL4                 ACTUAL NUMBER OF SPOTS                       
BFGDOL   DS    XL4                 GOAL DOLLARS (PENNIES)                       
BFGEQDOL DS    XL4                 GOAL EQ DOLS                                 
BFGDEM   DS    XL4                 GOAL DEMO                                    
BFGEQDEM DS    XL4                 GOAL EQ DEMO                                 
BFBDOL   DS    XL4                 BUY DOLLARS (PENNIES)                        
BFBEQDOL DS    XL4                 BUY EQ DOLLARS                               
BFBDEM   DS    XL4                 BUY DEMO                                     
BFBEQDEM DS    XL4                 BUY EQ DEMO                                  
BFDATAL  EQU   *-BFDATA            L'DATA                                       
BFDATAN  EQU   (*-BFDATA)/L'BFDATA N'DATA COLUMNS                               
                                                                                
BFRECL   EQU   *-BFREC             L'BUFFERIN RECORD                            
                                                                                
COMLINEN DS    XL1                 N'COMMENT LINES TO PRINT                     
COMLINEP DS    XL1                 N'COMMENT LINES PRINTED                      
COMLINEM EQU   14                  MAXIMUM N'COMMENT LINES                      
COMLINES DS    (COMLINEM)CL70      COMMENTS                                     
COMLINEL EQU   *-COMLINES          L'COMMENT BLOCK                              
                                                                                
         ORG   P1                                                               
R2L      DS    0C                  ** FLIGHT/SPOT LENGTH SUMMARY **             
                                                                                
R2LSTR   DS    CL1                 LINE START                                   
R2LTIT   DS    CL16                TITLE                                        
                                                                                
R2LC01   DS    CL1                 COLUMN SEPARATOR                             
R2LST1   DS    0CL18               ** STUNT1 **                                 
R2LST1D  DS    CL11                STUNT1 DOLLARS                               
R2LC02   DS    CL1                 COLUMN SEPARATOR                             
R2LST1R  DS    CL6                 STUNT1 RATING POINTS                         
                                                                                
R2LC03   DS    CL1                 COLUMN SEPARATOR                             
R2LFL1   DS    0CL18               ** FLIGHT1 **                                
R2LFL1D  DS    CL11                FLIGHT1 DOLLARS                              
R2LC04   DS    CL1                 COLUMN SEPARATOR                             
R2LFL1R  DS    CL6                 FLIGHT1 RATING POINTS                        
                                                                                
R2LC05   DS    CL1                 COLUMN SEPARATOR                             
R2LFL2   DS    0CL18               ** FLIGHT2 **                                
R2LFL2D  DS    CL11                FLIGHT2 DOLLARS                              
R2LC06   DS    CL1                 COLUMN SEPARATOR                             
R2LFL2R  DS    CL6                 FLIGHT2 RATING POINTS                        
                                                                                
R2LC07   DS    CL1                 COLUMN SEPARATOR                             
R2LFL3   DS    0CL18               ** FLIGHT3 **                                
R2LFL3D  DS    CL11                FLIGHT3 DOLLARS                              
R2LC08   DS    CL1                 COLUMN SEPARATOR                             
R2LFL3R  DS    CL6                 FLIGHT3 RATING POINTS                        
                                                                                
R2LC09   DS    CL1                 COLUMN SEPARATOR                             
R2LFL4   DS    0CL18               ** FLIGHT4 **                                
R2LFL4D  DS    CL11                FLIGHT4 DOLLARS                              
R2LC10   DS    CL1                 COLUMN SEPARATOR                             
R2LFL4R  DS    CL6                 FLIGHT4 RATING POINTS                        
                                                                                
R2LC11   DS    CL1                 COLUMN SEPARATOR                             
R2LFLT   DS    0CL18               ** ALL FLIGHT TOTALS **                      
R2LFLTD  DS    CL11                TOTAL DOLLARS                                
R2LC12   DS    CL1                 COLUMN SEPARATOR                             
R2LFLTR  DS    CL6                 TOTAL RATING POINTS                          
                                                                                
R2LEND   DS    CL1                 LINE END                                     
R2LL     EQU   *-R2L                                                            
         ORG                                                                    
                                                                                
         ORG   P1                                                               
R3L      DS    0C                  ** DAYPART SUMMARY **                        
                                                                                
R3LSTR   DS    CL1                 LINE START                                   
R3LTIT   DS    CL22                TITLE                                        
                                                                                
R3LC01   DS    CL1                 COLUMN SEPARATOR                             
R3LTOT#T DS    0CL15               TOTAL # TRPS                                 
R3LT#TG  DS    CL7                 GOALS                                        
R3LC02   DS    CL1                 COLUMN SEPARATOR                             
R3LT#TB  DS    CL7                 ACHIEVED                                     
R3LC03   DS    CL1                 COLUMN SEPARATOR                             
R3LT#NDX DS    CL7                 INDEX ACHIEVED TO GOAL                       
                                                                                
R3LC04   DS    CL1                 COLUMN SEPARATOR                             
R3LTOTPT DS    0CL15               TOTAL % TRPS                                 
R3LTPTG  DS    CL7                 GOALS                                        
R3LC05   DS    CL1                 COLUMN SEPARATOR                             
R3LTPTB  DS    CL7                 ACHIEVED                                     
                                                                                
R3LC06   DS    CL1                 COLUMN SEPARATOR                             
R3LCPP   DS    0C                  ** CPP **                                    
R3LC15G  DS    CL9                 15 SECOND GOAL CPP                           
R3LC07   DS    CL1                 COLUMN SEPARATOR                             
R3LC15A  DS    CL9                 15 SECOND ACHIEVED CPP                       
R3LC08   DS    CL1                 COLUMN SEPARATOR                             
R3LC30G  DS    CL9                 30 SECOND GOAL CPP                           
R3LC09   DS    CL1                 COLUMN SEPARATOR                             
R3LC30A  DS    CL9                 30 SECOND ACHIEVED CPP                       
                                                                                
R3LC10   DS    CL1                 COLUMN SEPARATOR                             
R3L#TRP  DS    0CL20               ** # OF TRPS **                              
R3L#15   DS    CL6                 15 SECOND # TRPS                             
R3LC11   DS    CL1                 COLUMN SEPARATOR                             
R3L#30   DS    CL6                 30 SECOND # TRPS                             
                                                                                
R3LC12   DS    CL1                 COLUMN SEPARATOR                             
R3LPTRP  DS    0CL20               ** % OF TRPS **                              
R3LP15   DS    CL6                 15 SECOND % TRPS                             
R3LC13   DS    CL1                 COLUMN SEPARATOR                             
R3LP30   DS    CL6                 30 SECOND % TRPS                             
                                                                                
R3LEND   DS    CL1                 LINE END                                     
R3LL     EQU   *-R3L                                                            
         ORG                                                                    
                                                                                
         ORG   P1                                                               
R4L      DS    0C                  ** STATION/AFFILIATE SUMMARY **              
                                                                                
R4LSTR   DS    CL1                 LINE START                                   
R4LTIT   DS    0CL17               ** TITLE **                                  
R4LSTA   DS    CL7                 TITLE 1                                      
R4LC01   DS    CL1                 COLUMN SEPARATOR                             
R4LAFF   DS    CL9                 TITLE 2                                      
                                                                                
R4LC02   DS    CL1                 COLUMN SEPARATOR                             
R4LDOLS  DS    CL11                DOLLARS                                      
                                                                                
R4LC03   DS    CL1                 COLUMN SEPARATOR                             
R4LSPOTS DS    CL5                 NUMBER OF SPOTS                              
                                                                                
R4LC04   DS    CL1                 COLUMN SEPARATOR                             
R4LTRPS  DS    CL6                 TOTAL TRPS                                   
                                                                                
R4LC05   DS    CL1                 COLUMN SEPARATOR                             
R4LPDOLS DS    CL7                 % OF DOLLARS                                 
                                                                                
R4LC06   DS    CL1                 COLUMN SEPARATOR                             
R4LPTRPS DS    CL7                 % OF TRPS                                    
                                                                                
R4LEND   DS    CL1                 LINE END                                     
                                                                                
R4LL     EQU   *-R4L                                                            
         ORG                                                                    
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPPR02 02/17/09'                                      
         END                                                                    
