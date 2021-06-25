*          DATA SET PPREP9902  AT LEVEL 050 AS OF 03/21/14                      
*PHASE PP9902A,+0                                                               
*INCLUDE BINSRCH                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'PPSTAT - PRINTPAK STATISTICS - PP9902'                          
*                                                                               
*        OPTIONS                                                                
*        QOPT1-1 I = REPORT IDESK INSERTIONS ONLY                               
*        QOPT1 - N = DON'T PRINT                                                
*        QOPT2 - N = NO TOTALS                                                  
*        QOPT3 - T = INCLUDE TEST BUYS                                          
*        QOPT4 - D = INCLUDE DELETED BUYS                                       
*        QOPT5 - W = REPORT WEB I/O'S INSTEAD OF OLD STYLE                      
*        QOPT6 - C = SKIP JUST COST RANGE ANALYSIS                              
*        QOPT6 - X = SKIP ALL DETAIL USAGE ANALYSIS                             
*        QOPT6 - B = SKIP DETAIL USAGE ANALYSIS + COST RANGE                    
*        QOPT7 - D = DOWNLOAD  (USE WITH QOPT6 X)                               
*                    (ALSO DELETES $ RECAP)                                     
*                    USES QSTART AND QEND FOR INSERTION ADDED DATE              
*                                                                               
         PRINT NOGEN                                                            
PPSTAT   CSECT                                                                  
         NMOD1 0,PPSTAT,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
*                                                                               
RELO     DC    F'0'                                                             
*                                                                               
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,PPSTAT+4095                                                   
         LA    R9,1(R9)                                                         
         USING PPSTAT+4096,R9                                                   
*                                                                               
*        LA    R9,4095(RC)                                                      
*        LA    R9,1(R9)                                                         
*        USING PPFILED,RC,R9                                                    
         USING PPSTATWK,R8                                                      
         LA    R7,SPACEND                                                       
         USING PP99WRK,R7                                                       
         SPACE 2                                                                
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
         CLI   MODE,FBUYCLI                                                     
         BE    CLTF                                                             
         CLI   MODE,OFCFRST        FIRST BUY FOR OFFICE                         
         BE    OFFF                                                             
         CLI   MODE,OFCLAST        LAST BUY FOR OFFICE                          
         BE    LBUYR               MOSLTY THE SAME AS LBUYREQ                   
         CLI   MODE,FBUYREQ                                                     
         BE    FBUYR                                                            
         CLI   MODE,LBUYREQ                                                     
         BE    LBUYR                                                            
         CLI   MODE,RUNFRST                                                     
         BE    RFRST                                                            
         CLI   MODE,RUNLAST                                                     
         BE    RLAST                                                            
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XIT                                                                    
         EJECT                                                                  
*                                  RUN FIRST                                    
         SPACE 2                                                                
RFRST    DS    0H                                                               
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
*                                                                               
         L     R2,=V(BINSRCH)                                                   
         A     R2,RELO                                                          
         ST    R2,VBINSRCH                                                      
         L     R2,=V(DLFLD)                                                     
         A     R2,RELO                                                          
         ST    R2,VDLFLD                                                        
         L     R2,=A(TOTS1)                                                     
         A     R2,RELO                                                          
         ST    R2,ATOTS1                                                        
         L     R2,=A(TOTS2)                                                     
         A     R2,RELO                                                          
         ST    R2,ATOTS2                                                        
         BAS   RE,CLEAR                                                         
         L     R2,=A(TOTS3)                                                     
         A     R2,RELO                                                          
         ST    R2,ATOTS3                                                        
         BAS   RE,CLEAR                                                         
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         OI    DMOUTBTS,X'FD'                                                   
         MVI   MYDASHS,C'-'                                                     
         MVC   MYDASHS+1(L'MYDASHS-1),MYDASHS                                   
*                                                                               
         MVC   MYTODAY(2),RCDATE+6 YR                                           
         MVC   MYTODAY+2(2),RCDATE MONTH                                        
         MVC   MYTODAY+4(2),RCDATE+2 DAY                                        
*                                                                               
         MVC   PSTART(16),SPACES                                                
*                                                                               
         GOTO1 DATCON,DMCB,(0,MYTODAY),(3,BTODAY)                               
*                                                                               
         MVI   MAXLINES,60                                                      
*                                  SET BSPARS                                   
         SR    R0,R0                                                            
         L     R1,=A(STOTSC)                                                    
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,L'STOTS                                                       
         LA    R4,3                                                             
         LA    R5,300                                                           
         STM   R0,R5,BSPARS                                                     
*                                                                               
         MVI   FCRDTEST,C'Y'     SO I'LL READ STEWARDSHIP ESTS                  
         MVI   MODE,FBUYREQ      TRICK DOWNLD                                   
         GOTO1 =A(DOWNLD),DMCB,RR=RELO                                          
         MVI   MODE,RUNFRST     RESTORE TO RUNFRST                              
*                                QOPT3 WILL FILTER OUT REAL TEST BUYS           
         XC    MYSTART(6),MYSTART   CLEAR MYSTART AND MYEND                     
         MVI   MULTOFF,C'N'                                                     
         MVI   RPTACT,C'N'                                                      
*                                                                               
**                                                                              
**       CODE BELOW NO-OPED  - WAS ATTEMPT TO GET REQUEST DETAILS               
**                             FOR SOON REQUESTS                                
**                             DIDN'T WORK?                                     
**                                                                              
**       L     RF,PPWORK2C                                                      
**       USING PPWORK2D,RF                                                      
**                                                                              
**       L     R1,VMASTC           ADDRESS OF MASTER CONTROLS                   
**       USING MASTD,R1                                                         
**       OC    MCREMPQK,MCREMPQK   SEE IF SOON REQUEST                          
**       BZ    *+8                                                              
**       MVI   RCREQREP,C'Y'       IF SO SET FOR REQUEST DETAILS                
**       DROP  R1                  (OVERRIDE PPREP9901)                         
**       DROP  RF                                                               
         B     EXIT                                                             
*                                  FIRST FOR REQUEST                            
         SPACE 2                                                                
FBUYR    DS    0H                                                               
*                                                                               
**                                                                              
**       CODE BELOW NO-OPED  - WAS ATTEMPT TO GET REQUEST DETAILS               
**                             FOR SOON REQUESTS                                
**       L     RF,PPWORK2C                                                      
**       USING PPWORK2D,RF                                                      
**       L     R1,VMASTC        ADDRESS OF MASTER CONTROLS                      
**       USING MASTD,R1                                                         
**       CLI   QOPT7,C'D'       SEE IF DOWNLOADING                              
**       BNE   *+8                                                              
**       MVI   MCDOWNLD,1       SET DOWNLOAD (HOPE IT'S NOT TOO LATE)           
**       DROP  R1                                                               
**       DROP  RF                                                               
*                                                                               
         CLI   RCALLAGY,C'Y'      ALL AGENCY REQ?                               
         BNE   FBU1X              IF NOT, SKIP TEST AGENCY CHECK                
*                                                                               
*        SKIP TEST AGENCIES                                                     
*                                                                               
         LA    R1,TSTATAB                                                       
FBU1     CLI   0(R1),X'FF'        END OF TABLE                                  
         BE    FBU1X                                                            
         CLC   QAGENCY,0(R1)                                                    
         BE    PBSKIP                                                           
         LA    R1,2(R1)                                                         
         B     FBU1                                                             
*                                                                               
PBSKIP   MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
FBU1X    DS    0H                                                               
         MVI   ONECLT,C'N'                                                      
         CLC   QCLIENT,=C'ALL'                                                  
         BE    FBU1X5                                                           
         CLI   QCLIENT,C'$'     OFFICE LIST REQ                                 
         BE    FBU1X5                                                           
         CLI   QCLIENT,C'*'     OFFICE REQ                                      
         BE    FBU1X5                                                           
         MVI   ONECLT,C'Y'      SET ON ONE CLIENT INDICATOR                     
*                                                                               
FBU1X5   DS    0H                                                               
         CLI   RCMULTIQ,C'Y'    MULTI-MEDIA REQUEST?                            
         BNE   FBUYR0                                                           
         MVC   QSORT,=C'NO'     RESET TO NO                                     
*                                                                               
FBUYR0   CLC   =C'ZZ',QAGENCY                                                   
         BNE   FBR1M                                                            
         MVC   QCLIENT,=C'ALL'      SINCE IT MIGHT HAVE BEEN $*                 
*                                                                               
****     GOTO1 =A(TOTR),DMCB,ATOTS2,RR=RELO                                     
****                                                                            
         L     R2,ATOTS2                                                        
         L     R3,ATOTS3                                                        
         BAS   RE,ROLLUP                                                        
         L     R2,ATOTS2                                                        
         BAS   RE,CLEAR                                                         
*                                                                               
*                                                                               
FBR1M    DS    0H                                                               
         MVC   SVOPTS,QOPT1    SAVE OPTIONS FOR RLAST                           
         L     R2,ATOTS1                                                        
         BAS   RE,CLEAR                                                         
         MVC   PAGE,=H'1'                                                       
*****    CLI   QOPT7,C'D'          SEE IF DOING DOWNLOAD                        
*****    BNE   FBR5                QSTART AND QEND                              
*****                    USE DATE ADDED FOR REPORT AND DOWNLOAD                 
         OC    MYSTART,MYSTART     ALREADY SET?                                 
         BNZ   FBR4X                                                            
         GOTO1 DTCNV,DMCB,QSTART,(1,MYSTART)                                    
         GOTO1 DTCNV,DMCB,QEND,(1,MYEND)                                        
         OC    MYSTART,MYSTART                                                  
         BZ    FBR2                                                             
         GOTO1 DTCNV,DMCB,(1,MYSTART),(3,PSTART)                                
*                                                                               
FBR2     DS    0H                                                               
         OC    MYEND,MYEND                                                      
         BZ    FBR4                                                             
         GOTO1 DTCNV,DMCB,(1,MYEND),(3,PEND)                                    
*                                                                               
FBR4     DS    0H                                                               
*        SET QSTART TO 2 YEARS BEFORE REQUESTED START                           
*        SET QEND TO 2 YEARS AFTER REQUESTED END                                
*                                                                               
         MVC   WORK(12),QSTART                                                  
         L     R0,=F'-2'                                                        
         GOTO1 ADDAY,DMCB,(C'Y',WORK),QSTART,(R0)                               
         L     R0,=F'2'                                                         
         GOTO1 ADDAY,DMCB,(C'Y',WORK+6),QEND,(R0)                               
*                                                                               
FBR4X    DS    0H                                                               
*                                  SO THAT ALL BUYS WILL BE READ                
FBR5     MVC   NDIVS,=H'10'        NO OF DIVISIONS IN COST RANKING              
         CLC   QPAY(3),SPACES                                                   
         BE    FBR6                                                             
         PACK  DUB,QPAY(3)                                                      
         CVB   R0,DUB                                                           
         STH   R0,NDIVS                                                         
FBR6     DS    0H                                                               
*                                  SET TEAM FROM TEAMTAB                        
         LA    R1,TEAMTAB                                                       
FBR8     CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    FBR10                                                            
         CLC   0(2,R1),QAGENCY                                                  
         BE    FBR10                                                            
         LA    R1,10(R1)                                                        
         B     FBR8                                                             
*                                                                               
FBR10    MVC   SVTEAM(08),2(R1)                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TEAMTAB  DS    0C                                                               
*        PRINT 1                                                                
         DC    C'DF',CL08'BLACK'                                                
         DC    C'DT',CL08'BLACK'                                                
         DC    C'PC',CL08'BLACK'                                                
         DC    C'TH',CL08'BLACK'                                                
*        PRINT 2                                                                
         DC    C'AG',CL08'BLUE'                                                 
         DC    C'B$',CL08'RED'                                                  
         DC    C'C0',CL08'RED'                                                  
         DC    C'D9',CL08'BLUE'                                                 
         DC    C'H9',CL08'BLACK'                                                
         DC    C'I5',CL08'BLUE'                                                 
         DC    C'KP',CL08'BLUE'                                                 
         DC    C'OO',CL08'BLUE'                                                 
         DC    C'QU',CL08'WEST'                                                 
         DC    C'T$',CL08'WEST'                                                 
         DC    C'TR',CL08'WEST'          ?                                      
*        PRINT 3                                                                
         DC    C'DV',CL08'BLACK'                                                
         DC    C'IJ',CL08'BLUE'                                                 
         DC    C'NE',CL08'BLUE'                                                 
         DC    C'OH',CL08'UNKNOWN'       DONINO WHITE & PARTNERS                
         DC    C'RB',CL08'BLACK'                                                
         DC    C'S$',CL08'BLUE'                                                 
*        PRINT 4                                                                
         DC    C'CY',CL08'CANADA'                                               
         DC    C'DA',CL08'CANADA'       ?  DONER SCHUR                          
         DC    C'FR',CL08'RED'                                                  
         DC    C'F6',CL08'CANADA'       ?  SHARP BLACKMORE                      
         DC    C'GK',CL08'RED'                                                  
         DC    C'HW',CL08'CANADA'       ?  RANSOMBE & CO.                       
         DC    C'HO',CL08'CANADA'                                               
         DC    C'H7',CL08'RED'                                                  
         DC    C'JE',CL08'CANADA'                                               
         DC    C'JW',CL08'RED'                                                  
         DC    C'O0',CL08'BLACK'                                                
         DC    C'PA',CL08'CANADA'                                               
         DC    C'PT',CL08'CANADA'                                               
         DC    C'SO',CL08'CANADA'                                               
         DC    C'U#',CL08'CANADA'                                               
         DC    C'YR',CL08'CANADA'       ? YOUNG AND RUBICAM                     
*        PRINT 5                                                                
         DC    C'G+',CL08'BLUE'                                                 
         DC    C'JT',CL08'CANADA'                                               
         DC    C'M2',CL08'RED'                                                  
         DC    C'NT',CL08'CANADA'       ?  MPG CANADA                           
         DC    C'O$',CL08'RED'                                                  
         DC    C'TB',CL08'CANADA'       ? TEST? ZENITHOPTIMEDIA                 
*        PRINT 6                                                                
         DC    C'BD',CL08'BLUE'                                                 
         DC    C'BN',CL08'BLUE'                                                 
         DC    C'DQ',CL08'WEST'                                                 
         DC    C'G&&',CL08'BLACK'                                               
         DC    C'G7',CL08'BLUE'                                                 
         DC    C'LH',CL08'UNKNOWN'      LUNCH ADVERTISING TEST?                 
         DC    C'M$',CL08'BLUE'                                                 
         DC    C'MC',CL08'BLUE'                                                 
*        PRINT 7                                                                
         DC    C'DM',CL08'BLUE'                                                 
         DC    C'MK',CL08'BLUE'                                                 
*        PRINT 8                                                                
         DC    C'DN',CL08'BLUE'                                                 
         DC    C'FM',CL08'UNKNOWN'      SFM MEDIA LLC  CANADA?                  
         DC    C'GZ',CL08'BLACK'                                                
         DC    C'OU',CL08'BLUE'                                                 
         DC    C'RP',CL08'WEST'                                                 
         DC    C'RX',CL08'UNKNOWN'      BARON COMMUNICATIONS                    
         DC    C'S4',CL08'CANADA'                                               
         DC    C'WD',CL08'WEST'                                                 
         DC    C'YN',CL08'RED'                                                  
         DC    C'YP',CL08'RED'                                                  
*        PRINT 9                                                                
         DC    C'M1',CL08'UNKNOWN'                                              
         DC    C'UB',CL08'BLACK'                                                
         DC    C'WI',CL08'UNKNOWN'                                              
         DC    C'WT',CL08'CANADA'       ?                                       
*                                                                               
         DC    X'FFFF',CL8'UNKNOWN'                                             
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
OFFF     DS    0H                  FIRST FOR OFFICE                             
         MVI   MULTOFF,C'Y'        SET PROCESSING OFFICE LIST REQ               
         MVI   OFFACT,C'N'         SET OFF ACTIVITY SWITCH                      
         L     R2,ATOTS1                                                        
         BAS   RE,CLEAR                                                         
         B     EXIT                                                             
         EJECT                                                                  
CLTF     DS    0H                  FIRST BUY FOR CLIENT                         
*                                                                               
*        SET SAVCOFF HERE FOR ALL REQUESTS                                      
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         MVC   SAVCOFF(1),PCLTOFF    SAVE OFFICE FOR HEADLINES                  
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'P'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGENCY                                              
         MVC   OFFD.OFCPMED,QMEDIA                                              
         MVC   OFFD.OFCOFC,PCLTOFF                                              
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK),(0,VCOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   CLTF5                                                            
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
CLTF5    DS    0H                                                               
         CLC   SAVCOFF,=X'0000'   IF STILL ZEROS, MAKE SPACES                   
         BNE   *+10                                                             
         OC    SAVCOFF,SPACES    NEEDED FOR CLIENTS WITHOUT AN OFFICE           
         B     EXIT                                                             
         EJECT                                                                  
PRBUY    DS    0H                  PROCBUY                                      
*                                                                               
         CLI   QOPT1-1,C'I'   SEE IF REPORTING IDESK INSERTIONS ONLY            
         BNE   PB1                                                              
         TM    PBDSTAT2,X'20'       ADDED VIA IDESK                             
         BZ    EXIT                                                             
*                                                                               
PB1      TM    PBDSTAT2,X'40'      STEWARDSHIP BUY?                             
         BNO   PB1A            NO                                               
         CLI   QOPT3,C'T'      INCLUDING TEST                                   
         BE    PB1C            THEN DON'T CLEAR $                               
         XC    GROSS,GROSS     CLEAR $                                          
         B     PB1C        AND PROCESS EVEN IF NOT INCLUDING TEST BUYS          
*                                                                               
PB1A     CLI   QOPT3,C'T'          SEE IF INCLUDING TEST BUYS                   
         BE    PB1C                                                             
         CLI   PBDBFD,C'T'                                                      
         BE    EXIT                OTHERWISE IGNORE                             
*                                                                               
PB1C     DS    0H                                                               
******   CLI   QOPT7,C'D'           DOWNLOAD?                                   
******   BNE   PB1F                                                             
******                   USE DATE ADDED FOR REPORT AND DOWNLOAD                 
         CLC   PBDBUYDT,MYSTART     CREATION DATE MUST BE BETWEEN               
         BL    EXIT                 MYSTART AND MYEND                           
         CLC   PBDBUYDT,MYEND                                                   
         BH    EXIT                                                             
*                                                                               
PB1F     CLI   QOPT4,C'D'           SEE IF INCLUDING DELETED                    
         BE    PB1X                                                             
         TM    PBUYCNTL,X'C0'       PROCESS CLOSED-OUT                          
         BO    PB1X                                                             
         TM    PBUYCNTL,X'80'       OTHERWISE SKIP DELETED                      
         BNZ   EXIT                                                             
*                                                                               
PB1X     DS    0H                                                               
         MVI   OFFACT,C'Y'         SET ON ACTIVITY SWITCH                       
         L     R8,ATOTS1                                                        
         L     RF,TOTBUYS          TOTAL BUYS                                   
         LA    RF,1(RF)                                                         
         ST    RF,TOTBUYS                                                       
*                                                                               
         TM    PBDSTAT2,X'80'      ADDED VIA ADBUYER?                           
         BZ    PB1X0                                                            
         L     RF,ADBBUYS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,ADBBUYS                                                       
*                                                                               
PB1X0    TM    PBDSTAT2,X'40'      STEWARDSHIP BUY?                             
         BZ    PB1X0C                                                           
         L     RF,STWBUYS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,STWBUYS                                                       
*                                                                               
***X0C   LA    R2,PBUYREC+33                                                    
***      MVI   MYELCODE,X'B0'  LOOK FOR IDESK UPLOAD ELEMENT                    
***      BAS   RE,NEXTEL                                                        
***      BNE   PB1X0D                                                           
***      USING PBYDKELD,R2                                                      
***      TM    PBYDKST1,BYDKADDQ  ADDED VIA IDESK?                              
***      BZ    PB1X0E                                                           
***      L     RF,IDKBUYS                                                       
***      LA    RF,1(RF)                                                         
***      ST    RF,IDKBUYS                                                       
***      B     PB1X0E                                                           
***      DROP  R2                                                               
*                                                                               
PB1X0C   TM    PBDSTAT2,X'20'       ADDED VIA IDESK                             
         BZ    PB1X0E                                                           
         L     RF,IDKBUYS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,IDKBUYS                                                       
*                                                                               
PB1X0E   LA    R2,PBUYREC+33                                                    
         MVI   MYELCODE,X'A7'  LOOK FOR PID ELEMENT                             
         BAS   RE,NEXTEL                                                        
         BNE   PB1X0X                                                           
         USING PPIDELD,R2                                                       
         CLI   PPIDELL,10                LENGTH MUST BE AT LEAST 10             
         BL    PB1X0X                                                           
         CLI   PPIDPRG,PPIDCPYQ    SFM COPY                                     
         BE    PB1X0E5                                                          
         CLI   PPIDPRG,PPIDMOVQ    SFM MOVE                                     
         BNE   PB1X0X                                                           
PB1X0E5  L     RF,SFMBUYS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SFMBUYS                                                       
         DROP  R2                                                               
*                                                                               
PB1X0X   DS    0H                                                               
*                                                                               
PB1X1    LA    R4,ELTAB        ELEMENT TABLE                                    
         LA    R5,PBUBUYS      FIRST ACCUMULATOR                                
*                                                                               
PB1X2    CLI   0(R4),X'FF'     END OF TABLE                                     
         BE    PB1X7                                                            
         MVC   MYELCODE,0(R4)                                                   
         LA    R2,PBUYREC+33                                                    
PB1X2A   BAS   RE,NEXTEL                                                        
         BNE   PB1X3                                                            
         CLI   MYELCODE,X'71'    WEB I/O?                                       
         BNE   PB1X2A2                                                          
         OC    2(3,R2),2(R2)     CHECK FOR DATE                                 
         BZ    PB1X2A            NONE - KEEP LOOKING                            
         B     PB1X2B            GO TO COUNT                                    
*                                                                               
PB1X2A2  CLI   MYELCODE,X'70'    NORMAL I/O?                                    
         BNE   PB1X2A4                                                          
         OC    2(3,R2),2(R2)     CHECK FOR DATE                                 
         BZ    PB1X2A            NONE - KEEP LOOKING                            
         B     PB1X2B                                                           
*                                                                               
PB1X2A4  CLI   QOPT1-1,C'I'      SEE IF DOING IDESK ONLY REPORT                 
         BNE   PB1X2B                                                           
         CLI   MYELCODE,X'CC'    CUSTOM COLUMN ELEMENT                          
         BNE   PB1X2B                                                           
         USING BYCCELD,R2                                                       
         CLC   =AL2(8215),BYCCSQN  CHECK FOR IDESK 3                            
         BNE   PB1X2A            IF NOT-KEEP LOOKING                            
         CLI   BYCCDATA,C'C'     MUST START WITH C                              
         BNE   PB1X2A            KEEP LOOKING                                   
         B     PB1X2B                                                           
         DROP  R2                                                               
*                                                                               
PB1X2B   L     RF,0(R5)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,0(R5)                                                         
*                                                                               
PB1X3    LA    R4,1(R4)    NEXT ELEMENT CODE                                    
         LA    R5,4(R5)    NEXT ACCUMULATOR                                     
         B     PB1X2                                                            
*                                                                               
PB1X7    DS    0H      NOW CHECK FOR CERTAIN TYPES OF X'70' ELEMENTS            
         MVI   MYELCODE,X'70'                                                   
         MVI   TASW,0                                                           
         MVI   FAXSW,0                                                          
         LA    R2,PBUYREC+33                                                    
PB1X7B   BAS   RE,NEXTEL                                                        
         BNE   PB1X7X                                                           
         OC    2(3,R2),2(R2)     MUST HAVE A DATE                               
         BZ    PB1X7B                                                           
*                                                                               
         USING PIOELEM,R2                                                       
         CLI   PIOFAX,C'F'                                                      
         BNE   PB1X7D                                                           
         OI    FAXSW,1           SET FAXED I/O FOUND                            
*                                                                               
PB1X7D   CLI   PIOTURN,C'T'      SEE IF TURNAROUND                              
         BNE   PB1X7B            KEEP LOOKING                                   
         OI    TASW,1            SET T/A I/O FOUND                              
         B     PB1X7B            KEEP LOOKING                                   
*                                                                               
PB1X7X   DS    0H                                                               
         CLI   FAXSW,1           FAXED I/O FOUND?                               
         BNE   PB1X7X5                                                          
         L     RF,FAXION                                                        
         LA    RF,1(RF)                                                         
         ST    RF,FAXION                                                        
PB1X7X5  CLI   TASW,1            AUTO TURNAROUND I/O FOUND?                     
         BNE   PB1X7X9                                                          
         L     RF,TAION                                                         
         LA    RF,1(RF)                                                         
         ST    RF,TAION                                                         
*                                                                               
PB1X7X9  DS    0H                                                               
         DROP  R2                                                               
*                                                                               
PB1X20   TM    PBUYCNTL,X'C0'                                                   
         BNO   PB2                                                              
         L     RF,CLSOUTS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,CLSOUTS                                                       
         B     PB3                                                              
*                                  DELETES                                      
PB2      DS    0H                                                               
         TM    PBUYCNTL,X'80'                                                   
         BZ    PB3                                                              
         L     RF,DELETS                                                        
         LA    RF,1(RF)                                                         
         ST    RF,DELETS                                                        
*                                                                               
PB3      CLI   PBDJOB,X'FF'       ADID ALONE?                                   
         BNE   PB4                                                              
         L     RF,ADIDS                                                         
         LA    RF,1(RF)                                                         
         ST    RF,ADIDS                                                         
*                                                                               
PB4      DS    0H                                                               
         L     R0,GROSS                                                         
         SRDA  R0,32                                                            
         LA    RF,100                                                           
         BAS   RE,DIV                                                           
         ST    R1,GROSS                                                         
*                                  GROSS                                        
         A     R1,DLLRS                                                         
         ST    R1,DLLRS                                                         
*                                  BYTES                                        
         MVC   HALF,PBUYLEN                                                     
         L     RF,BYTES                                                         
         AH    RF,HALF                                                          
         ST    RF,BYTES                                                         
*                                                                               
*                                  ZZZ PRODUCT ELEMS                            
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   PB5                                                              
         L     RF,ZBUYS                                                         
         LA    RF,1(RF)                                                         
         ST    RF,ZBUYS                                                         
*                                                                               
         SR    R4,R4                                                            
         MVI   BYTE,C'E'                                                        
         LA    R2,PBUYREC+33                                                    
         MVI   MYELCODE,X'21'                                                   
PB4C     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PB4E                                                             
         LA    R4,1(R4)                                                         
         CLC   5(1,R2),6(R2)       COST VS SPACE SHARE                          
         BE    *+8                                                              
         MVI   BYTE,C'U'                                                        
         B     PB4C                                                             
*                                                                               
PB4E     DS    0H                                                               
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R4,NPRDS                                                         
         CH    R4,=H'7'                                                         
         BNH   *+8                                                              
         LA    R4,7                                                             
         SLL   R4,2                X4                                           
         L     RF,ZPRDS1-4(R4)                                                  
         LA    RF,1(RF)                                                         
         ST    RF,ZPRDS1-4(R4)                                                  
*                                                                               
*                                                                               
*                                                                               
         CLI   BYTE,C'E'                                                        
         BE    PB4G                                                             
         L     RF,ZCSUNEQS                                                      
         LA    RF,1(RF)                                                         
         ST    RF,ZCSUNEQS                                                      
*                                                                               
PB4G     DS    0H                                                               
         TM    PBDWTSUM,X'80'      UNEQUAL ALLOC                                
         BZ    PB4I                                                             
         L     RF,ZUNEQS                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ZUNEQS                                                        
*                                                                               
PB4I     DS    0H                                                               
*                                                                               
PB5      DS    0H                                                               
*                                  BILL ELEMENTS                                
         SR    R4,R4                                                            
         LA    R2,PBUYREC+33                                                    
         MVI   MYELCODE,X'26'                                                   
PB6      DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PB8                                                              
         OC    5(3,R2),5(R2)                                                    
         BZ    PB6                                                              
         LA    R4,1(R4)                                                         
         B     PB6                                                              
*                                                                               
PB8      DS    0H                                                               
         CLC   PBUYKPRD,=C'ZZZ'    FOR ZZZ COUNT BILLS/PROD                     
         BNE   PB8C                                                             
         L     RF,NPRDS                                                         
         LR    R1,R4                                                            
         SR    R0,R0                                                            
         BAS   RE,DIV                                                           
         LR    R4,R1                                                            
*                                                                               
PB8C     DS    0H                                                               
         CLC   BTODAY(2),PBDBDATE                                               
         BNH   PB9                                                              
*                                  BILLABLE DATE PAST                           
         L     RF,BILDP                                                         
         LA    RF,1(RF)                                                         
         ST    RF,BILDP                                                         
*                                                                               
         LTR   R4,R4                                                            
         BNZ   PB9                                                              
*                                  UNBILLED                                     
         L     RF,BILDPU                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BILDPU                                                        
*                                                                               
PB9      DS    0H                                                               
         CH    R4,=H'4'                                                         
         BNH   *+8                                                              
         LA    R4,4                                                             
         SLL   R4,2                X4                                           
         L     RF,BILLD0(R4)       NO. OF TIMES BILLED                          
         LA    RF,1(RF)                                                         
         ST    RF,BILLD0(R4)                                                    
*                                                                               
*                             PAY ELEMS                                         
         SR    R4,R4                                                            
         LA    R2,PBUYREC+33                                                    
         MVI   MYELCODE,X'25'                                                   
PB10     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PB12                                                             
         OC    2(3,R2),2(R2)                                                    
         BZ    PB10                                                             
         LA    R4,1(R4)                                                         
         B     PB10                                                             
*                                                                               
PB12     DS    0H                                                               
         CH    R4,=H'4'                                                         
         BNH   *+8                                                              
         LA    R4,4                                                             
         SLL   R4,2                X4                                           
         L     RF,PAYD0(R4)        NO. OF TIMES PAID                            
         LA    RF,1(RF)                                                         
         ST    RF,PAYD0(R4)                                                     
*                                                                               
*                                  INSERTION ORDERS                             
         SR    R4,R4                                                            
         LA    R2,PBUYREC+33                                                    
         MVI   MYELCODE,X'70'                                                   
         CLI   QOPT5,C'W'          DOING WEB I/O'S?                             
         BNE   *+8                                                              
         MVI   MYELCODE,X'71'                                                   
PB14     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PB16                                                             
*                                                                               
         USING PIOELEM,R2                                                       
         OC    PIODATE,PIODATE                                                  
         BZ    PB14                                                             
         LA    R4,1(R4)                                                         
*                                                                               
         L     RF,TOTIOS           TOTAL IO'S                                   
         LA    RF,1(RF)                                                         
         ST    RF,TOTIOS                                                        
*                                                                               
         LA    RF,NEWIOS                                                        
         CLI   PIOTYP,C'N'         NEW                                          
         BE    PB140                                                            
         LA    RF,CHGIOS                                                        
         CLI   PIOTYP,C'C'         CHANGE                                       
         BE    PB140                                                            
         LA    RF,CANIOS                                                        
         CLI   PIOTYP,C'D'         DELETE                                       
         BE    PB140                                                            
         LA    RF,UCHIOS                                                        
         CLI   PIOTYP,C'U'         UNCHANGED (WEB I/O ONLY)                     
         BE    PB140                                                            
         B     PB14                SKIP IF UNKNOWN TYPE                         
*                                                                               
PB140    L     R1,0(RF)                                                         
         LA    R1,1(R1)                                                         
         ST    R1,0(RF)                                                         
*                                                                               
         CLI   PIOFAX,C'F'         SEE IF FAXED                                 
         BNE   PB14A                                                            
         L     R1,FAXIOS                                                        
         LA    R1,1(R1)                                                         
         ST    R1,FAXIOS                                                        
*                                                                               
PB14A    CLI   MYELCODE,X'71'      DOING WEB I/O?                               
         BNE   PB14B                                                            
         LA    RF,REMIOS           COUNT AS REMOTE                              
         B     PB14X                                                            
*                                                                               
PB14B    CLI   PIOTURN,C'T'        AUTD                                         
         BNE   *+8                                                              
         LA    RF,AUTIOS                                                        
         CLI   PIOTURN,C'R'        REQUEST                                      
         BNE   *+8                                                              
         LA    RF,REQIOS                                                        
         CLI   PIOTURN,0           REMOTE                                       
         BNE   *+8                                                              
         LA    RF,REMIOS                                                        
PB14X    L     R1,0(RF)                                                         
         LA    R1,1(R1)                                                         
         ST    R1,0(RF)                                                         
*                                                                               
         OC    PIOCDATE,PIOCDATE                                                
         BZ    PB15                                                             
         L     RF,IGNIOS                                                        
         LA    RF,1(RF)                                                         
         ST    RF,IGNIOS                                                        
*                                                                               
PB15     DS    0H                                                               
         B     PB14                                                             
*                                                                               
PB16     DS    0H                                                               
         CH    R4,=H'4'                                                         
         BNH   *+8                                                              
         LA    R4,4                                                             
         SLL   R4,2                X4                                           
         L     RF,NIOS0(R4)        NO. OF IOS                                   
         LA    RF,1(RF)                                                         
         ST    RF,NIOS0(R4)                                                     
*                                                                               
*                                  COST                                         
         SR    R0,R0                                                            
         L     R1,GROSS                                                         
         LTR   R1,R1                                                            
         LA    RF,NEGBUYS          MINUS BUYS                                   
         BM    *+12                                                             
         LA    RF,ZERBUYS          ZERO BUYS                                    
         BP    PB18                                                             
*                                                                               
         LH    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STH   RE,0(RF)                                                         
         B     PB20                                                             
*                                                                               
PB18     DS    0H                                                               
         C     R1,=A(RLIM-1)                                                    
         BNH   *+8                                                              
         L     R1,=A(RLIM-1)                                                    
         D     R0,=A(RDIV)                                                      
         SLL   R1,1                X2  (HALF WORDS)                             
         LH    RF,BUYCOSTS(R1)     COST SLOT                                    
         LA    RF,1(RF)                                                         
         STH   RF,BUYCOSTS(R1)                                                  
*                                                                               
PB20     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                  LAST FOR REQ                                 
         SPACE 2                                                                
LBUYR    DS    0H                                                               
         CLC   QAGENCY(2),=C'ZZ'                                                
         BE    EXIT                                                             
*                                                                               
LBUYR0   CLI   MULTOFF,C'Y'     DOING MULTI-OFFICE REQ?                         
         BNE   LBUYR1                                                           
         CLI   MODE,OFCLAST    LAST FOR OFFICE MODE?                            
         BNE   LBUYR10          MUST REALLY BE LBUYREQ                          
         CLI   OFFACT,C'Y'      ANY ACTIVITY?                                   
         BNE   EXIT                                                             
*                                                                               
LBUYR1   CLI   QOPT7,C'D'       DOING DOWNLOAD?                                 
         BNE   LBUYR2                                                           
         L     R8,ATOTS1                                                        
         OC    TOTBUYS,TOTBUYS    ANY BUYS?                                     
         BZ    LBUYR4                                                           
         MVI   MODE,0                                                           
         GOTO1 =A(DOWNLD),DMCB,ATOTS1,RR=RELO                                   
         MVI   MODE,OFCLAST                                                     
         CLI   MULTOFF,C'Y'     DOING MULTI-OFFICE REQ?                         
         BE    *+8                                                              
         MVI   MODE,LBUYREQ                                                     
         B     LBUYR4                                                           
*                                                                               
LBUYR2   CLI   QOPT1,C'N'                                                       
         BE    LBUYR3              DONT PRINT                                   
         GOTO1 =A(TOTR),DMCB,ATOTS1,RR=RELO                                     
*                                                                               
LBUYR3   DS    0H                                                               
         CLI   QOPT2,C'N'          NO TOTALS                                    
         BE    EXIT                                                             
LBUYR4   L     R2,ATOTS1                                                        
         L     R3,ATOTS2                                                        
         BAS   RE,ROLLUP                                                        
         L     R8,ATOTS1                                                        
         BAS   RE,SAVTOTS          ADD TO RECAP TOTS                            
         B     EXIT                                                             
         SPACE 3                                                                
LBUYR10  DS    0H          LBUYREQ FOR MULTI-OFFICE REQ                         
*                          JUST LAST FOR THIS MEDIA FOR MEDIA * REQ             
         MVI   MULTOFF,C'N'        TRY SETTING THIS OFF                         
         XC    KEY,KEY             AND CLEARING KEYS                            
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   QSORT,=C'NO'     RESET TO NO                                     
         B     EXIT                                                             
*                                  LAST FOR RUN                                 
         SPACE 3                                                                
RLAST    DS    0H                                                               
         MVC   QOPT1(7),SVOPTS     RESTORE OPTIONS                              
         L     R8,ATOTS3                                                        
         OC    TOTBUYS,TOTBUYS                                                  
         BNZ   *+8                                                              
         L     R8,ATOTS2                                                        
         OC    TOTBUYS,TOTBUYS                                                  
         BZ    NOBUYS                                                           
         CLI   RCALLAGY,C'Y'                                                    
         BNE   RL2                                                              
         MVC   PAGYKAGY,SPACES                                                  
         MVC   PAGYNAME,SPACES                                                  
         MVC   PAGYNAME(3),=C'ALL'                                              
*                                                                               
RL2      DS    0H                                                               
         CLI   RCMEDFLT,C' '                                                    
         BH    RL4                                                              
         MVI   PAGYKMED,C' '                                                    
         MVC   PAGYMED,SPACES                                                   
         MVC   PAGYMED(3),=C'ALL'                                               
RL4      DS    0H                                                               
         CLI   QOPT7,C'D'       DOING DOWNLOAD?                                 
         BNE   RL6                                                              
         CLI   MULTOFF,C'Y'     SEE IF DOING OFFICE REPORT                      
         BE    RL4X             IF SO, DON'T DO A TOTAL NOW                     
         CLI   QOPT2,C'N'       SEE IF SUPPRESSING TOTALS                       
         BE    RL4X                                                             
         MVI   MODE,0                                                           
         GOTO1 =A(DOWNLD),DMCB,(R8),RR=RELO                                     
         MVI   MODE,RUNLAST        RESTORE MODE TO RUNLAST                      
RL4X     CLI   RCREQREP,C'N'       REQ DETAILS SUPPRESSED?                      
         BNE   RLAST5                                                           
*                                  AT RUNLAST                                   
         GOTO1 =A(DOWNLD),DMCB,RR=RELO                                          
*                                  I MUST CLOSE THE DOWNLOADED REPORT           
*                                                                               
RLAST5   DS    0H                                                               
         B     EXIT             NOTHING ELSE                                    
*                                                                               
RL6      DS    0H                                                               
         OC    TOTBUYS,TOTBUYS     ANY INSERTIONS PROCESSED?                    
         BNZ   RL6C                                                             
*                                                                               
NOBUYS   DS    0H                                                               
         CLI   RPTACT,C'Y'        DID I PRINT A REPORT?                         
         BE    EXIT               JUST EXIT                                     
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(21),=C'BUY RECORD STATISTICS'                                  
         MVC   PSECOND(21),MYDASHS                                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   SPACING,2                                                        
         MVC   P(23),=C'** NO DATA PROCESSED **'                                
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
                                                                                
*                                                                               
RL6C     GOTO1 =A(TOTR),DMCB,(R8),RR=RELO                                       
*                                                                               
*                                  RECAP TOTALS                                 
         MVI   RCSUBPRG,10                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    RTOTS(44),RTOTS                                                  
*                                                                               
*                                                                               
         L     R2,BSPARS                                                        
         L     R1,BSNUM                                                         
         MH    R1,BSLEN+2                                                       
         LA    R3,0(R2,R1)                                                      
         MVI   0(R3),X'FF'         SET EOL                                      
*                                                                               
RL8      DS    0H                                                               
         USING STOTSD,R2                                                        
         CLI   STMED,X'FF'                                                      
         BE    RL40                                                             
         CLI   STMED,0                                                          
         BNE   RL9                                                              
         MVC   TTOTS,STOTS         TOTAL TOTALS                                 
         L     R1,TDLLRS                                                        
         M     R0,=F'100'                                                       
         L     RF,TTOTBUYS                                                      
         BAS   RE,DIV                                                           
         ST    R1,TCPB                                                          
         B     RL20                                                             
RL9      DS    0H                                                               
         CLI   STAGY,0                                                          
         BNE   RL12                                                             
         OC    MTOTBUYS,MTOTBUYS                                                
         BZ    RL10                                                             
         MVC   RTOTS,MTOTS                                                      
         BAS   RE,RCFMT            PRINT LAST MEDIA TOTS                        
RL10     DS    0H                                                               
         MVC   MTOTS,STOTS         SAVE THESE MEDIA                             
         L     R1,MDLLRS                                                        
         M     R0,=F'100'                                                       
         L     RF,MTOTBUYS                                                      
         BAS   RE,DIV                                                           
         ST    R1,MCPB                                                          
         B     RL20                                                             
*                                                                               
RL12     DS    0H                                                               
         MVC   RTOTS,STOTS                                                      
         BAS   RE,RCFMT            PRINT THESE AG/M TOTS                        
*                                                                               
RL20     DS    0H                                                               
         A     R2,BSLEN                                                         
         B     RL8                 NEXT                                         
*                                                                               
RL40     DS    0H                                                               
         MVC   RTOTS,MTOTS                                                      
         BAS   RE,RCFMT                                                         
         MVC   RTOTS,TTOTS                                                      
         XC    MTOTS,MTOTS                                                      
         XC    MCPB,MCPB                                                        
         BAS   RE,RCFMT            PRINT LAST                                   
*                                                                               
         DROP  R2                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
CLEAR    DS    0H                                                               
         LR    R0,RE                                                            
         LR    RE,R2                                                            
         LH    RF,=Y(PPSTATWX-PPSTATWK)                                         
         XCEF                                                                   
*                                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   MYELCODE,0(R2)                                                   
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
ROLLUP   DS    0H                                                               
         LA    R4,PPSFWDS                                                       
RU2      DS    0H                                                               
         L     R0,0(R3)                                                         
         A     R0,0(R2)                                                         
         ST    R0,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,RU2                                                           
         LH    R4,=Y(PPSHWDS)                                                   
RU4      DS    0H                                                               
         LH    R0,0(R3)                                                         
         AH    R0,0(R2)                                                         
         STH   R0,0(R3)                                                         
         LA    R2,2(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,RU4                                                           
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
DIV      DS    0H                                                               
         SLDA  R0,1                                                             
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
SAVTOTS  NTR1                                                                   
         SPACE 2                                                                
         LA    R6,WORK                                                          
         USING STOTSD,R6                                                        
         XC    STOTS,STOTS                                                      
         MVC   STMED,QMEDIA                                                     
         MVC   STAGY,QAGENCY                                                    
         MVC   STOTBUYS,TOTBUYS                                                 
         MVC   SDLLRS,DLLRS                                                     
*                                                                               
         BAS   R9,SAVUP                                                         
         XC    STAGY,STAGY                                                      
         BAS   R9,SAVUP                                                         
         MVI   STMED,0                                                          
         BAS   R9,SAVUP                                                         
         B     EXIT                                                             
         SPACE 2                                                                
SAVUP    DS    0H                                                               
         ST    R6,BSPARS                                                        
         MVI   BSPARS,1            SET TO ADD                                   
         GOTO1 VBINSRCH,BSPARS                                                  
*                                                                               
         CLI   BSPARS,1                 TEST ALREADY THERE                      
         BER   R9                       NO - RETURN                             
*                                       YES - ADD IN                            
         L     R5,BSPARS                                                        
         LA    R5,4(R5)                                                         
         LA    R4,4(R6)                                                         
         LA    R3,2                                                             
SAVUP4   DS    0H                                                               
         L     R0,0(R5)                                                         
         A     R0,0(R4)                                                         
         ST    R0,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,SAVUP4                                                        
         BR    R9                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                             PRINT RECAP LINE                                  
RCFMT    NTR1                                                                   
         SPACE 2                                                                
         LA    R8,RTOTS                                                         
         USING STOTSD,R8                                                        
         MVC   P+1(2),STAGY                                                     
         CLI   STAGY,0                                                          
         BNE   *+10                                                             
         MVC   P(3),=C'ALL'                                                     
*                                                                               
         MVC   P+5(1),STMED                                                     
         CLI   STMED,0                                                          
         BNE   *+10                                                             
         MVC   P+4(3),=C'ALL'                                                   
*                                  TOTAL BUYS                                   
         L     R1,STOTBUYS                                                      
         LA    R6,P+8                                                           
         BAS   RE,EDT1                                                          
*                                  PCT OF MEDIA                                 
         M     R0,=F'10000'                                                     
         L     RF,MTOTBUYS                                                      
         BAS   RE,DIV                                                           
         LA    R6,P+23                                                          
         BAS   RE,EDT2                                                          
*                                  PCT OF TOTAL                                 
         L     R1,STOTBUYS                                                      
         M     R0,=F'10000'                                                     
         L     RF,TTOTBUYS                                                      
         BAS   RE,DIV                                                           
         LA    R6,P+31                                                          
         BAS   RE,EDT2                                                          
*                                  DOLLARS                                      
         L     R1,SDLLRS                                                        
         LA    R6,P+39                                                          
         BAS   RE,EDT1                                                          
*                                  RCT OF MEDIA                                 
         M     R0,=F'10000'                                                     
         L     RF,MDLLRS                                                        
         BAS   RE,DIV                                                           
         LA    R6,P+54                                                          
         BAS   RE,EDT2                                                          
*                                                                               
*                                  PCT OF TOTAL                                 
         L     R1,SDLLRS                                                        
         M     R0,=F'10000'                                                     
         L     RF,TDLLRS                                                        
         BAS   RE,DIV                                                           
         LA    R6,P+62                                                          
         BAS   RE,EDT2                                                          
*                                  COST PER BUY                                 
         L     R1,SDLLRS                                                        
         M     R0,=F'100'                                                       
         L     RF,STOTBUYS                                                      
         BAS   RE,DIV                                                           
         EDIT  (R1),(10,P+70),2,COMMAS=YES                                      
*                                  INDEX US MEDIA                               
         LR    R3,R1                                                            
         M     R0,=F'100'                                                       
         L     RF,MCPB                                                          
         BAS   RE,DIV                                                           
         LA    R6,P+84                                                          
         BAS   RE,EDT3                                                          
*                                  INDEX VS TOTAL                               
         LR    R1,R3                                                            
         M     R0,=F'100'                                                       
         L     RF,TCPB                                                          
         BAS   RE,DIV                                                           
         LA    R6,P+94                                                          
         BAS   RE,EDT3                                                          
*                                                                               
         CLI   STAGY,0                                                          
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
EDT1     DS    0H                                                               
         EDIT  (R1),(13,(R6)),COMMAS=YES                                        
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
EDT2     DS    0H                                                               
         LTR   R1,R1                                                            
         BZR   RE                                                               
         EDIT  (R1),(6,(R6)),2                                                  
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
EDT3     DS    0H                                                               
         EDIT  (R1),(5,(R6))                                                    
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
ELTAB    DC    X'90'               PBU UPLOAD                                   
         DC    X'44'               ADDITIONAL CHARGE                            
         DC    X'CC'               CUSTOM COLUMN                                
         DC    X'50'               OLD MATCH                                    
         DC    X'51'               NEW INV. MATCH                               
         DC    X'72'               ENHANCED SPACE RESERVATION                   
         DC    X'A9'               PURCHASE ORDER                               
         DC    X'71'               WEB I/O                                      
         DC    X'70'               NORMAL I/O                                   
         DC    X'45'               PLANNED COST (NEW)                           
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
*                                                                               
*        TABLE OF TEST AGENCIES                                                 
*                                                                               
TSTATAB  DC    C'SJ'                                                            
         DC    C'HD'                                                            
         DC    C'H1'                                                            
         DC    C'ED'                                                            
         DC    C'XD'                                                            
         DC    C'T1'                                                            
         DC    C'T2'                                                            
         DC    C'T3'                                                            
         DC    C'T9'                                                            
         DC    C'TC'                                                            
         DC    C'TG'                                                            
         DC    C'FG'                                                            
         DC    C'SX'                                                            
         DC    C'SW'                                                            
         DC    C'WJ'                                                            
         DC    C'*B'                                                            
         DC    C'W+'                METEST                                      
*                                                                               
         DC    C'JQ'                INACTIVE                                    
         DC    C'M4'                INACTIVE                                    
         DC    C'SE'                INACTIVE                                    
         DC    X'FF'                                                            
*                                                                               
         DROP  R9                                                               
*                                  PRINT OUT TOTALS                             
         EJECT                                                                  
TOTR     CSECT                                                                  
         NMOD1 0,TOTR                                                           
         SPACE 2                                                                
         USING PPWORKD,RA                                                       
         USING PPFILED,RC                                                       
         USING PPSTATWK,R8                                                      
         USING PP99WRK,R7                                                       
         L     RC,PPFILEC                                                       
         LA    R9,TOTR+4095                                                     
         LA    R9,1(R9)                                                         
         USING TOTR+4096,R9       SECOND BASE REGISTER                          
         SPACE 2                                                                
         L     R8,0(R1)                                                         
         OC    TOTBUYS,TOTBUYS                                                  
         BZ    TEXIT                                                            
         SPACE 2                                                                
         MVI   RPTACT,C'Y'                                                      
         MVC   P(21),=C'BUY RECORD STATISTICS'                                  
         MVC   PSECOND(21),MYDASHS                                              
         MVI   SPACING,2                                                        
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,RPRT                                                          
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 PFMT,PFPARS,TOTBUYS,TOTBUYS,0,=C'TOTAL BUYS'                     
*                                                                               
         L     R2,TOTBUYS                                                       
         S     R2,CLSOUTS                                                       
         S     R2,DELETS                                                        
         ST    R2,PFFULL                                                        
         GOTO1 (RF),(R1),PFFULL,TOTBUYS,0,=C'ACTIVE BUYS'                       
*                                                                               
         GOTO1 (RF),(R1),CLSOUTS,,0,=C'CLOSE OUTS'                              
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),DELETS,,0,=C'DELETES'                                  
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),ADBBUYS,TOTBUYS,0,=C'ADDED VIA ADBUYER'                
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),IDKBUYS,TOTBUYS,0,=C'ADDED VIA IDESK'                  
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),SFMBUYS,TOTBUYS,0,=C'SFM COPY/MOVE'                    
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),PBUBUYS,TOTBUYS,0,=C'ADDED VIA PBU UPLOAD'             
*                                                                               
         L     R2,TOTBUYS                                                       
         S     R2,ADBBUYS                                                       
         S     R2,IDKBUYS                                                       
         S     R2,SFMBUYS                                                       
         S     R2,PBUBUYS                                                       
         ST    R2,PFFULL                                                        
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),PFFULL,TOTBUYS,0,=C'ADDED VIA BUY PROG.'               
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),STWBUYS,TOTBUYS,0,=C'STEWARDSHIP BUY'                  
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),ADDCHG,TOTBUYS,0,=C'HAVE ADDTNL CHARGES'               
*                                                                               
         MVI   SPACING,2                                                        
         CLI   QOPT1-1,C'I'      SEE IF DOING IDESK ONLY REPORT                 
         BNE   TOTR10                                                           
         GOTO1 (RF),(R1),CUSCOL,TOTBUYS,0,=C'ADDED VIA IDESK3'                  
         B     TOTR15                                                           
*                                                                               
TOTR10   GOTO1 (RF),(R1),CUSCOL,TOTBUYS,0,=C'HAVE CUSTOM COLUMNS'               
*                                                                               
TOTR15   MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),OLDMAT,TOTBUYS,0,=C'USED OLD MATCH'                    
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),NEWMAT,TOTBUYS,0,=C'USED NEW INV MATCH'                
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),ESRES,TOTBUYS,0,=C'ENHANCED SPACE RES.'                
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),PONUM,TOTBUYS,0,=C'HAS PURCHASE ORDER'                 
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),WEBION,TOTBUYS,0,=C'HAD A WEB I/O'                     
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),NORMION,TOTBUYS,0,=C'HAD AN INSOR I/O'                 
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),FAXION,TOTBUYS,0,=C'HAD A FAXED I/O'                   
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),TAION,TOTBUYS,0,=C'HAD AUTO T/A I/O'                   
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),PLANCST,TOTBUYS,0,=C'NEW PLANNED COST'                 
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),ADIDS,TOTBUYS,0,=C'ADID ALONE'                         
*                                                                               
         CLI   QOPT6,C'B'          SEE IF SKIPPING TO COST                      
         BE    CPBANAL                                                          
         CLI   SVOPTS+5,C'B'                                                    
         BE    CPBANAL                                                          
*                                                                               
         CLI   QOPT6,C'X'         SKIPPING DETAIL ANALYSIS                      
         BE    TEXIT                                                            
         CLI   SVOPTS+5,C'X'                                                    
         BE    TEXIT                                                            
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),BYTES,0,0,=C'TOTAL BUYREC BYTES'                       
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),(C'D',BYTES),TOTBUYS,0,=C'AVE. BYTES/RECORD'           
*                                                                               
         MVC   P(14),=C'DETAIL BILLED-'                                         
         BAS   RE,RPRT                                                          
         GOTO1 (RF),(R1),BILLD0,TOTBUYS,0,=C'  0 TIMES'                         
*                                                                               
         L     R2,TOTBUYS                                                       
         S     R2,BILLD0                                                        
         ST    R2,PFFULL                                                        
         GOTO1 (RF),(R1),PFFULL,,PFFULL,=C'  1+'                                
*                                                                               
*                                                                               
         GOTO1 (RF),(R1),BILLD1,,,=C'  1'                                       
*                                                                               
         GOTO1 (RF),(R1),BILLD2,,,=C'  2'                                       
*                                                                               
         GOTO1 (RF),(R1),BILLD3,,,=C'  3'                                       
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),BILLD4,,,=C'  4+'                                      
*                                                                               
         GOTO1 (RF),(R1),BILDP,,BILDP,=C'BILL DATE PASSED'                      
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),BILDPU,,,=C'  NOT DETAIL BILLED'                       
*                                                                               
*                                                                               
         MVC   P(05),=C'PAID-'                                                  
         BAS   RE,RPRT                                                          
*                                                                               
         GOTO1 (RF),(R1),PAYD0,TOTBUYS,0,=C'  0 TIMES'                          
*                                                                               
         L     R2,TOTBUYS                                                       
         S    R2,PAYD0                                                          
         ST    R2,PFFULL                                                        
         GOTO1 (RF),(R1),PFFULL,,PFFULL,=C'  1+'                                
*                                                                               
         GOTO1 (RF),(R1),PAYD1,,,=C'  1'                                        
*                                                                               
         GOTO1 (RF),(R1),PAYD2,,,=C'  2'                                        
*                                                                               
         GOTO1 (RF),(R1),PAYD3,,,=C'  3'                                        
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),PAYD4,,,=C'  4+'                                       
*                                                                               
         MVI   SPACING,2                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(18),=C'ZZZ BUY STATISTICS'                                     
         MVC   PSECOND(18),MYDASHS                                              
         BAS   RE,RPRT                                                          
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),ZBUYS,ZBUYS,TOTBUYS,=C'ZZZ BUYS'                       
*                                                                               
         MVC   P(18),=C'NUMBER OF PRODUCTS'                                     
         BAS   RE,RPRT                                                          
         GOTO1 (RF),(R1),ZPRDS1,,0,=C'   1'                                     
*                                                                               
         GOTO1 (RF),(R1),ZPRDS2,,,=C'   2'                                      
*                                                                               
         GOTO1 (RF),(R1),ZPRDS3,,,=C'   3'                                      
*                                                                               
         GOTO1 (RF),(R1),ZPRDS4,,,=C'   4'                                      
*                                                                               
         GOTO1 (RF),(R1),ZPRDS5,,,=C'   5'                                      
*                                                                               
         GOTO1 (RF),(R1),ZPRDS6,,,=C'   6'                                      
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),ZPRDS7,,,=C'   7+'                                     
*                                                                               
         GOTO1 (RF),(R1),ZUNEQS,,,=C'WITH UNEQUAL ALLOCATIONS'                  
*                                                                               
         MVI   SPACING,3                                                        
         GOTO1 (RF),(R1),ZCSUNEQS,,,=C'COST/SPACE UNEQUAL'                      
*                                                                               
*                                                                               
         MVI   SPACING,2                                                        
         MVC   P(08),=C'WEB I/OS'                                               
         CLI   QOPT5,C'W'                                                       
         BE    *+10                                                             
         MVC   P(16),=C'INSERTION ORDERS'                                       
         MVC   PSECOND(16),MYDASHS                                              
         BAS   RE,RPRT                                                          
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),TOTIOS,TOTIOS,0,(10,=C'TOTAL IO''S')                   
*                                                                               
         GOTO1 (RF),(R1),FAXIOS,TOTIOS,0,(10,=C'FAXED IO''S')                   
*                                                                               
         GOTO1 (RF),(R1),NEWIOS,TOTIOS,0,=C'NEW'                                
*                                                                               
         GOTO1 (RF),(R1),CHGIOS,,0,=C'CHANGES'                                  
*                                                                               
         GOTO1 (RF),(R1),CANIOS,,0,=C'CANCELLATIONS'                            
*                                                                               
         GOTO1 (RF),(R1),UCHIOS,,0,=C'UNCHANGED'                                
*                                                                               
         GOTO1 (RF),(R1),AUTIOS,,0,=C'AUTO GENERATED'                           
*                                                                               
         GOTO1 (RF),(R1),REQIOS,,0,=C'REQUESTED'                                
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),REMIOS,,0,=C'REMOTE PRINTED'                           
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),IGNIOS,,0,=C'WITH IGNORE DATE'                         
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),TOTBUYS,TOTBUYS,0,=C'TOTAL BUYS'                       
*                                                                               
         MVC   P(13),=C'IO''S PER BUY-'                                         
         BAS   RE,RPRT                                                          
         GOTO1 (RF),(R1),NIOS0,TOTBUYS,0,=C'  0'                                
*                                                                               
         L     R2,TOTBUYS                                                       
         S     R2,NIOS0                                                         
         ST    R2,PFFULL                                                        
         GOTO1 (RF),(R1),PFFULL,,PFFULL,=C'  1+'                                
*                                                                               
*                                                                               
         GOTO1 (RF),(R1),NIOS1,,,=C'  1'                                        
*                                                                               
         GOTO1 (RF),(R1),NIOS2,,,=C'  2'                                        
*                                                                               
         GOTO1 (RF),(R1),NIOS3,,,=C'  3'                                        
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),NIOS4,,,=C'  4+'                                       
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),(C'D',TOTIOS),PFFULL,0,=C'AVE. PER BUY W/IO'           
*                                                                               
*                                                                               
*                                                                               
CPBANAL  MVC   P(21),=C'COST PER BUY ANALYSIS'                                  
         MVC   PSECOND(21),MYDASHS                                              
         MVI   SPACING,2                                                        
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,RPRT                                                          
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),DLLRS,0,0,=C'TOTAL DOLLARS'                            
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),TOTBUYS,0,0,=C'TOTAL BUYS'                             
*                                                                               
         GOTO1 (RF),(R1),(C'D',DLLRS),TOTBUYS,0,=C'AVE. COST PER BUY'           
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 (RF),(R1),(C'D',DLLRS),BYTES,0,=C'AVE. COST PER BYTE'            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),NEGBUYS                                                
         GOTO1 (RF),(R1),FULL,TOTBUYS,0,=C'BUYS WITH NEGATIVE COST'             
*                                                                               
         MVI   SPACING,2                                                        
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),ZERBUYS                                                
         GOTO1 (RF),(R1),,,0,=C'BUYS WITH ZERO COST'                            
*                                                                               
         SPACE 3                                                                
*                                  COST RANGES                                  
         CLI   QOPT6,C'C'          SKIP?                                        
         BE    TEXIT                                                            
         CLI   QOPT6,C'B'          SKIP?                                        
         BE    TEXIT                                                            
         CLI   QOPT6,C'X'          SKIP?                                        
         BE    TEXIT                                                            
*                                                                               
         CLI   SVOPTS+5,C'C'       NEEDED FOR RUNLAST CALL                      
         BE    TEXIT                                                            
         CLI   SVOPTS+5,C'B'       NEEDED FOR RUNLAST CALL                      
         BE    TEXIT                                                            
         CLI   SVOPTS+5,C'X'       NEEDED FOR RUNLAST CALL                      
         BE    TEXIT                                                            
*                                                                               
         LA    R2,P                                                             
         MVC   000(54,R2),=C'      RANGE             BUYS    PCT   APPRX        
               OX. $    PCT'                 --                                 
         MVC   132(54,R2),=C'----------------       -----   ----  -----X        
               -----  -----'                                                    
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         L     RF,TOTBUYS                                                       
         SH    RF,NEGBUYS                                                       
         SH    RF,ZERBUYS                                                       
         ST    RF,POSBUYS          POSITIVE COST BUYS                           
         BZ    TEXIT                                                            
*                                                                               
         XC    NPDIV,NPDIV                                                      
         XC    NPTOT,NPTOT                                                      
         XC    NPHAV,NPHAV                                                      
*                                                                               
         LA    R2,BUYCOSTS                                                      
         ST    R2,DIVSTRT          START OF THIS RANGE                          
TR8      DS    0H                                                               
         L     R1,NPDIV                                                         
         A     R1,POSBUYS                                                       
         ST    R1,NPDIV                                                         
         SR    R0,R0                                                            
         LH    RF,NDIVS                                                         
         BAS   RE,TDIV                                                          
         ST    R1,NPTOT            RUNNING GOAL                                 
         SR    R5,R5                                                            
         SR    R4,R4                                                            
         L     R2,DIVSTRT                                                       
TR10     DS    0H                                                               
         LH    R6,0(R2)                                                         
         AR    R4,R6                                                            
         LR    R3,R2                                                            
         LA    R0,BUYCOSTS                                                      
         SR    R3,R0                                                            
         SRL   R3,1                /2                                           
         MH    R3,=Y(RDIV)                                                      
         LA    R3,RDIV/2(R3)                                                    
         LR    R1,R6                                                            
         MR    R0,R3                                                            
         AR    R5,R1                                                            
*                                                                               
         LR    R0,R4                                                            
         A     R0,NPHAV                                                         
         C     R0,NPTOT                                                         
         BNL   TR12                                                             
         LA    R2,2(R2)                                                         
         B     TR10                                                             
*                                                                               
TR12     DS    0H                                                               
         LR    R0,R4                                                            
         A     R0,NPHAV                                                         
         ST    R0,NPHAV            RUNNING TOTAL                                
         MVI   P+8,C'-'                                                         
         LA    R3,(RDIV/2)-1(R3)   END OF RANGE                                 
         EDIT  (R3),(7,P+9),COMMAS=YES                                          
*                                                                               
         L     R0,DIVSTRT                                                       
         LA    R2,2(R2)                                                         
         ST    R2,DIVSTRT                                                       
         LR    R3,R0                                                            
         LA    R0,BUYCOSTS                                                      
         SR    R3,R0                                                            
         SRL   R3,1                                                             
         MH    R3,=Y(RDIV)                                                      
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,1                                                             
         EDIT  (R3),(8,P),COMMAS=YES,FLOAT=$                                    
*                                                                               
         EDIT  (R4),(8,P+20),COMMAS=YES                                         
*                                                                               
         EDIT  (R5),(10,P+37),COMMAS=YES                                        
*                                                                               
         LR    R1,R4                                                            
         M     R0,=F'10000'                                                     
         L     RF,POSBUYS                                                       
         BAS   RE,TDIV                                                          
         EDIT  (R1),(6,P+29),2                                                  
*                                                                               
         LR    R1,R5                                                            
         M     R0,=F'10000'                                                     
         L     RF,DLLRS                                                         
         BAS   RE,TDIV                                                          
         EDIT  (R1),(6,P+48),2                                                  
*                                                                               
         BAS   RE,RPRT                                                          
         CLC   NPHAV,POSBUYS                                                    
         BL    TR8                                                              
*                                                                               
TR16     DS    0H                                                               
         EDIT  (B4,POSBUYS),(10,P+18),COMMAS=YES                                
*                                                                               
         MVC   P+29(6),=C'100.00'                                               
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
*                                  MEDIAN                                       
         L     RF,POSBUYS                                                       
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
*                                                                               
         LA    R2,BUYCOSTS                                                      
         SR    R4,R4                                                            
TR17     DS    0H                                                               
         AH    R4,0(R2)                                                         
         CR    R4,RF                                                            
         BNL   TR18                                                             
         LA    R2,2(R2)                                                         
         B     TR17                                                             
*                                                                               
TR18     DS    0H                                                               
         LA    R0,BUYCOSTS                                                      
         SR    R2,R0                                                            
         SRL   R2,1                                                             
         MH    R2,=Y(RDIV)                                                      
         LA    R2,RDIV/2(R2)                                                    
         ST    R2,PFFULL                                                        
         GOTO1 PFMT,PFPARS,PFFULL,0,0,=C'MEDIAN COST'                           
*                                                                               
*                                                                               
TEXIT    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
TDIV     DS    0H                                                               
         SLDA  R0,1                                                             
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
PFMT     NTR1                                                                   
         SPACE 2                                                                
         LM    R2,R5,0(R1)                                                      
*                                  R2 = A(NUMBER)                               
*                                  R3 = A(PCT BASE) - IF ANY                    
*                                  R4 = (2ND PCT BASE)                          
*                                  R5 = A(DESC)                                 
         SR    RF,RF                                                            
         IC    RF,12(R1)           RF = LENGTH OF DESC                          
         CH    RF,=H'24'                                                        
         BNH   *+8                                                              
         LA    RF,24                                                            
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         CLI   0(R1),C'D'                                                       
         BE    PFMT4                                                            
         L     R2,0(R2)                                                         
         LTR   R2,R2                                                            
         BNZ   *+12                                                             
         MVI   P+37,C'0'                                                        
         B     PFMT2                                                            
         EDIT  (R2),(13,P+25),COMMAS=YES                                        
*                                                                               
PFMT2    DS    0H                                                               
         LTR   R3,R3                                                            
         BZ    PFMT3               NO 1ST PCT                                   
         LR    R1,R2                                                            
         M     R0,=F'10000'                                                     
         L     RF,0(R3)                                                         
         BAS   RE,TDIV                                                          
         EDIT  (R1),(6,P+40),2                                                  
*                                                                               
*                                                                               
PFMT3    DS    0H                                                               
         LTR   R4,R4                                                            
         BZ    PFMT20              NO 2ND PCT                                   
         LR    R1,R2                                                            
         M     R0,=F'10000'                                                     
         L     RF,0(R4)                                                         
         BAS   RE,TDIV                                                          
         EDIT  (R1),(6,P+48),2                                                  
*                                                                               
         B     PFMT20                                                           
PFMT4    DS    0H                                                               
         L     R1,0(R2)                                                         
         M     R0,=F'100'                                                       
         L     RF,0(R3)                                                         
         BAS   RE,TDIV                                                          
         EDIT  (R1),(10,P+28),2                                                 
*                                                                               
PFMT20   DS    0H                                                               
         BAS   RE,RPRT                                                          
         B     TEXIT                                                            
         SPACE 3                                                                
RPRT     NTR1                                                                   
         SPACE 2                                                                
         CLI   RCALLAGY,C'Y'                                                    
         BNE   RPRT5                                                            
         MVC   HEAD4+60(5),=C'PRINT'                                            
         MVC   HEAD4+66(2),SYSNAID                                              
*                                                                               
RPRT5    DS    0H                                                               
         MVC   HEAD4(06),=C'AGENCY'                                             
         MVC   HEAD4+9(2),PAGYKAGY                                              
         MVC   HEAD4+13(33),PAGYNAME                                            
*                                                                               
         MVC   HEAD5(05),=C'MEDIA'                                              
         MVC   HEAD5+9(1),PAGYKMED                                              
         MVC   HEAD5+13(10),PAGYMED                                             
*                                                                               
         CLC   QCLIENT,=C'ALL'                                                  
         BE    RPRT12                                                           
         CLC   QCLIENT,SPACES                                                   
         BE    RPRT12                                                           
         CLI   QCLIENT,C'*'         ONE OFFICE?                                 
         BNE   RPRT7                                                            
         MVC   HEAD6(06),=C'OFFICE'                                             
         MVC   HEAD6+09(2),SAVCOFF                                              
         B     RPRT12                                                           
*                                                                               
RPRT7    CLC   QCLIENT(2),=C'$*'    ALL OFFICES IN OFFICE ORDER                 
         BNE   RPRT8                                                            
         MVC   HEAD6(06),=C'OFFICE'                                             
         MVC   HEAD6+09(2),SAVCOFF                                              
         B     RPRT12                                                           
*                                                                               
RPRT8    CLI   QCLIENT,C'$'        OFFICE LIST REQUEST?                         
         BNE   RPRT9                                                            
         MVC   HEAD6(06),=C'OFFICE'                                             
         MVC   HEAD6+09(2),SAVCOFF                                              
         MVC   HEAD6+13(11),=C'OFFICE LIST'                                     
         MVC   HEAD6+25(1),QCLIENT+1                                            
         B     RPRT12                                                           
*                                                                               
RPRT9    DS    0H                  MUST BE ONE CLIENT                           
         MVC   HEAD6(06),=C'CLIENT'                                             
         MVC   HEAD6+09(3),QCLIENT                                              
*                                                                               
RPRT12   DS    0H                                                               
         CLC   PSTART(16),SPACES                                                
         BE    RPRT14                                                           
         MVC   HEAD6+53(8),PSTART                                               
         MVC   HEAD6+62(4),=C'THRU'                                             
         MVC   HEAD6+67(8),PEND                                                 
         MVC   HEAD7+50(29),=C'PERIOD BUYS ADDED TO THE FILE'                   
         CLI   QOPT1-1,C'I'                                                     
         BNE   RPRT14                                                           
         MVC   HEAD7(15),=C'IDESK BUYS ONLY'                                    
*                                                                               
RPRT14   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     TEXIT                                                            
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         DROP  R7                                                               
         DROP  R8                                                               
*                          FOR DOWNLOAD PRINTING (QOPT4 = D)                    
*                          OR PUBLICATION DOWNLOAD (QOPT5 = P)                  
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
*                                                                               
DNP1     DS    0H                                                               
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R8,0(R1)                                                         
         USING PPSTATWK,R8                                                      
         LA    R7,SPACEND                                                       
         USING PP99WRK,R7                                                       
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         CLI   MODE,RUNLAST       SEE IF END OF RUN                             
         BE    DNP70                                                            
*                                                                               
         CLI   MODE,FBUYREQ       SEE IF I NEED TO INTIALIZE                    
         BE    DNP80             ACTUALLY DONE AT RUNFIRST                      
*                                                                               
         MVC   DNLINE,P          SAVE CONTENTS OF PRINTLINE                     
         MVC   P,SPACES                                                         
         CLI   DHEADIND,1        ALREADY DONE?                                  
         BE    DNP1HX                                                           
*                                                                               
DNP1H1   DS    0H                DOWNLOAD HEADERS (FIRST LINE)                  
*                                                                               
         MVC   HALF,=H'4'        THIS LOOKS WRONG BUT WORKS?                    
         CLC   QCLIENT,=C'ALL'                                                  
         BNE   *+10                                                             
         MVC   HALF,=H'3'         NO OFFICE FIELD FOR ALL CLTS                  
         BAS   RE,EMPTYF                                                        
*                                                                               
         MVC   DLCBFLD(5),=C'TOTAL'                                             
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(2),=C'AB'                                                
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'PBU'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(5),=C'IDESK'                                             
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'SFM'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'BUY'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        STEWARDSHIP COUNT NO-OPED                                              
*                                                                               
***      MVC   DLCBFLD(11),=C'STEWARDSHIP'                                      
***      MVI   DLCBTYP,C'T'      TEXT FIELD                                     
***      MVI   DLCBACT,DLCBPUT                                                  
***      GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(5),=C'ADD"L'                                             
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'CUST'                                             
         CLI   QOPT1-1,C'I'       SEE IF DOINF IDESK ONLY REPORT                
         BNE   *+10                                                             
         MVC   DLCBFLD(6),=C'IDESK3'                                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(05),=C'MATCH'                                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(02),=C'AB'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   HALF,=H'1'       (OVER ESR)                                      
         BAS   RE,EMPTYF                                                        
*                                                                               
*        PURCHASE ORDER COUNT NO-OPED                                           
*                                                                               
***      MVC   DLCBFLD(08),=C'PURCHASE'                                         
***      MVI   DLCBTYP,C'T'      TEXT FIELD                                     
***      MVI   DLCBACT,DLCBPUT                                                  
***      GOTO1 VDLFLD                                                           
*                                                                               
         MVC   HALF,=H'1'       (OVER EIO)                                      
         BAS   RE,EMPTYF                                                        
*                                                                               
         MVC   DLCBFLD(05),=C'INSOR'                                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(03),=C'FAX'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
***      MVC   HALF,=H'3'                                                       
***      BAS   RE,EMPTYF                                                        
*                                                                               
**FUT    MVC   DLCBFLD(07),=C'PLANNED'                                          
**FUT    MVI   DLCBTYP,C'T'      TEXT FIELD                                     
**FUT    MVI   DLCBACT,DLCBPUT                                                  
**FUT    GOTO1 VDLFLD                                                           
**FUT                                                                           
**FUT    MVC   DLCBFLD(05),=C'AD-ID'                                            
**FUT    MVI   DLCBTYP,C'T'      TEXT FIELD                                     
**FUT    MVI   DLCBACT,DLCBPUT                                                  
**FUT    GOTO1 VDLFLD                                                           
*                                                                               
*                                                                               
         MVI   DLCBACT,DLCBEOL    END OF LINE                                   
         GOTO1 VDLFLD                                                           
*                                                                               
DNP1H2   DS    0H                DOWNLOAD HEADERS (SECOND LINE)                 
*                                                                               
         MVC   DLCBFLD(3),=C'AGY'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'NAME'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'TEAM'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLC   QCLIENT,=C'ALL'    NO OFFICE FOR CLT=ALL                         
         BE    DNP1H5                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'OFF'    OTHERWISE SHOW OFFICE                      
         CLI   ONECLT,C'Y'           ONE CLIENT REQUEST?                        
         BNE   *+10                                                             
         MVC   DLCBFLD(6),=C'CLIENT' THEN SHOW CLIENT                           
*                                                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP1H5   MVC   DLCBFLD(3),=C'MED'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'BUYS'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'BUYS'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'BUYS'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'BUYS'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'BUYS'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'PROG'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        STEWARDSHIP COUNT NO-OPED                                              
*                                                                               
***      MVC   DLCBFLD(4),=C'BUYS'                                              
***      MVI   DLCBTYP,C'T'      TEXT FIELD                                     
***      MVI   DLCBACT,DLCBPUT                                                  
***      GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'CHGS'                                             
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'COLS'                                             
         CLI   QOPT1-1,C'I'      SEE IF DOING IDESK ONLY REPORT                 
         BNE   *+10                                                             
         MVC   DLCBFLD(4),=C'BUYS'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(03),=C'INV'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(03),=C'INV'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(03),=C'ESR'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        PURCHASE ORDER COUNT NO-OPED                                           
*                                                                               
***      MVC   DLCBFLD(05),=C'ORDER'                                            
***      MVI   DLCBTYP,C'T'      TEXT FIELD                                     
***      MVI   DLCBACT,DLCBPUT                                                  
***      GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(03),=C'EIO'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(02),=C'IO'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(02),=C'IO'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
**FUT    MVC   DLCBFLD(04),=C'COST'                                             
**FUT    MVI   DLCBTYP,C'T'      TEXT FIELD                                     
**FUT    MVI   DLCBACT,DLCBPUT                                                  
**FUT    GOTO1 VDLFLD                                                           
**FUT                                                                           
**FUT    MVC   DLCBFLD(05),=C'ALONE'                                            
**FUT    MVI   DLCBTYP,C'T'      TEXT FIELD                                     
**FUT    MVI   DLCBACT,DLCBPUT                                                  
**FUT    GOTO1 VDLFLD                                                           
**FUT                                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    END OF LINE                                   
         GOTO1 VDLFLD                                                           
         MVI   DHEADIND,1         SET HEADERS SENT                              
*                                                                               
DNP1HX   DS    0H                DATA FIELDS                                    
*                                                                               
*        FILE TOTALS SKIPPED FOR DOWNLOAD                                       
*                                                                               
         CLC   PAGYNAME(5),=C'ALL  '                                            
         BE    DNPX                      IGNORE (FOR NOW)                       
*                                                                               
         MVC   DLCBFLD(2),PAGYKAGY                                              
***      CLC   PAGYNAME(5),=C'ALL  '     FILE TOTAL                             
***      BNE   *+10                                                             
***      MVC   DLCBFLD(3),=C'ALL'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(20),PAGYNAME                                             
***      CLC   PAGYNAME(5),=C'ALL  '     FILE TOTAL                             
***      BNE   *+10                                                             
***      MVC   DLCBFLD(20),=CL20'ALL'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(L'SVTEAM),SVTEAM  CLIENT SERVICE TEAM                    
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLC   QCLIENT,=C'ALL'    NO OFFICE FOR CLT=ALL                         
         BE    DNP20                                                            
*                                                                               
         MVC   DLCBFLD(2),SAVCOFF  OFFICE                                       
         CLI   ONECLT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   DLCBFLD(3),QCLIENT                                               
                                                                                
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP20    MVC   DLCBFLD(1),PAGYKMED                                              
         CLC   PAGYNAME(5),=C'ALL  '     FILE TOTAL                             
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'ALL'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  TOTBUYS,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                      
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  ADBBUYS,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                      
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  PBUBUYS,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                      
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  IDKBUYS,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                      
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  SFMBUYS,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                      
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         L     R0,TOTBUYS                                                       
         S     R0,ADBBUYS                                                       
         S     R0,PBUBUYS                                                       
         S     R0,IDKBUYS                                                       
         S     R0,SFMBUYS                                                       
         ST    R0,MYFULL                                                        
         EDIT  MYFULL,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                       
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        STEWARDHIP BUY COUNT NO-OPED                                           
*                                                                               
***      ST    R1,SAVER1                                                        
***      EDIT  STWBUYS,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                      
***      L     R1,SAVER1                                                        
***      MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
***      MVI   DLCBACT,DLCBPUT                                                  
***      GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  ADDCHG,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                       
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  CUSCOL,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                       
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  OLDMAT,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                       
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  NEWMAT,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                       
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  ESRES,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                        
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        PURCHASE ORDER COUNT NO-OPED                                           
*                                                                               
***      ST    R1,SAVER1                                                        
***      EDIT  PONUM,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                        
***      L     R1,SAVER1                                                        
***      MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
***      MVI   DLCBACT,DLCBPUT                                                  
***      GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  WEBION,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                       
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  NORMION,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                      
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  FAXION,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                       
         L     R1,SAVER1                                                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
**FUT    ST    R1,SAVER1                                                        
**FUT    EDIT  PLANCST,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                      
**FUT    L     R1,SAVER1                                                        
**FUT    MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
**FUT    MVI   DLCBACT,DLCBPUT                                                  
**FUT    GOTO1 VDLFLD                                                           
**FUT                                                                           
**FUT    ST    R1,SAVER1                                                        
**FUT    EDIT  ADIDS,(07,DLCBFLD),MINUS=YES,ZERO=NOBLANK                        
**FUT    L     R1,SAVER1                                                        
**FUT    MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
**FUT    MVI   DLCBACT,DLCBPUT                                                  
**FUT    GOTO1 VDLFLD                                                           
*                                                                               
DNP60    DS    0H                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
DNP70    DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'R'        SET END OF REPORT                            
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
DNP80    DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD                                                           
DNPX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
DNPRINT  NTR1                                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,0                                                        
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         XIT1                                                                   
*                                                                               
EMPTYF   NTR1                                                                   
         LH    R5,HALF                                                          
EMPT5    MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         BCT   R5,EMPT5                                                         
         XIT1                                                                   
*                                SYSTEM FIELD - EMPTY                           
         LTORG                                                                  
DMTHSW   DS    CL1         M IF PROCESSING MOS LINE                             
SAVER1   DS    F                                                                
MYWORK   DS    CL12                                                             
DNLINE   DS    CL132                                                            
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
*                                                                               
DLCB     DS    XL256       DOWNLOAD BLOCK                                       
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
         SPACE 3                                                                
PP99WRK  DSECT                                                                  
*                                                                               
POSBUYS  DS    F                                                                
MYFULL   DS    F                                                                
NPDIV    DS    F                                                                
NPHAV    DS    F                                                                
NPTOT    DS    F                                                                
DIVSTRT  DS    A                                                                
ATOTS1   DS    A                                                                
ATOTS2   DS    A                                                                
ATOTS3   DS    A                                                                
VDLFLD   DS    A                                                                
VBINSRCH DS    A                                                                
VOFFICER DS    A                                                                
PFPARS   DS    6F                                                               
PFFULL   DS    F                                                                
MYTODAY  DS    CL6                                                              
BTODAY   DS    XL3                                                              
ONECLT   DS    CL1           Y= DOING ONE CLIENT                                
MULTOFF  DS    CL1                                                              
OFFACT   DS    CL1                                                              
RPTACT   DS    CL1           Y IF I PRINTED A REPORT                            
SAVCOFF  DS    CL2                                                              
SVFILE   DS    CL1                                                              
MYELCODE DS    X                                                                
SVTEAM   DS    CL08                                                             
DHEADIND DS    X                                                                
FAXSW    DS    XL1                                                              
TASW     DS    XL1                                                              
SVOPTS   DS    CL7           SAVED QOPTS FOR RLAST                              
MYDASHS  DS    CL25                                                             
PSTART   DC    CL8' '                                                           
PEND     DC    CL8' '                                                           
*                                                                               
MYSTART  DS    XL3          FOR DOWNLOAD - CREATION START DATE FILTER           
MYEND    DS    XL3          FOR DOWNLOAD - CREATION END DATE FILTER             
*                                                                               
NPRDS    DS    F                                                                
*                                                                               
         DS    0F                                                               
BSPARS   DS    0CL24                                                            
         DS    F                                                                
BSTAB    DS    F                                                                
BSNUM    DS    F                                                                
BSLEN    DS    F                                                                
BSKLEN   DS    F                                                                
BSMAX    DS    F                                                                
*                                                                               
RTOTS    DS    0CL12                                                            
         DS    F                                                                
RTOTBUYS DS    F                                                                
RDLLRS   DS    F                                                                
*                                                                               
MTOTS    DS    0CL12                                                            
         DS    F                                                                
MTOTBUYS DS    F                                                                
MDLLRS   DS    F                                                                
*                                                                               
TTOTS    DS    0CL12                                                            
         DS    F                                                                
TTOTBUYS DS    F                                                                
TDLLRS   DS    F                                                                
*                                                                               
MCPB     DS    F                                                                
TCPB     DS    F                                                                
*                                                                               
*                                                                               
NDIVS    DS    H                                                                
RLIM     EQU   120000                                                           
RDIV     EQU   10                                                               
*                                                                               
*                                                                               
         SPACE 3                                                                
TOTS1    CSECT                                                                  
         DS    25000C                                                           
TOTS2    CSECT                                                                  
         DS    25000C                                                           
TOTS3    CSECT                                                                  
         DS    25000C                                                           
STOTSC   CSECT                                                                  
         DS    3600C         12 X 300                                           
         EJECT                                                                  
PPSTATWK DSECT                                                                  
PPSFULLS DS    0F                                                               
TOTBUYS  DS    F                                                                
ADBBUYS  DS    F         ADDED VIA ADBUYER                                      
STWBUYS  DS    F         STEWARDSHIP BUY                                        
IDKBUYS  DS    F         IDESK BUY                                              
SFMBUYS  DS    F         SFM COPY/MOVE BUY                                      
ADIDS    DS    F         ADID ALONE                                             
*                                                                               
*   THE NEXT 10 ACCUMULATORS ARE ADDED TO USING ELEMENT SEARCHES                
*   AT PB1X2                                                                    
*                                                                               
PBUBUYS  DS    F         ADDED VIA PBU UPLOAD                                   
ADDCHG   DS    F         HAVE ADDITIONAL CHARGE                                 
CUSCOL   DS    F         HAVE CUSTOM COLUMN DATA                                
OLDMAT   DS    F         USED OLD MATCH PROGRAM                                 
NEWMAT   DS    F         USED NEW INVOICE MATCH                                 
ESRES    DS    F         ENHANCED SPACE RESERVATION ISSUED                      
PONUM    DS    F         PURCHASE ORDER                                         
WEBION   DS    F         HAD A WEB I/O                                          
NORMION  DS    F         HAD A NORMAL I/O                                       
PLANCST  DS    F         HAD A PLANNED COST ELEMENT                             
*                                                                               
FAXION   DS    F         HAD A FAXED I/O                                        
TAION    DS    F         HAD TURNAROUND I/O                                     
*                                                                               
CLSOUTS  DS    F                                                                
DELETS   DS    F                                                                
DLLRS    DS    F                                                                
BYTES    DS    F                                                                
BILLD0   DS    F                                                                
BILLD1   DS    F                                                                
BILLD2   DS    F                                                                
BILLD3   DS    F                                                                
BILLD4   DS    F                                                                
BILDP    DS    F                                                                
BILDPU   DS    F                                                                
PAYD0    DS    F                                                                
PAYD1    DS    F                                                                
PAYD2    DS    F                                                                
PAYD3    DS    F                                                                
PAYD4    DS    F                                                                
TOTIOS   DS    F                                                                
NEWIOS   DS    F                                                                
CHGIOS   DS    F                                                                
CANIOS   DS    F                                                                
UCHIOS   DS    F                                                                
FAXIOS   DS    F                                                                
AUTIOS   DS    F                                                                
REQIOS   DS    F                                                                
REMIOS   DS    F                                                                
IGNIOS   DS    F                                                                
NIOS0    DS    F                                                                
NIOS1    DS    F                                                                
NIOS2    DS    F                                                                
NIOS3    DS    F                                                                
NIOS4    DS    F                                                                
*                                                                               
ZBUYS    DS    F                                                                
ZPRDS1   DS    F                                                                
ZPRDS2   DS    F                                                                
ZPRDS3   DS    F                                                                
ZPRDS4   DS    F                                                                
ZPRDS5   DS    F                                                                
ZPRDS6   DS    F                                                                
ZPRDS7   DS    F                                                                
ZUNEQS   DS    F                                                                
ZCSUNEQS DS    F                                                                
*                                                                               
PPSFULLX EQU   *                                                                
PPSHALFS DS    0H                                                               
NEGBUYS  DS    H                                                                
ZERBUYS  DS    H                                                                
BUYCOSTS DS    0H                                                               
         ORG   *+2*(RLIM/RDIV)                                                  
BUYCOSTX EQU   *                                                                
PPSHALFX EQU   *                                                                
PPSTATWX EQU   *                                                                
PPSFWDS  EQU   (PPSFULLX-PPSFULLS)/4                                            
PPSHWDS  EQU   (PPSHALFX-PPSHALFS)/2                                            
*                                                                               
STOTSD   DSECT                                                                  
STOTS    DS    0CL12                                                            
STMED    DS    CL1                                                              
STAGY    DS    CL2                                                              
         DS    CL1                                                              
STOTBUYS DS    F                                                                
SDLLRS   DS    F                                                                
         SPACE 2                                                                
*                                                                               
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPPPIDEL                                                       
       ++INCLUDE PPGENBYDK                                                      
       ++INCLUDE PPGENBYCC                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDREPMASTD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050PPREP9902 03/21/14'                                      
         END                                                                    
