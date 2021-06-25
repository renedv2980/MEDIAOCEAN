*          DATA SET REREP9502  AT LEVEL 061 AS OF 05/01/02                      
*          DATA SET REREP9502 AT LEVEL 019 AS OF 08/09/99                       
*PHASE RE9502A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREP9502 - NATIONAL PUBLIC SPOT COUNT REPORT'                  
*********************************************************************           
*                                                                   *           
*        REREP9502 --- NATPUB SPOT COUNT REPORT                     *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* APR18/01 (BU ) --- INITIAL ENTRY:  SPOT COUNT REPORT              *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE9502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE9502,R7,RR=RE                                              
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
*                                                                               
         DC    AL1(REQFRST),AL3(INITIAL)  REQUEST A CONTRACT                    
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*                                                                               
         DC    AL1(PROCCONT),AL3(POST)                                          
*                                                                               
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
*                                                                               
INITIAL  NTR1                                                                   
*                                                                               
**       MVC   P+1(19),=C'RE95 ENTERED       '                                  
**       GOTO1 REPORT                                                           
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         CLC   QSTART,SPACES       START DATE ENTERED?                          
         BH    *+6                 YES                                          
         DC    H'0'                MUST HAVE START DATE                         
         GOTO1 DATCON,DMCB,(0,QSTART),(3,SDATE)                                 
         CLC   QEND,SPACES         END   DATE ENTERED?                          
         BH    *+6                 YES                                          
         DC    H'0'                MUST HAVE START DATE                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,EDATE)                                   
         MVC   QEND+4(2),=C'01'    SET MONTH INTO END DATE                      
*                                                                               
*   ESTABLISH REASONABLE STARTING DATE FOR KEYS                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(3,DATEWORK)                                
         ZIC   RF,DATEWORK         STRIP YEAR FROM 3-CHAR DATE                  
         SH    RF,=H'2'            BACK UP TWO YEARS                            
         STC   RF,DATEWORK         PUT IT BACK                                  
         GOTO1 DATCON,DMCB,(3,DATEWORK),(2,KDATE)                               
*                                                                               
***      MVC   P+1(20),=C'DATES: START/END/KEY'                                 
***      GOTO1 DATCON,DMCB,(3,SDATE),(0,P+23)                                   
***      GOTO1 DATCON,DMCB,(3,EDATE),(0,P+33)                                   
***      GOTO1 DATCON,DMCB,(2,KDATE),(0,P+43)                                   
***      GOTO1 REPORT                                                           
         GOTO1 BLDTABLE            BUILD BROADCAST TABLE                        
         B     MODEEXIT                                                         
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
         EJECT                                                                  
*                                                                               
*                                                                               
POST     NTR1                                                                   
*                                                                               
**       MVC   P+1(19),=C'CONTRACT RETURNED ='                                  
**       MVC   P+20(27),RCONKEY                                                 
**       GOTO1 HEXOUT,DMCB,RCONKCON,P+50,04,=C'TOG'                             
**       GOTO1 REPORT                                                           
*                                                                               
*        L     RF,PULSECTR                                                      
*        LA    RF,1(RF)                                                         
*        ST    RF,PULSECTR         REPLACE COUNTER                              
*        L     RF,TOTALCTR                                                      
*        LA    RF,1(RF)                                                         
*        ST    RF,TOTALCTR         REPLACE COUNTER                              
*        CLC   PULSECTR,=F'5000'                                                
*        BNE   POST0050                                                         
*        XC    PULSECTR,PULSECTR                                                
*        MVC   P+01(10),=C'PROCESSED:'                                          
*        EDIT  TOTALCTR,(10,P+12)                                               
*        MVC   P+24(09),=C'CONTRACTS'                                           
*        MVC   P+36(27),KEY                                                     
*        GOTO1 REPORT                                                           
POST0050 EQU   *                                                                
*                                                                               
         CLC   RCONDATE+3(2),SDATE CONTRACT END VS REQUEST START                
         BL    MODEEXIT            SKIP:  ENDS BEFORE REQUEST DATE              
         CLC   EDATE(2),RCONDATE      REQUEST END VS CONTRACT START             
         BL    MODEEXIT            SKIP: STARTS AFTER REQUEST END               
*                                                                               
*        MVC   P+1(19),=C'CONTRACT RECOVERED='                                  
*        MVC   P+20(27),RCONKEY                                                 
*        GOTO1 HEXOUT,DMCB,RCONKCON,P+50,04,=C'TOG'                             
*        GOTO1 REPORT                                                           
*        MVC   P+1(07),=C'GROUP :'                                              
*        MVC   P+20(27),RGRPKEY                                                 
*        GOTO1 REPORT                                                           
*        MVC   P+1(07),=C'OFFICE:'                                              
*        MVC   P+20(27),ROFFKEY                                                 
*        GOTO1 REPORT                                                           
*        MVC   P+1(08),=C'STATION:'                                             
*        MVC   P+20(27),RSTAKEY                                                 
*        GOTO1 REPORT                                                           
*        MVC   P+1(08),=C'S/P    :'                                             
*        MVC   P+20(27),RSALKEY                                                 
*        GOTO1 REPORT                                                           
*        MVC   P+1(08),=C'AGENCY :'                                             
*        MVC   P+20(27),RAGYKEY                                                 
*        GOTO1 REPORT                                                           
*        MVC   P+1(08),=C'ADVERT :'                                             
*        MVC   P+20(27),RADVKEY                                                 
*        GOTO1 REPORT                                                           
POST0080 EQU   *                                                                
         BAS   RE,SETLINE          SET UP PRINTLINE FOR CONTRACT                
*                                                                               
         BAS   RE,CLEARBUX         CLEAR BUCKETS IN TABLE                       
*                                                                               
         BAS   RE,CYCLBUYS         PROCESS ORDER'S BUYLINES                     
*                                                                               
         LA    R6,MYP                                                           
         USING MYPDSECT,R6                                                      
         LA    R5,BDCSTABL         SET A(TABLE OF MONTHS)                       
POST0090 EQU   *                                                                
         OC    DBSTDATE(4,R5),DBSTDATE(R5)                                      
*                                  ANY ENTRY IN SLOT/END OF TABLE?              
         BZ    POST0800            NO  - END OF TABLE                           
*                                                                               
         OC    DBSPTCTR(8,R5),DBSPTCTR(R5)                                      
*                                  SPOTS OR DOLLARS IN MONTH?                   
         BZ    POST0120            NO  - NOTHING TO DISPLAY                     
         GOTO1 DATCON,DMCB,(3,DBENDATE(R5)),(6,PMONTH)                          
*                                                                               
         EDIT  (4,DBSPTCTR(R5)),(4,PSPTSMO),ZERO=NOBLANK                        
         EDIT  (4,DBDOLCTR(R5)),(12,PCURBLD),2,ZERO=NOBLANK                     
*                                                                               
         CLI   RCDNLOAD,C'Y'       DOWNLOAD REQUEST?                            
         BE    POST0100            YES                                          
         MVC   P(LMYPPT#1),PGROUP                                               
         GOTO1 REPORT                                                           
         MVC   P(LMYPPT#2),PPRDNAME                                             
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     POST0120            GO BACK FOR NEXT TABLE ENTRY                 
POST0100 EQU   *                                                                
         MVC   P(200),MYP          SET INPUT                                    
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
POST0120 EQU   *                                                                
         LA    R5,DBONESET(R5)     BUMP TO NEXT BUCKETS                         
         B     POST0090            GO BACK FOR NEXT TABLE ENTRY                 
*                                                                               
         DROP  R6                                                               
*                                                                               
POST0800 EQU   *                                                                
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        CLEARBUX --- RESET THE TABLE                                           
*                                                                               
CLEARBUX NTR1                                                                   
         LA    R5,BDCSTABL                                                      
         LA    R0,12                                                            
CBUX0020 EQU   *                                                                
         XC    DBSPTCTR(8,R5),DBSPTCTR(R5)                                      
*                                  CLEAR SPOT CTR, DOLLAR CTR                   
         LA    R5,DBONESET(R5)     BUMP TO NEXT SET                             
         BCT   R0,CBUX0020                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        CYCLBUYS --- READ ALL BUYS, TABLE VALUES                               
*                                                                               
CYCLBUYS NTR1                                                                   
*                                                                               
*                                  CONVERT CON# TO REVERSE                      
*                                                                               
*   CALCULATE NEW CONTRACT NUMBER REVERSED FOR BUYLINES                         
*                                                                               
         ZAP   MYWORK+30(5),=P'0'                                               
         MVO   MYWORK+30(5),RCONKCON                                            
         ZAP   MYWORK+25(5),=P'99999999'                                        
         SP    MYWORK+25(5),MYWORK+30(5) GET 9'S COMPLEMENT                     
         MVO   MYWORK+20(5),MYWORK+25(5) CHANGE TO PWOS                         
                                                                                
         PACK  CONRV+0(1),MYWORK+23(1) REVERSED 9'COMP OF K NUM                 
         PACK  CONRV+1(1),MYWORK+22(1)                                          
         PACK  CONRV+2(1),MYWORK+21(1)                                          
         PACK  CONRV+3(1),MYWORK+20(1)                                          
*                                                                               
         MVC   SVCONKEY,KEY        SAVE CONTRACT KEY FOR RESTART                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP FOR FIRST BUY KEY                     
         MVC   KEY+16(2),RCREPFL   INSERT REP CODE INTO KEY                     
         MVC   KEY+18(4),CONRV     INSERT CON#/9 COMP/REV                       
         GOTO1 HIGH                                                             
         B     CYCL0040                                                         
CYCL0020 EQU   *                                                                
         GOTO1 SEQ                                                              
CYCL0040 EQU   *                                                                
*                                                                               
         CLC   KEYSAVE(22),KEY     SAME CONTRACT FOUND?                         
         BNE   CYCL0800            NO  - CONTRACT FINISHED                      
         BAS   RE,BUYPROC          PROCESS THIS BUY RECORD                      
         B     CYCL0020            GO BACK FOR NEXT                             
CYCL0800 EQU   *                                                                
**       MVC   P+1(13),=C'TABLE DISPLAY'                                        
**       GOTO1 REPORT                                                           
*                                                                               
**       LA    R5,BDCSTABL         DISPLAY THE DATE TABLE                       
**       LA    R0,12                                                            
CYCLT080 EQU   *                                                                
**       MVC   P+1(16),0(R5)       UNLOAD FIRST SET OF BUCKETS                  
**       GOTO1 REPORT                                                           
**       LA    R5,DBONESET(R5)     BUMP TO NEXT BUCKETS                         
**       BCT   R0,CYCLT080         GO BACK FOR NEXT ENTRY                       
**                                                                              
         MVC   KEY,SVCONKEY        RESET KEY                                    
         GOTO1 HIGH                RESTART AT CONTRACT KEY                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   BUYPROC:  ANALYSIS OF EACH BUY RECORD, TO INSERT IT INTO THE                
*        MONTHLY TABLE.                                                         
*        EACH EFFECTIVE DATE ELEMENT (X'03') IN THE RECORD WILL                 
*        HAVE EACH WEEK WITHIN ITS DATE RANGE INSERTED INTO THE                 
*        TABLE BY COMPARING THE DATE WITH THE START/END DATES                   
*        IN EACH BUCKET.  WHEN THERE IS A FIT, THE # OF SPOTS AS                
*        WELL AS THE DOLLARS WILL BE ACCUMULATED IN THAT BUCKET.                
*                                                                               
BUYPROC  NTR1                                                                   
***      MVC   P+1(11),=C'BUY RECORD='                                          
***      MVC   P+12(27),KEY                                                     
***      GOTO1 REPORT                                                           
         GOTO1 GETBUY              RETRIEVE THE BUY RECORD                      
         MVC   BUYCOST,RBUYCOS     SAVE BUY COST                                
         LA    R6,RBUYELEM                                                      
BUYP0020 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    BUYP0800            DONE                                         
         CLI   0(R6),X'03'         EFFECTIVE DATE ELEMENT?                      
         BE    BUYP0040            YES - PROCESS IT                             
BUYP0030 EQU   *                                                                
         ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     BUYP0020            GO BACK FOR NEXT                             
BUYP0040 EQU   *                                                                
*                                                                               
***      MVC   P+1(09),=C'03 FOUND:'                                            
***      MVC   P+12(11),0(R6)                                                   
***      GOTO1 REPORT                                                           
*                                                                               
         USING RBUYDTEL,R6                                                      
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,BUYSTDAT)                            
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,BUYENDAT)                            
*                                  SET EBCDIC START/END DATES                   
*                                                                               
***      MVC   P+1(09),=C'03 DATES:'                                            
***      MVC   P+12(12),BUYSTDAT                                                
***      GOTO1 REPORT                                                           
*                                                                               
BUYP0060 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,BUYSTDAT),(3,DATEWORK)                            
*                                  SET WEEK'S DATE TO BINARY                    
         LA    R5,BDCSTABL         SET A(BROADCAST DATE TABLE)                  
BUYP0080 EQU   *                                                                
         OC    DBSTDATE(4,R5),DBSTDATE(R5)                                      
*                                  END OF TABLE REACHED?                        
         BZ    BUYP0200            YES - SKIP ENTRY                             
*                                                                               
***      MVC   P+1(09),=C'COMPARES:'                                            
***      MVC   P+12(03),DATEWORK                                                
***      MVI   P+15,C'/'                                                        
***      MVC   P+16(06),DBSTDATE(R5)                                            
***      MVI   P+22,C'/'                                                        
***      MVC   P+23(06),DBENDATE(R5)                                            
***      GOTO1 REPORT                                                           
*                                                                               
         CLC   DATEWORK(3),DBENDATE(R5) EFF DATE VS BUCKET END  :               
         BNH   BUYP0100                                                         
         LA    R5,DBONESET(R5)     AFTER BUCKET - BUMP TO NEXT                  
         B     BUYP0080            GO BACK AND CHECK                            
BUYP0100 EQU   *                                                                
         CLC   DATEWORK(3),DBSTDATE(R5) EFF DATE VS BUCKET START                
         BL    BUYP0200            PRIOR TO BUCKET START:                       
*                                     NOT IN TABLE - SKIP DATE                  
*                                                                               
***      MVC   P+1(34),=C'FOUND 12345678 IN 12345678 W/#    '                   
***      GOTO1 DATCON,DMCB,(3,DATEWORK),(5,P+7)                                 
***      GOTO1 DATCON,DMCB,(3,DBSTDATE(R5)),(5,P+19)                            
***      EDIT  RBUYDTNW,(2,P+30),WRK=EDITWORK                                   
***      GOTO1 REPORT                                                           
*                                                                               
*                                  ACCUMULATE W/PREVIOUS SPOTS                  
         L     RF,DBSPTCTR(R5)     BUCKET DATES FOUND: BUMP SPOTS               
         ZIC   RE,RBUYDTNW                                                      
         AR    RF,RE               INCREMENT SPOTS                              
         ST    RF,DBSPTCTR(R5)     REPLACE NEW COUNT                            
*                                                                               
*                                  SPOTS * COST + PREVIOUS $$                   
         SR    R2,R2               CLEAR REGISTER PAIR FOR MULTIPLIC            
         ZIC   R3,RBUYDTNW         GET SPOTS AGAIN FOR DOLLAR CALC              
         ZICM  R1,BUYCOST,4        SET COST                                     
         MR    R2,R1                                                            
         L     RE,DBDOLCTR(R5)     BUMP DOLLARS                                 
         AR    RE,R3                                                            
         ST    RE,DBDOLCTR(R5)     REPLACE NEW DOLLARS                          
         B     BUYP0210                                                         
EDITWORK DS    5F                                                               
BUYP0200 EQU   *                                                                
*                                                                               
***      MVC   P+1(34),=C'SKIP  12345678 IN 12345678 W/#    '                   
***      GOTO1 DATCON,DMCB,(3,DATEWORK),(5,P+7)                                 
***      GOTO1 DATCON,DMCB,(3,DBSTDATE(R5)),(5,P+19)                            
***      EDIT  RBUYDTNW,(2,P+30),WRK=EDITWORK                                   
***      GOTO1 REPORT                                                           
*                                                                               
BUYP0210 EQU   *                                                                
         LA    R7,7                BUMP DATES 1 WEEK                            
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK?                              
         BNO   BUYP0220            NO                                           
         LA    R7,14                                                            
BUYP0220 EQU   *                                                                
*                                                                               
*        MVC   P+1(09),=C'PREADDAY:'                                            
*        MVC   P+12(06),BUYSTDAT                                                
*        MVI   P+18,C'/'                                                        
*        MVC   P+19(06),BUYENDAT                                                
*        MVI   P+25,C'/'                                                        
*        ST    R7,FULL                                                          
*        MVC   P+26(04),FULL                                                    
*        GOTO1 REPORT                                                           
*                                                                               
         GOTO1 ADDAY,DMCB,BUYSTDAT,DATEWORK+6,(R7)                              
         MVC   BUYSTDAT,DATEWORK+6     RESET START DATE                         
*                                                                               
*        MVC   P+1(09),=C'PSTADDAY:'                                            
*        MVC   P+12(06),BUYSTDAT                                                
*        MVI   P+18,C'/'                                                        
*        MVC   P+19(06),BUYENDAT                                                
*        MVI   P+25,C'/'                                                        
*        ST    R7,FULL                                                          
*        MVC   P+26(04),FULL                                                    
*        GOTO1 REPORT                                                           
*                                                                               
         CLC   BUYSTDAT,BUYENDAT                                                
         BNH   BUYP0060            STILL WITHIN FLIGHT: PROCESS                 
         B     BUYP0030            GO BACK FOR NEXT EFFECTIVE DATE ELT          
BUYP0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        LOCALREP --- LOCAL REPORT INTERFACE                                    
*                                                                               
LOCALREP NTR1                                                                   
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         L     R1,0(R1)            R1 -> PASSED DEFINITION LIST                 
LREP10   EQU   *                                                                
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LREP20                                                           
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LREP10                                                           
LREP20   EQU   *                                                                
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
*                                                                               
LREPGOOD EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
ALLTEXT  DC    C'T',AL1(01),C'O',AL1(01)     GROUP                              
         DC    C'T',AL1(10),C'O',AL1(01)     GROUP NAME                         
         DC    C'T',AL1(02),C'O',AL1(01)     OFFICE                             
         DC    C'T',AL1(10),C'O',AL1(01)     OFFICE NAME                        
         DC    C'T',AL1(05),C'O',AL1(01)     STATION                            
         DC    C'T',AL1(14),C'O',AL1(01)     STATION MARKET                     
         DC    C'T',AL1(03),C'O',AL1(01)     SALESPERSON                        
         DC    C'T',AL1(20),C'O',AL1(01)     AGENCY NAME                        
         DC    C'T',AL1(20),C'O',AL1(01)     ADVERTISER NAME                    
         DC    C'T',AL1(08),C'O',AL1(01)     CONTRACT NUMBER                    
         DC    C'T',AL1(20),C'O',AL1(01)     PRODUCT NAME                       
         DC    C'T',AL1(08),C'O',AL1(01)     CONTRACT CREATE DATE               
         DC    C'T',AL1(08),C'O',AL1(01)     MONTH OF SERVICE                   
         DC    C'N',AL1(04),C'O',AL1(01)     SPOTS PER MONTH                    
         DC    C'T',AL1(10),C'O',AL1(01)     ESTIMATE NUMBER                    
         DC    C'N',AL1(12)                  CURRENT BILLING                    
         DC    X'0000'                                                          
*                                                                               
*   SETLINE:                                                                    
*   FORMAT THE BASIC PRINTLINE IN MYP.  USE THIS TO FEED EACH OF                
*        THE MONTHLY DISPLAYS                                                   
SETLINE  NTR1                                                                   
         LA    R5,MYP              SET A(PRINT SETUP LINE)                      
         USING MYPDSECT,R5                                                      
         MVI   MYP,C' '                                                         
         MVC   MYP+1(L'MYP-1),MYP  SET MYP TO SPACES                            
         MVC   PGROUP,RCONKGRP                                                  
         MVC   PGRPNAME,RGRPNAME   INSERT GROUP NAME                            
         MVC   POFFICE,RCONKOFF                                                 
         MVC   POFFNAME,ROFFNAME                                                
         MVC   PSTATION,RCONKSTA                                                
         MVC   PSTANAME,RSTAMKT                                                 
         MVC   PSALESP,RCONSAL                                                  
         MVC   PAGYNAME,RAGYNAM1                                                
         MVC   PADVNAME,RADVNAME                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,PCON#,4,=C'TOG'                             
         GOTO1 DATCON,DMCB,(3,RCONCREA),(5,PCREDATE)                            
         LA    R6,RCONELEM                                                      
SETL0020 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    SETL0200            YES - NO ELEMENT FOUND                       
         CLI   0(R6),X'05'         PRODUCT ELEMENT?                             
         BE    SETL0060            YES -                                        
         CLI   0(R6),X'A2'         EASI CODE ELEMENT?                           
         BE    SETL0080            YES -                                        
SETL0040 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     SETL0020                                                         
SETL0060 EQU   *                                                                
         MVC   PPRDNAME,RCONEXPR-RCONEXEL(R6)                                   
*                                  MOVE PRODUCT NAME TO OUTPUT                  
         B     SETL0040            KEEP LOOKING FOR A2 ELEMENT                  
SETL0080 EQU   *                                                                
         MVC   PCONEST#,RCONXEST-RCONIEL(R6)                                    
*                                  MOVE ESTIMATE NUNBER TO OUTPUT               
         OC    PCONEST#,SPACES     SET TO SPACES                                
SETL0200 EQU   *                                                                
         CLC   RCONPRD,SPACES      PRODUCT CODE ENTERED?                        
         BE    SETL0220            YES - FINISHED                               
         BAS   RE,PRODREC          NO  - RETRIEVE THE PRODUCT CODE              
SETL0220 EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*  PRODREC:                                                                     
*        PRODUCT CODE HAS BEEN ENTERED.  RETRIEVE THE PRODUCT RECORD,           
*        AND GET THE PRODUCT NAME FROM IT.                                      
*                                                                               
PRODREC  NTR1                                                                   
*                                                                               
***      MVC   P+1(14),=C'PRODUCT FOUND='                                       
***      MVC   P+15(27),KEY                                                     
***      GOTO1 REPORT                                                           
*                                                                               
         MVC   SVCONKEY,KEY        SAVE THE CONTRACT KEY                        
         XC    KEY,KEY                                                          
         MVI   KEY,9               SET KEY TYPE                                 
         MVC   KEY+25(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+18(4),RCONKADV  INSERT ADVERTISER CODE                       
         MVC   KEY+22(3),RCONPRD   INSERT PRODUCT CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                KEY MUST BE ON FILE                          
         GOTO1 GETPROD             RETRIEVE THE PRODUCT RECORD                  
         LA    R5,MYP              SET A(PRINT SETUP LINE)                      
         USING MYPDSECT,R5                                                      
         MVC   PPRDNAME,RPRDNAME   INSERT PRODUCT NAME INTO LINE                
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVC   KEY,SVCONKEY        RESET THE CONTRACT KEY                       
         GOTO1 HIGH                RESTART AT CONTRACT RECORD                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  BLDTABLE:                                                                    
*        BUILD BROADCAST MONTH TABLE, WITH BUCKETS FOR SPOT COUNT AND           
*        MONTHLY DOLLARS                                                        
*                                                                               
BLDTABLE NTR1                                                                   
         XC    BDCSTDEL,BDCSTDEL   ' SET TABLE DELIMITER                        
         XC    BDCSTABL(LBDCSTAB),BDCSTABL                                      
         LA    R5,BDCSTABL         SET A(BDCSTABL)                              
         MVC   DATEWORK+6(3),SDATE LOAD START YYMM                              
         MVI   DATEWORK+8,15       SET DAY TO 15                                
BLDT0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,DATEWORK+6),(0,DATEWORK)                          
*                                  SET EBCDIC START DATE                        
         GOTO1 =V(GETBROAD),DMCB,(0,DATEWORK),DATEWORK+12,0,0                   
         GOTO1 DATCON,DMCB,(0,DATEWORK+12),(3,DBSTDATE(R5))                     
         GOTO1 DATCON,DMCB,(0,DATEWORK+18),(3,DBENDATE(R5))                     
*                                  INSERT START/END DATES INTO TABLE            
         LA    R5,DBONESET(R5)     BUMP TO NEXT BUCKETS                         
         CLC   DATEWORK+6(2),EDATE LAST MONTH ALREADY DONE?                     
         BE    BLDT0060            YES                                          
         CLI   DATEWORK+7,12       BINARY DATE'S MONTH = 12?                    
         BE    BLDT0040            YES - NEED TO CHANGE YEAR                    
         ZIC   RF,DATEWORK+7       NO  - BUMP MONTH FROM BINARY DATE            
         LA    RF,1(RF)                                                         
         STC   RF,DATEWORK+7       PUT IT BACK                                  
         B     BLDT0020            GO BACK FOR NEXT MONTH                       
BLDT0040 EQU   *                                                                
         MVI   DATEWORK+7,1        SET MONTH THE 1 (JANUARY)                    
         ZIC   RF,DATEWORK+6       BUMP YEAR                                    
         LA    RF,1(RF)                                                         
         STC   RF,DATEWORK+6                                                    
         B     BLDT0020            GO BACK FOR NEXT MONTH                       
BLDT0060 EQU   *                                                                
***      LA    R5,BDCSTABL         DISPLAY THE DATE TABLE                       
***      LA    R0,12                                                            
BLDT0080 EQU   *                                                                
***      MVC   P+1(16),0(R5)       UNLOAD FIRST SET OF BUCKETS                  
***      GOTO1 REPORT                                                           
***      LA    R5,DBONESET(R5)     BUMP TO NEXT BUCKETS                         
***      BCT   R0,BLDT0080         GO BACK FOR NEXT ENTRY                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
ALTHDR   NTR1                                                                   
         USING ALTHDR,RF           ESTABLISH ADDRESSABILITY                     
         LA    R1,SAVEREGS-ALTHDR                                               
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETBUY   LA    RF,RBUYREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETPROD  LA    RF,RPRDREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTCON   LA    RF,RCONREC                                                       
         B     PUTFILE                                                          
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     MODEEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
PUTFILE  NTR1                                                                   
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    MODEEXIT            YES - DON'T REWRITE IT                       
         ST    RF,AIOAREA                                                       
         GOTO1 PREC                                                             
         B     MODEEXIT                                                         
         SPACE 3                                                                
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*              WORK SPACE ETC                                                   
         SPACE 1                                                                
*                                                                               
PULSECTR DS    F                   CONTRACTS PROCESSED CTR                      
TOTALCTR DS    F                                                                
SMLRCTR  DS    F                                                                
SAMESIZE DS    F                                                                
FIRSTCON DS    CL2                                                              
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
SVCONKEY DS    CL27                ANOTHER   KEY SAVE AREA                      
KEYTYPE  DS    X                   KEYTYPE BEING PROCESSED                      
SDATE    DS    XL3                 REQUEST START DATE BINARY                    
EDATE    DS    XL3                 REQUEST END   DATE BINARY                    
KDATE    DS    XL2                 KEY     START DATE COMPRESSED                
BUYSTDAT DS    CL6                 EBCDIC X'03' START DATE                      
BUYENDAT DS    CL6                 EBCDIC X'03' END   DATE                      
BUYCOST  DS    XL4                                                              
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
DATEWORK DS    CL48                                                             
NEW23ELT DS    CL10                                                             
MYP      DS    CL200                                                            
MYWORK   DS    CL64                                                             
CONRV    DS    CL4                 NEW      CONTRACT NUMBER (REV)               
BDCSTABL DS    48F                 BROADCAST MONTH TABLE                        
*                                  WORD 1: BYTES 0-2 = BDCST MON STDATE         
*                                  WORD 2: BYTES 0-2 = BDCST MON ENDATE         
*                                  WORD 3: BYTES 0-3 = SPOT COUNTER             
*                                  WORD 4: BYTES 0-3 = DOLLAR COUNTER           
DBSTDATE EQU   0                                                                
DBENDATE EQU   4                                                                
DBSPTCTR EQU   8                                                                
DBDOLCTR EQU   12                                                               
DBONESET EQU   16                                                               
*                                                                               
BDCSTDEL DS    F                   BDCSTABL DELIMITER                           
LBDCSTAB EQU   *-BDCSTABL                                                       
*                                                                               
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
*                                                                               
*   COMMISSION RECORD IS ORG'D TO THE BUY RECORD, WHICH ISN'T USED,             
*    RATHER THAN THE CONTRACT RECORD, WHICH IS                                  
*                                                                               
         ORG RBUYREC                                                            
       ++INCLUDE REGENCOM                                                       
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                                                              
BRDEND   DS    XL3                                                              
BRDWEEKS DS    XL1                                                              
BRDLEN   EQU   *-BRDSTART                                                       
         SPACE 2                                                                
MYPDSECT DSECT                                                                  
PGROUP   DS    CL1                 GROUP ONLY - NO SUBGROUP                     
         DS    CL1                                                              
PGRPNAME DS    CL10                GROUP NAME                                   
         DS    CL1                                                              
POFFICE  DS    CL2                 OFFICE                                       
         DS    CL1                                                              
POFFNAME DS    CL10                OFFICE NAME                                  
         DS    CL1                                                              
PSTATION DS    CL5                 STATION                                      
         DS    CL1                                                              
PSTANAME DS    CL14                STATION MARKET                               
         DS    CL1                                                              
PSALESP  DS    CL3                 SALESPERSON                                  
         DS    CL1                                                              
PAGYNAME DS    CL20                AGENCY NAME                                  
         DS    CL1                                                              
PADVNAME DS    CL20                ADVERT NAME                                  
         DS    CL1                                                              
PCON#    DS    CL08                CONTRACT NUMBER                              
         DS    CL1                                                              
LMYPPT#1 EQU   *-PGROUP                                                         
*                                                                               
PPRDNAME DS    CL20                PRODUCT NAME                                 
         DS    CL1                                                              
PCREDATE DS    CL08                CONTRACT CREATE DATE                         
         DS    CL1                                                              
PMONTH   DS    CL08                MONTH OF SERVICE                             
         DS    CL1                                                              
PSPTSMO  DS    CL04                SPOTS PER MONTH                              
         DS    CL1                                                              
PCONEST# DS    CL10                CONTRACT ESTIMATE NUMBER                     
         DS    CL1                                                              
PCURBLD  DS    CL12                CURRENT BILLING                              
         SPACE 2                                                                
LMYPPT#2 EQU   *-PPRDNAME                                                       
LMYPDSEC EQU   *-PGROUP                                                         
*                                                                               
*   DEACTIVATED CODE:  DELETE WHEN JOB DEBUGGED                                 
*                                                                               
*        XC    KEY,KEY                                                          
*        MVI   KEYTYPE,X'8D'       SET 'NO STATION FILTER' KEY                  
*        MVC   KEY+1(2),RCREPFL    INSERT REP CODE INTO KEY                     
*        CLC   QSTATION,SPACES     ANY STATION FILTER?                          
*        BNH   INIT0010            NO                                           
*        MVI   KEYTYPE,X'8E'       SET 'STATION FILTER' KEY                     
*        MVC   KEY+3(5),QSTATION   INSERT STATION INTO KEY                      
*NIT0010 EQU   *                                                                
*        MVC   KEY(1),KEYTYPE      INSERT KEYTYPE INTO KEY                      
*        MVC   KEY+8(2),KDATE      INSERT STARTING KEY DATE                     
*        GOTO1 HIGH                                                             
*        B     INIT0040                                                         
*NIT0020 EQU   *                                                                
*        GOTO1 SEQ                                                              
*NIT0040 EQU   *                                                                
*        CLC   KEY(1),KEYTYPE      8D/E KEYS FINISHED?                          
*        BNE   POST0080            YES - FINISHED                               
*        CLI   KEY,X'8E'           STATION FILTER KEY?                          
*        BNE   INIT0045            NO                                           
*        CLC   KEY+3(5),QSTATION   SAME AS FILTER STATION?                      
*        BH    POST0080            FINISHED IF HIGH                             
*NIT0045 EQU   *                                                                
*        CLI   KEY+16,1            FIRST  SUBKEY TYPE?                          
*        BNE   INIT0020            NO  - SKIP ALL BUT TYPE 1'S                  
*        CLC   EDATE,KEY+8         REQUEST END DATE BEFORE KEYSTART?            
*        BL    POST0080            YES - FINISHED                               
*        CLC   KEY+10(2),SDATE     KEYEND DATE BEFORE REQUEST START?            
*        BL    INIT0020            YES - NOT IN REQUEST PERIOD                  
*                                     SKIP IT                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061REREP9502 05/01/02'                                      
         END                                                                    
