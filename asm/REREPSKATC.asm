*          DATA SET REREPSKATC AT LEVEL 103 AS OF 05/01/02                      
*PHASE REST02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE SCANNER                                                                
         TITLE 'REREPSKAT - CONTRACT/BUYLINE/MG/COVER/DARE CONVERT'             
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPSKAT -- STRIP A SET OF CONTRACT INFO FROM INDICATED  *            
*                     REP, PRODUCING AN OUTPUT TAPE TO MERGE INTO  *            
*                     A LOAD.  OPTIONALLY MARK ORIGINAL ORDERS FOR *            
*                     DELETION.                                    *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* SEP28/98 (SKU) --- ORIGINAL ENTRY                                *            
*                                                                  *            
* OCT22/99 (BU ) --- INSERT MISSING AGY/ADV CODE TABLES            *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     =                                                *            
*     QUESTOR+1   =                                                *            
*                                                                  *            
*     QRECORD+36  =  'FROM' REP                                    *            
*     QRECORD+38  =  'TO  ' REP                                    *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REST02   CSECT                                                                  
         NMOD1 0,**ST02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
         XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         OPEN  (INTAPE1,(INPUT))                                                
         OPEN  (INTAPE2,(INPUT))                                                
*                                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         GOTO1 LOADTABL,DMCB,(RC)                                               
*                                                                               
         CLOSE (INTAPE1,REWIND)                                                 
         CLOSE (INTAPE2,REWIND)                                                 
*                                                                               
         GOTO1 CONTPROC,DMCB,(RC)  PROCESS CONTRACT RECORDS                     
*                                                                               
         GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
*                                                                               
         GOTO1 MISSINGS,DMCB,(RC)  DISPLAY MISSING CODES                        
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         XIT1                      EXIT                                         
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
******************************************************************              
INITIAL  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         XC    HIGHCON,HIGHCON                                                  
         MVC   LOWCON,=X'FFFFFFFF'                                              
*                                                                               
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1800000,1800000                                 
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         MVC   P+1(30),=C'CANNOT GET SPACE: COVAIL ABORT'                       
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVI   RCSUBPRG,2          SET HEADING FLAG                             
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   AAGYAREA,P2         A(AGENCY TABLE)                              
         MVC   ANEXTAGY,P2         A(NEXT AGENCY SLOT)                          
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         A     RF,=F'60000'        60K FOR AGENCY STORAGE:                      
*                                     5,000 ENTRIES -                           
*                                        6 BYTES OLD AGY/OFF                    
*                                        6 BYTES NEW AGY/OFF                    
         ST    RF,ASALAREA         ESTABLISH SALESPERSON AREA                   
         ST    RF,ANEXTSAL         A(NEXT S/P SLOT)                             
         A     RF,=F'19500'        19.5K FOR SALESPERSON STORAGE:               
*                                     1,500 ENTRIES -                           
*                                        5 BYTES FOR STATION                    
*                                        3 BYTES OLD S/P                        
*                                        3 BYTES NEW S/P                        
*                                        2 BYTES NEW S/P TEAM                   
         ST    RF,AADVAREA         A(ADVERTUSER TABLE)                          
         ST    RF,ANEXTADV         A(NEXT ADVERTISER SLOT)                      
         A     RF,=F'80000'        80K FOR ADVERT STORAGE:                      
*                                     10,000 ENTRIES -                          
*                                        4 BYTES OLD ADV                        
*                                        4 BYTES NEW ADV                        
         ST    RF,AIO2             A(IOAREA #2)                                 
*                                                                               
         A     RF,=F'4000'         4K FOR ALTERNATE IO AREA                     
         ST    RF,ATAPEREC         A(TAPE RECORD READ AREA)                     
*                                                                               
         A     RF,=F'4000'         4K FOR TAPE READ AREA                        
         ST    RF,AWORKBLK         A(WORK BLOCK FOR SCANNER)                    
*                                                                               
         A     RF,=F'6000'         4K FOR WORK BLOCK AREA                       
         ST    RF,ASPOFFTB         A(SALESPERSON BY STA/OFF TABLE)              
         ST    RF,ANEXTSPO         A(NEXT S/P    BY STA/OFF TABLE)              
*                                                                               
*                                  LEAVE A BIG SPACE HERE                       
         A     RF,=F'60000'        60K FOR SALESPERSON BY STA/OFF               
         ST    RF,AMISSAGY         A(MISSING AGENCY TABLE)                      
         A     RF,=F'60000'        60K FOR MISSING AGENCY CODESFF               
         ST    RF,AMISSADV         A(MISSING ADVERT TABLE)                      
*                                                                               
         L     RF,AMISSAGY                                                      
         MVC   0(6,RF),=X'FFFFFFFFFFFF'                                         
         L     RF,AMISSADV                                                      
         MVC   0(4,RF),=X'FFFFFFFF'                                             
         L     RF,MISSAGYS                                                      
         LA    RF,1(RF)                                                         
         ST    RF,MISSAGYS                                                      
         L     RF,MISSADVS                                                      
         LA    RF,1(RF)                                                         
         ST    RF,MISSADVS                                                      
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
                                                                                
         XC    CONCTR,CONCTR       CLEAR COUNTERS                               
         XC    CONCTR2,CONCTR2     CLEAR COUNTERS                               
         XC    CONCTR3,CONCTR3     CLEAR COUNTERS                               
         XC    CONCTR4,CONCTR4     CLEAR COUNTERS                               
         XC    BUYCTR,BUYCTR                                                    
         XC    BUYCTR2,BUYCTR2                                                  
         XC    MKGCTR,MKGCTR                                                    
         XC    ADVCTR,ADVCTR                                                    
         XC    AGYCTR,AGYCTR                                                    
         XC    CTYCTR,CTYCTR                                                    
         XC    SALCTR,SALCTR                                                    
         XC    CONBYTES,CONBYTES                                                
         XC    BUYBYTES,BUYBYTES                                                
         MVC   OLDREP,QRECORD+36   'FROM' REP                                   
         MVC   NEWREP,QRECORD+38   'TO' REP                                     
         MVC   REPREP,QRECORD+40   'REPLACEMENT' REP                            
         MVC   NOCODES,QRECORD+42  SKIP CODE REPLACEMENT TEST                   
         LA    RF,UTLTABLE         SET ORIG/NEW UTLS                            
INIT0040 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BE    INIT0200            YES - NOT IN TABLE: ABORT                    
         CLC   OLDREP,0(RF)        OLDREP FOUND IN TABLE?                       
         BE    INIT0060            YES - SET ORIGUTL                            
         LA    RF,LUTLTABL(RF)     NO  - BUMP TO NEXT ENTRY                     
         B     INIT0040                                                         
INIT0060 EQU   *                                                                
         MVC   ORIGUTL,2(RF)       SAVE ORIGINAL UTL #                          
         LA    RF,UTLTABLE         SET ORIG/NEW UTLS                            
INIT0080 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BE    INIT0200            YES - NOT IN TABLE: ABORT                    
         CLC   NEWREP,0(RF)        NEWREP FOUND IN TABLE?                       
         BE    INIT0100            YES - SET NEWUTL                             
         LA    RF,LUTLTABL(RF)     NO  - BUMP TO NEXT ENTRY                     
         B     INIT0080                                                         
INIT0100 EQU   *                                                                
         MVC   NEWUTL,2(RF)        SAVE NEW      UTL #                          
*                                                                               
*   DISPLAY TEST                                                                
**       MVC   P+1(20),=C'UTLS:  OLD=     NEW='                                 
**       MVC   P+13(1),ORIGUTL                                                  
**       MVC   P+22(1),NEWUTL                                                   
**       GOTO1 REPORT                                                           
*   DISPLAY TEST END                                                            
*                                                                               
         XIT1                                                                   
INIT0200 EQU   *                                                                
         DC    H'0'                NO REFERENCE FOR THIS REP                    
         EJECT                                                                  
******************************************************************              
*   LOAD TKO VALUE TABLE                                                        
******************************************************************              
LOADTABL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    RF,REC              SET A(RECORD)                                
         ST    RF,AREC                                                          
*                                                                               
         L     R2,ADCONLST                                                      
         USING ADCONSD,R2                                                       
         L     R6,MASTC                                                         
         USING MASTD,R6                                                         
         L     RE,MCUTL            SET A(UTL)                                   
*                                                                               
         DROP R2,R6                                                             
*                                                                               
         MVC   4(1,RE),NEWUTL      SET NEW FILE UTL                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',FLIST,AREC                       
*                                     OPEN NEW REP FILE                         
*   TEST FILE SWITCHING LOGIC                                                   
**       XC    KEY,KEY                                                          
**       MVI   KEY,1                                                            
**       GOTO1 HIGHDIR                                                          
**       MVC   P+1(17),=C'NEW FILE: 1ST REP'                                    
**       MVC   P+20(2),NEWREP                                                   
***      MVC   P+26(27),KEY        DISPLAY KEY FOUND                            
**       GOTO1 REPORT                                                           
         L     R5,ATAPEREC         SET A(TAPE INPUT AREA)                       
LDIR0020 EQU   *                                                                
         XC    0(250,R5),0(R5)     CLEAR INPUT AREA                             
         GET   INTAPE1,(R5)        READ AGY/ADV TAPE RECORD INTO RDA            
*                                     TAPE WILL BE PARSED ON ',' SEPS           
*        GOTO1 REPORT                                                           
***      MVC   P+1(13),=C'INPUT RECORD:'                                        
***      MVC   P+14(80),0(R5)                                                   
***      GOTO1 REPORT                                                           
         XC    MYWORK,MYWORK       CLEAR WORK SPACE                             
         ZICM  RF,0(R5),2          GET LENGTH OF INPUT RECORD                   
         BCTR  RF,0                SUBTRACT 1 FOR MOVE                          
         EX    RF,LDIRMOV1         MOVE BY LENGTH                               
         B     LDIR0040                                                         
LDIRMOV1 MVC   MYWORK+8(0),4(R5)                                                
LDIR0040 EQU   *                                                                
         ZICM  RF,0(R5),2          LOAD LENGTH OF RECORD                        
         SH    RF,=H'4'            SUBTRACT CONTROL LENGTH                      
         STC   RF,MYWORK+5                                                      
***      MVC   P+1(13),=C'MYWORK  RECORD:'                                      
***      MVC   P+14(80),MYWORK                                                  
***      GOTO1 REPORT                                                           
*                                                                               
         CLC   =C'AGY',4(R5)       AGENCY INPUT?                                
         BE    LDIR0060            YES - PROCESS AS AGENCY                      
         CLC   =C'ADV',4(R5)       ADVERT INPUT?                                
         BE    LDIR0280            YES - PROCESS AS ADVERT                      
         B     LDIR0020            NO  - SKIP AS UNRECOGNIZED                   
LDIR0060 EQU   *                                                                
         BAS   RE,SCANQUOT                                                      
*        MVC   P+1(13),=C'QUOTE RECORD:'                                        
*        MVC   P+14(80),MYWORK                                                  
*        GOTO1 REPORT                                                           
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         GOTO1 =V(SCANNER),DMCB,(30,MYWORK),AWORKBLK,C',=,='                    
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   LDIR0080                                                         
         DC    H'0'                                                             
*                                                                               
LDIR0080 EQU   *                                                                
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(12),=C'SCANNER OKAY'                                         
*        GOTO1 REPORT                                                           
*        L     R4,AWORKBLK         A(PRODUCT RECORD)                            
*        LA    RF,256                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         LA    RF,256(RF)                                                       
         ST    RF,AWRKBLK2                                                      
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         L     RE,AWORKBLK         SET A(WORKSPACE)                             
         XC    MYWORK2,MYWORK2                                                  
         MVC   MYWORK2+8(12),64(RE) MOVE IN OLD AGENCY                          
         MVC   MYWORK2+5(01),52(RE) MOVE IN LENGTH                              
*                                                                               
         GOTO1 =V(SCANNER),DMCB,MYWORK2,AWRKBLK2,C',=-='                        
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ANEXTAGY         SET A(NEXT AGENCY CODE)                      
         L     R4,AWRKBLK2                                                      
         TM    2(R4),X'80'         NUMERIC IN FIELD?                            
         BNO   LDIR0160            NO  - TAKE ENTIRE FIELD                      
         CLI   0(R4),4             YES - FOUR CHARACTERS?                       
         BE    LDIR0160            YES - TAKE ENTIRE FIELD                      
         MVC   0(4,R1),=C'0000'    LOAD ZEROS TO FIELD                          
         CLI   0(R4),3             LENGTH = 3?                                  
         BNE   LDIR0100            NO                                           
         MVC   1(3,R1),12(R4)      MOVE THREE CHARACTERS                        
         B     LDIR0180                                                         
LDIR0100 EQU   *                                                                
         CLI   0(R4),2             LENGTH = 2?                                  
         BNE   LDIR0120            NO                                           
         MVC   2(2,R1),12(R4)      MOVE TWO CHARACTERS                          
         B     LDIR0180                                                         
LDIR0120 EQU   *                                                                
         CLI   0(R4),1             LENGTH = 1?                                  
         BNE   LDIR0140            NO                                           
         MVC   3(1,R1),12(R4)      MOVE 1 CHARACTER                             
         B     LDIR0180                                                         
LDIR0140 EQU   *                                                                
         DC    H'0'                SHOULDN'T HAPPEN                             
LDIR0160 EQU   *                                                                
         MVC   0(4,R1),12(R4)      INSERT OLD AGENCY CODE IN TABLE              
LDIR0180 EQU   *                                                                
         MVC   4(2,R1),44(R4)      INSERT OLD AGENCY OFFICE INTO TABLE          
         L     RE,AWORKBLK         SET A(WORKSPACE)                             
         XC    MYWORK2,MYWORK2                                                  
         MVC   MYWORK2+8(12),220(RE) MOVE IN OLD AGENCY                         
         MVC   MYWORK2+5(01),208(RE) MOVE IN LENGTH                             
*                                                                               
         GOTO1 =V(SCANNER),DMCB,MYWORK2,AWRKBLK2,C',=-='                        
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ANEXTAGY         SET A(NEXT AGENCY CODE)                      
         L     R4,AWRKBLK2                                                      
         TM    2(R4),X'80'         NUMERIC IN FIELD?                            
         BNO   LDIR0260            NO  - TAKE ENTIRE FIELD                      
         CLI   0(R4),4             YES - FOUR CHARACTERS?                       
         BE    LDIR0260            YES - TAKE ENTIRE FIELD                      
         MVC   6(4,R1),=C'0000'    LOAD ZEROS TO FIELD                          
         CLI   0(R4),3             LENGTH = 3?                                  
         BNE   LDIR0200            NO                                           
         MVC   7(3,R1),12(R4)      MOVE THREE CHARACTERS                        
         B     LDIR0270                                                         
LDIR0200 EQU   *                                                                
         CLI   0(R4),2             LENGTH = 2?                                  
         BNE   LDIR0220            NO                                           
         MVC   8(2,R1),12(R4)      MOVE TWO CHARACTERS                          
         B     LDIR0270                                                         
LDIR0220 EQU   *                                                                
         CLI   0(R4),1             LENGTH = 1?                                  
         BNE   LDIR0240            NO                                           
         MVC   9(1,R1),12(R4)      MOVE 1 CHARACTER                             
         B     LDIR0270                                                         
LDIR0240 EQU   *                                                                
         DC    H'0'                SHOULDN'T HAPPEN                             
LDIR0260 EQU   *                                                                
         MVC   6(4,R1),12(R4)      INSERT NEW AGENCY CODE IN TABLE              
LDIR0270 EQU   *                                                                
         MVC   10(2,R1),44(R4)     INSERT NEW AGENCY OFFICE INTO TABLE          
*                                                                               
         OC    0(AGYTABLN,R1),SPACES                                            
         LA    R1,AGYTABLN(R1)     BUMP TO NEXT AVAILABLE SLOT                  
         ST    R1,ANEXTAGY                                                      
         L     R1,AGYCTR                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         ST    R1,AGYCTR           SAVE COUNT                                   
         B     LDIR0020            GO BACK FOR NEXT RECORD                      
LDIR0280 EQU   *                                                                
         BAS   RE,SCANQUOT                                                      
*        MVC   P+1(13),=C'QUOTE RECORD:'                                        
*        MVC   P+14(80),MYWORK                                                  
*        GOTO1 REPORT                                                           
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         GOTO1 =V(SCANNER),DMCB,(30,MYWORK),AWORKBLK,C',=,='                    
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   LDIR0300                                                         
         DC    H'0'                                                             
*   TEST PRNTBL END                                                             
*                                                                               
LDIR0300 EQU   *                                                                
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(12),=C'SCANNER OKAY'                                         
*        GOTO1 REPORT                                                           
*        L     R4,AWORKBLK         A(PRODUCT RECORD)                            
*        LA    RF,256                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         LA    RF,256(RF)                                                       
         ST    RF,AWRKBLK2                                                      
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         L     RE,AWORKBLK         SET A(WORKSPACE)                             
         XC    MYWORK2,MYWORK2                                                  
         MVC   MYWORK2+8(12),64(RE) MOVE IN OLD ADVERT                          
         MVC   MYWORK2+5(01),52(RE) MOVE IN LENGTH                              
*                                                                               
         GOTO1 =V(SCANNER),DMCB,MYWORK2,AWRKBLK2,C',=-='                        
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ANEXTADV         SET A(NEXT ADVERT CODE)                      
         L     R4,AWRKBLK2                                                      
         TM    2(R4),X'80'         NUMERIC IN FIELD?                            
         BNO   LDIR0380            NO  - TAKE ENTIRE FIELD                      
         CLI   0(R4),4             YES - FOUR CHARACTERS?                       
         BE    LDIR0380            YES - TAKE ENTIRE FIELD                      
         MVC   0(4,R1),=C'0000'    LOAD ZEROS TO FIELD                          
         CLI   0(R4),3             LENGTH = 3?                                  
         BNE   LDIR0320            NO                                           
         MVC   1(3,R1),12(R4)      MOVE THREE CHARACTERS                        
         B     LDIR0390                                                         
LDIR0320 EQU   *                                                                
         CLI   0(R4),2             LENGTH = 2?                                  
         BNE   LDIR0340            NO                                           
         MVC   2(2,R1),12(R4)      MOVE TWO CHARACTERS                          
         B     LDIR0390                                                         
LDIR0340 EQU   *                                                                
         CLI   0(R4),1             LENGTH = 1?                                  
         BNE   LDIR0360            NO                                           
         MVC   3(1,R1),12(R4)      MOVE 1 CHARACTER                             
         B     LDIR0390                                                         
LDIR0360 EQU   *                                                                
         DC    H'0'                SHOULDN'T HAPPEN                             
LDIR0380 EQU   *                                                                
         MVC   0(4,R1),12(R4)      INSERT ADVERT CODE IN TABLE                  
LDIR0390 EQU   *                                                                
         L     RE,AWORKBLK         SET A(WORKSPACE)                             
         XC    MYWORK2,MYWORK2                                                  
         MVC   MYWORK2+8(12),220(RE) MOVE IN OLD ADVERT                         
         MVC   MYWORK2+5(01),208(RE) MOVE IN LENGTH                             
*                                                                               
         GOTO1 =V(SCANNER),DMCB,MYWORK2,AWRKBLK2,C',=-='                        
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ANEXTADV         SET A(NEXT ADVERT CODE)                      
         L     R4,AWRKBLK2                                                      
         TM    2(R4),X'80'         NUMERIC IN FIELD?                            
         BNO   LDIRX380            NO  - TAKE ENTIRE FIELD                      
         CLI   0(R4),4             YES - FOUR CHARACTERS?                       
         BE    LDIRX380            YES - TAKE ENTIRE FIELD                      
         MVC   4(4,R1),=C'0000'    LOAD ZEROS TO FIELD                          
         CLI   0(R4),3             LENGTH = 3?                                  
         BNE   LDIRX320            NO                                           
         MVC   5(3,R1),12(R4)      MOVE THREE CHARACTERS                        
         B     LDIRX390                                                         
LDIRX320 EQU   *                                                                
         CLI   0(R4),2             LENGTH = 2?                                  
         BNE   LDIRX340            NO                                           
         MVC   6(2,R1),12(R4)      MOVE TWO CHARACTERS                          
         B     LDIRX390                                                         
LDIRX340 EQU   *                                                                
         CLI   0(R4),1             LENGTH = 1?                                  
         BNE   LDIRX360            NO                                           
         MVC   7(1,R1),12(R4)      MOVE 1 CHARACTER                             
         B     LDIRX390                                                         
LDIRX360 EQU   *                                                                
         DC    H'0'                SHOULDN'T HAPPEN                             
LDIRX380 EQU   *                                                                
         MVC   4(4,R1),12(R4)      INSERT NEW ADVERT CODE IN TABLE              
LDIRX390 EQU   *                                                                
         LA    R1,ADVTABLN(R1)     BUMP TO NEXT AVAILABLE SLOT                  
         ST    R1,ANEXTADV                                                      
         L     R1,ADVCTR                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         ST    R1,ADVCTR           SAVE COUNT                                   
         B     LDIR0020            GO BACK FOR NEXT RECORD                      
**ADV                                                                           
LDIR0400 EQU   *                                                                
         L     R4,AAGYAREA         SET A(TABLE)                                 
         L     R3,AGYCTR           SET COUNTER                                  
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),12,6,0                                  
*                                                                               
         B     LDIR0440            **NOOP FOR TABLE PRINT                       
*                                                                               
         L     R2,AAGYAREA         SET A(AGENCY TABLE)                          
*                                                                               
         LA    R3,1                SET COUNTER                                  
LDIR0420 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    LDIR0440            YES                                          
*                                                                               
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'AGY:'                                                
         MVC   P+23(AGYTABLN),0(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,AGYTABLN(R2)     BUMP TO NEXT SLOT                            
         B     LDIR0420            GO BACK FOR NEXT SLOT                        
LDIR0440 EQU   *                                                                
**       MVC   P+1(07),=C'AGYCTR='                                              
**       EDIT  AGYCTR,(6,P+10)                                                  
**       GOTO1 REPORT                                                           
         L     R4,AADVAREA         SET A(TABLE)                                 
         L     R3,ADVCTR           SET COUNTER                                  
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),8,4,0                                   
*                                                                               
         B     LDIR0480            **NOOP FOR TABLE PRINT                       
*                                                                               
         L     R2,AADVAREA         SET A(ADVERT TABLE AREA)                     
         LA    R3,1                SET COUNTER                                  
LDIR0460 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    LDIR0480            YES                                          
*                                                                               
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'ADV:'                                                
         MVC   P+23(ADVTABLN),0(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,ADVTABLN(R2)     BUMP TO NEXT SLOT                            
         B     LDIR0460            GO BACK FOR NEXT SLOT                        
*                                                                               
LDIR0480 EQU   *                                                                
***      MVC   P+1(07),=C'ADVCTR='                                              
***      EDIT  ADVCTR,(6,P+10)                                                  
***      GOTO1 REPORT                                                           
*                                                                               
*   TEST                                                                        
****>>   DC    H'0'                                                             
*   TEST                                                                        
*                                                                               
         LA    R2,LISTSEL          SET A(SELTEL STATION LIST)                   
         CLC   OLDREP(4),=C'SZAM'  SELTEL TO KAMNY RUN?                         
         BE    LDIR0500            YES - USE SELTEL STATIONS                    
         LA    R2,LISTCON          SET A(CONTINENTAL STATION LIST)              
         CLC   OLDREP(4),=C'CQAM'  CONTINENTAL TO KAMNY RUN?                    
         BE    LDIR0500            YES - USE CONTINENTAL STATIONS               
         LA    R2,LISTKAM          SET A(KAMNY STATION LIST)                    
         CLC   OLDREP(4),=C'AMSZ'  KAMNY TO SELTEL RUN?                         
         BE    LDIR0500            YES - USE KAMNY STATIONS                     
         LA    R2,LISTAMQ          SET A(AM/CQ STATION LIST)                    
         CLC   OLDREP(4),=C'AMCQ'  KAMNY TO KCONY  RUN?                         
         BE    LDIR0500            YES - USE KAMNY STATIONS                     
         DC    H'0'                UNRECOGNIZED OLD REP CODE                    
LDIR0500 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    LDIR0560            YES - FINISHED                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           SET KEY TYPE                                 
         MVC   KEY+20(2),NEWREP    INSERT NEW REP                               
         MVC   KEY+22(5),0(R2)     INSERT STATION                               
         GOTO1 HIGHDIR             READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    LDIR0520            YES                                          
         MVC   DNEWGRP(2,R2),=C'??'                                             
*                                  NO  - INSERT INDICATOR                       
         B     LDIR0540                                                         
LDIR0520 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         LA    R3,REC              SET A(RECORD AREA)                           
         USING RSTAREC,R3                                                       
         MVC   DNEWGRP(2,R2),RSTAGRUP                                           
*                                  INSERT GROUP INTO SLOT                       
LDIR0540 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
***      MVC   P+1(14),=C'STATION ENTRY:'                                       
***      MVC   P+16(LLISTSTA),0(R2)                                             
***      GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
         LA    R2,LLISTSTA(R2)     BUMP TO NEXT ENTRY                           
         B     LDIR0500            GO BACK FOR NEXT                             
*                                                                               
LDIR0560 EQU   *                                                                
*                                                                               
*   LOAD STATION/OFF -> SALESPERSON CODE TABLE INFORMATION                      
*                                                                               
****>>                                                                          
         MVC   P(64),SPACES        CLEAR PRINT LINE                             
**SPOFF                                                                         
         L     R5,ATAPEREC         SET A(TAPE INPUT AREA)                       
LDIR0580 EQU   *                                                                
         XC    0(250,R5),0(R5)     CLEAR INPUT AREA                             
         GET   INTAPE2,(R5)        READ S/P BY STA/OFF TAPE RECORD              
*                                     TAPE WILL BE PARSED ON ',' SEPS           
*        GOTO1 REPORT                                                           
*        MVC   P+1(13),=C'INPUT RECORD:'                                        
*        MVC   P+14(80),0(R5)                                                   
*        GOTO1 REPORT                                                           
         XC    MYWORK,MYWORK       CLEAR WORK SPACE                             
         ZICM  RF,0(R5),2          GET LENGTH OF INPUT RECORD                   
         BCTR  RF,0                SUBTRACT 1 FOR MOVE                          
         EX    RF,LSTAMOV1         MOVE BY LENGTH                               
         B     LDIR0600                                                         
LSTAMOV1 MVC   MYWORK+8(0),4(R5)                                                
LDIR0600 EQU   *                                                                
         ZICM  RF,0(R5),2          LOAD LENGTH OF RECORD                        
         SH    RF,=H'4'            SUBTRACT CONTROL LENGTH                      
         STC   RF,MYWORK+5                                                      
*        MVC   P+1(14),=C'MYWORK RECORD:'                                       
*        MVC   P+15(80),MYWORK                                                  
*        GOTO1 REPORT                                                           
*                                                                               
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         GOTO1 =V(SCANNER),DMCB,MYWORK,AWORKBLK,C',=,='                         
*                                                                               
         CLI   DMCB+4,0                                                         
         BNE   LDIR0620                                                         
         DC    H'0'                                                             
*                                                                               
LDIR0620 EQU   *                                                                
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(12),=C'SCANNER OKAY'                                         
*        GOTO1 REPORT                                                           
*        L     R4,AWORKBLK         A(PRODUCT RECORD)                            
*        LA    RF,256                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         L     R2,ANEXTSPO         SET A(NEXT OPEN SLOT)                        
         L     R4,AWORKBLK         SET A(WORK BLOCK AREA)                       
         CLI   0(R4),0             ANY ENTRY IN FIRST FIELD?                    
         BE    LDIR0640            NO  - USE LAST STATION                       
         MVC   LASTSTA,12(R4)                                                   
LDIR0640 EQU   *                                                                
         MVC   0(5,R2),LASTSTA     INSERT STATION                               
         MVC   5(2,R2),76(R4)      INSERT OFFICE                                
         MVC   7(3,R2),108(R4)     INSERT NEW S/P                               
         OC    0(LSPOFSTA,R2),SPACES                                            
         LA    R2,LSPOFSTA(R2)     BUMP TO NEXT SLOT                            
         ST    R2,ANEXTSPO                                                      
         L     R2,SALCTR                                                        
         LA    R2,1(R2)            INCREMENT COUNTER                            
         ST    R2,SALCTR                                                        
         B     LDIR0580            GO BACK FOR NEXT                             
LDIR0660 EQU   *                                                                
*                                                                               
         B     LDIR0700            **NOOP TO DISPLAY TABLE**                    
*                                                                               
         L     R2,ASPOFFTB         SET A(SPOFF TABLE FOR REDISPLAY              
LDIR0680 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    LDIR0700                                                         
         MVC   P+1(08),=C'SPOFFTB:'                                             
         MVC   P+09(LSPOFSTA),0(R2)                                             
         GOTO1 REPORT                                                           
         LA    R2,LSPOFSTA(R2)                                                  
         B     LDIR0680            GO BACK FOR NEXT                             
LDIR0700 EQU   *                                                                
**SPOFF                                                                         
         L     R2,ASPOFFTB         SET A(SPOFF TABLE)                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           SET KEY TYPE                                 
         MVC   KEY+22(2),NEWREP    INSERT NEW REP                               
LDIR0720 EQU   *                                                                
         CLC   =C'??',DSPOFF(R2)   DEFAULT STA/OFF ENCOUNTERED?                 
         BNE   LDIR0740                                                         
*                                                                               
*   TEST DISPLAY                                                                
         MVC   P+1(08),=C'DEFAULT:'                                             
         MVC   P+10(LSPOFSTA),0(R2)                                             
         GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         B     LDIR0800                                                         
LDIR0740 EQU   *                                                                
         OC    0(2,R2),0(R2)       END OF TABLE/DELIMITER?                      
         BZ    LDIR0820            YES - FINISHED                               
         BAS   RE,LIVESTA          STATION BEING PROCESSED?                     
         BNZ   LDIR0800            NO  - SKIP THIS ENTRY                        
         MVC   KEY+24(3),DSPSP(R2) INSERT NEW S/P CODE                          
         GOTO1 HIGHDIR             RETURN HIGH KEY                              
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    LDIR0760            YES                                          
*                                                                               
*   TEST DISPLAY                                                                
*        MVC   P+1(07),=C'KEY NF:'                                              
*        MVC   P+8(27),KEY                                                      
*        MVC   P+36(27),KEYSAVE                                                 
*        MVI   P+35,C'/'                                                        
*        GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         XC    DSPNEWOF(4,R2),DSPNEWOF(R2)                                      
*                                  NOT ON FILE:  SET ZERO OFFICE/TEAM           
         MVC   P+32(11),=C'NOT ON FILE'                                         
         B     LDIR0780                                                         
LDIR0760 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         LA    R3,REC              SET A(RECORD AREA)                           
         USING RSALREC,R3                                                       
         MVC   DSPNEWOF(2,R2),RSALOFF    INSERT OFFC INTO TABLE                 
         MVC   DSPNEWTM(2,R2),RSALTEAM   INSERT TEAM INTO TABLE                 
LDIR0780 EQU   *                                                                
*                                                                               
*   TEST DISPLAY S/P TEAM                                                       
**       MVC   P+1(04),=C'NSP:'                                                 
**       MVC   P+8(LSPOFSTA),0(R2)                                              
**       GOTO1 REPORT                                                           
*   TEST DISPLAY S/P TEAM END                                                   
*                                                                               
LDIR0800 EQU   *                                                                
         LA    R2,LSPOFSTA(R2)     BUMP TO NEXT TABLE ENTRY                     
         B     LDIR0720            GO BACK FOR NEXT                             
*                                                                               
         DROP  R3                                                               
*                                                                               
LDIR0820 EQU   *                                                                
         L     R4,ASPOFFTB         SET A(TABLE)                                 
         L     R3,SALCTR           SET COUNTER                                  
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),14,7,0                                  
                                                                                
**REDISP                                                                        
         L     R2,ASPOFFTB         SET A(SPOFF TABLE FOR REDISPLAY              
LDIR0840 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    LDIR0860                                                         
***      MVC   P+1(08),=C'S/P RED:'                                             
***      MVC   P+09(LSPOFSTA),0(R2)                                             
***      GOTO1 REPORT                                                           
         LA    R2,LSPOFSTA(R2)                                                  
         B     LDIR0840            GO BACK FOR NEXT                             
LDIR0860 EQU   *                                                                
**REDISP                                                                        
****>>                                                                          
         L     R2,ADCONLST                                                      
         USING ADCONSD,R2                                                       
         L     R6,MASTC                                                         
         USING MASTD,R6                                                         
         L     RE,MCUTL            SET A(UTL)                                   
*                                                                               
         DROP R2,R6                                                             
*                                                                               
         MVC   4(1,RE),ORIGUTL     SET OLD FILE UTL                             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,1                                                            
*                                                                               
*   TEST                                                                        
**       MVC   P+1(04),=C'UTL:'                                                 
**       MVC   P+5(1),ORIGUTL                                                   
**       GOTO1 REPORT                                                           
*   TEST DISPLAY                                                                
*                                                                               
         GOTO1 HIGHDIR                                                          
**       MVC   P+1(17),=C'OLD FILE: 1ST REP'                                    
**       MVC   P+20(2),OLDREP                                                   
**       MVC   P+26(27),KEY        DISPLAY KEY FOUND                            
**       GOTO1 REPORT                                                           
                                                                                
         XIT1                                                                   
*                                                                               
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'NREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
NEWUTL   DS    X                                                                
ORIGUTL  DS    X                                                                
AREC     DS    A                                                                
AREPSLST DC    F'0'                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*   LIVESTA:  CHECK AGAINST LIST OF STATIONS BEING PROCESSED                    
******************************************************************              
LIVESTA  NTR1                                                                   
         LA    R1,LISTSEL          SET A(SELTEL STATION LIST)                   
         CLC   OLDREP(4),=C'SZAM'  SELTEL TO KAMNY RUN?                         
         BE    LIVE0020            YES - USE SELTEL STATIONS                    
         LA    R1,LISTCON          SET A(CONTINENTAL STATION LIST)              
         CLC   OLDREP(4),=C'CQAM'  CONTINENTAL TO KAMNY RUN?                    
         BE    LIVE0020            YES - USE CONTINENTAL STATIONS               
         LA    R1,LISTKAM          SET A(KAMNY STATION LIST)                    
         CLC   OLDREP(4),=C'AMSZ'  KAMNY TO SELTEL RUN?                         
         BE    LIVE0020            YES - USE KAMNY STATIONS                     
         LA    R1,LISTAMQ          SET A(AM/CQ STATION LIST)                    
         CLC   OLDREP(4),=C'AMCQ'  KAMNY TO KCONY  RUN?                         
         BE    LIVE0020            YES - USE KAMNY STATIONS                     
         DC    H'0'                UNRECOGNIZED OLD REP CODE                    
LIVE0020 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE?                                
         BE    LIVE0100            YES - SET CC NOT ZERO                        
         CLC   DSPSTA(5,R2),0(R1)  STATION IN TABLE?                            
         BE    LIVE0120            YES                                          
         LA    R1,LLISTSTA(R1)     NO  - BUMP TO NEXT ENTRY                     
         B     LIVE0020            GO BACK FOR NEXT                             
LIVE0100 EQU   *                                                                
         LTR   RB,RB                                                            
         B     LIVE0140                                                         
LIVE0120 EQU   *                                                                
         SR    R0,R0                                                            
LIVE0140 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*   SCANQUOT:  REMOVE ',' FROM " INPUT                                          
******************************************************************              
SCANQUOT NTR1                                                                   
         LA    RF,MYWORK+8           SET A(INPUT)                               
SQUO0020 EQU   *                                                                
         CLI   0(RF),0             BINARY ZERO FOUND?                           
         BE    SQUO0800            YES - FINISHED                               
         CLI   0(RF),C'"'          DOUBLE QUOTE FOUND?                          
         BNE   SQUO0030            NO                                           
***      MVI   0(RF),C' '          YES - REPLACE WITH SPACE                     
         B     SQUO0040            YES                                          
SQUO0030 EQU   *                                                                
         LA    RF,1(RF)            NO  - GO BACK FOR NEXT                       
         B     SQUO0020                                                         
SQUO0040 EQU   *                                                                
         LA    RF,1(RF)            PICK UP NEXT CHARACTER                       
SQUO0060 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD REACHED?                       
         BNE   *+6                 NO                                           
         DC    H'0'                YES - MUST FIND SECOND "                     
         CLI   0(RF),C','                                                       
         BE    SQUO0080            COMMA FOUND                                  
         CLI   0(RF),C'"'                                                       
         BNE   SQUO0040            GO BACK FOR NEXT CHAR                        
***      MVI   0(RF),C' '          REPLACE " WITH SPACE                         
         B     SQUO0800            LAST " FOUND - FINISHED                      
SQUO0080 EQU   *                                                                
         MVI   0(RF),X'40'         REPLACE ',' WITH SPACE                       
         B     SQUO0040            GO BACK FOR NEXT                             
SQUO0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.                     *              
******************************************************************              
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R2,LISTSEL          SET A(SELTEL STATION LIST)                   
         CLC   OLDREP(4),=C'SZAM'  SELTEL TO KAMNY RUN?                         
         BE    CONT0020            YES - USE SELTEL STATIONS                    
         LA    R2,LISTCON          SET A(CONTINENTAL STATION LIST)              
         CLC   OLDREP(4),=C'CQAM'  CONTINENTAL TO KAMNY RUN?                    
         BE    CONT0020            YES - USE CONTINENTAL STATIONS               
         LA    R2,LISTKAM          SET A(KAMNY STATION LIST)                    
         CLC   OLDREP(4),=C'AMSZ'  KAMNY TO SELTEL RUN?                         
         BE    CONT0020            YES - USE KAMNY STATIONS                     
         LA    R2,LISTAMQ          SET A(KAMNY STATION LIST)                    
         CLC   OLDREP(4),=C'AMCQ'  KAMNY TO KCONY  RUN?                         
         BE    CONT0020            YES - USE KAMNY STATIONS                     
         DC    H'0'                UNRECOGNIZED OLD REP CODE                    
CONT0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    CONT0960            YES - FINISHED                               
*                                                                               
*   TEST                                                                        
         CLC   =C'4S',REPREP       TEST RUN FOR WNEP?                           
         BNE   CONT0040            NO  - PROCESS ALL STATIONS IN LIST           
**       CLC   =C'WNEP',0(R2)      SINGLE STATION ONLY: KAMNY TO SELNY          
**       BE    CONT0040                                                         
         CLC   =C'WNNE',0(R2)      SINGLE STATION ONLY: SELNY TO KAMNY          
         BE    CONT0040                                                         
         LA    R2,LLISTSTA(R2)     BUMP TO NEXT STATION IN LIST                 
         B     CONT0020            GO BACK FOR NEXT                             
*   TEST                                                                        
*                                                                               
CONT0040 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6          SET RECORD DEFINITION                        
         MVI   RCON8ETP,X'8E'      SET REPIO PASSIVE KEY TYPE                   
         MVC   RCON8ERP,OLDREP     INSERT OLD REP CODE                          
         MVC   RCON8EST,0(R2)      INSERT STATION                               
*                                                                               
*   TEST START                                                                  
****     MVC   RCON8EFS,=X'C734'   INSERT SEP20/99                              
*   TEST START END                                                              
*                                                                               
         MVC   NEWGROUP,DNEWGRP(R2)       SAVE NEW GROUP                        
*                                                                               
         GOTO1 HIGHDIR                                                          
         B     CONT0080                                                         
                                                                                
CONT0060 DS    0H                                                               
         LA    R6,KEY              RESET R6 ON SEQ READS                        
         GOTO1 SEQDIR                                                           
*                                                                               
*   TEST DISPLAY                                                                
*        MVC   P+1(09),=C'KEY READ:'                                            
*        MVC   P+10(27),RCON8ETP                                                
*        MVC   P+40(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST DISPLAY                                                                
*                                                                               
                                                                                
CONT0080 DS    0H                                                               
         CLI   RCON8EID,1          FIRST RECORD OF 3-REC SET?                   
         BNE   CONT0060            NO  - SKIP OVER RECS 2/3                     
         CLC   KEY(08),KEYSAVE     SAME KEY/REP/STATION?                        
         BE    CONT0100            NO  - GET NEXT TABLE ENTRY                   
         LA    R2,LLISTSTA(R2)     BUMP TO NEXT STATION IN LIST                 
         B     CONT0020            GO BACK FOR NEXT                             
CONT0100 DS    0H                                                               
         L     RF,CONCTR           YES - COUNT TOTAL RECORDS SCANNED            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR           RESTORE COUNTER                              
*                                                                               
CONT0120 EQU   *                                                                
***>>>   CLC   RCON8EFE,=X'C421'   FLIGHT END PRE 1/1/98?                       
         CLC   RCON8EFE,=X'C39D'   FLIGHT END PRE 12/29/97?                     
         BNL   CONT0140                                                         
***      MVC   P+1(16),=C'SKIPPED: DATE   '                                     
***      GOTO1 DATCON,DMCB,(2,RCON8EFE),(5,P+20)                                
***      GOTO1 HEXOUT,DMCB,RCON8ECN,P+32,4,=C'TOG'                              
***      GOTO1 REPORT                                                           
         B     CONT0840                                                         
*                                                                               
         DROP  R6                                                               
CONT0140 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
***      MVC   P+1(09),=C'CONTRACT:'                                            
***      MVC   P+10(27),KEY                                                     
***      GOTO1 REPORT                                                           
         L     RF,CONCTR2          YES - COUNT TOTAL RECORDS IN DATE            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR2          RESTORE COUNTER                              
***      B     CONT0060            GO BACK FOR NEXT                             
*   TEST DISPLAY END                                                            
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED                                 
         GOTO1 GETRECRD                                                         
*                                                                               
         LA    R5,REC              IN CASE RECORD IS DELETED BUT THE            
         USING RCONREC,R5             PASSIVE POINTERS AREN'T                   
*                                                                               
**>>>>                                                                          
         NI    DMINBTS,X'FF'-X'08' RESET                                        
                                                                                
         TM    RCONCNTL,X'80'      SKIP IF RECORD IS REALLY MARKED              
         BO    CONT0820               FOR DELETION                              
*                                                                               
**       CLI   RCONTYPE,C'N'       DON'T TAKE OVER NETWORK ORDERS               
**       BE    CONT0860                                                         
**       CLI   RCONTYPE,C'X'       DON'T TAKE OVER NETWORK ORDERS               
**       BE    CONT0860                                                         
*                                                                               
*        NEED TO CHECK SOFT CONTYPE FOR NETWORK HERE                            
*                                                                               
*        CHECK CONFIRM/VERSION/WIP STATUS:  SKIP WIP'S                          
*                                                                               
*                                                                               
         MVI   DAREORDR,C'N'       SET DARE ORDER TO NO                         
         MVI   DARKTYP,C' '        CLEAR DARE TYPE                              
         LA    R4,RCONELEM         FIND X'1D' DARE ELEMENT                      
CONT0160 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0300            YES - NOT DARE - SKIP WIP CHECK              
         CLI   0(R4),X'1D'         DARE INFO ELEMENT?                           
         BE    CONT0180            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0160            GO BACK FOR NEXT                             
CONT0180 EQU   *                                                                
         MVC   OLDDRLK,RCONDRLK-RCONDREL(R4)                                    
*                                  SAVE ORIG AGENCY ORDER NUMBER                
         MVC   OLDAGY,RCONKAGY     SAVE ORIGINAL AGENCY/OFF                     
         MVI   DAREORDR,C'Y'       SET DARE ORDER TO YES                        
         L     RF,DARECTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DARECTR                                                       
*>*>****                                                                        
         LA    R4,RCONELEM         FIND X'1F' ELEMENT                           
CONT0200 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0240            YES - OFFLINE ORDER - CHECK FURTHER          
         CLI   0(R4),X'1F'         DARE INFO ELEMENT?                           
         BE    CONT0220            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0200            GO BACK FOR NEXT                             
CONT0220 EQU   *                                                                
         TM    RCONCONF-RCONXEL(R4),X'40'                                       
*                                  CONFIRMED NOW?                               
         BO    CONT0230            YES - ACCEPT IT                              
         TM    RCONCONF-RCONXEL(R4),X'20'                                       
*                                  NO  - PREVIOUSLY CONFIRMED?                  
         BNO   CONT0900            NO  - SKIP THIS ORDER                        
CONT0230 EQU   *                                                                
*                                                                               
         MVC   DARKTYP,=X'41'      SET DARE TYPE RECORD                         
         TM    RCONCONF-RCONXEL(R4),X'40'+X'20'                                 
         BZ    CONT0240                                                         
         MVC   DARKTYP,=X'51'      SET DARE TYPE RECORD: FORCE 51S              
*                                                                               
CONT0240 EQU   *                                                                
*                                                                               
         LA    R4,RCONELEM         FIND X'20' ELEMENT                           
CONT0260 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0880            YES - SKIP THIS ORDER                        
*                                     SHOULD HAVE BEEN THERE                    
         CLI   0(R4),X'20'         SEND INFO ELEMENT?                           
         BE    CONT0280            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0260            GO BACK FOR NEXT                             
CONT0280 EQU   *                                                                
         TM    RCONSENF-RCONSEND(R4),X'02'                                      
*                                  LAST CONFIRMED BY STATION?                   
         BO    CONT0300            YES - ACCEPT IT                              
         TM    RCONSENF-RCONSEND(R4),X'20'+X'10'                                
*                                  STA # ADVANCED?                              
         BO    CONT0300            BOTH SIDES ON:  ACCEPT IT                    
         B     CONT0900            IN PROCESS ON ONE SIDE OR                    
*                                     OTHER:  SKIP IT                           
CONT0300 DS    0H                                                               
*                                                                               
*   RETRIEVE CORRECT SALESPERSON CONVERSION INFORMATION                         
*        SALESPERSON CONVERSION CODES HAVE BEEN GIVEN BY K. HOLLERAN            
*        FOR CONTINENTAL TO AMERICAN, SO THEY ARE BEING APPLIED                 
*                                                                               
         GOTO1 SETSALES,DMCB,(RC),(R5)                                          
         BNZ   CONT0920                                                         
*                                                                               
*   RETRIEVE CORRECT AGY/ADVERT CONVERSION INFORMATION                          
*        AGY/ADVERT CONVERSION CODES ARE COMMON BETWEEN CONTINENTAL             
*        AND AMERICAN, SO CODE SETTING AND APPLICATION IS SKIPPED               
*                                                                               
         MVC   OLDADV,RCONKADV     SAVE OLD ADVERTISER CODE                     
         CLC   =C'CQAM',OLDREP     CONTINENTAL TO AMERICAN?                     
         BE    CONT0310            YES - DON'T CHANGE CODES                     
         CLC   =C'AMCQ',OLDREP     AMERICAN TO CONTINENTAL?                     
         BE    CONT0310            YES - DON'T CHANGE CODES                     
*                                                                               
         CLI   NOCODES,C'Y'        SKIP CODES PROCESSING?                       
         BE    CONT0310            YES                                          
         GOTO1 =A(SETCODES),DMCB,(RC),(R5)                                      
         BNZ   CONT0940                                                         
CONT0310 DS    0H                                                               
*>*>****                                                                        
*                                                                               
*  TEST                                                                         
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT0320                                                         
         CLC   CONCTR4,=F'100'     PROCESS 1ST N RECS ACCEPTED                  
         BH    CONT0320                                                         
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 DISPPUT,DMCB,(RC),4 DISPLAY PRE-CHANGE                           
CONT0320 DS    0H                                                               
*  TEST END                                                                     
*                                                                               
**>>>>                                                                          
         MVC   SAVEKEY,KEY         SAVE KEY FOR RESTART                         
         MVC   RCONKREP,REPREP     REP REP CODE                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,MYWORK,4,=C'TOG'                            
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'CON#:'                                                
*        MVC   P+6(8),MYWORK                                                    
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         PACK  DUB(8),MYWORK(8)                                                 
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'PAK#:'                                                
*        MVC   P+6(8),DUB                                                       
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   =C'SZAM',OLDREP     SELTEL TO KAMNY RUN?                         
         BNE   CONT0340            NO  -                                        
         AP    DUB(8),=P'2000000'  YES - ADD THIS CONT# ADJUSTMENT              
         B     CONT0400                                                         
CONT0340 EQU   *                                                                
         CLC   =C'CQAM',OLDREP     CONTINENTAL TO KAMNY RUN?                    
         BNE   CONT0360            NO  -                                        
         AP    DUB(8),=P'1600000'  YES - ADD THIS CONT# ADJUSTMENT              
         B     CONT0400                                                         
CONT0360 EQU   *                                                                
         CLC   =C'AMSZ',OLDREP     KAMNY TO SELTEL RUN?                         
         BNE   CONT0380            NO  -                                        
         AP    DUB(8),=P'1000000'  YES - ADD THIS CONT# ADJUSTMENT              
         B     CONT0400                                                         
CONT0380 EQU   *                                                                
         CLC   =C'AMCQ',OLDREP     KAMNY TO CONTINENTAL RUN?                    
         BNE   CONT0390            NO  -                                        
         AP    DUB(8),=P'1000000'  YES - ADD THIS CONT# ADJUSTMENT              
         B     CONT0400                                                         
CONT0390 EQU   *                                                                
         DC    H'0'                UNRECOGNIZED OLD REP CODE                    
CONT0400 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'PAK+:'                                                
*        MVC   P+6(8),DUB                                                       
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         UNPK  MYWORK(8),DUB(8)                                                 
         OI    MYWORK+7,X'F0'      TURN ON ALL ZONE BITS                        
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'UNP#:'                                                
*        MVC   P+6(8),MYWORK                                                    
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 =V(HEXIN),DMCB,MYWORK,MYWORK+12,8,=C'TOG'                        
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'C#IN:'                                                
*        MVC   P+6(4),MYWORK+12                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   OLDCON,RCONKCON     SAVE ORIGINAL CONTRACT #                     
         CLC   HIGHCON,OLDCON      GREATER THAN LAST HIGH?                      
         BH    CONT0420            NO                                           
         MVC   HIGHCON,OLDCON      YES - REPLACE LAST WITH THIS ONE             
CONT0420 EQU   *                                                                
         CLC   LOWCON,OLDCON       LESS THAN LAST LOW?                          
         BL    CONT0440            NO                                           
         MVC   LOWCON,OLDCON       YES - REPLACE LAST WITH THIS ONE             
CONT0440 EQU   *                                                                
         MVC   NEWCON,MYWORK+12    SAVE NEW CON# FOR BUYS/MG/ETC                
*                                                                               
*   CALCULATE NEW CONTRACT NUMBER REVERSED FOR BUYLINES                         
*                                                                               
         ZAP   MYWORK+30(5),=P'0'                                               
         MVO   MYWORK+30(5),NEWCON                                              
         ZAP   MYWORK+25(5),=P'99999999'                                        
         SP    MYWORK+25(5),MYWORK+30(5) GET 9'S COMPLEMENT                     
         MVO   MYWORK+20(5),MYWORK+25(5) CHANGE TO PWOS                         
                                                                                
         PACK  NEWCONRV+0(1),MYWORK+23(1) REVERSED 9'COMP OF K NUM              
         PACK  NEWCONRV+1(1),MYWORK+22(1)                                       
         PACK  NEWCONRV+2(1),MYWORK+21(1)                                       
         PACK  NEWCONRV+3(1),MYWORK+20(1)                                       
*                                                                               
*   TEST DISPLAY                                                                
**       MVC   P+1(13),=C'BUYLINE CON#:'                                        
**       MVC   P+14(4),NEWCONRV                                                 
**       GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         MVC   RCONKCON,MYWORK+12  INSERT ADJUSTED CONTRACT NUMBER              
         MVC   RCONKGRP,NEWGROUP   INSERT NEW GROUP FOR STATION                 
         MVC   RCONSAL,SAVESAL     INSERT NEW S/P                               
         MVC   RCONKOFF,SAVEKOFF   INSERT NEW OFFICE                            
         MVC   RCONTEM,SAVETEM     INSERT NEW TEAM                              
         CLC   =C'CQAM',OLDREP     CONTINENTAL TO AMERICAN?                     
         BE    CONT0450            YES - USE EXISTING CODES                     
         CLC   =C'AMCQ',OLDREP     AMERICAN TO CONTINENTAL?                     
         BE    CONT0450            YES - USE EXISTING CODES                     
         MVC   RCONKAGY(6),SAVEAGY INSERT AGENCY/AGY OFF                        
         MVC   RCONKADV,SAVEADV    INSERT ADVERTISER                            
CONT0450 EQU   *                                                                
         GOTO1 SETELTS,DMCB,(RC),(R5)                                           
         GOTO1 SASSIST,DMCB,(RC),(R5)                                           
*                                                                               
**+++                                                                           
         LA    R4,RCONELEM         FIND X'20' ELEMENT                           
CONT0460 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0490            YES -                                        
         CLI   0(R4),X'20'         SEND INFO ELEMENT?                           
         BE    CONT0480            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0460            GO BACK FOR NEXT                             
CONT0480 EQU   *                                                                
**+++                                                                           
         XC    2(2,R4),2(R4)       FOUND:  SET SEND ID TO ZERO                  
*                                                                               
*   REP SENDING ID IS BEING CLEARED.  CONTRACT PROGRAM WILL CHECK               
*        SELTEL, AMERICAN, AND CONTINENTAL FOR EMPTY, AND REINSERT.             
*                                                                               
         MVI   COVERFLG,C'N'       SET COVERFLAG TO NO                          
         LA    R4,RCONELEM         FIND X'20' ELEMENT                           
CONT0490 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0496            YES -                                        
         CLI   0(R4),X'A6'         SEND INFO ELEMENT?                           
         BE    CONT0493            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0490            GO BACK FOR NEXT                             
CONT0493 EQU   *                                                                
         MVC   RCONCVNM+4-RCONCVEL(4,R4),NEWCON                                 
*                                  INSERT NEW CONTRACT NUMBER INTO ELT          
         MVI   COVERFLG,C'Y'       SET COVERFLAG TO YES                         
CONT0496 EQU   *                                                                
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BE    CONT0500            NO  - DON'T RETRIEVE                         
         GOTO1 PRODCODE,DMCB,(RC),RCONREC                                       
*                                  CHECK/PROCESS PRODUCT CODE                   
         L     RF,CONPROD                                                       
         LA    RF,1(RF)                                                         
         ST    RF,CONPROD                                                       
CONT0500 EQU   *                                                                
         MVC   OLDSTA,RCONKSTA     SAVE STATION CALL LETTERS                    
         MVC   OLDOFF,RCONKOFF     SAVE OFFICE CODE                             
*                                                                               
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR DISPLAY                    
         BAS   RE,PUTRECS                                                       
*                                                                               
         L     RF,CONCTR4          COUNT TOTAL RECORDS ACCEPTED                 
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR4          RESTORE COUNTER                              
*                                                                               
*                                                                               
*  TEST                                                                         
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT0520                                                         
         CLC   CONCTR4,=F'100'     PROCESS 1ST N RECS ACCEPTED                  
         BH    CONT0520                                                         
         GOTO1 DISPPUT,DMCB,(RC),0 DISPLAY POST-CHANGE                          
CONT0520 DS    0H                                                               
*  TEST END                                                                     
*                                                                               
         L     RF,CONCTR3          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR3                                                       
         CLC   CONCTR3,=F'1000'    DISPLAY EVERY N RECORDS                      
         BNE   CONT0540                                                         
         XC    CONCTR3,CONCTR3                                                  
***      MVC   P+1(21),=C'PROCESSING CONTRACTS:'                                
***      EDIT  CONCTR,(7,P+24)                                                  
***      GOTO1 REPORT                                                           
CONT0540 EQU   *                                                                
*                                                                               
*  TEST                                                                         
**       CLC   MISSAGYS,=F'3'                                                   
**       BL    TEST0020            BEGIN TESTING AT 5 MISSING                   
**       CLC   MISSADVS,=F'3'                                                   
**       BH    CONT0960            3+ AGIES, 3 ADVS: END                        
                                                                                
**       CLC   CONCTR4,=F'500'     PROCESS 1ST N RECS ACCEPTED                  
**       BH    CONT0960                                                         
TEST0020 EQU   *                                                                
*  TEST END                                                                     
*                                                                               
                                                                                
         ZICM  RF,RCONLEN,2        ADD ALL CONTRACT LENGTHS                     
         A     RF,CONBYTES                                                      
         ST    RF,CONBYTES                                                      
*                                                                               
                                                                                
         BAS   RE,BUYPROC2         PROCESS THIS CONTRACT'S BUYS                 
         BAS   RE,MKGDPROC         PROCESS THIS CONTRACT'S M/GS                 
         CLI   DAREORDR,C'N'       DARE ORDER?                                  
         BE    CONT0610            NO                                           
         GOTO1 =A(TAKEDARE),DMCB,(RC)                                           
CONT0610 EQU   *                                                                
         CLI   COVERFLG,C'N'       COVERFLAG PRESENT                            
         BE    CONT0620            NO  - DON'T PROCESS                          
         BAS   RE,TAKECOV          YES - GET COVERSHEET                         
CONT0620 EQU   *                                                                
         BAS   RE,TAKECFC          GET CONFIRM COMMENT                          
         MVC   KEY,SAVEKEY         RESTART CONTRACTS                            
         GOTO1 HIGHDIR             REREAD THIS KEY                              
         B     CONT0060                                                         
                                                                                
CONT0820 DS    0H                                                               
         L     RF,CONDEL           COUNT DELETED CONTRACTS                      
         LA    RF,1(RF)                                                         
         ST    RF,CONDEL                                                        
         MVC   P+1(16),=C'SKIPPED: DELETED'                                     
         GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0840 DS    0H                                                               
         L     RF,CONDATE          COUNT DATED CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,CONDATE                                                       
         B     CONT0060            GO BACK FOR NEXT                             
CONT0860 DS    0H                                                               
         L     RF,CONCTYPE         COUNT CTYPE CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTYPE                                                      
         MVC   P+1(16),=C'SKIPPED: CONTYPE'                                     
         MVC   P+18(1),RCONTYPE                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0880 DS    0H                                                               
         L     RF,NO20CTR          COUNT NO 20 CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,NO20CTR                                                       
         MVC   P+1(16),=C'SKIPPED: NO 20  '                                     
***>>    GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0900 DS    0H                                                               
         L     RF,CONWIP           COUNT WIP   CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,CONWIP                                                        
         CLC   RCONBUYR(8),=C'FORECAST'                                         
         BE    CONT0910                                                         
         MVC   P+1(17),=C'SKIPPED: WIP/DARE'                                    
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
         GOTO1 DATCON,DMCB,(3,RCONCREA),(5,P+30)                                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,P+40)                                
         MVI   P+48,C'-'                                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,P+49)                              
         MVC   P+59(20),RCONBUYR                                                
         GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0910 DS    0H                                                               
         L     RF,CONWIPFC         COUNT WIPFC CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,CONWIPFC                                                      
         B     CONT0060            GO BACK FOR NEXT                             
CONT0920 DS    0H                                                               
         MVC   P+1(16),=C'SKIPPED: NO S/P '                                     
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
         MVC   P+32(5),RCONKSTA                                                 
         MVC   P+38(2),RCONKOFF                                                 
         GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0940 DS    0H                                                               
**       MVC   P+1(22),=C'SKIPPED: NO AGY OR ADV'                               
**       GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0960 DS    0H                                                               
         XIT1                                                                   
         DROP  R5                                                               
LISTSEL  DC    C'WNNE T '          SELTEL TO KAMNY                              
         DC    X'0000'             DELIMITER                                    
LISTCON  DC    C'WLKY T '          CONTINENTAL TO KAMNY                         
         DC    X'0000'             DELIMITER                                    
LISTKAM  DC    C'WNEP L '          KAMNY TO SELTEL                              
LLISTSTA EQU   *-LISTKAM           LENGTH EQUATE                                
         DC    C'WVUE L '          KAMNY TO SELTEL                              
         DC    C'WFTC L '          KAMNY TO SELTEL                              
         DC    C'KUSI X '          KAMNY TO SELTEL                              
         DC    C'WAWS X '          KAMNY TO SELTEL                              
         DC    C'WUTV T '          KAMNY TO SELTEL                              
         DC    C'KLRT L '          KAMNY TO SELTEL                              
         DC    C'KASN L '          KAMNY TO SELTEL                              
         DC    C'KFOR L '          KAMNY TO SELTEL                              
         DC    C'WNAC T '          KAMNY TO SELTEL                              
         DC    C'WHO  T '          KAMNY TO SELTEL                              
         DC    C'WQAD L '          KAMNY TO SELTEL                              
         DC    C'WTEV T '          KAMNY TO SELTEL                              
***      DC    C'KTBU L '          KAMNY TO SELTEL (OUT AS OF 10/14)            
         DC    X'0000'             DELIMITER                                    
LISTAMQ  DC    C'KRQE X '          KAMNY TO KCONY                               
         DC    C'WDTN L '          KAMNY TO KCONY                               
         DC    C'KASY X '          KAMNY TO KCONY                               
         DC    C'KMTV T '          KAMNY TO KCONY                               
         DC    X'0000'             DELIMITER                                    
DNEWGRP  EQU   5                                                                
NEWGROUP DS    CL2                                                              
MYWORK   DS    CL64                                                             
MYWORK2  DS    CL64                                                             
OLDCON   DS    CL4                 ORIGINAL CONTRACT NUMBER                     
NEWCON   DS    CL4                 NEW      CONTRACT NUMBER                     
NEWCONRV DS    CL4                 NEW      CONTRACT NUMBER (REV)               
OLDSTA   DS    CL5                                                              
OLDOFF   DS    CL2                                                              
OLDADV   DS    CL4                                                              
OLDAGY   DS    CL6                 ORIGINAL AGENCY/OFFICE                       
OLDDRLK  DS    XL4                                                              
ELEM     DS    CL64                                                             
*                                                                               
*   SALESPERSON INSERTION TABLE.  EACH CONTRACT WILL BE ASSIGNED                
*        TO A S/P BASED ON STATION/OFF.  S/P OFFICE AS WELL AS                  
*        S/P TEAM WILL BE TAKEN FROM THE S/P RECORD.                            
*        A DEFAULT ENTRY MAY BE ENTERED AS OFFICE='??'.  IF STATION/            
*            OFFICE IS NOT FOUND, ?? VALUES WILL BE PLUGGED IN.                 
*        ENTRY DEFINITION:                                                      
*        BYTES  01 - 05  = SOURCE STATION CALLS                                 
*        BYTES  06 - 07  = SOURCE ORIGINAL OFFICE                               
*        BYTES  08 - 10  = TARGET SALESPERSON CODE                              
*        BYTES  11 - 12  = TARGET S/P OFFICE (FILL IN)                          
*        BYTES  13 - 14  = TARGET S/P TEAM   (FILL IN)                          
*                                                                               
*    THIS TABLE NOT USED.  TABLE IS LOADED FROM DISK FILE IN                    
*        THIS FORMAT AT ASPOFFTB.                                               
*                                                                               
**STAS/P                                                                        
SPOFFSEL DC    C'WNNE ??AAAAAAA'   DEFAULT FOR STATION                          
         DC    X'0000'             DELIMITER                                    
SPOFFCON DC    C'WLKY ??BBBBBBB'   CONTINENTAL TO KAMNY                         
         DC    X'0000'             DELIMITER                                    
SPOFFKAM DC    C'KRQE ??CCCCCCC'   KAMNY TO SELTEL                              
LSPOFSTA EQU   *-SPOFFKAM          LENGTH EQUATE                                
         DC    C'WDTN ??DDDDDDD'   KAMNY TO SELTEL                              
         DC    C'KASY ??EEEEEEE'   KAMNY TO SELTEL                              
         DC    C'WNEP ??FFFFFFF'   KAMNY TO SELTEL                              
         DC    C'WVUE ??GGGGGGG'   KAMNY TO SELTEL                              
         DC    C'WFTC ??HHHHHHH'   KAMNY TO SELTEL                              
         DC    C'KUSI ??IIIIIII'   KAMNY TO SELTEL                              
         DC    C'WAWS ??JJJJJJJ'   KAMNY TO SELTEL                              
         DC    C'WUTV ??KKKKKKK'   KAMNY TO SELTEL                              
         DC    C'KLRT ??LLLLLLL'   KAMNY TO SELTEL                              
         DC    C'KASN ??MMMMMMM'   KAMNY TO SELTEL                              
         DC    C'KFOR ??NNNNNNN'   KAMNY TO SELTEL                              
         DC    C'WNAC ??OOOOOOO'   KAMNY TO SELTEL                              
         DC    C'WHO  ??PPPPPPP'   KAMNY TO SELTEL                              
         DC    C'WQAD ??QQQQQQQ'   KAMNY TO SELTEL                              
         DC    C'WTEV ??RRRRRRR'   KAMNY TO SELTEL                              
         DC    C'KTBU ??SSSSSSS'   KAMNY TO SELTEL                              
         DC    X'0000'             DELIMITER                                    
**STAS/P                                                                        
DSPSTA   EQU   0                   DISPLACE TO STATION                          
DSPOFF   EQU   5                   DISPLACE TO OFFICE                           
DSPSP    EQU   7                   DISPLACE TO SALESPERSON                      
DSPNEWOF EQU   10                  DISPLACE TO NEW OFFICE                       
DSPNEWTM EQU   12                  DISPLACE TO NEW TEAM                         
         EJECT                                                                  
*                                                                               
******************************************************************              
*   TAKECOV:  MOVE COVERSHEET RECORDS.                                          
******************************************************************              
TAKECOV  NTR1                                                                   
*                                                                               
**       MVC   P+1(08),=C'TAKECOV '                                             
**       GOTO1 REPORT                                                           
*                                                                               
         LA    R2,REC                                                           
         USING RCOVREC,R2          SET ADDRESSABILITY TO COV RECORD             
*                                                                               
         XC    RCOVREC(32),RCOVREC                                              
         MVI   RCOVKTYP,X'49'      SET COVER SHEET RECORD TYPE                  
         MVC   RCOVKREP,OLDREP     INSERT SOURCE REP INTO KEY                   
         MVI   RCOVKNAM,X'FF'      SET 'CONTRACT COVERSHEET' FLAG               
         MVC   RCOVKNAM+4(4),OLDCON                                             
*                                  INSERT ORIG CONTRACT NUMBER                  
         MVC   KEY,RCOVREC                                                      
         GOTO1 HIGHDIR                                                          
         B     TKCV0040                                                         
TKCV0020 EQU   *                                                                
         GOTO1 SEQDIR              READ NEXT KEY                                
TKCV0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME KEY, UP TO SEQ #?                       
         BNE   TKCV0080            NO  - FINISHED WITH COVERSHEET               
         MVC   KEYSAVE,KEY         SAVE KEY ACCESSED FOR RESTART                
TKCV0060 EQU   *                                                                
         GOTO1 GETRECRD                                                         
*                                                                               
         XC    REC-4(4),REC-4                                                   
         MVC   RCOVKREP,REPREP     INSERT TARGET REP INTO KEY                   
         MVC   RCOVKNAM+4(4),NEWCON                                             
*                                  INSERT NEW CONTRACT NUMBER                   
         MVC   REC-4(2),RCOVLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 PUTRECS                                                          
*                                                                               
         L     RF,COVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,COVCTR                                                        
*                                                                               
         CLC   COVCTR,=F'25'                                                    
         BH    TKCV0020                                                         
*                                                                               
***      MVC   P+1(11),=C'COVERSHEET:'                                          
***      MVC   P+12(27),RCOVREC                                                 
***      GOTO1 REPORT                                                           
*                                                                               
         B     TKCV0020            GO BACK FOR NEXT RECORD                      
TKCV0080 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**TKCV**                                                                        
**CONFIRM COMMENT                                                               
*                                                                               
*   TAKECFC:  LOOK FOR CONFIRM COMMENT RECORD.  IF PRESENT, MOVE                
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
TAKECFC  NTR1                                                                   
***>>>                                                                          
         LA    R2,REC                                                           
         USING RCFCREC,R2          SET ADDRESSABILITY TO CFC RECORD             
*                                                                               
         XC    RCFCREC(32),RCFCREC                                              
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,OLDREP     INSERT SOURCE REP INTO KEY                   
         MVC   RCFCKCON,OLDCON     INSERT SOURCE CON # INTO KEY                 
         MVC   KEY,RCFCREC                                                      
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RCFCKEY),KEYSAVE                                           
         BNE   TKCF0100            NOT FOUND - EXIT                             
TKCF0020 EQU   *                                                                
         GOTO1 GETRECRD                                                         
*                                                                               
         MVC   RCFCKREP,REPREP     INSERT TARGET REP INTO KEY                   
         MVC   RCFCKCON,NEWCON     INSERT NEW CONTRACT NUMBER                   
*                                                                               
         XC    REC-4(4),REC-4                                                   
         MVC   REC-4(2),RCFCLEN    INSERT RECORD LENGTH                         
         GOTO1 PUTRECS                                                          
         L     RF,CFCCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CFCCTR                                                        
*                                                                               
         CLC   CFCCTR,=F'25'                                                    
         BH    TKCF0100                                                         
*                                                                               
**       MVC   P+1(11),=C'CONFIRM   :'                                          
**       MVC   P+12(27),RCFCREC                                                 
**       GOTO1 REPORT                                                           
TKCF0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
*                                                                               
**CONFIRM COMMENT                                                               
         EJECT                                                                  
**COVERSHEET                                                                    
******************************************************************              
*  SETSALES: FIND STATION/OFFICE IN STATION/OFFICE TABLE.        *              
*        IF FOUND, REPLACE SALESPERSON, S/P TEAM, S/P OFFICE     *              
*        IN CONTRACT.                                            *              
*        IF NOT  , SET CC NOT ZERO ON RETURN, SKIP CONTRACT      *              
*                                                                *              
*                                                                *              
******************************************************************              
SETSALES NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD IN PROCESS)          
*                                                                               
         USING RCONREC,R5                                                       
         L     R2,ASPOFFTB         SET A(SALESPERSON STA/OFF LIST)              
         MVC   DUB(5),RCONKSTA     SET UP SEARCH ARGUMENT                       
         MVC   DUB+5(2),RCONKOFF                                                
         L     R4,SALCTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),14,7,(R4)                         
                                                                                
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SSAL0020            YES                                          
         L     RF,NOSPCTR          COUNT MISSING S/P ORDERS                     
         LA    RF,1(RF)                                                         
         ST    RF,NOSPCTR                                                       
         B     SSAL0800            EXIT CC NOT ZERO                             
SSAL0020 EQU   *                                                                
         L     R2,DMCB             SET A(RECORD FOUND)                          
*                                                                               
*                                                                               
**       MVC   P+1(04),=C'SAL:'                                                 
**       MVC   P+05(05),RCONKSTA                                                
**       MVI   P+10,C'/'                                                        
**       MVC   P+11(05),DSPSTA(R2)                                              
**       GOTO1 REPORT                                                           
*                                                                               
         OC    DSPSP(7,R2),DSPSP(R2)                                            
*                                  ANY ENTRY ON NEW SIDE?                       
         BZ    SSAL0800            NO  - NEW SIDE IS EMPTY                      
         MVC   SAVESAL,DSPSP(R2)   YES - SAVE   NEW S/P                         
         MVC   SAVEKOFF,DSPNEWOF(R2)     SAVE   NEW OFFICE                      
         MVC   SAVETEM,DSPNEWTM(R2)      SAVE   NEW TEAM                        
         SR    R0,R0               SET CC ZERO                                  
         B     SSAL0820            EXIT                                         
SSAL0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
SSAL0820 EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
SAVESAL  DS    CL3                                                              
SAVEKOFF DS    CL2                                                              
SAVETEM  DS    CL2                                                              
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SETELTS:  ADD 1C DARE TAKEOVER ELEMENT AND 2A MOVE HISTORY    *              
*        ELEMENT.  THIS PERMITS TRACKING A DUPLICATE TAKEOVER    *              
*        LOCKOUT.                                                *              
*        TREAT COMMENTS TO INSERT TKO EFF COMMENT                *              
*                                                                *              
******************************************************************              
SETELTS  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD IN PROCESS)          
         USING RCONREC,R5                                                       
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'2A',RCONREC),0,0            
*                                  DELETE OLD TKO/MOVE HIST ELT                 
*                                  INSERT NEW TKO/MOVE HIST ELT                 
         XC    ELEM(RCONMMLQ),ELEM                                              
         MVI   ELEM,X'2A'                                                       
         MVI   ELEM+1,RCONMMLQ     NEW ELEMENT LENGTH                           
         GOTO1 DATCON,DMCB,(5,WORK),(2,ELEM+2)                                  
*                                  INSERT DATE OF TAKEOVER                      
         MVC   ELEM+4(4),OLDCON    INSERT ORIGINAL CONTRACT NUMBER              
         MVC   ELEM+8(2),OLDREP    INSERT ORIGINAL SOURCE REP                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELEM,0                 
*                                  INSERT NEW ELEMENT INTO RECORD               
*                                                                               
*   IF THERE WAS AN ORIGINAL 1C ELT, IT IS NOT DROPPED BY THE TAKEOVER          
*        PROGRAM, SO IT IS NOT BEING DROPPED HERE.  CONSISTENCY.                
*                                                                               
***>>>   GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'1C',RCONREC),0,0            
*                                  DELETE DARE TAKEOVER ELT                     
*                                  INSERT DARE TAKEOVER ELT                     
         XC    ELEM(RCONTKLQ),ELEM                                              
         MVI   ELEM,X'1C'                                                       
         MVI   ELEM+1,RCONTKLQ     NEW ELEMENT LENGTH                           
         MVC   ELEM+2(2),OLDREP    INSERT SOURCE REP                            
         MVC   ELEM+4(4),OLDCON    INSERT ORIGINAL CONTRACT NUMBER              
         GOTO1 DATCON,DMCB,(5,WORK),(2,ELEM+8)                                  
*                                  INSERT DATE OF TAKEOVER                      
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,ELEM+10        INSERT TIME                                  
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELEM,0                 
*                                  INSERT NEW ELEMENT INTO RECORD               
**TKO EFF                                                                       
***      GOTO1 DATCON,DMCB,(3,BEFFDATE),(5,NWCOMDAT)                            
         MVC   NWCOMDAT,=C'JAN01/98'                                            
*                                  INSERT EFFECTIVE DATE INTO ELEMENT           
         MVC   NWOLDREP,=C'SELTEL             '                                 
         CLC   =C'SZAM',OLDREP     SELTEL TO KAMNY RUN?                         
         BE    SETE0020            YES - USE SELTEL NAME                        
         MVC   NWOLDREP,=C'KATZ CONTINENTAL   '                                 
         CLC   =C'CQAM',OLDREP     CONTINENTAL TO KAMNY RUN?                    
         BE    SETE0020            YES - USE CONTINENTAL NAME                   
         MVC   NWOLDREP,=C'KATZ AMERICAN      '                                 
         CLC   =C'AMSZ',OLDREP     KAMNY TO SELTEL RUN?                         
         BE    SETE0020            YES - USE KAMNY NAME                         
         MVC   NWOLDREP,=C'KATZ AMERICAN      '                                 
         CLC   =C'AMCQ',OLDREP     KAMNY TO CONTINENTAL RUN?                    
         BE    SETE0020            YES - USE CONTINENTAL NAME                   
         DC    H'0'                UNRECOGNIZED OLD REP CODE                    
SETE0020 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,OLDCON,NWOLDCON,4,=C'TOG'                            
*                                  INSERT ORIGINAL CONTRACT NUMBER              
         SR    R1,R1               CLEAR COUNTER                                
         MVI   NWCOMELT,2          SET TO 'CONTRACT COMMENT ELEMENT'            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'        FIND CONTRACT COMMENT ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   SETE0220            NO COMMENT ELEMENT:  INSERT                  
*                                     TKO COMT AS CONTRACT COMMENT              
         B     SETE0180            COMMENT FOUND: CHECK IT                      
SETE0160 EQU   *                                                                
         BAS   RE,NEXTEL           LOOK FOR NEXT COMMENT                        
         BNE   SETE0240            NO COMMENT ELEMENT                           
SETE0180 EQU   *                                                                
         LA    R1,1(R1)            INCREMENT COUNTER                            
         CLC   =C'C=',2(R6)        STANDARD COMMENT?                            
         BE    SETE0200            YES - REUSE IT                               
         CLC   =C'SC=',2(R6)       STORED COMMENT?                              
         BNE   SETE0160            NO  - GO CHECK NEXT COMMENT                  
SETE0200 EQU   *                                                                
         MVI   0(R6),X'FF'         SET CONTRACT COMMENT TO DELETE               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0            
*                                  DELETE CONTRACT COMMENT                      
SETE0220 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NWCOMELT,0             
*                                  INSERT CONTRACT COMMENT ELEMENT              
         B     SETE0320            EXIT                                         
SETE0240 EQU   *                                                                
         STC   R1,BYTE             ONLY 1 COMMENT IN RECORD?                    
         CLI   BYTE,1                                                           
         BE    SETE0220            YES - INSERT TKO CMT AS 2ND COMMENT          
*                                  NO  - NO ROOM: CHECK ORDER COMMENT           
         SR    R1,R1               CLEAR COUNTER                                
         MVI   NWCOMELT,X'82'      SET TO 'ORDER COMMENT ELEMENT'               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'        FIND ORDER COMMENT ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   SETE0220            NO COMMENT ELEMENT:  INSERT                  
         B     SETE0280                                                         
*                                     TKO COMT AS ORDER COMMENT                 
SETE0260 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   SETE0300            NOT FOUND                                    
SETE0280 EQU   *                                                                
         ST    R6,FULL             SAVE A(LAST R6 FOUND)                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         B     SETE0260            GO BACK FOR NEXT                             
SETE0300 EQU   *                                                                
         STC   R1,BYTE             CHECK COUNTER                                
         CLI   BYTE,10             ALL ORDER COMMENTS IN USE?                   
         BNE   SETE0220            NO  - ADD COMMENT AS NEXT ONE                
         L     R6,FULL             YES - RESET A(LAST COMMENT)                  
         B     SETE0200            DELETE LAST COMMENT, INSERT                  
*                                     TAKEOVER COMMENT AS LAST                  
SETE0320 EQU   *                                                                
         XIT1                                                                   
*                                                                               
NWCOMELT DC    X'003C'     (2)     NEW COMMENT ELEMENT W/TAKEOVER               
         DC    X'FD'       (1)     FORCE TO SORT LAST                           
         DC    C'TKO '     (4)                                                  
         DC    C'EFF '     (4)                                                  
NWCOMDAT DS    CL8         (8)                                                  
         DC    C' OLD REP/CON '  (13)                                           
NWOLDREP DS    CL19              (19)                                           
         DC    C'#'              (1)                                            
NWOLDCON DS    CL8               (8)                                            
LNWCOMEL EQU   *-NWCOMELT                                                       
**TKO EFF                                                                       
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
******************************************************************              
*  SASSIST:  CLEAR THE SALES ASSISTANT ENTERED IN THE ORDER      *              
*                                                                *              
*                                                                *              
******************************************************************              
SASSIST  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD IN PROCESS)          
         USING RCONREC,R5                                                       
         LA    RF,RCONELEM                                                      
SASS0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    SASS0060            YES - NO X'9F' ELT                           
         CLI   0(RF),X'9F'         EXTRA DESCRIPTOR ELT?                        
         BE    SASS0040            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     SASS0020            GO BACK FOR NEXT                             
SASS0040 EQU   *                                                                
         XC    RCONXAST-RCONXXEL(9,RF),RCONXAST-RCONXXEL(RF)                    
*                                  RESET SALES ASSISTANT                        
SASS0060 EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
******************************************************************              
*  BUYPROC2:  RETRIEVE ALL BUY RECORDS ATTACHED TO CONTRACT EXTRACTED           
******************************************************************              
BUYPROC2 NTR1                                                                   
*                                                                               
*   USE OLD CONTRACT NUMBER TO RETRIEVE BUYLINES                                
*                                                                               
***      MVC   P+1(12),=C'BUY: OLDCON='                                         
***      MVC   P+12(4),OLDCON                                                   
***      GOTO1 REPORT                                                           
*                                                                               
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),OLDCON   CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5) GET 9'S COMPLEMENT                          
         MVO   WORK(5),WORK+5(5)   CHANGE TO PWOS                               
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP BUY RECD KEY                          
         MVC   KEY+16(2),OLDREP    INSERT OLD REP CODE                          
         PACK  KEY+18(1),WORK+3(1) REVERSED 9'COMP OF K NUM                     
         PACK  KEY+19(1),WORK+2(1)                                              
         PACK  KEY+20(1),WORK+1(1)                                              
         PACK  KEY+21(1),WORK(1)                                                
         MVC   SAVE9CMP,KEY+18     SAVE 9'S COMP VALUE                          
*                                     ORIGINAL CONTRACT NUMBER                  
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     BUY20                                                            
                                                                                
BUY10    DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
                                                                                
BUY20    DS    0H                                                               
*                                                                               
***      MVC   P+1(12),=C'BUY: KEYS  ='                                         
***      MVC   P+12(27),KEY                                                     
***      MVI   P+39,C'/'                                                        
***      MVC   P+40(27),KEYSAVE                                                 
***      GOTO1 REPORT                                                           
*                                                                               
         CLC   KEY(22),KEYSAVE     SAME CONTRACT?                               
         BNE   BUYX                NO  - FINISHED                               
                                                                                
         L     RF,BUYCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
                                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD            YES - RETRIEVE RECORD                        
*                                                                               
         LA    R6,REC                                                           
         USING RBUYREC,R6                                                       
*                                                                               
         CLI   QUESTOR+7,C'Y'                                                   
         BNE   BUY25                                                            
         CLC   BUYCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    BUY25                                                            
         MVC   P+1(13),=C'BUY LNE PRE :'                                        
         EDIT  RBUYKMLN,(3,P+15),ALIGN=LEFT                                     
         MVC   P+20(27),RBUYREC                                                 
         GOTO1 REPORT                                                           
                                                                                
BUY25    DS    0H                                                               
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         MVC   REC-4(2),RBUYLEN                                                 
         MVC   RBUYKREP,REPREP     INSERT REPLACEMENT REP CODE                  
         MVC   RBUYKCON,NEWCONRV   INSERT NEW CONTRACT #                        
                                                                                
         ZICM  RF,RBUYLEN,2        ADD ALL BUY LENGTHS                          
         A     RF,BUYBYTES                                                      
         ST    RF,BUYBYTES                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+7,C'Y'                                                   
         BNE   BUY30                                                            
         CLC   BUYCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    BUY30                                                            
         MVC   P+1(13),=C'BUY LNE POST:'                                        
         EDIT  RBUYKMLN,(3,P+15),ALIGN=LEFT                                     
         MVC   P+20(27),RBUYREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
BUY30    DS    0H                                                               
         B     BUY10                                                            
                                                                                
BUYX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  MKGDPROC:  RETRIEVE ALL BUY RECORDS ATTACHED TO CONTRACT EXTRACTED           
******************************************************************              
MKGDPROC NTR1                                                                   
*                                                                               
*   USE OLD CONTRACT NUMBER TO RETRIEVE BUYLINES                                
*                                                                               
***      MVC   P+1(12),=C'M/G: OLDCON='                                         
***      MVC   P+12(4),OLDCON                                                   
***      GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'11'           SET UP M/G RECD KEY                          
         MVC   KEY+06(2),OLDREP    INSERT OLD REP CODE                          
         MVC   KEY+08(2),OLDOFF    INSERT CONTRACT OFFICE                       
         MVC   KEY+10(5),OLDSTA                                                 
         MVC   KEY+15(4),SAVE9CMP  INSERT 9'S COMP CON#                         
                                                                                
*                                                                               
***      MVC   P+1(04),=C'MKG='                                                 
***      MVC   P+15(27),KEY                                                     
***      MVC   P+8(4),OLDCON                                                    
***      GOTO1 REPORT                                                           
*                                                                               
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     MKGD0040                                                         
                                                                                
MKGD0020 DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
                                                                                
MKGD0040 DS    0H                                                               
         CLC   KEY(19),KEYSAVE     SAME CONTRACT?                               
         BNE   MKGD0080            NO  - FINISHED                               
                                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD            YES - RETRIEVE RECORD                        
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         LA    R6,REC                                                           
         USING RMKGREC,R6                                                       
         MVC   REC-4(2),RMKGLEN                                                 
         MVC   RMKGKREP,REPREP     INSERT REPLACEMENT REP CODE                  
         MVC   RMKGKCON,NEWCONRV   INSERT NEW CON# 9'S COMP REV                 
                                                                                
         L     RF,MKGCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,MKGCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+11,C'Y'                                                  
         BNE   MKGD0060                                                         
         CLC   MKGCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    MKGD0060                                                         
         MVC   P+1(8),=C'M/G LNE:'                                              
         MVC   P+12(27),RMKGREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
MKGD0060 DS    0H                                                               
         B     MKGD0020                                                         
                                                                                
MKGD0080 DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
**-->                                                                           
***********************************************************************         
* PRODCODE: CHECK IF PRODUCT CODE HAS BEEN ENTERED.  IF SO,           *         
*        PRODUCT CODE FIELD MUST BE SET TO ZERO, AND A PRODUCT        *         
*        EXPANSION ELEMENT INSERTED INTO THE CONTRACT RECORD.         *         
* P1 HAS ADV CODE                                                     *         
* P2 HAS PRD CODE                                                     *         
* WORK WILL CONTAIN EXPANDED NAME                                     *         
***********************************************************************         
PRODCODE NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    WORK,WORK                                                        
         L     R2,4(R1)            SET A(RCONREC)                               
         USING RCONREC,R2                                                       
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY FOR RESTART                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   KEY+RPRDKTYP-RPRDREC,X'09'                                       
         MVC   KEY+RPRDKADV-RPRDREC(4),OLDADV     INSERT ORIG ADV CODE          
         MVC   KEY+RPRDKPRD-RPRDREC(3),RCONPRD    INSERT PRODUCT CODE           
         MVC   KEY+RPRDKREP-RPRDREC(2),OLDREP     INSERT ORIG REP CODE          
*                                                                               
*   TEST                                                                        
**       MVC   P+1(05),=C'PROD:'                                                
**       MVC   P+6(27),KEY                                                      
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         GOTO1 GETREC#2                                                         
*                                                                               
         L     R6,AIO2             SET A(IOAREA # 2)                            
         USING RPRDREC,R6                                                       
*                                                                               
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(09),=C'PRODREC:'                                             
*        ST    R6,P+12                                                          
*        GOTO1 REPORT                                                           
*        LR    R4,R6               A(PRODUCT RECORD)                            
*        LA    RF,256                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         MVC   WORK(2),=X'0516'    SET ELEMENT CODE + LENGTH                    
         MVC   WORK+2(L'RPRDNAME),RPRDNAME                                      
         DROP  R6                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(09),=C'PRODWORK:'                                            
**       MVC   P+10(22),WORK                                                    
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                 
*                                  INSERT PRODUCT EXPANSION                     
         MVC   RCONPRD,SPACES      CLEAR PRODUCT CODE                           
*                                                                               
*   TEST PRNTBL                                                                 
**       GOTO1 REPORT                                                           
**       LA    R4,RCONREC          A(CONTRACT RECORD)                           
**       SR    RF,RF                                                            
**       ICM   RF,3,RCONLEN                                                     
**       GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         MVC   KEY,SAVEKEY         RESTORE CONTRACT KEY                         
         GOTO1 HIGHDIR             RESTART CONTRACT KEY SEQUENCE                
         XIT1                                                                   
         DROP  R2                                                               
**-->                                                                           
******************************************************************              
* DISPLAY MISSING CODES                                                         
******************************************************************              
MISSINGS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         L     R3,AMISSAGY         SET A(MISSING AGENCIES)                      
MISS0020 EQU   *                                                                
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    MISS0040            YES                                          
         MVC   P+1(15),=C'MISSING AGENCY:'                                      
         MVC   P+20(6),0(R3)                                                    
         GOTO1 REPORT                                                           
         LA    R3,6(R3)                                                         
         B     MISS0020            GO BACK FOR NEXT                             
MISS0040 EQU   *                                                                
         L     R3,AMISSADV         SET A(MISSING ADVERTS)                       
MISS0060 EQU   *                                                                
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    MISS0080            YES                                          
         MVC   P+1(15),=C'MISSING ADVERT:'                                      
         MVC   P+20(4),0(R3)                                                    
         GOTO1 REPORT                                                           
         LA    R3,4(R3)                                                         
         B     MISS0060            GO BACK FOR NEXT                             
MISS0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'CONTRACTS     PROCESSED:'                             
         EDIT  CONCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/NOT IN DATES :'                             
         EDIT  CONDATE,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS W/IN DATE    :'                             
         EDIT  CONCTR2,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS - DARE ORDERS:'                             
         EDIT  DARECTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS - DARE/NODARE:'                             
         EDIT  DARENODR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/TOTAL WIPS   :'                             
         EDIT  CONWIP,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/WIP FORECAST :'                             
         EDIT  CONWIPFC,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/NO AGENCY    :'                             
         EDIT  NOAGYREC,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/NO ADVERT    :'                             
         EDIT  NOADVREC,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/WITH NO S/P  :'                             
         EDIT  NOSPCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/W/DELETE SET :'                             
         EDIT  CONDEL,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/W/ NG CTYPE  :'                             
         EDIT  CONCTYPE,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C' REJECTED/W/NO 20 ELT  :'                             
         EDIT  NO20CTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS ACCEPTED     :'                             
         EDIT  CONCTR4,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS W/PROD CODES :'                             
         EDIT  CONPROD,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUYS          PROCESSED:'                             
         EDIT  BUYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DARE RECORDS (41/51S)  :'                             
         EDIT  DARECCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DARE RECORDS (41S)     :'                             
         EDIT  DAREC41S,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DARE RECORDS (51S)     :'                             
         EDIT  DAREC51S,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MAKEGOODS     PROCESSED:'                             
         EDIT  MKGCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONFIRM COMTS PROCESSED:'                             
         EDIT  CFCCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'COVERSHEETS   PROCESSED:'                             
         EDIT  COVCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'LOW  CONTRACT NUMBER   :'                             
         GOTO1 HEXOUT,DMCB,LOWCON,P+30,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'HIGH CONTRACT NUMBER   :'                             
         GOTO1 HEXOUT,DMCB,HIGHCON,P+30,4,=C'TOG'                               
         GOTO1 REPORT                                                           
         L     RF,CONBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         LTR   RF,RF               ANY DIVIDEND?                                
         BZ    NODIV001            NO                                           
         D     RE,CONCTR4          NUM BYTES / # CONTRACTS                      
         MVC   P+1(24),=C'AVERAGE CONTRACT RECORD:'                             
         EDIT  (RF),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
NODIV001 EQU   *                                                                
         L     RF,BUYBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         LTR   RF,RF               ANY DIVIDEND?                                
         BZ    NODIV002            NO                                           
         D     RE,BUYCTR           NUM BYTES / # BUY RECORDS                    
         MVC   P+1(24),=C'AVERAGE BUY      RECORD:'                             
         EDIT  (RF),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
NODIV002 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*        4(R1):  IF PRE-PUT, WILL BE '4'                         *              
*                IF POST   , WILL BE '0'                         *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            LOAD PRE/POST VALUE                          
         MVC   P+1(18),=C'PRE -PUTREC RECORD'                                   
         EDIT  CONCTR4,(6,P+30)                                                 
         EDIT  NOSPCTR,(6,P+40)                                                 
         LTR   R2,R2               ANY VALUE?                                   
         BNZ   DIPU0020            YES - IT'S PRE                               
         MVC   P+1(18),=C'POST-PUTREC RECORD'                                   
DIPU0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         AR    RF,R2               ADD PRE/POST PUT LENGTH                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
PUTRECS  NTR1                                                                   
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETREC#2 LA    R6,GETREC                                                        
         B     LINKFIL2                                                         
         SPACE 2                                                                
LINKFIL2 NTR1                                                                   
         MVC   DMCB+12(4),AIO2                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               ,(0,DMWORK)                                                      
         B     DMCHECK                                                          
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLC   =X'05721739',KEY+12                                              
*        BNE   TEST0030                                                         
*        MVC   P+1(09),=C'CONTRACT:'                                            
*        MVC   P+12(4),DMCB+8                                                   
*        MVC   P+20(34),KEY                                                     
*        GOTO1 REPORT                                                           
*EST0030 EQU   *                                                                
*                                                                               
         TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'02'        TEST FOR RECORD DELETED                      
         BO    EQXIT               DELETE SET - PROCESS                         
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ASPOFFTB DS    A                                                                
ANEXTSPO DS    A                                                                
AMISSAGY DS    A                                                                
AMISSADV DS    A                                                                
SAVEAIO  DS    A                                                                
AIO2     DS    A                                                                
ABLDAREA DS    A                                                                
AAGYAREA DS    A                   AGENCY EQUIV AREA                            
ANEXTAGY DS    A                   NEXT AGENCY EQUIV                            
AGYCTR   DS    F                                                                
MISSAGYS DS    F                                                                
AADVAREA DS    A                   ADVERT EQUIV AREA                            
ANEXTADV DS    A                   NEXT ADVERT EQUIV                            
ADVCTR   DS    F                                                                
MISSADVS DS    F                                                                
ACTYAREA DS    A                   CONTYP EQUIV AREA                            
ANEXTCTY DS    A                   NEXT CONTYP EQUIV                            
CTYCTR   DS    F                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
ATAPEREC DS    A                                                                
AWORKBLK DS    A                                                                
AWRKBLK2 DS    A                                                                
SALCTR   DS    F                                                                
ASTNAREA DS    A                                                                
LBLDAREA DS    F                                                                
NEXTAREA DS    A                   NEXT OPEN SLOT                               
STRTSRCH DS    A                   A(START OF SEARCH)                           
SAVAGASS DS    F                   SAVE AREA: AGENCY ASSIGNMENT                 
CONBYTES DS    F                                                                
BUYBYTES DS    F                                                                
DARECTR  DS    F                                                                
DARECCTR DS    F                                                                
DAREC41S DS    F                                                                
DAREC51S DS    F                                                                
CONCTR   DS    F                                                                
CONCTR2  DS    F                                                                
CONCTR3  DS    F                                                                
CONCTR4  DS    F                                                                
CFCCTR   DS    F                                                                
COVCTR   DS    F                                                                
NOSPCTR  DS    F                                                                
NOAGYREC DS    F                                                                
NOADVREC DS    F                                                                
CONDEL   DS    F                                                                
CONDATE  DS    F                                                                
CONCTYPE DS    F                                                                
NO20CTR  DS    F                                                                
DARENODR DS    F                                                                
CONWIP   DS    F                                                                
CONWIPFC DS    F                                                                
CONPROD  DS    F                                                                
BUYCTR   DS    F                                                                
BUYCTR2  DS    F                                                                
STACTR   DS    F                                                                
OFFCTR   DS    F                                                                
MKGCTR   DS    F                                                                
LASTSTA  DS    CL5                                                              
OLDREP   DS    CL2                 OLD REP CODE                                 
NEWREP   DS    CL2                 NEW REP CODE                                 
REPREP   DS    CL2                 REP REP CODE (REPLACEMENT)                   
NOCODES  DS    CL1                 SKIP CODE REPLACEMENT TEST                   
HIGHCON  DS    XL4                                                              
LOWCON   DS    XL4                                                              
KEYTYPE  DS    CL1                                                              
DATEWORK DS    CL24                DATE WORK AREA                               
SAVEKEY  DS    CL(L'KEY)                                                        
SAVE9CMP DS    CL4                 9'S COMP REVERSED                            
COVERFLG DS    CL1                                                              
DAREORDR DS    CL1                                                              
DARKTYP  DS    XL1                                                              
DARAGYS  DS    CL20                AGENCY ASSIGNMENT SAVE AREA                  
DARAGYCD DS    CL4                 AGENCY CODE FROM AGENCY RECORD               
SAVEAGY  DS    CL6                                                              
SAVEADV  DS    CL4                                                              
UTLTABLE DC    C'AM',X'78'         KATZ AMERICAN                                
LUTLTABL EQU   *-UTLTABLE                                                       
         DC    C'CQ',X'78'         KATZ CONTINENTAL                             
         DC    C'SZ',X'98'         SELTEL                                       
         DC    X'0000'             DELIMITER                                    
*   EQUATES                                                                     
SALTABLN EQU   13                                                               
DSALTSTN EQU   0                   DISPLACE TO STATION                          
DSALTOSP EQU   5                   DISPLACE TO OLD S/P                          
DSALTNSP EQU   8                   DISPLACE TO NEW S/P                          
DSALTTEM EQU   11                  DISPLACE TO TEAM                             
*                                                                               
AGYTABLN EQU   12                                                               
DAGYOAGY EQU   0                   DISPLACE TO OLD AGENCY                       
DAGYNAGY EQU   6                   DISPLACE TO NEW AGENCY                       
*                                                                               
ADVTABLN EQU   8                                                                
DADVOADV EQU   0                   DISPLACE TO OLD ADVERT                       
DADVNADV EQU   4                   DISPLACE TO NEW ADVERT                       
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
         DS    0H                                                               
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE1  DCB   DDNAME=INTAPE1,DSORG=PS,RECFM=VB,LRECL=255,             X        
               BLKSIZE=6233,MACRF=GM,EODAD=LDIR0400                             
INTAPE2  DCB   DDNAME=INTAPE2,DSORG=PS,RECFM=VB,LRECL=255,             X        
               BLKSIZE=6233,MACRF=GM,EODAD=LDIR0660                             
         SPACE 3                                                                
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL4008              AREA FOR RECORD: 4K CONTRACT                 
         DS    0D                                                               
*  INCLUDE REGENREPA               REP RECORD                                   
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENMKG                MAKEGOOD RECORD                              
*  INCLUDE REGENTKO                TKO EQUIV RECORD                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
*                                                                               
NMODAREA CSECT                                                                  
******************************************************************              
*  SETCODES: FIND AGENCY/ADVERTISER CODES IN TABLE.              *              
*        IF FOUND, REPLACE WITH EQUIVALENCY CODES                *              
*        IF NOT  , SET CC NOT ZERO ON RETURN, SKIP CONTRACT      *              
*                                                                *              
*                                                                *              
******************************************************************              
SETCODES NMOD1 0,**SCOD**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD IN PROCESS)          
         USING RCONREC,R5                                                       
         MVC   DUB(6),RCONKAGY     SET UP SEARCH ARGUMENT                       
         L     R2,AAGYAREA         SET A(AGENCY TABLE)                          
         L     R4,AGYCTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),12,6,5000                         
                                                                                
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCOD0040            YES                                          
         L     RF,NOAGYREC         COUNT MISSING AGENCY ORDERS                  
         LA    RF,1(RF)                                                         
         ST    RF,NOAGYREC                                                      
*                                                                               
SCOD0020 EQU   *                                                                
         MVC   P+1(12),=C'MISSING AGY:'                                         
         MVC   P+13(6),DUB                                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,P+24,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         L     R2,AMISSAGY         SET A(MISSING AGENCIES)                      
         L     R4,MISSAGYS         SET A(NUMBER OF AGENCIES IN TABLE)           
         GOTO1 =V(BINSRCH),DMCB,(1,DUB),(R2),(R4),6,6,5000                      
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCOD0800            YES - DON'T ADD TO COUNTER                   
         MVC   MISSAGYS,DMCB+8     SAVE NEW COUNT                               
**       MVC   P+1(12),=C' AGY TABLED:'                                         
**       GOTO1 REPORT                                                           
*                                                                               
         B     SCOD0800            EXIT CC NOT ZERO                             
SCOD0040 EQU   *                                                                
         L     RF,DMCB             SET A(RECORD FOUND)                          
         CLC   =C'????',6(RF)      SKIP AGENCY INDICATOR?                       
         BNE   SCOD0060                                                         
         MVC   P+1(12),=C'????    AGY:'                                         
         MVC   P+13(6),DUB                                                      
         MVC   P+22(12),0(RF)                                                   
         GOTO1 HEXOUT,DMCB,RCONKCON,P+40,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     SCOD0020                                                         
SCOD0060 EQU   *                                                                
         MVC   SAVEAGY,6(RF)       SAVE NEW AGENCY FOUND                        
*                                                                               
*   FIND ADVERTISER IN TABLE                                                    
*                                                                               
         MVC   DUB(4),RCONKADV     SET UP SEARCH ARGUMENT                       
         L     R2,AADVAREA         SET A(ADVERT TABLE)                          
         L     R4,ADVCTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),8,4,10000                         
                                                                                
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCOD0100            YES                                          
         L     RF,NOADVREC         COUNT MISSING ADVERT ORDERS                  
         LA    RF,1(RF)                                                         
         ST    RF,NOADVREC                                                      
*                                                                               
SCOD0080 EQU   *                                                                
         MVC   P+1(12),=C'MISSING ADV:'                                         
         MVC   P+13(4),DUB                                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,P+24,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         L     R2,AMISSADV         SET A(MISSING ADVERTS)                       
         L     R4,MISSADVS         SET A(NUMBER OF ADVERTS IN TABLE)            
         GOTO1 =V(BINSRCH),DMCB,(1,DUB),(R2),(R4),4,4,5000                      
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCOD0800            YES - DON'T ADD TO COUNTER                   
         MVC   MISSADVS,DMCB+8     SAVE NEW COUNT                               
**       MVC   P+1(12),=C' ADV TABLED:'                                         
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         B     SCOD0800            EXIT CC NOT ZERO                             
SCOD0100 EQU   *                                                                
         L     RF,DMCB             SET A(RECORD FOUND)                          
         CLC   =C'????',4(RF)      SKIP ADVERT INDICATOR?                       
         BNE   SCOD0120                                                         
         MVC   P+1(12),=C'????    ADV:'                                         
         MVC   P+13(4),DUB                                                      
         MVC   P+22(8),0(RF)                                                    
         GOTO1 HEXOUT,DMCB,RCONKCON,P+40,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     SCOD0080                                                         
SCOD0120 EQU   *                                                                
         MVC   SAVEADV,4(RF)       SAVE NEW ADVERT FOUND                        
         SR    R0,R0               SET CC ZERO                                  
         B     SCOD0820            EXIT                                         
SCOD0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
SCOD0820 EQU   *                                                                
         XIT1                                                                   
         DROP R5                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TAKEDARE:  READ ALL DARE FOR ORIGINAL ORDER, MOVE TO NEW FILE               
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
TAKEDARE NMOD1 0,*TKDR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
**   TAKE OVER KATZ EDI ORDERS ALSO                                             
*                                                                               
***      TM    RCONDRFG-RCONDREL(R6),X'04'                                      
*                                  KATZ EDI ORDER?                              
***      BO    TKDR0180            YES - TREAT AS NOT DARE                      
*                                                                               
*                                                                               
         L     R2,AIO2             SET A(IOAREA 2)                              
         USING RAGY2REC,R2         SET ADDRESSABILITY TO AGENCY RECORD          
*                                                                               
         XC    RAGY2REC(32),RAGY2REC                                            
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY(6),OLDAGY  INSERT ORIGINAL AGENCY/OFFICE                
         MVC   RAGK2REP,OLDREP     INSERT SOURCE REP INTO KEY                   
         MVC   KEY,RAGY2REC                                                     
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 GETREC#2            READ INTO SECOND IO AREA                     
*                                                                               
         MVC   DARAGYS,RAGY2DAR    PULL OUT FOUR (MAX) AGY ASSIGNS              
         MVC   DARAGYCD,RAGK2AGY   SAVE AGENCY CODE                             
*                                                                               
*   DARE TYPE X'41' RECORDS WILL BE PROCESSED ONLY FOR NEVER BEEN               
*     CONFIRMED CONTRACTS. ALL ELSE, X'51' RECORDS WILL BE TAKEN                
*        OVER.  THEREFORE, THE TYPE IS SET AT THE TOP OF THE LOOP.              
*                                                                               
         CLI   DARKTYP,C' '        NO RECORD TYPE IN DARE?                      
         BNE   TKDR0008            NO  - PROCESS                                
         L     RF,DARENODR         DARE, BUT NO TYPE?                           
         LA    RF,1(RF)                                                         
         ST    RF,DARENODR                                                      
         B     TKDR0180            TREAT AS NOT DARE                            
TKDR0008 EQU   *                                                                
         MVI   DARKTYP,X'41'       PULL TYPE 41'S FIRST                         
TKDR0010 EQU   *                                                                
         DROP  R2                                                               
*                                                                               
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
*                                                                               
TKDR0012 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   RDARKTYP(1),DARKTYP                                              
         MVC   RDARKREP,OLDREP     INSERT ORIGINAL REP INTO KEY                 
         MVC   RDARKSTA(5),OLDSTA                                               
         CLI   RDARKSTA+4,C' '                                                  
         BNE   TKDR0015                                                         
         MVC   RDARKSTA+4(2),=C'T '                                             
TKDR0015 EQU   *                                                                
         OC    RDARKSTA(6),SPACES                                               
         MVC   RDARKAGY(5),DARAGYS                                              
*                                  LOAD THE 1ST EQUIVALENCY CODE                
         MVC   RDARKORD,OLDDRLK    ORIGINAL AGENCY ORDER NUMBER                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
*                                                                               
         LA    RF,DARAGYS          THERE ARE MAX 4 AGENCY ASSIGNMENT            
*                                     1ST IS ALREADY LOADED                     
         LA    RF,5(RF)            SKIP THE 1ST CODE                            
         ST    RF,SAVAGASS         SAVE A(2ND AGENCY ASSIGNMENT)                
         LA    R0,3                COMBINATIONS WE NEED TO CHECK                
*                                                                               
TKDR0020 DS    0H                                                               
         MVC   KEYSAVE(L'RDARKEY),KEY    SAVE KEY FOR RESTART                   
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    TKDR0040                                                         
         MVC   KEY,KEYSAVE         RESET KEY NOT FOUND                          
         L     RF,SAVAGASS         RESTORE A(AGENCY ASSIGNMENT)                 
         MVC   RDARKAGY(5),0(RF)   INSERT NEXT EQUIVALENCY CODE                 
         LA    RF,5(RF)            BUMP TO NEXT EQUIV CODE                      
         ST    RF,SAVAGASS         SAVE A(AGENCY ASSIGNMENT)                    
*                                                                               
*                                                                               
         BCT   R0,TKDR0020                                                      
*                                                                               
         CLI   DARKTYP,X'51'       PROCESSING X'51' RECS?                       
         BE    TKDR0160            YES - MAY BE NO TYPE 51'S                    
*                                                                               
         CLC   =C'SZ',OLDREP       SELTEL+AGENCY 1342 CHECK                     
         BNE   TKDR0150            NO  - GO CHECK TYPE X'51'S                   
         CLC   =C'1342  ',DARAGYCD YES - CHECK SPECIAL CODE                     
         BNE   TKDR0150            NO  - GO CHECK TYPE X'51'S                   
         MVC   RDARKAGY(5),=C'ED2DE'                                            
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    TKDR0040                                                         
         B     TKDR0150            NO  - GO CHECK TYPE X'51'S                   
*                                                                               
         DROP  R4                                                               
*                                                                               
TKDR0040 DS    0H                                                               
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RDARREC,R6                                                       
*                                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         MVC   REC-4(2),RDARLEN                                                 
         CLI   DARKTYP,X'41'       TYPE 41S IN PROGRESS?                        
         BNE   TKDR0050            NO  - ALL FINISHED                           
         CLC   DAREC41S,=F'025'    DISPLAY FIRST N RECORDS                      
         BH    TKDR0060                                                         
**       MVC   P+1(09),=C'41 RECORD'                                            
**       GOTO1 REPORT                                                           
**       GOTO1 DISPPUT2,DMCB,(RC),4 DISPLAY PRE -CHANGE                         
         B     TKDR0060                                                         
TKDR0050 EQU   *                                                                
         CLC   DAREC51S,=F'025'    DISPLAY FIRST N RECORDS                      
         BH    TKDR0060                                                         
***      MVC   P+1(09),=C'51 RECORD'                                            
***      GOTO1 REPORT                                                           
***      GOTO1 DISPPUT2,DMCB,(RC),4 DISPLAY PRE -CHANGE                         
TKDR0060 EQU   *                                                                
         MVC   RDARKREP,REPREP     INSERT TARGET REP INTO KEY                   
         CLI   RDARKRT,X'10'       AGENCY HEADER RECORD?                        
         BNE   TKDR0100            NO  - DON'T INSERT NEW CON#                  
         MVC   RDARREP#,NEWCON     NO  - INSERT NEW CONTRACT NUMBER             
TKDR0100 EQU   *                                                                
*                                                                               
**PUTRECS                                                                       
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         MVC   REC-4(2),RDARLEN                                                 
         MVC   RDARKREP,REPREP     INSERT REPLACEMENT REP CODE                  
*                                                                               
         DROP  R6                                                               
*                                                                               
**PUTRECS                                                                       
         GOTO1 PUTRECS                                                          
*                                                                               
         L     RF,DARECCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DARECCTR                                                      
         CLI   DARKTYP,X'41'       TYPE 41S IN PROGRESS?                        
         BNE   TKDR0104            NO  - ALL FINISHED                           
         L     RF,DAREC41S                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DAREC41S                                                      
         B     TKDR0106                                                         
TKDR0104 EQU   *                                                                
         L     RF,DAREC51S                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DAREC51S                                                      
TKDR0106 EQU   *                                                                
*                                                                               
         CLI   DARKTYP,X'41'       TYPE 41S IN PROGRESS?                        
         BNE   TKDR0108            NO  - ALL FINISHED                           
         CLC   DAREC41S,=F'025'    DISPLAY FIRST N RECORDS                      
         BH    TKDR0110                                                         
***      MVC   P+1(09),=C'41 RECORD'                                            
***      GOTO1 REPORT                                                           
***      GOTO1 DISPPUT2,DMCB,(RC),0 DISPLAY POST-CHANGE                         
         B     TKDR0110                                                         
TKDR0108 EQU   *                                                                
         CLC   DAREC51S,=F'025'    DISPLAY FIRST N RECORDS                      
         BH    TKDR0110                                                         
***      MVC   P+1(09),=C'51 RECORD'                                            
***      GOTO1 REPORT                                                           
***      GOTO1 DISPPUT2,DMCB,(RC),0 DISPLAY POST-CHANGE                         
                                                                                
TKDR0110 DS    0H                                                               
*                                                                               
         GOTO1 SEQDIR              ACCESS NEXT KEY                              
         CLC   KEY(24),KEYSAVE     SAME SET OF DARE RECORDS?                    
         BNE   TKDR0150            FINISHED: CHECK TYPE DARE                    
         MVC   KEYSAVE(27),KEY     SAVE KEY                                     
         B     TKDR0040            GO BACK FOR NEXT DARE                        
TKDR0150 EQU   *                                                                
         CLI   DARKTYP,X'41'       TYPE 41S IN PROGRESS?                        
         BNE   TKDR0160            NO  - ALL FINISHED                           
         MVI   DARKTYP,X'51'       YES - NOW DO TYPE 51S                        
         B     TKDR0010            GO BACK AND DO 51S                           
*                                                                               
TKDR0160 EQU   *                                                                
*                                                                               
TKDR0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPPUT2: DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*        4(R1):  IF PRE-PUT, WILL BE '4'                         *              
*                IF POST   , WILL BE '0'                         *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT2 NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            LOAD PRE/POST VALUE                          
         MVC   P+1(23),=C'DARE PRE -PUTREC RECORD'                              
         EDIT  DARECCTR,(6,P+30)                                                
         LTR   R2,R2               ANY VALUE?                                   
         BNZ   DPUT0020            YES - IT'S PRE                               
         MVC   P+1(23),=C'DARE POST-PUTREC RECORD'                              
DPUT0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         AR    RF,R2               ADD PRE/POST PUT LENGTH                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DPUT0090 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
RECD     DSECT                                                                  
RECORD   DS    CL2008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENREPA         REP RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          S/P RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDAR          DARE RECORD                                  
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCOV          COVERSHEET RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCFC          CONFIRM COMMENT RECORD                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKG          M/G     RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTKO          TKO EQUIV RECORD                             
         EJECT                                                                  
       ++INCLUDE REGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
       ++INCLUDE REGENAGY2         AGENCY  RECORD                               
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103REREPSKATC05/01/02'                                      
         END                                                                    
