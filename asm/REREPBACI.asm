*          DATA SET REREPBACI  AT LEVEL 197 AS OF 05/01/02                      
*PHASE REBK02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE SCANNER                                                                
*INCLUDE RECUP                                                                  
         TITLE 'REREPBACK - BACK BILLING EXTRACT: OLD->NEW REP'                 
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPBACK -- STRIP A SET OF CONTRACT INFO FROM INDICATED  *            
*                     REP, PRODUCING AN OUTPUT TAPE TO MERGE INTO  *            
*                     A LOAD.                                      *            
*                     ALL ORDERS PROCESSED REPRESENT BACK BILLING. *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* DEC09/99 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     =                                                *            
*     QUESTOR+1   =  Y  - SHOW AGY/ADV/SP/CTYPE TABLES             *            
*                                                                  *            
*     QRECORD+36  =  'FROM' REP                                    *            
*     QRECORD+38  =  'TO  ' REP                                    *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REBK02   CSECT                                                                  
         NMOD1 0,**BK02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
         XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         GOTO1 LOADTABL,DMCB,(RC)                                               
*                                                                               
         GOTO1 CONTPROC,DMCB,(RC)  PROCESS CONTRACT RECORDS                     
*                                                                               
***      DC    H'0'                                                             
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
         MVC   EARLYDTE,=X'C39D'   SET START BACKBILLING DATE                   
*                                     TO DEC29/97                               
*   VERIFY THE DATES!!!                                                         
*                                     TO DEC29/97                               
         MVC   LATEDTE,=X'C78C'    SET END   BACKBILLING DATE                   
*                                     TO DEC12/99                               
         GOTO1 DATCON,DMCB,(2,EARLYDTE),(3,EARLYDTB)                            
         GOTO1 DATCON,DMCB,(2,LATEDTE),(3,LATEDTB)                              
         MVC   EARLYMOS,=X'6201'   SET EARLY MONTH OF SERVICE                   
*                                     FOR BUCKET PROCESSING                     
*                                                                               
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         MVC   ACOMFACS,VCOMFACS                                                
         DROP  RF                                                               
*                                                                               
*                                                                               
         GOTOX LOADER,DMCB,=CL8'T00AAC',0                                       
         MVC   VREPFACS,4(R1)                                                   
*                                  EXTERNAL ROUTINE, MISC. SUBROUTINES          
         OC    VREPFACS,VREPFACS                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  GET WORKSPACE FOR PROCESSING                 
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
         ST    RF,ASTAAREA         A(STATION TABLE)                             
         ST    RF,ANEXTSTA         A(NEXT STATION SLOT)                         
         A     RF,=F'80000'        80K FOR STATION STORAGE:                     
*                                      8,000 ENTRIES -                          
*                                        5 BYTES STATION+MEDIA                  
*                                        2 BYTES NEW GROUP                      
*                                        3 BYTES SPARE                          
         ST    RF,AIO2             A(IOAREA #2)                                 
*                                                                               
         A     RF,=F'4008'         4K FOR ALTERNATE IO AREA                     
         ST    RF,ATAPEREC         A(TAPE RECORD READ AREA)                     
*                                                                               
         A     RF,=F'4008'         4K FOR TAPE READ AREA                        
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
         XC    CONCTR5,CONCTR5     CLEAR COUNTERS                               
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
         MVC   P+1(20),=C'UTLS:  OLD=     NEW='                                 
         MVC   P+13(1),ORIGUTL                                                  
         MVC   P+22(1),NEWUTL                                                   
         GOTO1 REPORT                                                           
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
         XC    KEY,KEY                                                          
         MVI   KEY,1                                                            
         GOTO1 HIGHDIR                                                          
         MVC   P+1(17),=C'NEW FILE: 1ST REP'                                    
         MVC   P+20(2),NEWREP                                                   
         MVC   P+26(27),KEY        DISPLAY KEY FOUND                            
         GOTO1 REPORT                                                           
LTAB0020 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'1F'           INSERT TKO RECORD TYPE                       
         MVC   KEY+16(2),NEWREP    INSERT NEW REP CODE INTO KEY                 
         MVC   KEY+18(2),OLDREP    INSERT OLD REP CODE INTO KEY                 
         MVI   KEY+20,1            SET TO 'AGENCY/OFFICE RECORD TYPE'           
         GOTO1 HIGHDIR                                                          
         B     LTAB0060                                                         
LTAB0040 EQU   *                                                                
         GOTO1 SEQDIR                                                           
LTAB0060 EQU   *                                                                
         CLC   KEY+16(2),NEWREP    RECORD FOR NEW REP FOUND?                    
         BNE   LTAB0080            NO  - NOTHING ON FILE                        
         CLC   KEY(21),KEYSAVE     SAME KEY THROUGH A/O RECORD TYPE?            
         BNE   LTAB0080                                                         
         GOTO1 GETRECRD            RETRIEVE THE RECORD                          
         LA    R3,REC              SET A(RECORD AREA)                           
         USING RTKOREC,R3                                                       
*                                                                               
***      MVC   P+1(13),=C'AGY   RECORD:'                                        
***      MVC   P+14(80),RTKOREC                                                 
***      GOTO1 REPORT                                                           
*                                                                               
         L     R1,ANEXTAGY         SET A(NEXT AGENCY CODE)                      
         MVC   DAGYOAGY(6,R1),RTKOKCOD    INSERT OLD AGENCY/OFF                 
         MVC   DAGYNAGY(6,R1),RTKOEQIV    INSERT NEW AGENCY/OFF                 
*                                                                               
*   TABLE DISPLAY                                                               
*        MVC   P+1(07),=C'AGYTAB:'                                              
*        MVC   P+8(12),0(R1)                                                    
*        GOTO1 REPORT                                                           
*   TABLE DISPLAY END                                                           
*                                                                               
         OC    0(AGYTABLN,R1),SPACES                                            
         LA    R1,AGYTABLN(R1)     BUMP TO NEXT AVAILABLE SLOT                  
         ST    R1,ANEXTAGY                                                      
         L     R1,AGYCTR                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         ST    R1,AGYCTR           SAVE COUNT                                   
         B     LTAB0040            GO BACK FOR NEXT RECORD                      
**NEWADV>>>                                                                     
LTAB0080 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'1F'           INSERT TKO RECORD TYPE                       
         MVC   KEY+16(2),NEWREP    INSERT NEW REP CODE INTO KEY                 
         MVC   KEY+18(2),OLDREP    INSERT OLD REP CODE INTO KEY                 
         MVI   KEY+20,2            SET TO 'ADVERTISER RECORD TYPE'              
         GOTO1 HIGHDIR                                                          
         B     LTAB0120                                                         
LTAB0100 EQU   *                                                                
         GOTO1 SEQDIR                                                           
LTAB0120 EQU   *                                                                
         CLC   KEY+16(2),NEWREP    RECORD FOR NEW REP FOUND?                    
         BNE   LTAB0140            NO  - NOTHING ON FILE                        
         CLC   KEY(21),KEYSAVE     SAME KEY THROUGH ADV RECORD TYPE?            
         BNE   LTAB0140                                                         
         GOTO1 GETRECRD            RETRIEVE THE RECORD                          
         LA    R3,REC              SET A(RECORD AREA)                           
         USING RTKOREC,R3                                                       
*                                                                               
***      MVC   P+1(13),=C'ADV   RECORD:'                                        
***      MVC   P+14(80),RTKOREC                                                 
***      GOTO1 REPORT                                                           
*                                                                               
         L     R1,ANEXTADV         SET A(NEXT ADVERT CODE)                      
         MVC   DADVOADV(4,R1),RTKOKCOD    INSERT OLD ADVERT                     
         MVC   DADVNADV(4,R1),RTKOEQIV    INSERT NEW AGENCY                     
*                                                                               
         OC    0(ADVTABLN,R1),SPACES                                            
         LA    R1,ADVTABLN(R1)     BUMP TO NEXT AVAILABLE SLOT                  
         ST    R1,ANEXTADV                                                      
         L     R1,ADVCTR                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         ST    R1,ADVCTR           SAVE COUNT                                   
         B     LTAB0100            GO BACK FOR NEXT RECORD                      
**NEWADV>>>                                                                     
**NEWSAL>>>                                                                     
LTAB0140 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'1F'           INSERT TKO RECORD TYPE                       
         MVC   KEY+16(2),NEWREP    INSERT NEW REP CODE INTO KEY                 
         MVC   KEY+18(2),OLDREP    INSERT OLD REP CODE INTO KEY                 
         MVI   KEY+20,3            SET TO 'SALESPERSN RECORD TYPE'              
         GOTO1 HIGHDIR                                                          
         B     LTAB0180                                                         
LTAB0160 EQU   *                                                                
         GOTO1 SEQDIR                                                           
LTAB0180 EQU   *                                                                
         CLC   KEY+16(2),NEWREP    RECORD FOR NEW REP FOUND?                    
         BNE   LTAB0200            NO  - NOTHING ON FILE                        
         CLC   KEY(21),KEYSAVE     SAME KEY THROUGH S/P RECORD TYPE?            
         BNE   LTAB0200                                                         
         GOTO1 GETRECRD            RETRIEVE THE RECORD                          
         LA    R3,REC              SET A(RECORD AREA)                           
         USING RTKOREC,R3                                                       
*                                                                               
***      MVC   P+1(13),=C'S/P   RECORD:'                                        
***      MVC   P+14(80),RTKOREC                                                 
***      GOTO1 REPORT                                                           
*                                                                               
         L     R1,ANEXTSAL         SET A(NEXT S/P CODE)                         
         MVC   DSALTOSP(3,R1),RTKOKCOD    INSERT OLD S/P                        
         MVC   DSALTNSP(5,R1),RTKOEQIV    INSERT NEW S/P+OFFICE                 
         MVC   DSALTTEM(2,R1),RTKOSPTM    INSERT NEW S/P TEAM                   
*                                                                               
         OC    0(SALTABLN,R1),SPACES                                            
         LA    R1,SALTABLN(R1)     BUMP TO NEXT AVAILABLE SLOT                  
         ST    R1,ANEXTSAL                                                      
         L     R1,SALCTR                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         ST    R1,SALCTR           SAVE COUNT                                   
         B     LTAB0160            GO BACK FOR NEXT RECORD                      
**NEWSAL>>>                                                                     
LTAB0200 EQU   *                                                                
         OC    AGYCTR,AGYCTR       ANY AGENCY RECS FOUND?                       
         BNZ   LTAB0220            YES                                          
         MVC   P+1(27),=C'NO AGENCY TKO RECORDS FOUND'                          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     LTAB0280                                                         
LTAB0220 EQU   *                                                                
         L     R4,AAGYAREA         SET A(TABLE)                                 
         L     R3,AGYCTR           SET COUNTER                                  
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),12,6,0                                  
*                                                                               
         CLI   QUESTOR+1,C'Y'      DISPLAY AGENCY TABLE?                        
         BNE   LTAB0260            NO                                           
*                                                                               
         L     R2,AAGYAREA         SET A(AGENCY TABLE)                          
*                                                                               
         LA    R3,1                SET COUNTER                                  
LTAB0240 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    LTAB0260            YES                                          
*                                                                               
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'AGY:'                                                
         MVC   P+20(AGYTABLN),0(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,AGYTABLN(R2)     BUMP TO NEXT SLOT                            
         B     LTAB0240            GO BACK FOR NEXT SLOT                        
LTAB0260 EQU   *                                                                
         MVC   P+1(07),=C'AGYCTR='                                              
         EDIT  AGYCTR,(6,P+10)                                                  
         GOTO1 REPORT                                                           
LTAB0280 EQU   *                                                                
         OC    ADVCTR,ADVCTR       ANY ADVERT RECS FOUND?                       
         BNZ   LTAB0300            YES                                          
         MVC   P+1(27),=C'NO ADVERT TKO RECORDS FOUND'                          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     LTAB0360                                                         
LTAB0300 EQU   *                                                                
         L     R4,AADVAREA         SET A(TABLE)                                 
         L     R3,ADVCTR           SET COUNTER                                  
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),8,4,0                                   
*                                                                               
         CLI   QUESTOR+1,C'Y'      DISPLAY ADVERT TABLE?                        
         BNE   LTAB0340            NO                                           
*                                                                               
         L     R2,AADVAREA         SET A(ADVERT TABLE AREA)                     
         LA    R3,1                SET COUNTER                                  
LTAB0320 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    LTAB0340            YES                                          
*                                                                               
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'ADV:'                                                
         MVC   P+20(ADVTABLN),0(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,ADVTABLN(R2)     BUMP TO NEXT SLOT                            
         B     LTAB0320            GO BACK FOR NEXT SLOT                        
*                                                                               
LTAB0340 EQU   *                                                                
         MVC   P+1(07),=C'ADVCTR='                                              
         EDIT  ADVCTR,(6,P+10)                                                  
         GOTO1 REPORT                                                           
LTAB0360 EQU   *                                                                
         OC    SALCTR,SALCTR       ANY S/P    RECS FOUND?                       
         BNZ   LTAB0380            YES                                          
         MVC   P+1(27),=C'NO S/P    TKO RECORDS FOUND'                          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     LTAB0440                                                         
LTAB0380 EQU   *                                                                
         L     R4,ASALAREA         SET A(TABLE)                                 
         L     R3,SALCTR           SET COUNTER                                  
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),13,3,0                                  
*                                                                               
         CLI   QUESTOR+1,C'Y'      DISPLAY S/P TABLE?                           
         BNE   LTAB0420            NO                                           
*                                                                               
         L     R2,ASALAREA         SET A(S/P TABLE AREA)                        
         LA    R3,1                SET COUNTER                                  
LTAB0400 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    LTAB0420            YES                                          
*                                                                               
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'SAL:'                                                
         MVC   P+20(SALTABLN),0(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,SALTABLN(R2)     BUMP TO NEXT SLOT                            
         B     LTAB0400            GO BACK FOR NEXT SLOT                        
*                                                                               
LTAB0420 EQU   *                                                                
         MVC   P+1(07),=C'SALCTR='                                              
         EDIT  SALCTR,(6,P+10)                                                  
         GOTO1 REPORT                                                           
LTAB0440 EQU   *                                                                
****>>                                                                          
         L     R2,ADCONLST         SWITCH TO SOURCE FILE                        
         USING ADCONSD,R2                                                       
         L     R6,MASTC                                                         
         USING MASTD,R6                                                         
         L     RE,MCUTL            SET A(UTL)                                   
*                                                                               
         DROP R2,R6                                                             
*                                                                               
         MVC   4(1,RE),ORIGUTL     SET OLD FILE UTL                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',FLIST,AREC                       
*                                     OPEN OLD REP FILE                         
         L     R2,ASTAAREA         SET A(STATION LIST)                          
*                                                                               
*   READ ALL STATIONS FROM SOURCE REP                                           
*                                                                               
LTAB0480 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           SET KEY TYPE                                 
         MVC   KEY+20(2),OLDREP    INSERT OLD REP                               
         GOTO1 HIGHDIR             READ KEY                                     
         B     LTAB0520                                                         
LTAB0500 EQU   *                                                                
         GOTO1 SEQDIR              READ NEXT KEY                                
LTAB0520 EQU   *                                                                
         CLI   KEY,2               STILL STATION KEY?                           
         BNE   LTAB0540            NO  - STATION KEYS ALL LOADED                
         CLC   KEY+20(2),OLDREP    STILL OLD REP?                               
         BNE   LTAB0540            NO  - STATION KEYS ALL LOADED                
         CLI   KEY+26,C'C'         COMBO STATION?                               
         BE    LTAB0500            YES - DON'T PUT IT IN TABLE                  
         MVC   0(5,R2),KEY+22      YES - ENTER KEY IN TABLE                     
         LA    R2,LSTALIST(R2)     BUMP TO NEXT SLOT                            
         XC    0(LSTALIST,R2),0(R2) CLEAR NEXT SLOT                             
         L     RF,STACTR           BUMP STATION COUNT                           
         LA    RF,1(RF)                                                         
         ST    RF,STACTR                                                        
         B     LTAB0500            GO BACK FOR NEXT RECORD                      
LTAB0540 EQU   *                                                                
         L     R2,ADCONLST         SWITCH TO TARGET FILE                        
         USING ADCONSD,R2                                                       
         L     R6,MASTC                                                         
         USING MASTD,R6                                                         
         L     RE,MCUTL            SET A(UTL)                                   
*                                                                               
         DROP R2,R6                                                             
*                                                                               
         MVC   4(1,RE),NEWUTL      SET NEW FILE UTL                             
*                                                                               
         L     R2,ASTAAREA         SET A(STATION LIST)                          
*                                                                               
*   SCAN STATION TABLE.  READ TARGET RECORDS, AND INSERT GROUP                  
*                                                                               
LTAB0560 EQU   *                                                                
         OC    0(LSTALIST,R2),0(R2) ANY ENTRY IN SLOT?                          
         BZ    LTAB0600            NO  - FINISHED                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           SET KEY TYPE                                 
         MVC   KEY+20(2),NEWREP    INSERT NEW REP                               
         MVC   KEY+22(5),0(R2)                                                  
         GOTO1 HIGHDIR             READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    LTAB0580            YES                                          
         MVC   DNEWGRP(2,R2),=C'??'                                             
*                                                                               
*   TEST DISPLAY                                                                
         MVC   P+1(14),=C'STATION MISS :'                                       
         MVC   P+16(LSTALIST),0(R2)                                             
         GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                  NO  - INSERT INDICATOR                       
         LA    R2,LSTALIST(R2)     BUMP TO NEXT SLOT                            
         L     RF,MSTACTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,MSTACTR                                                       
         B     LTAB0560            GO BACK FOR NEXT RECORD                      
LTAB0580 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         LA    R3,REC              SET A(RECORD AREA)                           
         USING RSTAREC,R3                                                       
         MVC   DNEWGRP(2,R2),RSTAGRUP                                           
*                                  INSERT GROUP INTO SLOT                       
         DROP  R3                                                               
*                                                                               
*   TEST DISPLAY                                                                
*        MVC   P+1(14),=C'STATION ENTRY:'                                       
*        MVC   P+16(LSTALIST),0(R2)                                             
*        GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
         LA    R2,LSTALIST(R2)     BUMP TO NEXT ENTRY                           
         B     LTAB0560            GO BACK FOR NEXT                             
LTAB0600 EQU   *                                                                
         MVC   P+1(07),=C'STACTR='                                              
         EDIT  STACTR,(6,P+10)                                                  
         MVC   P+20(14),=C'MISSINGSTACTR='                                      
         EDIT  MSTACTR,(6,P+40)                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
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
         MVC   P+1(04),=C'UTL:'                                                 
         MVC   P+5(1),ORIGUTL                                                   
         GOTO1 REPORT                                                           
*   TEST DISPLAY                                                                
*                                                                               
         GOTO1 HIGHDIR                                                          
         MVC   P+1(17),=C'OLD FILE: 1ST REP'                                    
         MVC   P+20(2),OLDREP                                                   
         MVC   P+26(27),KEY        DISPLAY KEY FOUND                            
         GOTO1 REPORT                                                           
                                                                                
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
*        LA    R1,LISTSEL          SET A(SELTEL STATION LIST)                   
*        CLC   OLDREP(4),=C'SZAM'  SELTEL TO KAMNY RUN?                         
*        BE    LIVE0020            YES - USE SELTEL STATIONS                    
*        LA    R1,LISTCON          SET A(CONTINENTAL STATION LIST)              
*        CLC   OLDREP(4),=C'CQAM'  CONTINENTAL TO KAMNY RUN?                    
*        BE    LIVE0020            YES - USE CONTINENTAL STATIONS               
*        LA    R1,LISTKAM          SET A(KAMNY STATION LIST)                    
*        CLC   OLDREP(4),=C'AMSZ'  KAMNY TO SELTEL RUN?                         
*        BE    LIVE0020            YES - USE KAMNY STATIONS                     
*        LA    R1,LISTAMQ          SET A(AM/CQ STATION LIST)                    
*        CLC   OLDREP(4),=C'AMCQ'  KAMNY TO KCONY  RUN?                         
*        BE    LIVE0020            YES - USE KAMNY STATIONS                     
*        DC    H'0'                UNRECOGNIZED OLD REP CODE                    
LIVE0020 EQU   *                                                                
*        CLI   0(R1),0             END OF TABLE?                                
*        BE    LIVE0100            YES - SET CC NOT ZERO                        
*        CLC   DSPSTA(5,R2),0(R1)  STATION IN TABLE?                            
*        BE    LIVE0120            YES                                          
*        LA    R1,LSTALIST(R1)     NO  - BUMP TO NEXT ENTRY                     
*        B     LIVE0020            GO BACK FOR NEXT                             
LIVE0100 EQU   *                                                                
*        LTR   RB,RB                                                            
*        B     LIVE0140                                                         
LIVE0120 EQU   *                                                                
*        SR    R0,R0                                                            
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
         L     R2,ASTAAREA         SET A(FIRST STATION IN LIST)                 
CONT0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    CONT0900            YES - FINISHED                               
*                                                                               
*   TEST                                                                        
         CLC   =C'4S',REPREP       TEST RUN FOR SINGLE STATION?                 
         BNE   CONT0040            NO  - PROCESS ALL STATIONS IN LIST           
         LR    RF,R2               SINGLE STA ONLY: CLEAR NEXT ENTRY            
         LA    RF,LSTALIST(RF)     TO TERMINATE ON NEXT STATION                 
         XC    0(LSTALIST,RF),0(RF)                                             
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
                                                                                
CONT0080 DS    0H                                                               
         CLI   RCON8EID,1          FIRST RECORD OF 3-REC SET?                   
         BNE   CONT0060            NO  - SKIP OVER RECS 2/3                     
*                                                                               
*   TEST DISPLAY                                                                
*        MVC   P+1(09),=C'KEY READ:'                                            
*        MVC   P+10(27),RCON8ETP                                                
*        MVC   P+40(02),OLDREP                                                  
*        MVC   P+44(05),0(R2)                                                   
*        GOTO1 REPORT                                                           
*   TEST DISPLAY                                                                
*                                                                               
         CLC   KEY(08),KEYSAVE     SAME KEY?                                    
         BE    CONT0100            YES - CONTINUE WITH STATION                  
         MVC   P+1(08),=C'STATION='                                             
         MVC   P+9(5),0(R2)        INSERT STATION                               
         MVC   P+15(11),=C'TOTAL SCAN='                                         
         EDIT  CONCTR,(8,P+26)                                                  
         MVC   P+35(17),=C'RUNNING FOR DATE='                                   
         EDIT  CONCTR2,(8,P+52)                                                 
         MVC   P+61(14),=C'STATION COUNT='                                      
         EDIT  CONCTR5,(8,P+75)                                                 
         GOTO1 REPORT                                                           
         XC    CONCTR5,CONCTR5     CLEAR STATION COUNT                          
         LA    R2,LSTALIST(R2)     BUMP TO NEXT STATION IN LIST                 
         B     CONT0020            GO BACK FOR NEXT                             
CONT0100 DS    0H                                                               
         L     RF,CONCTR           YES - COUNT TOTAL RECORDS SCANNED            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR           RESTORE COUNTER                              
*                                                                               
         MVI   ERLYLATE,0          SET CUT FLAG:  SEE NOTE                      
*                                                                               
         CLC   RCON8EFS,LATEDTE    FLIGHT START POST 12/13/99?                  
         BH    CONT0120                                                         
         CLC   RCON8EFE,EARLYDTE   FLIGHT END PRE 12/29/97?                     
         BNL   CONT0140                                                         
CONT0120 EQU   *                                                                
**       MVC   P+1(16),=C'SKIPPED: DATE   '                                     
**       GOTO1 DATCON,DMCB,(2,RCON8EFE),(5,P+20)                                
**       GOTO1 HEXOUT,DMCB,RCON8ECN,P+32,4,=C'TOG'                              
**       GOTO1 REPORT                                                           
         B     CONT0760                                                         
CONT0140 EQU   *                                                                
         CLC   RCON8EFE,LATEDTE    FLIGHT END POST 12/13/99?                    
         BL    CONT0160            NO  - NO CUT AT END                          
         OI    ERLYLATE,CUTLATE    YES - CUT AT END                             
CONT0160 EQU   *                                                                
         CLC   RCON8EFS,EARLYDTE   FLIGHT START PRE 12/29/97?                   
         BNL   CONT0180            NO                                           
         OI    ERLYLATE,CUTEARLY   CUT EARLY                                    
*                                                                               
         DROP  R6                                                               
CONT0180 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
**       MVC   P+1(09),=C'CONTRACT:'                                            
**       MVC   P+10(27),KEY                                                     
**       GOTO1 REPORT                                                           
         L     RF,CONCTR2          YES - COUNT TOTAL RECORDS IN DATE            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR2          RESTORE COUNTER                              
         L     RF,CONCTR5          COUNT TOTAL RECORDS IN STATION               
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR5          RESTORE COUNTER                              
***>>>   B     CONT0060            GO BACK FOR NEXT                             
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
         BO    CONT0740               FOR DELETION                              
*                                                                               
**       CLI   RCONTYPE,C'N'       DON'T TAKE OVER NETWORK ORDERS               
**       BE    CONT0780                                                         
**       CLI   RCONTYPE,C'X'       DON'T TAKE OVER NETWORK ORDERS               
**       BE    CONT0780                                                         
*                                                                               
*        NEED TO CHECK SOFT CONTYPE FOR NETWORK HERE                            
*                                                                               
*        CHECK CONFIRM/VERSION/WIP STATUS:  SKIP WIP'S                          
*                                                                               
*                                                                               
         MVI   DAREORDR,C'N'       SET DARE ORDER TO NO                         
         MVI   DARKTYP,C' '        CLEAR DARE TYPE                              
         LA    R4,RCONELEM         FIND X'1D' DARE ELEMENT                      
CONT0200 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0360            YES - NOT DARE - SKIP WIP CHECK              
         CLI   0(R4),X'1D'         DARE INFO ELEMENT?                           
         BE    CONT0220            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0200            GO BACK FOR NEXT                             
CONT0220 EQU   *                                                                
         MVC   OLDDRLK,RCONDRLK-RCONDREL(R4)                                    
*                                  SAVE ORIG AGENCY ORDER NUMBER                
         MVC   OLDAGY,RCONKAGY     SAVE ORIGINAL AGENCY/OFF                     
         MVI   DAREORDR,C'Y'       SET DARE ORDER TO YES                        
         L     RF,DARECTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DARECTR                                                       
         LA    R4,RCONELEM         FIND X'1F' ELEMENT                           
CONT0240 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0300            YES - OFFLINE ORDER - CHECK FURTHER          
         CLI   0(R4),X'1F'         DARE INFO ELEMENT?                           
         BE    CONT0260            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0240            GO BACK FOR NEXT                             
CONT0260 EQU   *                                                                
         TM    RCONCONF-RCONXEL(R4),X'40'                                       
*                                  CONFIRMED NOW?                               
         BO    CONT0280            YES - ACCEPT IT                              
         TM    RCONCONF-RCONXEL(R4),X'20'                                       
*                                  NO  - PREVIOUSLY CONFIRMED?                  
         BNO   CONT0820            NO  - SKIP THIS ORDER                        
CONT0280 EQU   *                                                                
*                                                                               
         MVC   DARKTYP,=X'41'      SET DARE TYPE RECORD                         
         TM    RCONCONF-RCONXEL(R4),X'40'+X'20'                                 
         BZ    CONT0300                                                         
         MVC   DARKTYP,=X'51'      SET DARE TYPE RECORD: FORCE 51S              
*                                                                               
CONT0300 EQU   *                                                                
*                                                                               
         LA    R4,RCONELEM         FIND X'20' ELEMENT                           
CONT0320 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0800            YES - SKIP THIS ORDER                        
*                                     SHOULD HAVE BEEN THERE                    
         CLI   0(R4),X'20'         SEND INFO ELEMENT?                           
         BE    CONT0340            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0320            GO BACK FOR NEXT                             
CONT0340 EQU   *                                                                
         TM    RCONSENF-RCONSEND(R4),X'02'                                      
*                                  LAST CONFIRMED BY STATION?                   
         BO    CONT0360            YES - ACCEPT IT                              
         TM    RCONSENF-RCONSEND(R4),X'20'+X'10'                                
*                                  STA # ADVANCED?                              
         BO    CONT0360            BOTH SIDES ON:  ACCEPT IT                    
         B     CONT0820            IN PROCESS ON ONE SIDE OR                    
*                                     OTHER:  SKIP IT                           
CONT0360 DS    0H                                                               
*                                                                               
*   RETRIEVE CORRECT SALESPERSON CONVERSION INFORMATION                         
*        NOT EVEN SURE AT THIS POINT HOW S/P WILL BE DONE                       
*                                                                               
***>>>   GOTO1 SETSALES,DMCB,(RC),(R5)                                          
***>>>   BNZ   CONT0860                                                         
*                                                                               
*   RETRIEVE CORRECT AGY/ADVERT CONVERSION INFORMATION                          
*                                                                               
         MVC   OLDADV,RCONKADV     SAVE OLD ADVERTISER CODE                     
*                                                                               
         CLI   NOCODES,C'Y'        SKIP CODES PROCESSING?                       
         BE    CONT0380            YES                                          
         GOTO1 =A(SETCODES),DMCB,(RC),(R5)                                      
         BNZ   CONT0880                                                         
CONT0380 DS    0H                                                               
*>*>****                                                                        
*                                                                               
*  TEST                                                                         
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT0400                                                         
         CLC   CONCTR4,=F'020'     DISPLAY 1ST N RECS ACCEPTED                  
         BH    CONT0400                                                         
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 DISPPUT,DMCB,(RC),4 DISPLAY PRE-CHANGE                           
CONT0400 DS    0H                                                               
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
         AP    DUB(8),=P'2000000'  YES - ADD THIS CONT# ADJUSTMENT              
CONT0420 EQU   *                                                                
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
         BH    CONT0440            NO                                           
         MVC   HIGHCON,OLDCON      YES - REPLACE LAST WITH THIS ONE             
CONT0440 EQU   *                                                                
         CLC   LOWCON,OLDCON       LESS THAN LAST LOW?                          
         BL    CONT0460            NO                                           
         MVC   LOWCON,OLDCON       YES - REPLACE LAST WITH THIS ONE             
CONT0460 EQU   *                                                                
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
         MVC   RCONKAGY(6),SAVEAGY INSERT AGENCY/AGY OFF                        
         MVC   RCONKADV,SAVEADV    INSERT ADVERTISER                            
CONT0480 EQU   *                                                                
         GOTO1 SETELTS,DMCB,(RC),(R5)                                           
         GOTO1 SASSIST,DMCB,(RC),(R5)                                           
*                                                                               
**+++                                                                           
         LA    R4,RCONELEM         FIND X'20' ELEMENT                           
CONT0500 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0540            YES -                                        
         CLI   0(R4),X'20'         SEND INFO ELEMENT?                           
         BE    CONT0520            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0500            GO BACK FOR NEXT                             
CONT0520 EQU   *                                                                
**+++                                                                           
         XC    2(2,R4),2(R4)       FOUND:  SET SEND ID TO ZERO                  
*                                                                               
*   REP SENDING ID IS BEING CLEARED.  CONTRACT PROGRAM WILL CHECK               
*        SELTEL, AMERICAN, AND CONTINENTAL FOR EMPTY, AND REINSERT.             
*                                                                               
         MVI   COVERFLG,C'N'       SET COVERFLAG TO NO                          
         LA    R4,RCONELEM         FIND X'20' ELEMENT                           
CONT0540 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0580            YES -                                        
         CLI   0(R4),X'A6'         SEND INFO ELEMENT?                           
         BE    CONT0560            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0540            GO BACK FOR NEXT                             
CONT0560 EQU   *                                                                
         MVC   RCONCVNM+4-RCONCVEL(4,R4),NEWCON                                 
*                                  INSERT NEW CONTRACT NUMBER INTO ELT          
         MVI   COVERFLG,C'Y'       SET COVERFLAG TO YES                         
CONT0580 EQU   *                                                                
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BE    CONT0600            NO  - DON'T RETRIEVE                         
         GOTO1 PRODCODE,DMCB,(RC),RCONREC                                       
*                                  CHECK/PROCESS PRODUCT CODE                   
         L     RF,CONPROD                                                       
         LA    RF,1(RF)                                                         
         ST    RF,CONPROD                                                       
CONT0600 EQU   *                                                                
         MVC   OLDSTA,RCONKSTA     SAVE STATION CALL LETTERS                    
         MVC   OLDOFF,RCONKOFF     SAVE OFFICE CODE                             
*                                                                               
         L     RF,AIO2             SET A(IO AREA 2)                             
         MOVE  ((RF),4008),REC     MOVE CONTRACT TO HOLD AREA                   
*                                                                               
*  TEST                                                                         
**       CLC   MISSAGYS,=F'3'                                                   
**       BL    TEST0020            BEGIN TESTING AT 5 MISSING                   
**       CLC   MISSADVS,=F'3'                                                   
**       BH    CONT0900            3+ AGIES, 3 ADVS: END                        
                                                                                
**       CLC   CONCTR4,=F'500'     PROCESS 1ST N RECS ACCEPTED                  
**       BH    CONT0900                                                         
TEST0020 EQU   *                                                                
*  TEST END                                                                     
*                                                                               
                                                                                
         ZICM  RF,RCONLEN,2        ADD ALL CONTRACT LENGTHS                     
         A     RF,CONBYTES                                                      
         ST    RF,CONBYTES                                                      
*                                                                               
                                                                                
         CLI   ERLYLATE,0          ANY CUTBACK IN THIS ORDER?                   
         BE    CONT0620            NO  - JUST TAKE THE ORDER                    
         GOTO1 =A(SAVEBUKS),DMCB,(RC)                                           
*                                  YES - SAVE ORIGINAL BUCKETS                  
CONT0620 EQU   *                                                                
         GOTO1 =A(BUYPROC2),DMCB,(RC)                                           
*                                  PROCESS THIS CONTRACT'S BUYS                 
         CLI   ERLYLATE,0          ANY CUTBACK IN THIS ORDER?                   
         BE    CONT0630            NO  - DON'T PLAY W/BUCKS ANY MORE            
         GOTO1 =A(RESETBUK),DMCB,(RC)                                           
*                                  YES - RESET ORIGINAL BUCKETS                 
CONT0630 EQU   *                                                                
         BAS   RE,MKGDPROC         PROCESS THIS CONTRACT'S M/GS                 
         CLI   DAREORDR,C'N'       DARE ORDER?                                  
         BE    CONT0640            NO                                           
         GOTO1 =A(TAKEDARE),DMCB,(RC)                                           
CONT0640 EQU   *                                                                
         CLI   COVERFLG,C'N'       COVERFLAG PRESENT                            
         BE    CONT0660            NO  - DON'T PROCESS                          
         BAS   RE,TAKECOV          YES - GET COVERSHEET                         
CONT0660 EQU   *                                                                
         BAS   RE,TAKECFC          GET CONFIRM COMMENT                          
*                                                                               
*   ALL SECONDARY RECORDS (BUY/DARE/COVERSHEET/MKGDS/ETC HAVE                   
*        BEEN PROCESSED.  ORIGINAL CONTRACT MUST BE REESTABLISHED               
*        IN ORDER TO BE REWRITTEN WITH CHANGES INTRODUCED BY                    
*        BUY CUTBACKS.  CONTRACT RECORD HAS BEEN HELD IN IO AREA 2              
*                                                                               
         MVC   KEY,SAVEKEY         RESTART CONTRACTS                            
         GOTO1 HIGHDIR             REREAD THIS KEY                              
         OI    DMINBTS,X'08'       READ DELETED                                 
         GOTO1 GETRECRD            GET RECORD AGAIN IN MAIN AREA                
*                                                                               
         L     RE,AIO2                                                          
         MOVE  (REC,4008),(RE)     MOVE CONTRACT BACK TO REC AREA               
         MVC   RCONBUYR,SPACES                                                  
         MVC   RCONBUYR(06),=C'ACC-BB'                                          
*                                  SET TO 'BACK BILLING'                        
*                                                                               
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR DISPLAY                    
         BAS   RE,PUTRECS          WRITE THE CONTRACT RECORD OUT                
*                                                                               
         L     RF,CONCTR4          COUNT TOTAL RECORDS ACCEPTED                 
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR4          RESTORE COUNTER                              
*                                                                               
*                                                                               
*  TEST                                                                         
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT0680                                                         
         CLC   CONCTR4,=F'020'     PROCESS 1ST N RECS ACCEPTED                  
         BH    CONT0680                                                         
         GOTO1 DISPPUT,DMCB,(RC),0 DISPLAY POST-CHANGE                          
CONT0680 DS    0H                                                               
*  TEST END                                                                     
*                                                                               
         L     RF,CONCTR3          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR3                                                       
         CLC   CONCTR3,=F'1000'    DISPLAY EVERY N RECORDS                      
         BNE   CONT0700                                                         
         XC    CONCTR3,CONCTR3                                                  
***      MVC   P+1(21),=C'PROCESSING CONTRACTS:'                                
***      EDIT  CONCTR,(7,P+24)                                                  
***      GOTO1 REPORT                                                           
CONT0700 EQU   *                                                                
         B     CONT0060                                                         
                                                                                
CONT0740 DS    0H                                                               
         L     RF,CONDEL           COUNT DELETED CONTRACTS                      
         LA    RF,1(RF)                                                         
         ST    RF,CONDEL                                                        
         MVC   P+1(16),=C'SKIPPED: DELETED'                                     
         GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0760 DS    0H                                                               
         L     RF,CONDATE          COUNT DATED CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,CONDATE                                                       
         B     CONT0060            GO BACK FOR NEXT                             
CONT0780 DS    0H                                                               
         L     RF,CONCTYPE         COUNT CTYPE CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTYPE                                                      
         MVC   P+1(16),=C'SKIPPED: CONTYPE'                                     
         MVC   P+18(1),RCONTYPE                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0800 DS    0H                                                               
         L     RF,NO20CTR          COUNT NO 20 CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,NO20CTR                                                       
         MVC   P+1(16),=C'SKIPPED: NO 20  '                                     
***>>    GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0820 DS    0H                                                               
         L     RF,CONWIP           COUNT WIP   CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,CONWIP                                                        
         CLC   RCONBUYR(8),=C'FORECAST'                                         
         BE    CONT0840                                                         
         MVC   P+1(17),=C'SKIPPED: WIP/DARE'                                    
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
         GOTO1 DATCON,DMCB,(3,RCONCREA),(5,P+30)                                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,P+40)                                
         MVI   P+48,C'-'                                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,P+49)                              
         MVC   P+59(20),RCONBUYR                                                
         GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0840 DS    0H                                                               
         L     RF,CONWIPFC         COUNT WIPFC CONTRACTS                        
         LA    RF,1(RF)                                                         
         ST    RF,CONWIPFC                                                      
         B     CONT0060            GO BACK FOR NEXT                             
CONT0860 DS    0H                                                               
         MVC   P+1(16),=C'SKIPPED: NO S/P '                                     
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
         MVC   P+32(5),RCONKSTA                                                 
         MVC   P+38(2),RCONKOFF                                                 
         GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0880 DS    0H                                                               
**       MVC   P+1(22),=C'SKIPPED: NO AGY OR ADV'                               
**       GOTO1 REPORT                                                           
         B     CONT0060            GO BACK FOR NEXT                             
CONT0900 DS    0H                                                               
         XIT1                                                                   
         DROP  R5                                                               
DNEWGRP  EQU   5                                                                
LSTALIST EQU   10                                                               
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
         GOTO1 DATCON,DMCB,(3,EARLYDTB),(5,NWCOMDAT)                            
*                                  INSERT EFFECTIVE DATE INTO ELEMENT           
         MVC   NWOLDREP,=C'CLEAR CHANNEL/IR   '                                 
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
*  MKGDPROC:  RETRIEVE ALL M/G RECORDS ATTACHED TO CONTRACT EXTRACTED           
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
         MVC   P+1(20),=C'PRE -PUTREC CONTRACT'                                 
         EDIT  CONCTR4,(6,P+30)                                                 
         EDIT  NOSPCTR,(6,P+40)                                                 
         LTR   R2,R2               ANY VALUE?                                   
         BNZ   DIPU0020            YES - IT'S PRE                               
         MVC   P+1(20),=C'POST-PUTREC CONTRACT'                                 
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
*  DISPBUY:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*        4(R1):  IF PRE-PUT, WILL BE '4'                         *              
*                IF POST   , WILL BE '0'                         *              
*                                                                *              
******************************************************************              
*                                                                               
DISPBUY  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            LOAD PRE/POST VALUE                          
         MVC   P+1(18),=C'PRE -PUTREC BUY   '                                   
         EDIT  CONCTR4,(6,P+30)                                                 
         EDIT  NOSPCTR,(6,P+40)                                                 
         LTR   R2,R2               ANY VALUE?                                   
         BNZ   DIBU0020            YES - IT'S PRE                               
         MVC   P+1(18),=C'POST-PUTREC BUY   '                                   
DIBU0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         AR    RF,R2               ADD PRE/POST PUT LENGTH                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIBU0090 EQU   *                                                                
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
VREPFACS DS    V                                                                
ACOMFACS DS    A                                                                
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
ASTAAREA DS    A                   STATION AREA                                 
ANEXTSTA DS    A                   NEXT STATION SLOT                            
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
CONCTR5  DS    F                                                                
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
MSTACTR  DS    F                                                                
OFFCTR   DS    F                                                                
MKGCTR   DS    F                                                                
SVTOTSPT DS    F                                                                
SVTOTWKS DS    F                                                                
LASTSTA  DS    CL5                                                              
OLDREP   DS    CL2                 OLD REP CODE                                 
NEWREP   DS    CL2                 NEW REP CODE                                 
REPREP   DS    CL2                 REP REP CODE (REPLACEMENT)                   
NOCODES  DS    CL1                 SKIP CODE REPLACEMENT TEST                   
HIGHCON  DS    XL4                                                              
LOWCON   DS    XL4                                                              
KEYTYPE  DS    CL1                                                              
CODFOUND DS    CL1                                                              
MGBITFLG DS    CL1                                                              
BUCKFLGS DS    XL1                 BUCKETING FLAGS                              
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
EARLYDTE DS    CL2                 START BACKBILLING DATE COMPRESSED            
LATEDTE  DS    CL2                 END   BACKBILLING DATE COMPRESSED            
EARLYDTB DS    CL3                 START BACKBILLING DATE 3-CHAR BIN            
LATEDTB  DS    CL3                 END   BACKBILLING DATE 3-CHAR BIN            
EARLYMOS DS    CL2                 START MONTH OF SERVICE CUTOFF DATE           
EACTDATE DS    CL2                 EARLY ACTIVITY DATE: COMPRESSED              
LACTDATE DS    CL2                 LATE  ACTIVITY DATE: COMPRESSED              
ERLYLATE DS    CL1                 X'02' = CUT AT EARLY END                     
CUTEARLY EQU   02                                                               
*                                  X'01' = CUT AT LATE END                      
CUTLATE  EQU   01                                                               
*                                  X'00' = NO CUT TO ORDER                      
*                                                                               
UTLTABLE DC    C'AM',X'78'         KATZ AMERICAN/EAGLE                          
LUTLTABL EQU   *-UTLTABLE                                                       
         DC    C'BL',X'08'         BLAIR                                        
         DC    C'NU',X'58'         CLEAR CHANNEL (KATZ)                         
         DC    C'CN',X'68'         CLEAR CHANNEL (IR)                           
         DC    C'SJ',X'08'         SJR/DDS TEST                                 
         DC    C'B3',X'38'         EJOR/DDS TEST                                
         DC    X'0000'             DELIMITER                                    
*   EQUATES                                                                     
SALTABLN EQU   13                                                               
DSALTOSP EQU   0                   DISPLACE TO OLD S/P                          
DSALTNSP EQU   3                   DISPLACE TO NEW S/P+OFFICE                   
DSALTTEM EQU   8                   DISPLACE TO TEAM                             
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
         MVI   CODFOUND,C'Y'       SET 'CODE(S) FOUND'                          
         USING RCONREC,R5                                                       
         MVC   SAVEAGY,RCONKAGY    SET SAVEAREAS TO THIS ORDER                  
         MVC   SAVEADV,RCONKADV                                                 
         MVC   DUB(6),RCONKAGY     SET UP SEARCH ARGUMENT                       
         L     R2,AAGYAREA         SET A(AGENCY TABLE)                          
         L     R4,AGYCTR           CURRENT # OF TABLE ENTRIES                   
         LTR   R4,R4               ANYTHING IN COUNTER?                         
         BZ    SCOD0010            NO  - SET AS MISSING                         
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),12,6,5000                         
                                                                                
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCOD0040            YES                                          
SCOD0010 EQU   *                                                                
         L     RF,NOAGYREC         COUNT MISSING AGENCY ORDERS                  
         LA    RF,1(RF)                                                         
         ST    RF,NOAGYREC                                                      
*                                                                               
SCOD0020 EQU   *                                                                
**       MVC   P+1(12),=C'MISSING AGY:'                                         
**       MVC   P+13(6),DUB                                                      
**       GOTO1 HEXOUT,DMCB,RCONKCON,P+24,4,=C'TOG'                              
**       GOTO1 REPORT                                                           
         L     R2,AMISSAGY         SET A(MISSING AGENCIES)                      
         L     R4,MISSAGYS         SET A(NUMBER OF AGENCIES IN TABLE)           
         GOTO1 =V(BINSRCH),DMCB,(1,DUB),(R2),(R4),6,6,5000                      
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCOD0800            YES - DON'T ADD TO COUNTER                   
         MVC   MISSAGYS,DMCB+8     SAVE NEW COUNT                               
**       MVC   P+1(12),=C' AGY TABLED:'                                         
**       GOTO1 REPORT                                                           
*                                                                               
         MVI   CODFOUND,C'N'       SET 'CODE NOT FOUND'                         
         B     SCOD0065            LOOK FOR ADVERTISER CODE NOW                 
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
SCOD0065 EQU   *                                                                
         MVC   DUB(4),RCONKADV     SET UP SEARCH ARGUMENT                       
         L     R2,AADVAREA         SET A(ADVERT TABLE)                          
         L     R4,ADVCTR           CURRENT # OF TABLE ENTRIES                   
         LTR   R4,R4               ANYTHING IN COUNTER?                         
         BZ    SCOD0070            NO  - SET AS MISSING                         
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),8,4,10000                         
                                                                                
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCOD0100            YES                                          
SCOD0070 EQU   *                                                                
         L     RF,NOADVREC         COUNT MISSING ADVERT ORDERS                  
         LA    RF,1(RF)                                                         
         ST    RF,NOADVREC                                                      
*                                                                               
SCOD0080 EQU   *                                                                
**       MVC   P+1(12),=C'MISSING ADV:'                                         
**       MVC   P+13(4),DUB                                                      
**       GOTO1 HEXOUT,DMCB,RCONKCON,P+24,4,=C'TOG'                              
**       GOTO1 REPORT                                                           
         L     R2,AMISSADV         SET A(MISSING ADVERTS)                       
         L     R4,MISSADVS         SET A(NUMBER OF ADVERTS IN TABLE)            
         GOTO1 =V(BINSRCH),DMCB,(1,DUB),(R2),(R4),4,4,5000                      
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCOD0800            YES - DON'T ADD TO COUNTER                   
         MVC   MISSADVS,DMCB+8     SAVE NEW COUNT                               
         MVI   CODFOUND,C'N'       SET 'CODE NOT FOUND'                         
         B     SCOD0140            EXIT CC NOT ZERO                             
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
SCOD0140 EQU   *                                                                
         CLI   CODFOUND,C'N'       MISSING CODE?                                
*                                                                               
***?>>   BE    SCOD0800            YES - EXIT CC NOT ZERO                       
*                                                                               
         SR    R0,R0               NO  - SET CC ZERO                            
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
******************************************************************              
*  BUYPROC2:  RETRIEVE ALL BUY RECORDS ATTACHED TO CONTRACT EXTRACTED           
******************************************************************              
BUYPROC2 NMOD1 0,*BPRO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
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
         B     BPRO0040                                                         
                                                                                
BPRO0020 DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
                                                                                
BPRO0040 DS    0H                                                               
*                                                                               
***      MVC   P+1(12),=C'BUY: KEYS  ='                                         
***      MVC   P+12(27),KEY                                                     
***      MVI   P+39,C'/'                                                        
***      MVC   P+40(27),KEYSAVE                                                 
***      GOTO1 REPORT                                                           
*                                                                               
         CLC   KEY(22),KEYSAVE     SAME CONTRACT?                               
         BNE   BPRO0280            NO  - FINISHED                               
                                                                                
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
         BNE   BPRO0050                                                         
         CLC   BUYCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    BPRO0050                                                         
         MVC   REC-4(2),RBUYLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 DISPBUY,DMCB,(RC),4 DISPLAY PRE-CHANGE                           
BPRO0050 DS    0H                                                               
*  TEST END                                                                     
*                                                                               
**<<                                                                            
***>>>ADDITIONAL READ CODE START                                                
         CLI   ERLYLATE,0          ANY CUTBACK IN THIS ORDER?                   
         BE    BPRO0055            NO  - DON'T NEED TO CUT BACK                 
         MVC   P+1(18),=C'BUY BEING CUT BACK'                                   
         EDIT  RBUYKMLN,(3,P+30),ALIGN=LEFT                                     
         MVC   P+35(27),RBUYREC                                                 
         GOTO1 REPORT                                                           
         GOTO1 =A(BUYBACK),DMCB,(RC)                                            
*                                  YES - CUT BACK BUYS AND REGENERATE           
*                                     ESTIMATE BUCKETS                          
         B     BPRO0060                                                         
BPRO0055 EQU   *                                                                
*                                                                               
*   NO BUCKUP FOR THIS ORDER                                                    
         MVC   P+1(11),=C'NO BUCKUP  '                                          
         MVC   P+35(27),RBUYREC                                                 
         GOTO1 REPORT                                                           
*   BUCKUP CALL END                                                             
*                                                                               
BPRO0060 EQU   *                                                                
         SR    R0,R0                                                            
         LA    R5,RBUYELEM                                                      
BPRO0080 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    BPRO0140            YES                                          
         CLI   0(R5),X'56'         MAKEGOOD ELEMENT?                            
         BNE   BPRO0100            NO                                           
         USING RBYMGSEL,R5                                                      
*                                                                               
         CLC   RBYMGSDT,LATEDTB    MISSED DATE AFTER LATE DATE?                 
         BH    BPRO0090            YES - DOESN'T BELONG                         
         CLC   RBYMGSDT,EARLYDTB   NO  - MISSED DATE BEFORE TKO DATE?           
         BNL   BPRO0120            NO  - KEEP ELEMENT                           
*                                                                               
         DROP  R5                                                               
*                                                                               
BPRO0090 EQU   *                                                                
         MVI   0(R5),X'FF'         YES - SET ELEMENT FOR DELETE                 
         LA    R0,1                SET ELEMENTS FOUND FOR DELETE FLAG           
         B     BPRO0120            BUMP TO NEXT ELEMENT                         
*                                                                               
BPRO0100 EQU   *                                                                
         CLI   0(R5),X'66'         MAKEGOOD OFFER ELEMENT?                      
         BNE   BPRO0120            NO                                           
         USING RBMGMSEL,R5                                                      
*                                                                               
         CLC   RBYMGSDT,LATEDTB    MISSED DATE AFTER LATE DATE?                 
         BH    BPRO0110            YES - DOESN'T BELONG                         
         CLC   RBMGMSDT,EARLYDTB   NO  - MISSED DATE BEFORE TKO DATE?           
         BNL   BPRO0120            NO  - KEEP ELEMENT                           
*                                                                               
         DROP  R5                                                               
*                                                                               
BPRO0110 EQU   *                                                                
         MVI   0(R5),X'FF'         YES - SET ELEMENT FOR DELETE                 
         LA    R0,1                SET ELEMENTS FOUND FOR DELETE FLAG           
BPRO0120 EQU   *                                                                
         ZIC   RF,1(R5)                                                         
         AR    R5,RF               BUMP TO NEXT ELEMENT                         
         B     BPRO0080            GO BACK FOR NEXT ELEMENT                     
BPRO0140 EQU   *                                                                
         LTR   R0,R0               ANYTHING TO DELETE?                          
         BZ    BPRO0160                                                         
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RBUYREC),0,0            
*                                  DELETE MKG OFFR ELTS PRIOR TO                
*                                     TKO DATE                                  
BPRO0160 EQU   *                                                                
         LA    RF,RBUYELEM                                                      
BPRO0170 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    BPRO0175            YES - NO EFF ELTS IN RECORD                  
         CLI   0(RF),3             EFFECTIVE DATE ELEMENT?                      
         BE    BPRO0180            YES - OUTPUT RECORD                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     BPRO0170            GO BACK FOR NEXT                             
BPRO0175 EQU   *                                                                
         CLI   ERLYLATE,0          NO  - CUTS MADE TO BUYS?                     
         BE    BPRO0180            NO  - OUTPUT VIRGIN BUY WITH NO              
*                                     EFF ELTS PRESENT                          
         B     BPRO0240            DON'T OUTPUT W/NO EFFELTS - DELETED          
*                                     BY CUTS                                   
*                                                                               
BPRO0180 EQU   *                                                                
***>>>ADDITIONAL READ CODE END                                                  
                                                                                
BPRO0200 DS    0H                                                               
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         MVC   REC-4(2),RBUYLEN                                                 
         MVC   RBUYKREP,REPREP     INSERT REPLACEMENT REP CODE                  
         MVC   RBUYKCON,NEWCONRV   INSERT NEW CONTRACT #                        
                                                                                
         ZICM  RF,RBUYLEN,2        ADD ALL BUY LENGTHS                          
         A     RF,BUYBYTES                                                      
         ST    RF,BUYBYTES                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+7,C'Y'                                                   
         BNE   BPRO0220                                                         
         CLC   BUYCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    BPRO0220                                                         
         MVC   REC-4(2),RBUYLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 DISPBUY,DMCB,(RC),0 DISPLAY POST-CHANGE                          
BPRO0220 EQU   *                                                                
         B     BPRO0020                                                         
BPRO0240 EQU   *                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         MVC   REC-4(2),RBUYLEN                                                 
         MVC   RBUYKREP,REPREP     INSERT REPLACEMENT REP CODE                  
         MVC   RBUYKCON,NEWCONRV   INSERT NEW CONTRACT #                        
         CLI   QUESTOR+7,C'Y'                                                   
         BNE   BPRO0260                                                         
         CLC   BUYCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    BPRO0260                                                         
         MVC   P+1(27),=C'BUY HAS NO EFFELTS: DROPPED'                          
         EDIT  RBUYKMLN,(3,P+30),ALIGN=LEFT                                     
         MVC   P+35(27),RBUYREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
BPRO0260 DS    0H                                                               
         B     BPRO0020                                                         
BPRO0280 DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  SAVEBUKS:  STRIP ALL BUCKETS FROM RECORD FOR REINSERTION                     
*        NOTE:  BUCKETS MUST BE DROPPED BY MONTH OF SERVICE.                    
*        BECAUSE THE EARLIEST START DATE MAY BE IN THE MONTH                    
*        PRIOR TO THE MONTH OF SERVICE, A SEPARATE DATE IS                      
*        REQUIRED FOR THIS TESTING.  FOR INSTANCE, START DATE                   
*        OF THE HISTORY IS BROADCAST MONTH JAN/98, WHICH BEGINS                 
*        DEC29/97.  A SEPARATE DATE TO DROP BUCKETS PRIOR TO                    
*        JAN/98 IS NEEDED.                                                      
*                                                                               
******************************************************************              
SAVEBUKS NMOD1 0,*SBUK*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     RF,AWORKBLK         CLEAR ANY PREVIOUS BUCKETS                   
         XCEFL 0(RF),4000                                                       
         L     RF,AWORKBLK         RESET A(WORKSTORAGE SPACE)                   
         L     R2,AIO2             POINT TO STORED CONTRACT                     
         USING RCONREC,R2                                                       
         LA    R3,RCONELEM                                                      
SBUK0020 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    SBUK0200            YES -                                        
         CLI   0(R3),3             ESTIMATE BUCKET?                             
         BNE   SBUK0040            NO                                           
         MVC   0(10,RF),0(R3)      YES - MOVE TO STORAGE                        
         LA    RF,10(RF)           BUMP STORAGE                                 
         MVI   0(R3),X'FF'         SET ELEMENT TO DELETE                        
         B     SBUK0080                                                         
SBUK0040 EQU   *                                                                
         CLI   0(R3),4             INVOICE BUCKET?                              
         BNE   SBUK0080            NO                                           
         CLC   2(2,R3),EARLYMOS                                                 
         BNL   SBUK0060                                                         
         MVI   0(R3),X'FF'         EARLIER THAN EARLY: DROP IT                  
SBUK0060 EQU   *                                                                
         CLC   LATEDTB(2),2(R3)                                                 
         BNL   SBUK0080                                                         
         MVI   0(R3),X'FF'         LATER THAN LATE: DROP IT                     
SBUK0080 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     SBUK0020            GO BACK FOR NEXT                             
SBUK0200 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0            
*                                  DELETE OLD 03/04 ELEMENTS                    
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*  RESETBUK:                                                                    
*        AT THIS POINT, THE BUCKETS HAVE BEEN REBUILT FROM THE                  
*        BUY RECORDS.  ONLY THE BUCKETS REPRESENTING THOSE MONTHS               
*        WHICH HAVE BEEN CUT WILL BE KEPT WITHIN THE RECORD.                    
*        THE OTHERS WILL BE DROPPED.  THESE MONTHS WILL BE REPLACED             
*        BY THE BUCKETS FROM THE ORIGINAL RECORD/PRE PROCESSING.                
*                                                                               
*        1.  DROP ALL NON-CUT BUCKETS FROM CONTRACT RECORD.                     
*        2.  MOVE ALL NON-CUT BUCKETS BACK TO CONTRACT FROM                     
*            STORAGE AREA.                                                      
*        3.  SET ACTIVITY DATE IN ALL CUT MONTH BUCKETS TO                      
*            THE EARLIEST ACTIVITY DATE IN THE STORAGE BUCKET                   
*            FOR THAT MONTH.                                                    
*                                                                               
******************************************************************              
RESETBUK NMOD1 0,*RBUK*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,AIO2             POINT TO STORED CONTRACT                     
         USING RCONREC,R2                                                       
         LA    R3,RCONELEM         SET A(01 ELEMENT OF CONTRACT)                
RBUK0020 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    RBUK0060            YES -                                        
         CLI   0(R3),3             ESTIMATE BUCKET?                             
         BNE   RBUK0040            NO                                           
         CLC   2(2,R3),EARLYMOS    YES - EQUAL TO EARLY MONTH?                  
         BE    RBUK0040            YES - LEAVE ALONE                            
         CLC   2(2,R3),LATEDTB     YES - EQUAL TO LATE  MONTH?                  
         BE    RBUK0040            YES - LEAVE ALONE                            
         MVI   0(R3),X'FF'         NO  - SET ELEMENT TO DELETE                  
RBUK0040 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     RBUK0020            GO BACK FOR NEXT                             
RBUK0060 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0            
*                                  DELETE 03 ELTS FOR NON-CUTBACK               
*                                     MONTHS OF SERVICE                         
         L     R3,AWORKBLK         SET A(BUCKET STORAGE)                        
         XC    EACTDATE(8),EACTDATE                                             
*                                  CLEAR ACTIVITY DATES, EARLY/LATE             
*                                  DERIVE EARLY/LATE ACTIVITY DATES             
*                                     FROM ORIGINAL DATA                        
RBUK0080 EQU   *                                                                
         OC    0(10,R3),0(R3)      ANY BUCKET IN SLOT?                          
         BZ    RBUK0160            NO  - FINISHED                               
         CLC   2(2,R3),EARLYMOS    YES - EQUAL TO EARLY MONTH?                  
         BNE   RBUK0100            NO  - CHECK LATE ACTIVITY DATE               
         OC    EACTDATE,EACTDATE   YES - EARLY ACTIVITY DATE FOUND?             
         BNZ   RBUK0140            YES - DON'T SET AGAIN                        
         MVC   EACTDATE,4(R3)      NO  - SAVE EARLY ACTIVITY DATE               
         B     RBUK0140                                                         
RBUK0100 EQU   *                                                                
         CLC   2(2,R3),LATEDTB     YES - EQUAL TO LATE  MONTH?                  
         BNE   RBUK0120            NO  - SKIP TO NEXT BUCKET                    
         OC    LACTDATE,LACTDATE   YES - LATE  ACTIVITY DATE FOUND?             
         BNZ   RBUK0140            YES - DON'T SET AGAIN                        
         MVC   LACTDATE,4(R3)      NO  - SAVE LATE  ACTIVITY DATE               
         B     RBUK0140                                                         
RBUK0120 EQU   *                                                                
         CLC   2(2,R3),EARLYMOS    BUCKET PRE-EARLY DATE?                       
         BL    RBUK0140            YES - DON'T REINSERT INTO CONTRACT           
         CLC   2(2,R3),LATEDTB     BUCKET POST-LATE DATE?                       
         BH    RBUK0140            YES - DON'T REINSERT INTO CONTRACT           
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,0(R5),0                
*                                  NO  - INSERT 03 ELEMENT INTO RECORD          
RBUK0140 EQU   *                                                                
         LA    R3,10(R3)           BUMP TO NEXT SLOT                            
         B     RBUK0080            GO BACK FOR NEXT                             
RBUK0160 EQU   *                                                                
         LA    R3,RCONELEM         SET A(01 ELEMENT OF CONTRACT)                
RBUK0180 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    RBUK0240            YES -                                        
         CLI   0(R3),3             ESTIMATE BUCKET?                             
         BNE   RBUK0220            NO                                           
         CLC   2(2,R3),EARLYMOS    YES - EQUAL TO EARLY MONTH?                  
         BNE   RBUK0200            NO  -                                        
         MVC   4(2,R3),EACTDATE    YES - INSERT ORIGINAL EARLY ACT DATE         
         B     RBUK0220                                                         
RBUK0200 EQU   *                                                                
         CLC   2(2,R3),LATEDTB     YES - EQUAL TO LATE  MONTH?                  
         BNE   RBUK0220            NO  -                                        
         MVC   4(2,R3),LACTDATE    YES - INSERT ORIGINAL LATE ACT DATE          
RBUK0220 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     RBUK0180            GO BACK FOR NEXT                             
RBUK0240 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***>BUYBACK INSERT HERE                                                         
*                                                                               
*   BUYBACK:  ADJUSTS BUY FLIGHT DATES, BASED ON TAKEOVER DATE,                 
*        AND REGENERATES ESTIMATE BUCKETS IN THE CONTRACT RECORD                
*                                                                               
BUYBACK  NMOD1 0,*BBAK*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,REC                                                           
         USING RBUYREC,R6                                                       
         XC    SVTOTSPT,SVTOTSPT   CLEAR ACCUMULATORS                           
         XC    SVTOTWKS,SVTOTWKS                                                
*                                                                               
****>>>  GOTO1 DATCON,DMCB,(5,WORK),(3,RBUYCREA)                                
*                                  SET BUY CREATE TO DATE OF RUN                
***      MVC   RBUYCREA,EARLYDTB   SET BUY CREATE DATE TO TKO                   
         OC    RBUYCHGD,RBUYCHGD   HAS BUY BEEN CHANGED?                        
         BZ    BUYB0020            NO  - LEAVE AS ZERO                          
***>>>   MVC   RBUYCHGD,RBUYCREA   YES - SET BUY CHANGE DATE                    
BUYB0020 EQU   *                                                                
*                                                                               
*   PROCESS EACH 03 (EFFECTIVE DATE) ELEMENT:                                   
*        1.  IF END BEFORE EARLY DATE, DROP ELEMENT                             
*        2.  IF START AFTER LATE DATE, DROP ELEMENT                             
*        3.  IF START NOT EARLIER THAN EARLY DATE, USE AS-IS                    
*        4.  IF START EARLIER, USE EARLY DATE, ADJUST ELEMENT                   
*        5.  IF END   NOT LATER THAN LATE DATE, USE AS-IS                       
*        6.  IF END   LATER, USE LATE DATE, ADJUST ELEMENT                      
*        7.  RECALCULATE TOTAL BUY FIGURES FROM 03 ELEMENT DETAILS              
*                                                                               
         LA    R3,RBUYELEM                                                      
BUYB0040 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    BUYB0200            YES                                          
         CLI   0(R3),3             EFFECTIVE DATE ELEMENT?                      
         BNE   BUYB0180            NO  - BUMP TO NEXT ELEMENT                   
         USING RBUYDTEL,R3                                                      
*                                                                               
         CLC   EARLYDTB,RBUYDTED   EARLY HIST DATE VS EFF DATE END              
         BH    BUYB0160            EFF DATE END PRIOR EARLY HIST DATE           
*                                     DROP THE ELEMENT                          
*                                  EXAMPLE:  EARLYDTB = DEC29/97                
*                                            RBUYDTED = DEC15/97                
*                                                                               
         CLC   RBUYDTST,LATEDTB    EFF DATE START VS LATE HIST DATE             
         BNL   BUYB0160            EFF DATE START AFTER LATE  HIST DATE         
*                                     DROP THE ELEMENT                          
*                                  EXAMPLE:  LATEDTB   =  DEC12/99              
*                                            RBUYDTST  =  DEC20/99              
*                                                                               
         CLC   EARLYDTB,RBUYDTST   EARLY HIST DATE VS EFF DATE START            
         BNH   BUYB0060            EARLY HIST DATE PRIOR EFF DATE START         
         GOTO1 CUTSTART,DMCB,(R3)  CUT BACK ELEMENT - REGEN BUCKETS             
*                                  EXAMPLE:  EARLYDTB = DEC29/97                
*                                            RBUYDTST = DEC08/97                
         BNZ   BUYB0160            ERROR:  DROP ELEMENT                         
BUYB0060 EQU   *                                                                
*                                                                               
         CLC   LATEDTB,RBUYDTED    LATE HIST DATE VS EFF DATE END               
         BNL   BUYB0080            LATE HIST DATE NOT < EFF DATE END            
         GOTO1 CUTEND,DMCB,(R3)    CUT BACK ELEMENT - REGEN BUCKETS             
*                                  EXAMPLE:  LATEDTB  = DEC12/99                
*                                            RBUYDTED = DEC27/99                
         BNZ   BUYB0160            ERROR:  DROP ELEMENT                         
BUYB0080 EQU   *                                                                
*                                                                               
         B     BUYB0180            START/END DATES WITHIN HIST RANGE            
*                                     LEAVE ALONE - REGEN BUCKETS               
*                                  EXAMPLE:  EARLYDTB = DEC29/97                
*                                            EFF STRT = DEC29/97 (OR)           
*                                            EFF STRT = JAN12/98                
*                                            LATEDTB  = DEC12/99                
*                                            EFF END  = DEC12/99 (OR)           
*                                            EFF STRT = DEC05/99                
BUYB0140 EQU   *                                                                
BUYB0160 EQU   *                                                                
         MVI   0(R3),X'FF'         SET ELEMENT FOR DELETION                     
BUYB0180 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     BUYB0040            GO BACK FOR NEXT ELEMENT                     
BUYB0200 EQU   *                                                                
*                                                                               
*   PROCESS EACH 05 (BUY MG REF ELT)/56 (MG SPLITOUT ELT)                       
*        1.  IF DATE BEFORE TKO DATE, DROP ELEMENT                              
*                                                                               
         LA    R3,RBUYELEM                                                      
         SR    RF,RF               CLEAR REG FOR INDICATOR                      
         MVI   MGBITFLG,0          CLEAR FLAG BITS                              
         XC    ELEM,ELEM           CLEAR ELEMENT BUILD AREA                     
BUYB0220 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    BUYB0300            YES                                          
         CLI   0(R3),4             BUY COMMENT ELEMENT?                         
         BNE   BUYB0230            NO  -                                        
         LTR   RF,RF               YES - PRIOR COMMENT FOUND?                   
         BNZ   BUYB0230            YES - DON'T SAVE ADDRESS                     
         LR    RF,R3               NO  - SAVE A(COMMENT RECORD)                 
BUYB0230 EQU   *                                                                
         CLI   0(R3),5             BUY MG REF ELT?                              
         BE    BUYB0240            YES - PROCESS IT                             
         CLI   0(R3),X'56'         BUY MG SPLITOUT ELT?                         
         BE    BUYB0260            YES - PROCESS IT                             
         CLI   0(R3),X'07'         CREDIT XREF ELEMENT?                         
         BE    BUYB0232            YES - PROCESS IT                             
         B     BUYB0280            NO  - BUMP TO NEXT ELT                       
BUYB0232 EQU   *                                                                
         USING RBUYCREL,R3                                                      
*                                                                               
         CLC   RBUYCRDT,EARLYDTB   CREDIT MISSED DATE < EFF DATE?               
*                                                                               
         DROP  R3                                                               
*                                                                               
         BNL   BUYB0280            NO  - LEAVE ELT ALONE                        
         MVI   0(R3),X'FF'         MARK ELEMENT FOR DELETION                    
         B     BUYB0280                                                         
BUYB0240 EQU   *                                                                
         USING RBUYMGEL,R3                                                      
*                                                                               
         CLC   =C'CR=',2(RF)       COMMENT INDICATES CREDIT?                    
         BE    BUYB0242            YES - CHECK CREDIT DATE IN 05                
         CLC   =C'CR>',2(RF)       NO  - COMMENT = CREDIT   MODIFIED?           
         BE    BUYB0242            YES - CHECK CREDIT DATE IN 05                
*                                  NO  - MAKEGOOD                               
         TM    MGBITFLG,X'40'      HAS ONE X'05' BEEN DROPPED                   
*                                     FOR THIS BUYLINE?                         
         BO    BUYB0245            YES - DROP THEM ALL                          
*                                                                               
BUYB0242 EQU   *                                                                
         CLC   RBUYMGD1,EARLYDTB   MAKEGOOD MISSED DATE < EFF DATE?             
*                                                                               
         DROP  R3                                                               
*                                                                               
         BNL   BUYB0280            NO  - LEAVE ELT ALONE                        
         OI    MGBITFLG,X'40'      INDICATE AT LEAST ONE X'05' ELT              
*                                     HAS BEEN DROPPED FOR BUY                  
BUYB0245 EQU   *                                                                
         MVI   0(R3),X'FF'         YES - MARK ELEMENT FOR DELETE                
         CLC   =C'MG=',2(RF)       COMMENT INDICATES MAKEGOOD?                  
         BE    BUYB0250            YES                                          
         CLC   =C'CR=',2(RF)       COMMENT INDICATES CREDIT?                    
         BE    BUYB0250            YES                                          
         CLC   =C'MG>',2(RF)       NO  - COMMENT = MAKEGOOD MODIFIED?           
         BE    BUYB0280            YES                                          
         CLC   =C'CR>',2(RF)       NO  - COMMENT = CREDIT   MODIFIED?           
         BE    BUYB0280            YES                                          
         DC    H'0'                NO  - WHAT IS HERE???                        
BUYB0250 EQU   *                                                                
         MVC   RBUYKMLN,RBUYKLIN   SET MASTER LIN# = MAIN LIN#                  
         MVI   4(RF),C'>'          SET MODIFIED MAKEGOOD/CREDIT FLAG            
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  RE,0                BACK UP 1 FOR EX                             
         EX    RE,BUYB0255         MOVE BY LENGTH                               
         MVI   0(RF),X'FF'         SET ELEMENT FOR DELETE                       
         LA    RE,1(RE)            RESET ORIGINAL LENGTH                        
         LA    R1,ELEM             SET A(NEW ELEMENT)                           
         AR    R1,RE               SET TO PAST LAST CHAR                        
         MVC   0(9,R1),=C'-TAKEOVER'                                            
         ZIC   RE,ELEM+1                                                        
         LA    RE,9(RE)            ADD 9 TO ELEMENT LENGTH                      
         STC   RE,ELEM+1           PUT NEW LENGTH BACK                          
         B     BUYB0258                                                         
BUYB0255 EQU   *                                                                
         MVC   ELEM(0),0(RF)       MOVE COMMENT BY LENGTH                       
BUYB0258 EQU   *                                                                
         B     BUYB0280            BUMP TO NEXT ELEMENT                         
BUYB0260 EQU   *                                                                
         USING RBMGMSEL,R3                                                      
*                                                                               
         TM    MGBITFLG,X'40'      HAS ONE X'05' BEEN DROPPED                   
*                                     FOR THIS BUYLINE?                         
         BO    BUYB0270            YES - DROP THEM ALL                          
*                                                                               
         CLC   RBMGMSDT,EARLYDTB   MAKEGOOD MISSED DATE < EFF DATE?             
*                                                                               
         DROP  R3                                                               
*                                                                               
         BNL   BUYB0280            NO  - LEAVE ELT ALONE                        
         OI    MGBITFLG,X'80'      INDICATE AT LEAST ONE X'56' ELT              
*                                     HAS BEEN DROPPED FOR BUY                  
BUYB0270 EQU   *                                                                
         MVI   0(R3),X'FF'         YES - MARK ELEMENT FOR DELETE                
BUYB0280 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     BUYB0220            GO BACK FOR NEXT ELEMENT                     
BUYB0300 EQU   *                                                                
         OC    ELEM,ELEM           ANY NEW ELEMENT TO ADD?                      
         BZ    BUYB0320            NO  - SKIP ADD                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELEM,0                 
*                                  INSERT NEW ELEMENT INTO RECORD               
BUYB0320 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RBUYREC),0,0            
*                                  DELETE EFF DATE ELTS PRIOR TO                
*                                     TKO DATE                                  
         SR    RE,RE                                                            
         L     RF,SVTOTSPT         LOAD TOTAL # SPOTS                           
         M     RE,SVTOTWKS         TOT SPOTS * # WEEKS =                        
         STCM  RF,15,RBUYTCOS         TOTAL COST OF BUY                         
         MVC   RBUYTSPT,SVTOTSPT   LOAD TOTAL NUMBER OF SPOTS                   
         MVC   RBUYTWKS,SVTOTWKS   LOAD TOTAL NUMBER OF WEEKS                   
*                                                                               
*   BUCKUP CALL                                                                 
         MVC   P+1(11),=C'BUCKUP CALL'                                          
         MVC   P+35(27),RBUYREC                                                 
         GOTO1 REPORT                                                           
*   BUCKUP CALL END                                                             
*                                                                               
         GOTO1 BUCKUP,DMCB,RBUYREC                                              
BUYB0360 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   CUT BACK START DATE VS HISTORICAL START DATE                                
CUTSTART NTR1                                                                   
         L     R3,0(R1)            RESET A(RBUY 03 ELT)                         
         USING RBUYDTEL,R3                                                      
*                                                                               
*   EARLY HIST DATE IS ALWAYS A MONDAY.  CHECK BUY START DATE.  IF NOT          
*        MONDAY, ADJUST EARLYDTB INSERT INTO BUY TO PROVIDE                     
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  ORIGINAL BUY START DATE -> EBCDIC            
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,1              ORIGINAL START = MONDAY?                     
         BE    CUTS0080            YES - USE TKO DATE AS IS                     
*                                  NO  - OFFSET TO ORIG BUY START DAY           
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  RE,0                SUBTRACT 1 DAY                               
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 DATCON,DMCB,(3,EARLYDTB),(0,WORK)                                
*                                  CONVERT TKO DATE TO EBCDIC                   
         GOTO1 ADDAY,DMCB,WORK,WORK,,                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTST)                                
*                                  CONVERT OOWR START TO BINARY                 
         CLC   RBUYDTED,RBUYDTST                                                
         BL    CUTS0160            END BEFORE START:  DROP BUYLINE              
*                                     OOWR IN SINGLE WEEK:  SETS                
*                                     START AFTER END DATE                      
         B     CUTS0100                                                         
CUTS0080 EQU   *                                                                
         MVC   RBUYDTST,EARLYDTB   RESET EFF START TO TKO DATE                  
CUTS0100 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  EBCDIC START DATE                            
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK+6)                              
*                                  EBCDIC END   DATE                            
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
*                                  CALCULATE DATE RANGE FIGURES                 
         ZICM  RF,DMCB+12,2        GET # WEEKS (DAYS/7)                         
         ZICM  RE,DMCB+10,2        CHECK REMAINDER DAYS/7                       
         LTR   RE,RE                                                            
         BZ    CUTS0120            NO REMAINDER                                 
         LA    RF,1(RF)            REMAINDER:  ADD 1 TO NUM WEEKS               
CUTS0120 EQU   *                                                                
         STC   RF,RBUYDTWK         RESET NUMBER OF WEEKS                        
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK ELEMENT?                      
         BNO   CUTS0140            NO                                           
         TM    RBUYDTWK,X'01'      ODD NUMBER OF WEEKS?                         
*                                     (LOW-ORDER BIT SET?)                      
         BO    CUTS0130            YES - JUST RECALC # WEEKS                    
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  CONVERT BUY START DATE TO EBCDIC             
         GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
*                                  BUMP TO NEXT WEEK                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTST)                                
*                                  CONVERT NEW START TO BINARY                  
         ZIC   RF,RBUYDTWK         DECREASE WEEKS BY 1                          
         BCTR  RF,0                                                             
         STC   RF,RBUYDTWK                                                      
CUTS0130 EQU   *                   RECALCULATE NUMBER OF WEEKS                  
         ZIC   RF,RBUYDTWK         EXTRACT NUMBER OF WEEKS                      
         LA    RF,1(RF)            MAKE NUMBER OF WEEKS EVEN                    
         SRL   RF,1                DIVIDE NUMBER OF WEEKS BY 2                  
         STC   RF,RBUYDTWK         REPLACE NUMBER OF WEEKS                      
CUTS0140 EQU   *                                                                
         ZIC   RE,RBUYDTWK         GET NUMBER OF WEEKS                          
         L     RF,SVTOTWKS         CALCULATE TOTAL NUMBER OF WEEKS              
         AR    RF,RE                                                            
         ST    RF,SVTOTWKS         SAVE TOTAL NUMBER OF WEEKS                   
         MVC   HALF,RBUYDTNW       GET SPOTS/WEEK                               
         MH    RE,HALF             NUM WKS * SPTS/WK = TOTAL SPOTS              
         L     RF,SVTOTSPT         CALCULATE TOTAL NUMBER SPOTS                 
         AR    RF,RE                                                            
         ST    RF,SVTOTSPT         SAVE TOTAL NUMBER SPOTS                      
         B     CUTS0180                                                         
*                                                                               
         DROP  R3                                                               
CUTS0160 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CUTS0200                                                         
CUTS0180 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CUTS0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*   CUT BACK END DATE VS HISTORICAL END DATE                                    
CUTEND   NTR1                                                                   
         L     R3,0(R1)            RESET A(RBUY 03 ELT)                         
         USING RBUYDTEL,R3                                                      
*                                                                               
*   LATE  HIST DATE IS ALWAYS A SUNDAY.  CHECK BUY END DATE.  IF NOT            
*        SUNDAY, ADJUST LATEDTB INSERT INTO BUY TO PROVIDE                      
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK)                                
*                                  ORIGINAL BUY END DATE -> EBCDIC              
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,7              ORIGINAL END   = SUNDAY?                     
         BE    CUTE0080            YES - USE HIST END DATE AS IS                
*                                  NO  - OFFSET TO ORIG BUY END DAY             
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         LA    RF,7                CALCULATE A NEGATIVE VALUE                   
         SR    RE,RF                                                            
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE (NEGATIVE)            
         GOTO1 DATCON,DMCB,(3,LATEDTB),(0,WORK)                                 
*                                  CONVERT HIST END DATE TO EBCDIC              
         GOTO1 ADDAY,DMCB,WORK,WORK,,                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTED)                                
*                                  CONVERT OOWR END   TO BINARY                 
         CLC   RBUYDTED,RBUYDTST                                                
         BL    CUTE0160            END BEFORE START:  DROP BUYLINE              
*                                     OOWR IN SINGLE WEEK:  SETS                
*                                     START AFTER END DATE                      
         B     CUTE0100                                                         
CUTE0080 EQU   *                                                                
         MVC   RBUYDTED,LATEDTB    RESET EFF END TO HIST END DATE               
CUTE0100 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  EBCDIC START DATE                            
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK+6)                              
*                                  EBCDIC END   DATE                            
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
*                                  CALCULATE DATE RANGE FIGURES                 
         ZICM  RF,DMCB+12,2        GET # WEEKS (DAYS/7)                         
         ZICM  RE,DMCB+10,2        CHECK REMAINDER DAYS/7                       
         LTR   RE,RE                                                            
         BZ    CUTE0120            NO REMAINDER                                 
         LA    RF,1(RF)            REMAINDER:  ADD 1 TO NUM WEEKS               
CUTE0120 EQU   *                                                                
         STC   RF,RBUYDTWK         RESET NUMBER OF WEEKS                        
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK ELEMENT?                      
         BNO   CUTE0140            NO                                           
         TM    RBUYDTWK,X'01'      ODD NUMBER OF WEEKS?                         
*                                     (LOW-ORDER BIT SET?)                      
         BO    CUTE0130            YES - JUST RECALC # WEEKS                    
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK)                                
*                                  CONVERT BUY END   DATE TO EBCDIC             
         LA    RF,7                SET NEGATIVE 7                               
         LNR   RF,RF                                                            
         ST    RF,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK,,                                           
*                                  BUMP TO PREVIOUS WEEK                        
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTED)                                
*                                  CONVERT NEW END   TO BINARY                  
         ZIC   RF,RBUYDTWK         DECREASE WEEKS BY 1                          
         BCTR  RF,0                                                             
         STC   RF,RBUYDTWK                                                      
CUTE0130 EQU   *                   RECALCULATE NUMBER OF WEEKS                  
         ZIC   RF,RBUYDTWK         EXTRACT NUMBER OF WEEKS                      
         LA    RF,1(RF)            MAKE NUMBER OF WEEKS EVEN                    
         SRL   RF,1                DIVIDE NUMBER OF WEEKS BY 2                  
         STC   RF,RBUYDTWK         REPLACE NUMBER OF WEEKS                      
CUTE0140 EQU   *                                                                
         ZIC   RE,RBUYDTWK         GET NUMBER OF WEEKS                          
         L     RF,SVTOTWKS         CALCULATE TOTAL NUMBER OF WEEKS              
         AR    RF,RE                                                            
         ST    RF,SVTOTWKS         SAVE TOTAL NUMBER OF WEEKS                   
         MVC   HALF,RBUYDTNW       GET SPOTS/WEEK                               
         MH    RE,HALF             NUM WKS * SPTS/WK = TOTAL SPOTS              
         L     RF,SVTOTSPT         CALCULATE TOTAL NUMBER SPOTS                 
         AR    RF,RE                                                            
         ST    RF,SVTOTSPT         SAVE TOTAL NUMBER SPOTS                      
         B     CUTE0180                                                         
*                                                                               
         DROP  R3                                                               
CUTE0160 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CUTE0200                                                         
CUTE0180 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CUTE0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO ADD REC TO CONTRACT   P1=A(BUYREC OR PLNREC)                       
*                                     IF BYTE 0=X'FF'-SUBTRACTION               
BUCKUP   NTR1                                                                   
         L     R4,AIO2                                                          
         USING RCONREC,R4                                                       
*                                                                               
         L     R2,0(R1)            A(BUYREC)                                    
         L     R0,=V(RECUP)                                                     
         GOTOX (RFBUCKUP,VREPFACS),DMCB,(R2),(BUCKFLGS,RCONREC),       +        
               ACOMFACS,GETBROAD,(R0)                                           
         BNE   BUUP0100                                                         
         XIT1                                                                   
*                                                                               
BUUP0100 EQU   *                                                                
         DC    H'0',C'$ABEND'                                                   
*                                  CAN'T UPDATE !!                              
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE REPFACSQ                                                       
         LTORG                                                                  
         EJECT                                                                  
*INSRT*>                                                                        
***>BUYBACK INSERT HERE                                                         
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
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'197REREPBACI 05/01/02'                                      
         END                                                                    
