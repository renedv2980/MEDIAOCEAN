*          DATA SET NEWRI82    AT LEVEL 012 AS OF 04/24/17                      
*PHASE T32082A                                                                  
*INCLUDE MOBILE                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE MSPACKS                                                                
*INCLUDE SPBVAL                                                                 
*INCLUDE SPFMTINO                                                               
*        TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELEMENTS'             
*******************************************************************             
*          ***    CALLED FROM NEWRI20 FOR NET WRITER  ***                       
*                                                                               
********** THIS IS LIVE VERSION FOR 3  CHARACTER PRODUCT CODE ********          
********** NEWRI82N IN MY LIBRARY FOR REFERENCE **********************          
*                                                                               
*                                                                               
*                                                                               
* - 1 - READS UNIT BILLING ELEMENTS AND PASS TO DRIVER                          
*   2 - READS BILLING HEADER RECORDS                                            
*                                                                               
*                                                                               
* - RDBELEM READS THROUGH THE UNIT BILLING ELEMENTS AND GETS                    
* - BILLING RECORD THAT MATCHES. THEN IT GOES TO DRIVER.                        
*                                                                               
* - THUS IT GOES TO DRIVER MULTIPLE TIMES FOR A SINGLE UNIT.                    
*                                                                               
* - NBRDBELS = 2 AFTER 1ST DRIVER CALL TO TURN OFF ACCGEN ROUTINES              
* - IN NEWRIDRIVE SO ADDITIVE COLUMNS (E.G. ACTUAL COST) ARE                    
* - ONLY PASSED ONCE FOR EACH UNIT.                                             
*                                                                               
*                                                                               
* - NOTE R3 RESERVED FOR BILLING BLOCK DSECT                                    
* - NOTE R6 RESERVED FOR GLOBALD                                                
**********************************************************************          
T32082   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKLEN,T32082**,RA,R8,CLEAR=YES                                  
         LR    R7,RC                                                            
         USING WRKAREA,R7                                                       
         L     RC,0(R1)            *CALLING PROGRAMS RC                         
         USING GEND,RC                                                          
         L     RE,8(R1)            *CALLING PROGRAMS BHBLOCK AREA               
         ST    RE,AMYIO                                                         
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,NDGLOBAL                                                      
         USING GLOBALD,R6                                                       
*                                                                               
         MVC   ACSHIERC,16(R1)     A(CASHIER CONTROL BLOCK)                     
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AA5' GET DDCASHIER ADDRESS                     
         GOTO1 NBCALLOV,DMCB                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VCASHIER,DMCB       SAVE ADDRESS                                 
*                                                                               
* - SET UP I/O AREAS                                                            
*                                                                               
         MVC   SVAIO,AIO           SAVE CURRENT A(I/O AREA)                     
*                                                                               
         L     R3,AMYIO            PASS A(MYIO)                                 
         ST    R3,AIO                                                           
*                                                                               
**       A     R3,=F'4000'         SET UP A(BHBLOCK)                            
         A     R3,=F'6000'         SET UP A(BHBLOCK)                            
         ST    R3,NDCIDTBL         PASS ADDR TO DRIVER                          
*                                                                               
         USING BHBLOCK,R3                                                       
*                                                                               
         LA    RF,HOOK             SET IN NEWRI20 AND RESET HERE SINCE          
         ST    RF,GLAHOOK          DRIVER IS CALLED FROM BOTH MODULES           
         L     RF,12(R1)           * REQUESTED ROUTINE NUMBER                   
         SLL   RF,2                                                             
         B     BRANCHTB(RF)                                                     
*                                                                               
         MVI   SVRDBELS,0          INIT NBRDDELS SAVEAREA                       
*                                                                               
BRANCHTB DS    0H                                                               
         B     RDBHEAD             0=READ BILLING HEADERS                       
         B     RDBELEM             1=READ UNIT BILLING ELEMENTS                 
*                                                                               
EXIT     DS    0H                                                               
         MVC   AIO,SVAIO           RESTORE AIO                                  
         XIT1                                                                   
*                                                                               
         TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELMS-RDBELEM'         
***********************************************************************         
*                                                                     *         
*        READ BILL ELEMENTS                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDBELEM  DS    0H                                                               
*                                                                               
         MVI   NBRDBELS,0          CLEAR FLAG                                   
*                                                                               
         BRAS  RE,GTWRPRF         FIND WR PROFILE                               
*                                                                               
         TM    NDRDBCEL,X'01'      MUST WE READ BILL ELEMS                      
         BNO   EXIT                                                             
*                                                                               
         CLI   NBSPLTYP,C'S'     IF SECOND PASS FROM NETVALUE ON UNIT           
         BNE   *+14                                                             
         CLC   NBPRD,NBPRD2        AND PIGGYS ARE =                             
         BE    EXIT                WE ARE DONE                                  
*                                                                               
         BAS   RE,GETBFRML         SET BILLING FORMULA                          
*                                                                               
         L     R4,NBAIO            GET FIRST BILLING ELEMENT                    
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         ST    R4,ABELEM           SAVE FOUND ADDRESS                           
         BNE   RDB04               NONE FOUND                                   
*                                                                               
RDB01    DS    0H                                                               
*                                                                               
         USING NUBILD,R4           ESTABLISH BILLING ELEMENT                    
*                                                                               
         CLI   WRPROF+7,C'Y'       IF DROPPING REVERSED/REVERSAL BILLS          
         BNE   RDB03                                                            
*                                                                               
         TM    NUBILST,NUBILRDQ+NUBILRLQ IF NOT REVERSED/REVERSAL BILL          
         BZ    RDB03                  USE BILL                                  
*                                                                               
RDB02    DS    0H                  LOOK FO NEXT BILL                            
*                                                                               
         BAS   RE,NEXTEL           NEX BILL ELEMENT                             
         ST    R4,ABELEM           SAVE FOUND ADDRESS                           
         BNE   RDB04               NONE FOUND                                   
         B     RDB01                                                            
*                                                                               
RDB03    DS    0H                                                               
*                                                                               
         B     RDB05               YES/GOT IT                                   
*                                                                               
RDB04    DS    0H                                                               
*                                                                               
         TM    NDRDBCEL,X'04'      NO BILLING ELEM/NEED VENDOR?                 
         BZ    EXIT                NO                                           
*                                                                               
         MVI   NBRDBELS,1          YES                                          
         MVI   SVRDBELS,1          YES                                          
*                                                                               
         BAS   RE,GETVNDNM         GET VENODR NAME                              
*                                                                               
         MVI   SVBGRS,X'FF'                                                     
*                                                                               
         B     GBHCSHGO            GO TO DRIVER                                 
*                                                                               
RDB05    DS    0H                                                               
*                                                                               
         USING NUBILD,R4           ESTABLISH BILLING ELEMENT                    
*                                                                               
         BAS   RE,BHFILTRS         SEEKS/SETS BH FILTERS                        
*                                                                               
* - MOBILE SETS UP MONTH OF SERVICE TABLE                                       
*                                                                               
         BAS   RE,GOMOBILE                                                      
*                                                                               
         B     RDB15                                                            
*                                                                               
* - GETS NEXT ELEMENT                                                           
*                                                                               
RDB10    L     R4,ABELEM           GET ADDRESS OF NEXT BILLING ELEM             
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GBBHDDN                                                          
*                                                                               
         USING NUBILD,R4           ESTABLISH BILLING ELEMENT                    
*                                                                               
         ST    R4,ABELEM                                                        
*                                                                               
RDB15    DS    0H                                                               
*                                                                               
         TM    NUBILST,X'20'       IF UNBILLED                                  
         BO    RDB10               SKIP                                         
*                                                                               
         CLI   WRPROF+7,C'Y'       IF DROPPING REVERSED/REVERSAL BILLS          
         BNE   RDB15A                                                           
*                                                                               
         TM    NUBILST,NUBILRDQ+NUBILRLQ IF REVERSED OR REVERSAL BILL           
         BNZ   RDB10                  DROP BILL                                 
*                                                                               
RDB15A   DS    0H                                                               
*                                                                               
* - MATCH BILLING ELEM PROD WITH UNIT PROD                                      
*                                                                               
                                                                                
* - INVOICE FILTER                                                              
                                                                                
         ICM   R2,15,NBINVFLT                                                   
         BZ    INVFLTX                                                          
         OC    0(12,R2),0(R2)                                                   
         BZ    INVFLTX                                                          
                                                                                
         GOTO1 DATCON,DMCB,(2,NUBILDAT),DUB                                     
         MVC   DUB(2),DUB+2            GET MM                                   
         MVC   DUB+2(4),NUBILNUM                                                
                                                                                
         CLI   0(R2),C'*'          START INV NUMBER                             
         BE    *+14                                                             
         CLC   DUB(6),0(R2)                                                     
         BL    RDB10               NO- LOWER                                    
         CLI   6(R2),C'*'          END INV NUMBER                               
         BE    INVFLTX                                                          
         CLC   DUB(6),6(R2)                                                     
         BH    RDB10               NO- HIGHER                                   
INVFLTX  EQU   *                                                                
                                                                                
*                                                                               
         CLI   NUBILLEN,29         3 CHAR PROD ELEM?                            
         BL    RDB16                                                            
         CLI   NUBILPRC,0          3 CHAR PROD PRESENT?                         
         BE    RDB16               NO                                           
         CLC   NUBILPRC,NBSPLPR3   YES-USE THAT                                 
         BE    RDB17                                                            
         B     RDB10                                                            
*                                                                               
RDB16    CLC   NUBILPRD,NBSPLPRN                                                
         BE    RDB17               YES                                          
         B     RDB10                                                            
*                                                                               
RDB17    DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,RUNDATE)                             
*                                                                               
         MVC   INVNUMBR(2),RUNDMM               MONTH (2)                       
         MVC   INVNUMBR+2(4),NUBILNUM           INVNO (4)                       
*                                                                               
         MVC   WORKINV,INVNUMBR         ..GETBYMN RETURNS                       
         MVC   WORKYMD,RUNDATE                                                  
*                                                                               
         BAS   RE,GETBYMN                                                       
*                                                                               
         MVC   CURRBINV,WORK            ..3 BYTE(BINARY Y/M + INV NO)           
*                                                                               
         GOTO1 =V(SPFMTINO),DMCB,0,(C'P',NUBILNUM)                              
         L     RE,4(R1)                                                         
         MVC   CURRBINV+1(2),0(RE)                                              
*                                                                               
         BAS   RE,GETMOS           ..GET MO OF SERVICE OF NBACTDAT              
*                                  .. RETURNED IN BYMOS YR/MOS                  
*                                                                               
         EJECT                                                                  
*                                                                               
* - MATCH BILLING ELEMENT TO BILLING RECORD                                     
*                                                                               
         MVI   BYTE,0                                                           
*                                                                               
*        BUILD STARTING BILL RECORD KEY                                         
*                                                                               
         LA    R4,KEY                                                           
         USING BILLREC,R4                                                       
         XC    BKEY,BKEY           BUILD BILL REC KEY                           
*                                                                               
         MVC   BKEYAM,NBACTAM      FROM UNIT BILLING ELEM                       
         MVC   BKEYCLT,NBACTCLI                                                 
*                                                                               
         L     RF,ABELEM           POINT TO BILL ELEMENT                        
         USING NUBILD,RF                                                        
         CLI   NUBILLEN,29         NEW BILL ELEM?                               
         BL    RDB19                                                            
         CLI   NUBILPRC,0          3 CHAR ALPHA?                                
         BE    RDB19                                                            
         MVC   WORK,NUBILPRC       SET 3 CHAR PROD                              
         B     RDB19B                                                           
         DROP  RF                                                               
RDB19    MVC   WORK(1),NUBILPRD-NUBILD(RF)                                      
         BAS   RE,CHKCLT1                                                       
         BAS   RE,GETPRD3                                                       
RDB19B   MVC   BKEYPRD,WORK                                                     
*                                                                               
*                                                                               
         MVC   BKEYEST,NBACTEST                                                 
         MVC   BKEYYSRV,BYMOS              BYMOS SET FROM NBACTDAT              
         MVC   BKEYMSRV,BYMOS+1                                                 
         MVC   BKEYMBIL(3),CURRBINV        M/Y + 2 BYTE INV NO                  
         MVC   BKEYMBIL(3),CURRBINV        M/Y + 2 BYTE INV NO                  
*                                                                               
         XC    SVBHDELA,SVBHDELA   INIT CASHIER BUFFER ADDRESS                  
*                                                                               
         MVC   SVBKEY,BKEY         SAVE BILL HEADER KEY                         
*                                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
*        RETRIEVE BILL HEADER FROM TSAROFF BUFFER                               
*                                                                               
         L     R2,ACSHIERC         POINT TO CASHIER CONTROL BLOCK               
         USING CSHIERD,R2          ESTABLISH AREA                               
*                                                                               
         MVC   CSHAGYCH,NBSELAGY   SET AGENCY ALPHA                             
         MVC   CSHMED,NBSELMED     SET MEDIA ALPHA                              
         MVC   CSHPOSTP,NBPOSTYP   SET POSTING TYPE                             
*                                                                               
         MVI   CSHACT,CSHRDHQ      SET ACTION TO READ HIGH                      
*                                                                               
         ST    R4,CSHBLLA          SET A(BILLKEY)                               
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    READ BILL RECORD                        
*                                                                               
         ICM   RF,15,CSHRECA       POINT TO FOUND RECORD                        
         BZ    *+14                NONE FOUND                                   
         CLC   BKEY,0(RF)          CHECK IF RECORD FOUND                        
         BE    RDBHCSHF                                                         
*                                                                               
*        RETRIEVE BILL HEADER RECORD                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
*                                                                               
         CLC   BKEY,KEYSAVE        CHECK IF RECORD FOUND                        
         BE    RDBHRECF                                                         
*                                                                               
* - CAN NOT MATCH TO BILLING RECORD                                             
* - BUMP MOS DOWN BY ONE                                                        
*                                                                               
**************************************************************                  
*        IF MEDIAVEST - THEY CHANGED THEIR PROFILE                              
*        IF 680C IS MONTH OF SERVICE, TRY 680D                                  
*        TEMPORARY HARDCODE TILL MOBILE CHANGED 12/10/04                        
         CLC   NBSELAGY,=C'DU'                                                  
         BNE   NOTDU                                                            
**       CLC   BYMOS,=X'680C'                                                   
**       BNE   NOTDU                                                            
         CLI   BYMOS+1,X'0C'    MORE THAN JUST 680C                             
         BNE   NOTDU                                                            
         MVC   BKEY,KEYSAVE                                                     
**       MVI   BKEYYSRV,X'68'                                                   
         MVC   BKEYYSRV,BYMOS      SET YEAR                                     
         MVI   BKEYMSRV,X'0D'      SET MONTH                                    
         MVC   WORK(13),KEY        USE WORK AS KEYSAVE                          
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    SEARCH BUFFER FOR BILL RECORD           
         ICM   RF,15,CSHRECA       POINT TO FOUND RECORD                        
         BZ    *+14                NONE FOUND                                   
         CLC   BKEY,0(RF)          CHECK IF RECORD FOUND                        
         BE    RDBHCSHF                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
*                                                                               
         CLC   BKEY,WORK           CHECK IF RECORD FOUND                        
         BE    RDBHRECF                                                         
*                                  NO REASON TO RESTORE                         
*                                  BYMOS NOT TOUCHED (?)                        
***      MVC   BYMOS,=X'680C'      RESTORE MOS-DO NORMAL PROCESSING             
NOTDU    EQU   *                                                                
*****************************************************************               
         CLI   BYMOS+1,13          IF 13TH MMONTH                               
         BE    *+12                                                             
         CLI   BYMOS+1,14          OR 14TH                                      
         BNE   RDBHN08                                                          
         MVI   BYMOS+1,1           MAKE IT JANUARY                              
         ZIC   R1,BYMOS                                                         
         LA    R1,1(R1)            BUMP YEAR                                    
         STC   R1,BYMOS                                                         
         MVI   BYTE,13                                                          
         B     RDBHUPB                                                          
*                                                                               
RDBHN08  MVI   BYTE,C'D'           SET DOWN                                     
         ZIC   R1,BYMOS+1    LET'S TRY DOWN 1 MONTH                             
         BCTR  R1,0          DECREASE MOS                                       
         LTR   R1,R1         IF IT WAS JANUARY                                  
         BZ    RDBHN10                                                          
         STC   R1,BYMOS+1                                                       
         B     RDBHUPB                                                          
*                                                                               
*RDBHN10  LA    R1,12(R1)     MAKE IT DECEMBER                                  
RDBHN10  LA    R1,13(R1)    *MAKE IT 13TH MONTH/AND IF IT DOESN'T FIND          
*                           *WILL COME AROUND AGAIN WITH 12                     
RDBHN11  ZIC   RE,BYMOS      AND DECREASE YEAR                                  
         BCTR  RE,0                                                             
         STC   RE,BYMOS                                                         
         STC   R1,BYMOS+1                                                       
         B     RDBHUPB       AND LET'S FIND IT                                  
*                                                                               
RDBHBMUP DS    0H                  BUMP UP                                      
         ZIC   R1,BYMOS+1          RETRIEVE CURREMT MONTH                       
         LA    R1,2(R1)            1 FOR BUMPED DOWN + 1                        
         STC   R1,BYMOS+1                                                       
*                                                                               
         CHI   R1,12               ..IS IT PAST DECEMBER                        
         BNH   RDBHUPB             ..NO LET'S GET IT                            
*                                                                               
         MVI   BYMOS+1,1           ..YES SET MONTH TO JAN                       
         CHI   R1,13                                                            
         BE    *+8                                                              
         MVI   BYMOS+1,2           ..OR FEBRUARY                                
         ZIC   R1,BYMOS            ..AND BUMP YEAR                              
         LA    R1,1(R1)                                                         
         STC   R1,BYMOS                                                         
         B     RDBHUPB                                                          
*                                                                               
RDBHUPLP DS    0H                                                               
RDBHUPA  STC   R1,BYMOS+1                                                       
*                                                                               
RDBHUPB  XC    KEY,KEY                                                          
*                                                                               
         MVC   BKEY,KEYSAVE        RESTORE LAST USED KEY                        
         MVC   BKEYYSRV(2),BYMOS   SET NEW MONTH OF SERVICE                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    SEARCH BUFFER FOR BILL RECORD           
*                                                                               
         ICM   RF,15,CSHRECA       POINT TO FOUND RECORD                        
         BZ    *+14                NONE FOUND                                   
         CLC   BKEY,0(RF)          CHECK IF RECORD FOUND                        
         BE    RDBHCSHF                                                         
*                                  READ FOR BILL HEADER RECORD                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
*                                                                               
         CLC   BKEY,KEYSAVE                                                     
         BE    RDBHRECF            RECORD FOUND                                 
         CLI   BYTE,13             13TH MONTH?                                  
         BE    RDBHN08             YES                                          
         CLI   BYTE,C'D'           BUMPED DOWN?                                 
         BNE   RDBHDNDN            YES                                          
         MVI   BYTE,C'U'           NO-BUMP UP                                   
         B     RDBHBMUP            AND TRY AGAIN                                
*                                                                               
*                                                                               
*                                                                               
RDBHDNDN DS    0H                                                               
*                                                                               
* - TRY UNITS NBACTDAT AS MOS WITHOUT GOING TO MOBILE                           
* - SEE IF THIS WILL FIX                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(3,MYWORK)                              
         MVC   BYMOS(2),MYWORK                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEYSAVE                                                  
         MVC   BKEYYSRV(2),BYMOS                                                
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    SEARCH BUFFER FOR BILL RECORD           
*                                                                               
         ICM   RF,15,CSHRECA       POINT TO FOUND RECORD                        
         BZ    *+14                NONE FOUND                                   
         CLC   BKEY,0(RF)          CHECK IF RECORD FOUND                        
         BE    RDBHCSHF                                                         
*                                  READ FOR BILL HEADER RECORD                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
*                                                                               
         CLC   BKEY,KEYSAVE                                                     
         BE    RDBHRECF            RECORD FOUND                                 
*                                                                               
***      DC    H'0'                CAN NOT MATCH BILL ELEM TO BIL REC           
* LET'S LOOPBACK 13 MONTHS FOR THAT SPECIAL CLOROX CALENDAR                     
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(3,MYWORK)                              
         MVC   BYMOS(2),MYWORK     ORIGINAL STATING MOS                         
         LA    R5,15               LOOP COUNTER - 13 MONTHS MAX                 
RDBHDNLP DS    0H                                                               
         ZIC   R1,BYMOS+1                                                       
         BCTR  R1,0                                                             
         STC   R1,BYMOS+1                                                       
         C     R1,=F'0'            ..WERE WE IN JANUARY                         
         BNE   RDBHDNB                                                          
         ZIC   R1,BYMOS            ..YES DECREASE YEAR                          
         BCTR  R1,0                                                             
         STC   R1,BYMOS                                                         
         MVI   BYMOS+1,12          ..AND SET MONTH TO DECEMBER                  
*                                                                               
RDBHDNB  XC    KEY,KEY                                                          
         MVC   BKEY,KEYSAVE        RESTORE LAST USED KEY                        
         MVC   BKEYYSRV(2),BYMOS   SET NEW MONTH OF SERVICE                     
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VCASHIER,DMCB,CSHIERD    SEARCH BUFFER FOR BILL RECORD           
         ICM   RF,15,CSHRECA       POINT TO FOUND RECORD                        
         BZ    *+14                NONE FOUND                                   
         CLC   BKEY,0(RF)          CHECK IF RECORD FOUND                        
         BE    RDBHCSHF                                                         
*                                  READ FOR BILL HEADER RECORD                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
*                                                                               
         CLC   BKEY,KEYSAVE                                                     
         BE    RDBHRECF            RECORD FOUND                                 
         BCT   R5,RDBHDNLP            TRY AGAIN                                 
         DC    H'0'                13 DOESN'T WORK EITHER                       
*****************************************************************               
RDBHRECF DS    0H                  BILL HEADER RECORD FOUND                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',KEY+14,AMYIO,MYDM           
*                                                                               
*        ADD BILL HEADER TO BUFFER                                              
*                                                                               
         MVI   CSHACT,CSHADDQ      SET ACTION TO ADD                            
*                                                                               
         MVC   CSHBLLA,AMYIO       SET A(BILL RECORD)                           
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    ADD BILL RECORD TO BUFFER               
*                                                                               
         CLI   CSHERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ENTRY IN CASHIER BUFFER FOUND                                          
*                                                                               
RDBHCSHF DS    0H                                                               
*                                                                               
*        SAVE BUFFER ENTRY                                                      
*                                                                               
         L     R0,CSHRECA          POINT TO CASHIER RECORD                      
         LA    RE,SVCSHREC         POINT TO SAVEAREA                            
         LHI   RF,L'SVCSHREC       SAVEAREA LENGTH                              
         LR    R1,RF               COPY LENGTH                                  
*                                                                               
         MVCL  RE,R0               SAVE CASHIER RECORD                          
*                                                                               
         LA    R4,SVCSHREC         POINT TO SAVED BILL HEADER RECORD            
         ST    R4,SVBHDELA         SAVE BILL RECORD ADDRESS                     
*                                  ALSO NOW HAS CASH APPLIED DATA               
*                                                                               
         ST    R4,AIO              EVERYONE THINKS BILLREC IS HERE              
*                                                                               
         L     RF,ABELEM           POINT TO UNIT BILL ELEMENT                   
         MVC   BHUBTYPE,NUBILTYP-NUBILD(RF)  SET BILL TYP FROM UNIT ELM         
*                                       FOR CASH FLOW REPORTING                 
         CLI   NBRDBELS,0          SET FLAG                                     
         BNE   *+8                                                              
         MVI   NBRDBELS,1                                                       
*                                                                               
         BAS   RE,TSTBHFLT         TEST BILL REC AGAINST BH FILTERS             
         BNE   RDB10             NO-GET NEXT BILLING ELEMENT                    
*                                                                               
         B     GB30              OK-ENTER BILLING HEADER READ ROUTINE           
*                                   TO FILL IN BHBLOCK                          
         TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELMS-RDBHEAD'         
***********************************************************************         
*                                                                     *         
* - THIS ROUTINE READS BILLING HEADER RECORDS IN BH FLAVOR            *         
* - ALSO USED BY RDBELEM TO BUILD BHBLOCK  (ENTRY AT GB30)            *         
*                                                                     *         
*                         I/O AREA = MYIO                             *         
*                         BHBLOCK  = MYIO+6000 !!!                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDBHEAD  DS    0H                                                               
*                                                                               
         L     R1,ANETWS1          SAVE CLIENT REC                              
         MVC   CLTRECSV,0(R1)      AND RESTORE AT EXIT                          
*                                                                               
         MVC   ESTMSK,NBESTMSK                                                  
         MVI   NBRDBELS,0                                                       
*                                                                               
* - IS THERE BH FILTER IN OPTIONS                                               
*                                                                               
         BAS   RE,BHFILTRS                                                      
*                                                                               
GB00     LA    R2,1                INIT LENGTH TO COMPARE KEYS ON               
*                                                                               
         LA    R4,KEY              ESTABLISH BILL RECORD KEY                    
         USING BKEY,R4                                                          
         XC    KEY,KEY                                                          
*                                                                               
         MVC   BKEYAM,BHAGYMD                                                   
*                                                                               
         CLI   BHSELCLI,0                                                       
         BE    GB12                                                             
*                                                                               
         MVC   BKEYCLT,BHSELCLI                                                 
         LA    R2,1(R2)                                                         
*                                                                               
GB5      CLI   BHSELPRD,0                                                       
         BE    GB12                                                             
*                                                                               
         LA    R2,2(R2)                                                         
         MVC   BKEYPRD,BHSELPRD                                                 
*                                                                               
GB10     MVC   BKEYEST,BHSELEST    SET ESTIMATE                                 
*                                                                               
         CLI   BHSELEST,0                                                       
         BE    GB12                                                             
*                                                                               
         CLI   BHSELESE,0          IF RANGE                                     
         BE    *+14                                                             
         XC    BKEYEST,BKEYEST     CLEAR AND LET ESTMASK FILTER IT              
         B     GB12                                                             
*                                                                               
         LA    R2,1(R2)                                                         
*                                                                               
GB12     DS    0H                                                               
*                                                                               
         STC   R2,COMPLEN          SAVE KEY LENGTH COMPARE                      
*                                                                               
GB16     NETGO NVSETSPT                                                         
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
GBBHDLP  DS    0H                                                               
*                                                                               
         ZIC   R2,COMPLEN          GET LENGTH OF KEY COMPARE                    
         EXCLC R2,KEY,KEYSAVE      IF MAJOR PART OF KEY CHANGES                 
         BE    *+12                                                             
         BAS   RE,RESTCLT             RESTORE CLIENT REC TO ANETWS1             
         B     GBBHDDN                ALL DONE                                  
*                                                                               
         CLI   KEY+8,0             MAKE SURE ITS BILL REC                       
         BE    GBBHDCN                                                          
*                                                                               
         BAS   RE,FILTBH           CHECK IF IT PASES FILTERS                    
         BNE   GBBHDCN                                                          
*                                                                               
* - CLIENT GROUP FILTERS                                                        
*                                                                               
**************************************                                          
* WITH FIRST CLIENT AND NO CLIENT GROUP FILTER                                  
* GO TO SETPRD ROUTINE TO CHECK FOR PRODGROUP FILTERS                           
         CLI   NBSELCGR,0          IF THERE IS CLIENT GROUP FILTER              
         BNE   SKIPTHSX            SKIP THIS AND CATCH LATER                    
         CLI   FIRST,C'N'                                                       
         BE    SKIPTHSX                                                         
         MVI   FIRST,C'N'                                                       
         MVC   MYKEY,KEY           SAVE BILL KEY                                
         BAS   RE,SETPRD                                                        
         XC    KEY,KEY             RESET BILL KEY                               
         MVC   KEY,MYKEY                                                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   AIO,AMYIO           RESET I/O AREA                               
SKIPTHSX DS    0H                                                               
**************************************                                          
         CLI   NBSELCGR,0                                                       
         BE    *+12                                                             
         BAS   RE,CGRPFILT                                                      
         BNE   GBBHDCN                                                          
*                                                                               
* - CLIENT OFFICE FILTERS                                                       
*                                                                               
* HAVE PROGRAM GO THROUGH COFFFILT EVEN IF NBSELOFF=0 - WILL SET                
* VARIOUS VALUES IN CASE CLIENT HAS CHANGED                                     
*                                                                               
         BAS   RE,COFFFILT                                                      
         BNE   GBBHDCN                                                          
*                                                                               
* - PRODUCT GROUP FILTERS                                                       
*                                                                               
         CLI   NBSELPGR,0          SKIP IF NO PRODUCT GROUP SPECIFIED           
         BE    GB27                                                             
*                                                                               
*****    BAS   RE,CHKCLT           GET CLIENT RECORD                            
*                                                                               
*****    MVC   WORK+1(3),BHACTPRD  SAVE BHACTPRD                                
*****    MVC   BHACTPRD,BKEYPRD    GETPRD1 USES BHACTPRD                        
*                                                                               
*****    BAS   RE,GETPRD1          RETURNS 1 BYTE PRODUCT IN WORK               
*                                                                               
*****    MVC   BHACTPRD,WORK+1     RESET BHACTPRD                               
*                                                                               
***      ZIC   R1,WORK             GET 1 BYTE PRODUCT CODE                      
***      BAS   RE,TESTMASK         TEST AGAINST PRODUCT FILTERS                 
*******  MVC   WORK(3),BHACTPRD    PASS 3 CHAR CODE                             
         MVC   WORK(3),BKEYPRD     PASS 3 CHAR CODE                             
         BAS   RE,TSTOPMSK         TEST AGAINST PRODUCT FILTERS                 
***      BE    GBBHDCN             REJECT PRODUCT - IS THIS RIGHT ???           
         BNE   GBBHDCN             HELL NO -                                    
*                                                                               
* - ESTIMATE FILTERS                                                            
*                                                                               
GB27     ZIC   R1,BKEYEST          FILTER ESTIMATES                             
         LA    R2,ESTMSK                                                        
         BAS   RE,TESTMASK                                                      
         BE    GBBHDCN          REJECT ESTIMATE - AND HERE ALSO ???             
*                                                                               
*        BILL HEADER PASSES ALL FILTERS - I.E. WE WANT IT                       
*                                                                               
*        CHECK CASHIER TO SEE IF BILL HEADER RECORD READ PREVIOUSLY             
*                                                                               
         XC    SVBHDELA,SVBHDELA   INIT CASHIER BUFFER ADDRESS                  
*                                                                               
         MVC   SVBKEY,BKEY         SAVE BILL HEADER KEY                         
*                                                                               
*        RETRIEVE BILL HEADER FROM TSAROFF BUFFER                               
*                                                                               
         L     R2,ACSHIERC         POINT TO CASHIER CONTROL BLOCK               
         USING CSHIERD,R2          ESTABLISH AREA                               
*                                                                               
         MVC   CSHAGYCH,NBSELAGY   SET AGENCY ALPHA                             
         MVC   CSHMED,NBSELMED     SET MEDIA                                    
*                                                                               
         MVI   CSHACT,CSHRDHQ      SET ACTION TO READ HIGH                      
*                                                                               
         ST    R4,CSHBLLA          SET A(BILLKEY)                               
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    READ BILL RECORD                        
*                                                                               
         ICM   RF,15,CSHRECA       POINT TO FOUND RECORD                        
         BZ    *+14                   NONE FOUND                                
         CLC   BKEY,0(RF)          SKIP IF RECORD FOUND                         
         BE    GBCSHF                 YES                                       
*                                                                               
*        RETRIEVE BILL HEADER RECORD                                            
*                                                                               
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
* - FILTERS - MANUAL BILLS/B1 ETC BILLS/ DATE FILTERS                           
*                                                                               
         BAS   RE,TSTBHFLT                                                      
         BNE   GBBHDCN                                                          
*                                                                               
*        ADD BILL HEADER TO BUFFER                                              
*                                                                               
         MVI   CSHACT,CSHADDQ      SET ACTION TO ADD                            
*                                                                               
         MVC   CSHBLLA,AIO         SET A(BILL RECORD)                           
*                                                                               
         GOTO1 VCASHIER,DMCB,CSHIERD    ADD BILL RECORD TO BUFFER               
*                                                                               
         CLI   CSHERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ENTRY IN CASHIER BUFFER FOUND                                          
*                                                                               
GBCSHF   DS    0H                                                               
*                                                                               
*        SAVE BUFFER ENTRY                                                      
*                                                                               
         L     R0,CSHRECA          POINT TO CASHIER RECORD                      
         LA    RE,SVCSHREC         POINT TO SAVEAREA                            
         LHI   RF,L'SVCSHREC       SAVEAREA LENGTH                              
         LR    R1,RF               COPY LENGTH                                  
*                                                                               
         MVCL  RE,R0               SAVE CASHIER RECORD                          
*                                                                               
         LA    R4,SVCSHREC         POINT TO SAVED BILL HEADER RECORD            
         ST    R4,SVBHDELA         SAVE BILL RECORD ADDRESS                     
*                                  ALSO NOW HAS CASH APPLIED DATA               
*                                                                               
         TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELMS-GB30'            
***********************************************************************         
*                                                                     *         
*        FILL IN BILLING PORTION OF BHBLOCK                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GB30     DS    0H                                                               
*                                                                               
         MVC   SVRDBELS,NBRDBELS   SAVE SWITCH                                  
*                                                                               
         L     R4,SVBHDELA         ESTABLISH BILL HEADER RECORD                 
         USING BILLREC,R4                                                       
                                                                                
*                                                                               
         MVC   BHACTKEY,BKEY       SAVE BILL HEADER KEY                         
         MVC   BHACTCLT,BKEYCLT    CLIENT   CODE COMPRESSED                     
         MVC   BHACTPRD,BKEYPRD    PRODUCT  CODE                                
         MVC   BHACTEST,BKEYEST    ESTIMATE CODE                                
         MVC   BHACTYSR,BKEYYSRV   YEAR  OF SERVICE                             
         MVC   BHACTMSR,BKEYMSRV   MONTH OF SERVICE                             
         MVC   BHACTMBL,BKEYMBIL   BILLING MONTH                                
         MVC   BHACTINV,BKEYINV    BILL    NUMBER                               
*                                                                               
         MVC   BHMNSERV,BMONSERV   MONTH OF SERVICE - YYMM                      
         MVC   BHMNSVX,=C'01'      FUDGE FOR MONTH OF SERVICE YMD               
         GOTO1 DATCON,DMCB,(0,BHMNSERV),(X'20',WORK)                            
         MVC   BHMNSERV(6),WORK    PRINTABLE FORMAT                             
         GOTO1 DATCON,DMCB,(0,BHMNSERV),(2,BHMNSVX)  MOS - COMPRESSED           
*                                                                               
         MVC   BHBPOST,BILPOST     DATE OF POSTING TO ACC - COMPRESSED          
         MVC   BHINVNO,BINVNO      INVOICE NUMBER                               
         MVC   BHFLTID,BLFLT       FLIGHT ID                                    
         MVC   BHBDATE,BDATE       BILL DATE - YYMMDD                           
*                                                                               
         XC    BHBTYPE,BHBTYPE       ..CLEAR BILL TYPE FIELD                    
         MVC   BHBTYPE(2),BTYPE      ..SET BILL TYPE                            
         MVC   BHWEEKNO,BILWKNO      BILL WEEK NUMBER                           
*                                                                               
         TM    BILSTAT,BSTTAORQ      ..IF AOR                                   
         BNO   GB32                                                             
*                                                                               
         MVC   BHBTYPE(3),=C'AOR'       ..SET AOR                               
         MVC   BHBTYPE+3(1),BTYPE+1     ..AND NUMERIC BILL TYPE                 
*                                                                               
GB32     DS    0H                                                               
*                                                                               
         MVC   BHNET,BLMGR         NETWORK                                      
         MVC   BHDPT,BLDPT         DAYPART FILTER                               
         MVC   BHMED,BLMED         MEDIA   FILTER                               
         MVC   BHBSTAT,BILSTAT     STATUS                                       
***      MVC   BHQDATE,BQDATE      YYMMDD Y2K                                   
         GOTO1 DATCON,DMCB,(0,BQDATE),(X'20',BHQDATE)                           
         MVC   BHDUEDT,BDUEDATE                                                 
*                                                                               
         MVC   BHEBDATE,BEDIDTE    EDI TRANSMITTAL DATE                         
*                                                                               
         MVC   BHCTYP,BILCTYP      COST TYPE (TIME,CUT-IN ETC)                  
         MVI   BHPKNUM,0           PACKAGE NUMBER                               
*                                                                               
         CLC   =C'930830',BQDATE   IGNORE BEFORE THIS DATE                      
         BH    *+10                                                             
         MVC   BHPKNUM,BLPKG       PACKAGE NUMBER                               
*                                                                               
         MVC   BHGRPKNM,BLPKGNM    PKG NAME (IF GROUP PKG BILLING)              
*                                                                               
*        FIND BILLING FORMULA                                                   
*                                                                               
         OC    BILBFB(4),BILBFB    IF THE BILL FORMULA IS MISSING               
         BNZ   GB33                                                             
*                                                                               
         BAS   RE,GETBFRML            FIND IT                                   
         MVC   BILBFB(5),BILLFRML     MOVE IT INTO BILL RECORD                  
*                                                                               
GB33     CLI   NBRDBELS,0          IF READING UNIT BILL ELEMS                   
         BE    GB34                                                             
*                                                                               
* - GET DATA FROM UNIT ELEM THROUGH BILL FORMULA                                
*                                                                               
         L     R2,ABELEM           UNIT BILLING ELEMENT                         
         USING NUBILD,R2                                                        
*                                                                               
         MVC   BHCTYP,NUBILTYP     COST TYPE (TIME,INT ETC.)                    
*                                                                               
         ST    R2,BHBLLELA         SAVE A(BILLING ELM)                          
*                                                                               
         GOTO1 =V(SPBVAL),DMCB,(C'U',0(R2)),MYWORK,BILBFB                       
*                                                                               
         B     GB36                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
* - READING BILLS - GET DATA FROM BILLING RECORD                                
*                                                                               
GB34     GOTO1 =V(SPBVAL),DMCB,(C'B',0(R4)),MYWORK,BILBFB                       
*                                                                               
* - MYWORK FILLED IN BYSPBVAL                                                   
*                                                                               
GB36     LA    R2,MYWORK                                                        
         USING SPBVALD,R2                                                       
*                                                                               
         ZAP   BHBGRS,SPBVGRSP     BILLED GROSS                                 
         ZAP   BHBNET,SPBVNETP     BILLED NET                                   
         ZAP   BHBACT,SPBVACTP     BILLED ACTUAL                                
*                                                                               
         ZAP   BHTGA,BGRS2P        COS2 USED BY MIDAS                           
         ZAP   BHNGA,BNET2P        COS2 USED BY MIDAS                           
*                                                                               
         TM    BILSTAT,BSTTAORQ    ..IF TRUE AOR                                
         BNO   *+10                ..CLEAR NET                                  
         ZAP   BHBNET,=P'0'                                                     
*                                                                               
* - READ B1X PROFILE TO CHECK IF AGENCY USES START INVOICE YEAR                 
*                                                                               
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
*                                                                               
         CLC   CKEYCLT,BHACTCLT    ...DO WE NEED NEW CLIENT HEADER              
         BE    GB50                                                             
*                                                                               
         XC    KEY,KEY             ...YES/GET IT                                
         MVC   KEY+1(3),BHACTKEY+1     AGY/MED + CLIENT                         
         MVC   FILENAME,=C'SPTDIR  '                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1         CLIENT REC SITS IN ANETWS1                   
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AMYIO           RESET AIO AREA FOR BHREAD                    
*                                                                               
*                                                                               
GB50     XC    WORK,WORK           READ B1 PROFILE                              
         MVC   WORK(4),=C'S0B1'                                                 
         MVC   WORK+4(2),NBEFFAGY                                               
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),NBCLICOD                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF                                              
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         MVC   B1PROF,WORK+16                                                   
*                                                                               
         XC    WORK,WORK           READ B1X PROFILE                             
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'              LOWER CASE                               
         MVC   WORK+4(2),NBSELAGY      AGENCY                                   
         MVI   WORK+6,C'N'             MEDIA                                    
         GOTO1 NBCLUNPK,DMCB,(CPROF+6,BHACTCLT),WORK+7                          
         MVC   CLTSAV,WORK+7       SAVE 3 BYTE CLI CODE                         
         CLI   COFFICE,0                                                        
         BE    GB52                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
*                                                                               
GB52     GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         MVC   B1XPROF,WORK+16                                                  
*                                                                               
         CLI   WORK+20,0           CHECK IF INVOICE START YEAR                  
         BE    GB55                NO/                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,BHBDATE),(3,WORK+40)  DATE IN YMD                 
*                                                                               
         OC    WORK+30(2),WORK+30   IF THERE IS A CUTOFF DATE                   
         BZ    *+14                                                             
         CLC   WORK+40(2),WORK+30      SKIP IF PAST THE DATE                    
         BNL   GB55                                                             
*                                                                               
         ZIC   R1,WORK+20          YES/GET INV START YR INTO BYTE               
*                                                                               
         SR    R2,R2                                                            
         IC    R2,WORK+40          GET YEAR OF BILL INVOICE                     
         SR    R2,R1               GET NUMBER OF YEARS DIFFERENCE               
         MH    R2,=H'12'           NUMBER OF YRS X 12 MONTHS                    
*                                                                               
         SR    R1,R1                                                            
         IC    R1,WORK+41          GET MONTH OF BILL INVOICE                    
         AR    R2,R1     INV BILL MONTH + MONTHS FROM START INV YEAR            
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  WORK+5(3),DUB+6(2)                                               
         MVC   BHINVNO(2),WORK+6   SET TO START OF INV NUMBER                   
         B     GB55                                                             
******                                                                          
******   PACK  DUB,BHBDATE(2)      YEAR OF BILL INVOICE                         
******   CVB   R2,DUB                                                           
******   SR    R2,R1               GET NUMBER OF YEARS DIFFERENCE               
******   MH    R2,=H'12'           NUMBER OF YRS X 12 MONTHS                    
******   PACK  DUB,BHBDATE+2(2)    MONTH OF BILL INVOICE                        
******   CVB   R1,DUB                                                           
******   AR    R2,R1     INV BILL MONTH + MONTHS FROM START INV YEAR            
******   CVD   R2,DUB                                                           
******   UNPK  WORK+5(5),DUB+6(3)                                               
******   MVC   BHINVNO(2),WORK+6   SET TO START OF INV NUMBER                   
*                                                                               
         EJECT                                                                  
*                                                                               
* - FILL IN NETBLOCK TO FUDGE CLI/PROD/EST/DPT/MEDIA DRIVER ROUTINES            
* - IF RDBELEM READ DO NOT TOUCH NETBLOCK                                       
*                                                                               
GB55     DS    0H                                                               
***************************************************************                 
* PASS SPFMTINO FORMATED INVOIVCE NUBMER WITH DASHES/WITHOUT                    
                                                                                
         L     R4,SVBHDELA         ESTABLISH BILL HEADER RECORD                 
         USING BILLREC,R4                                                       
                                                                                
         LAY   R1,B1PROF           B1PROFILE                                    
         ST    R1,DMCB+8                                                        
         MVC   DMCB+8(1),NBEFFMED                                               
         LAY   R1,B1XPROF           B1PROFILE                                   
         ST    R1,DMCB+12                                                       
         ST    R4,DMCB+16          BILLHEADER RECORD                            
                                                                                
         GOTO1 =V(SPFMTINO),DMCB,(C'B',BDATE),(6,BINVNO)                        
         L     R1,DMCB                                                          
         MVC   BHINVM,0(R1)        FULL INV NUMBER                              
*                                                                               
         LR    RE,R1               INV NUMBER                                   
         LA    RF,10               LENGTH OF INV NUMBER                         
         LA    R5,BHINVMND         NON DASH OUTPUT                              
FRMTINVO CLI   0(RE),C'-'          DASH?                                        
         BE    *+14                DON'T MOVE                                   
         MVC   0(1,R5),0(RE)                                                    
         AHI   R5,1                BUMP OUTPUT POINTER                          
         AHI   RE,1                BUMP INV NUMBER POPINTER                     
         BCT   RF,FRMTINVO                                                      
****************************************************************                
*                                                                               
         CLI   NBRDBELS,0          IF NOT READING UNIT BILL ELEMS               
         BNE   GB57                                                             
*                                                                               
         MVC   NBCLICOD,CLTSAV            CLIENT                                
         MVC   NBACTEST,BHACTEST          EST                                   
         BAS   RE,GETPRD1                                                       
         MVC   NBSPLPRN,WORK              PRODUCT                               
         MVC   NBSPLPR3,BHACTPRD          3 CHAR PROD                           
         MVC   NDCURPRD,BHACTPRD          FOR AORKEYWORDS                       
         MVC   NBACTAM(3),BHACTKEY+1      AM/CLI                                
         MVC   NBACTDP,BHDPT              DAYPART FILTER                        
*                                                                               
GB57     CLI   BHMED,X'40'                                                      
         BH    *+8                                                              
         MVI   BHMED,C'N'          DEFAULT TO NETWORK                           
*                                                                               
         MVC   NBPOSTYP,BHMED                                                   
         MVC   NBSTATYP,BHMED                                                   
         OI    NBSPLOPT,X'C0'      SET SPLIT NETIO SWITCH                       
*                                                                               
* - READ PACKAGE RECORD TO GET PACKAGE NAME                                     
*                                                                               
         CLI   BHPKNUM,0           IF PKG NUMBER                                
         BE    GB58                                                             
*                                                                               
         XC    KEY,KEY                GO FOR PKG RECORD                         
         LA    R1,KEY                                                           
         USING NPRECD,R1                                                        
*                                                                               
         MVI   NPKTYPE,2           RECORD TYPE                                  
         MVC   NPKAM,BHACTKEY+1    AGENCY/MEDIA                                 
         MVC   NPKCLT,BHACTCLT     CLIENT - COMPRESSED                          
         MVC   NPKNET,BHNET        NETWORK                                      
         MVC   NPKEST,BHACTEST     ESTIMATE                                     
         MVC   NPKPACK,BHPKNUM     PACKAGE NUMBER                               
*                                                                               
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
         MVC   FILENAME,=C'UNTDIR  '                                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(20),KEYSAVE     IGNORE IF RECORD NOT FOUND                   
         BNE   GB58                                                             
*                                                                               
         MVC   FILENAME,=C'UNTFIL  '                                            
         MVC   AIO,AMYIO                                                        
         GOTO1 GETREC                                                           
*                                                                               
         L     R1,AIO              POINT TO FOUND RECORD                        
         USING NPRECD,R1           ESTABLISH PACKAGE RECORD                     
         MVC   BHPKNAM,NPAKNAME    PACKAGE NAME                                 
*                                                                               
         DROP  R1,R4                                                            
*                                                                               
GB58     DS    0H                                                               
*                                                                               
         TM    NDRDBCEL,X'04'      GET VENDOR NAME IF NEEDED                    
         BZ    *+8                                                              
         BAS   RE,GETVNDNM                                                      
*                                                                               
         TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELMS-GBCSH'           
***********************************************************************         
*                                                                     *         
*        FILL IN CASH APPLIES PORTION OF BHBLOCK                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GBHCSH   DS    0H                                                               
*                                                                               
         BAS   RE,GETBASE          FIND BASE DATE FOR DAYS TO DISBURSE          
*                                                                               
*        CASH DATA DEFAULTS TO BILL HEADER DATA IF NO CASH APPLIED              
*                                                                               
         GOTO1 DATCON,DMCB,(3,BHDUEDT),(0,DISBDATE) - DUE=DISBURSE DATE         
         ZAP   SVBGRS,BHBGRS       GROSS - SAVEAREA                             
         ZAP   SVBNET,BHBNET       NET   - SAVEAREA                             
*                                                                               
         CLC   BHINVNO,=CL6' '     IF UNBILLED                                  
         BH    GBHCSH0                                                          
*                                                                               
         CP    BHBGRS,=P'0'        IF ZERO BILLED                               
         BNZ   GBHCSH0                                                          
         CP    BHBNET,=P'0'                                                     
         BNZ   GBHCSH0                BILLED - DO CASH                          
*                                                                               
         MVI   SVBGRS,X'FF'        NOT BILLED - SET FLAG                        
*                                                                               
         B     GBHCSHGO            DON'T DO CASHFLOW BUT PASS BILL INFO         
*                                                                               
GBHCSH0  DS    0H                                                               
*                                                                               
*        SEARCH FOR CASH APPLIED DATA                                           
*                                                                               
         L     R5,SVBHDELA         POINT TO BILL HEADER FROM CASHIER            
*                                                                               
         OC    8(5,R5),8(R5)       MUST BE A BILL RECORD                        
         BNZ   *+12                                                             
         MVI   256(R5),0           FORCE END OF RECORD                          
         B     GBHCSH1                                                          
*                                                                               
         USING BILLREC,R5          ESTABLISH BILL RECORD                        
*                                                                               
******   OC    BNET,BNET           SKIP IF NOT ZERO BILL                        
******   BNZ   GBHCSH1                                                          
*                                                                               
         BRAS  RE,GTWRPRF          FIND WR PROFILE                              
*                                                                               
         CLI   WRPROF+15,C'Y'      SKIP IF NOT USING TODAY FOR CALCS            
         BNE   GBHCSH1                                                          
*                                  DEFAULT APPLICATION DATE                     
         GOTO1 DATCON,DMCB,(3,BTODAY),(1,CSHCHKDT) USE TODAY'S DATE             
         MVI   CSHUNAPP,C'*'          SET UNAPPLIED INDICATOR                   
         MVC   CSHDEPDT,CSHCHKDT   COPY TO DEPOSIT DATE                         
*                                                                               
GBHCSH1  DS    0H                                                               
*                                                                               
         LA    R5,256(R5)          POINT TO CASH APPLIED DATA                   
*                                                                               
         USING MBTELD,R5           ESTABLISH AS MEDIA TRANSFER ELEMENT          
*                                                                               
         CLI   MBTEL,0             SKIP IF NO CASH DATA                         
         BE    GBHDYS                                                           
*                                                                               
*        CASH APPLIED DATA CONSISTS OF MEDIA TRANSFER ELEMENT                   
*        FOLLOWED BY TRANSACTION ELEMENTS EACH                                  
*          FOLLOWED BY ALLOCATION ELEMENTS                                      
*        X'FF' IS SPECIAL TRANSACTION ELEMENT BUILT BY CASHIER                  
*          TO HOLD ALL CASH STILL OUTSTANDING                                   
*                                                                               
         MVI   GBHSWTCH,0          INIT SWITCH                                  
*                                                                               
GBHCSHLP DS           0H                                                        
*                                                                               
******   ZAP   CSHGRS,BHBGRS       DEFAULT GROSS                                
******   ZAP   CSHNET,BHBNET       DEFAULT NET                                  
         ZAP   CSHGRS,SVBGRS       DEFAULT GROSS                                
         ZAP   CSHNET,SVBNET       DEFAULT NET                                  
*                                                                               
         USING MBTELD,R5           ESTABLISH AS MEDIA TRANSFER ELEMENT          
*                                                                               
         CLI   MBTEL,0             DONE IF END OF RECORD REACHED                
         BNE   *+16                                                             
         CLI   GBHSWTCH,0          GO TO DRIVER IF NO TRNELD FND                
         BE    GBHDYS                                                           
         B     GBHCSHDN                                                         
*                                                                               
         CLI   MBTEL,MBTELQ        IF A MEDIA TRANSFER ELEMENT                  
         BNE   *+8                                                              
         ST    R5,CSMBTELA            PASS ADDRESS                              
*                                                                               
         USING TRNELD,R5           ESTABLISH AS TRANSACTION ELEMENT             
*                                                                               
         CLI   TRNEL,TRNELQ        IF A TRANSACTION ELEMENT                     
         BE    *+8                                                              
         CLI   TRNEL,X'FF'                                                      
         BNE   GBHCSHCN                                                         
*                                                                               
         CLI   TRNTYPE,TRNTMEBL    SKIP IF MEDIA BILLING ELEMENT                
         BE    GBHCSHCN                                                         
*                                                                               
         ST    R5,CSTRNELA            SAVE ADDRESS                              
         MVI   GBHSWTCH,C'T'       TRANSACTION ELEMENT FOUND                    
*                                                                               
         CLI   TRNEL,X'FF'            IF UNAPPLIED CASH ELEMENT                 
         BNE   GBHCSH05                                                         
*****                                                                           
*****    CP    TRNAMNT,=P'0'             IF CREDIT                              
*****    BL    *+10                                                             
*****    CP    CSHGRS,=P'0'              IF CREDIT                              
*****    BNL   GBHCSH04                                                         
*****                                                                           
*****    GOTO1 DATCON,DMCB,(3,BHDUEDT),(1,CSHCHKDT) USE BILL DUE DATE           
*****                                                                           
*****    B     GBHCSH06                                                         
*****                                                                           
GBHCSH04 DS    0H                                                               
*****                                                                           
*****    GOTO1 DATCON,DMCB,(3,BTODAY),(1,CSHCHKDT) USE TODAY'S DATE             
*****    MVI   CSHUNAPP,C'*'          SET UNAPPLIED INDICATOR                   
*****                                                                           
         B     GBHCSH06                                                         
*                                                                               
GBHCSH05 DS    0H                                                               
*                                                                               
         MVC   CSHAPPDT,TRNDATE    TRANSACTION DATE = CASH APPLIED DATE         
*                                                                               
GBHCSH06 DS    0H                                                               
*                                                                               
*        FIND FOLLOWING RECEIVABLE ALLOCATION ELEMENT                           
*                                                                               
         LR    R2,R5               FIND FOLLOWING RECEIVABLE ALLOCATION         
         SR    RF,RF                                                            
*                                                                               
         IC    RF,TRNLN            ELEMENT LENGTH                               
         LA    R2,0(RF,R2)         NEXT ELEMENT                                 
*                                                                               
GBHCSRLL DS    0H                                                               
*                                                                               
         USING RALELD,R2           ESTABLISH RECEIVABLE ALLOC ELM               
*                                                                               
         CLI   RALEL,0             DONE IF END OF RECORD REACHED                
         BE    GBHCSRLD                                                         
*                                                                               
         CLI   RALEL,TRNELQ        DONE IF NEW TRANSACTION ELM REACHED          
         BE    GBHCSRLD                                                         
*                                                                               
         CLI   RALEL,RALELQ        FIND RECEIVABLE ALLOC ELM                    
         BNE   GBHCSRLC                                                         
*                                                                               
         CLI   RALTYPE,RALTALC     FIND REGULAR ALLOCATION ELEMENT              
         BE    GBHCSRLF                                                         
*                                                                               
         CLI   RALTYPE,RALTOFS     FIND OFFSET             ELEMENT              
         BE    GBHCSRLF                                                         
*                                                                               
         CLI   RALTYPE,RALTWOF     FIND WRITE OFF          ELEMENT              
         BE    GBHCSRLF                                                         
*                                                                               
GBHCSRLC DS    0H                                                               
*                                                                               
         IC    RF,RALLN            ELEMENT LENGTH                               
         LA    R2,0(RF,R2)         NEXT ELEMENT                                 
*                                                                               
         B     GBHCSRLL                                                         
*                                                                               
GBHCSRLD DS    0H                                                               
*                                                                               
         SR    R2,R2               NO RECEIVABLE ALLOC ELEMENT FOUND            
         B     GBHCSRLX                                                         
*                                                                               
GBHCSRLF DS    0H                                                               
*                                                                               
         CLI   RALTYPE,RALTALC     IF REGULAR ALLOCATION ELEMENT                
         BNE   GBHCALCN                                                         
*                                                                               
         MVC   CSHCHKDT,RALADAT       RETURN CLIENT CHECK DATE                  
         MVI   CSHUNAPP,C' '          CLEAR UNAPPLIED INDICATOR                 
         MVC   CSHDEPDT,RALADEP       RETURN CHECK DEPOSITED DATE               
         MVC   CSHCHK,RALAREF         RETURN CLIENT CHECK NUMBER                
*                                                                               
         B     GBHCCHK1                                                         
*                                                                               
GBHCALCN DS    0H                                                               
*                                                                               
         CLI   RALTYPE,RALTOFS     IF OFFSET             ELEMENT                
         BNE   GBHCOFSN                                                         
*                                                                               
         MVC   CSHCHKDT,RALODAT       RETURN OFFSET DATE                        
         MVI   CSHCHKID,C'O'          SET OFFSET INDICATOR                      
         MVC   CSHDEPDT,RALODAT       RETURN OFFSET DATE                        
         MVI   CSHDEPID,C'O'          SET OFFSET INDICATOR                      
         MVI   CSHUNAPP,C' '          CLEAR UNAPPLIED INDICATOR                 
*                                                                               
         B     GBHCCHK1                                                         
*                                                                               
GBHCOFSN DS    0H                                                               
*                                                                               
         CLI   RALTYPE,RALTWOF     IF WRITE OFF          ELEMENT                
         BNE   GBHCWOFN                                                         
*                                                                               
         MVC   CSHCHKDT,RALWDAT       RETURN WRITE OFF DATE                     
         MVI   CSHCHKID,C'W'          SET WRITE OFF INDICATOR                   
         MVC   CSHDEPDT,RALWDAT       RETURN WRITE OFF DATE                     
         MVI   CSHDEPID,C'W'          SET WRITE OFF INDICATOR                   
         MVI   CSHUNAPP,C' '          CLEAR UNAPPLIED INDICATOR                 
*                                                                               
         B     GBHCCHK1                                                         
*                                                                               
GBHCWOFN DS    0H                                                               
*                                                                               
GBHCCHK1 DS    0H                                                               
*                                                                               
*                                                                               
GBHCSRLX DS    0H                                                               
*                                                                               
*        CALCULATE PERCENTAGE OF CASH APPLIED                                   
*                                                                               
GBHCSPCT DS    0H                                                               
*                                                                               
         ZAP   WPAID,=P'0'                                                      
         ZAP   WBILLED,=P'0'                                                    
         ZAP   CSHPCT,=P'0'        INITIALIZE                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         ICM   R1,15,CSMBTELA      POINT TO POSTING ELEMENT                     
         USING MBTELD,R1                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,MBTLLN           ELEMENT LENGTH                               
         LA    RE,MBTEL(RF)        POINT TO NEXT ELEMENT                        
*                                                                               
GBHPCTLP DS    0H                                                               
*                                                                               
         USING TRNELD,RE           ESTABLISH TRANSACTION ELEMENT                
*                                                                               
         CLI   TRNEL,0             DONE AT END OF RECORD                        
         BE    GBHPCTDN                                                         
*                                                                               
         CLI   TRNEL,MBTELQ        DONE IF MEDIA TRANSFER ELEMENT FOUND         
         BE    GBHPCTDN                                                         
*                                                                               
*****    CLI   TRNEL,RALELQ        DONE IF RECEIVABLE ALLOCATION FOUND          
*****    BE    GBHPCTDN                                                         
*                                                                               
         CLI   TRNEL,TRNELQ        SKIP IF NOT A TRANSACTION ELEMENT            
         BNE   GBHPCTCN                                                         
*                                                                               
         CLI   TRNTYPE,TRNTMEBL    IF A MEDIA BILL                              
         BNE   GBHPCT10                                                         
*                                                                               
         AP    WBILLED,TRNAMNT        INCREMENT BILLED AMOUNT                   
*                                                                               
         B     GBHPCT20                                                         
*                                                                               
GBHPCT10 DS    0H                                                               
*                                                                               
         CLI   TRNTYPE,TRNTCALC    IF A CASH ALLOCATION                         
         BNE   GBHPCTCN                                                         
*                                                                               
         AP    WPAID,TRNAMNT          INCREMENT PAID AMOUNT                     
*                                                                               
GBHPCT20 DS    0H                                                               
*                                                                               
GBHPCTCN DS    0H                                                               
*                                                                               
         IC    RF,TRNLN            GET ELEMENT LENGTH                           
         LA    RE,TRNEL(RF)        BUMP TO NEXT ELEMENT                         
         B     GBHPCTLP                                                         
*                                                                               
GBHPCTDN DS    0H                                                               
*                                                                               
         L     RE,CSTRNELA         POINT TO CURRENT TRANSACTION ELM             
         USING TRNELD,RE           ESTABLISH TRANSACTION ELEMENT                
*                                                                               
*****    CLI   TRNEL,X'FF'         IF UN-PAID BALANCE                           
*****    BNE   *+10                                                             
         ZAP   WPAID,TRNAMNT          USE AS PAID AMOUNT                        
*                                                                               
         CP    WBILLED,WPAID       IF BILLED EQUALS PAID                        
         BNE   GBHCSPC1                                                         
*                                                                               
         ZAP   CSHPCT,=PL4'100000'    RETURN 100%                               
*                                                                               
         B     GBHCSPCX                                                         
*                                                                               
GBHCSPC1 DS    0H                                                               
*                                                                               
         DROP  RE                                                               
*                                                                               
         CP    WBILLED,=P'0'       IF NOTHING BILLED                            
         BNE   *+14                                                             
         ZAP   PKQUOT,=P'0'                                                     
         B     GBHCSPC2                                                         
*                                                                               
         ZAP   PKWORK,WPAID        COPY CASH APPLIED                            
         SRP   PKWORK,6,0          *100.0000                                    
*                                                                               
         ZAP   PKDVSR,WBILLED      BILLED AMOUNT                                
         DP    PKWORK,PKDVSR       RATIO                                        
         SRP   PKQUOT,64-1,5       ROUND                                        
*                                                                               
GBHCSPC2 DS    0H                                                               
*                                                                               
         ZAP   CSHPCT,PKQUOT       RETURN PERCENTAGE                            
*                                                                               
         CP    CSHPCT,=PL4'100000'  DONE IF 100.000%                            
         BE    GBHCSPCX                                                         
*                                                                               
         ZAP   PKWORK,SVBGRS       GET GROSS BILLED ON BILL                     
*****    ZAP   PKWORK,CSHGRS       GET GROSS APPLIED                            
         MP    PKWORK,CSHPCT                                                    
         SRP   PKWORK,64-5,5       ROUND TO 2 DECIMALS                          
         ZAP   CSHGRS,PKWORK       AMOUNT OF BILLED GROSS PAID                  
*                                                                               
         ZAP   PKWORK,SVBNET       GET NET   BILLED ON BILL                     
*****    ZAP   PKWORK,CSHNET       GET NET   APPLIED                            
         MP    PKWORK,CSHPCT                                                    
         SRP   PKWORK,64-5,5       ROUND TO 2 DECIMALS                          
         ZAP   CSHNET,PKWORK       AMOUNT OF BILLED NET   PAID                  
*                                                                               
GBHCSPCX DS    0H                                                               
*                                                                               
         L     RE,CSTRNELA         POINT TO CURRENT TRANSACTION ELM             
         USING TRNELD,RE           ESTABLISH TRANSACTION ELEMENT                
*                                                                               
         CLI   TRNEL,X'FF'            IF UNAPPLIED CASH ELEMENT                 
         BNE   GBHCDT06                                                         
*                                                                               
         CP    CSHGRS,=P'0'              IF CREDIT                              
         BNL   GBHCDT04                                                         
*                                                                               
         BRAS  RE,GTWRPRF                   FIND WR PROFILE                     
*                                                                               
         CLI   WRPROF+14,C'Y'               SKIP IF CR'S USE TODAY              
         BE    GBHCDT04                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,BHDUEDT),(1,CSHCHKDT) USE BILL DUE DATE           
         MVI   CSHUNAPP,C'*'          SET UNAPPLIED INDICATOR                   
*                                                                               
         B     GBHCDT06                                                         
*                                                                               
GBHCDT04 DS    0H                                                               
*                                                                               
         TM    NBPPDOPT,NBPPDONQ   SKIP IF NOT PAYPEND OPT                      
         BNO   GBHCDT05                                                         
*                                                                               
         ZAP   CSHNET,=P'0'           CLEAR NET   AMOUNT                        
         ZAP   CSHGRS,=P'0'           CLEAR GROSS AMOUNT                        
*                                                                               
         B     GBHCDT06                                                         
*                                                                               
GBHCDT05 DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(1,CSHCHKDT) USE TODAY'S DATE             
         MVI   CSHUNAPP,C'*'          SET UNAPPLIED INDICATOR                   
         MVC   CSHDEPDT,CSHCHKDT      COPY TO DEPOSIT DATE                      
*                                                                               
GBHCDT06 DS    0H                                                               
*                                                                               
         DROP  RE                                                               
*                                                                               
         CP    CSHGRS,=P'0'        DROP PERCENT IF NOTHING BILLED               
         BNE   *+10                                                             
         ZAP   CSHPCT,=P'0'                                                     
*                                                                               
         SRP   CSHPCT,64-1,5       ROUND TO 2 DECIMALS                          
*                                                                               
         DROP  R1                                                               
*                                                                               
*        CALCULATE DAYS TO DISBURSEMENT                                         
*                                                                               
GBHDYS   DS    0H                                                               
*                                                                               
         MVI   GBHSWTCH,C'D'       GONE TO DRIVER                               
*                                                                               
         CLC   BASEDATE,=6C' '     SKIP IF NO BASEDATE AVAILABLE                
         BNH   GBHDYSX                                                          
*                                                                               
         BRAS  RE,GTWRPRF         FIND WR PROFILE                               
*                                                                               
         LA    RF,CSHCHKDT        ASSUME CLIENT CHECK DATE                      
*                                                                               
         CLI   WRPROF+10,C'Y'     IF CLIENT WANTS BANK DEPOSIT DATE             
         BNE   *+8                                                              
         LA    RF,CSHDEPDT           SWITCH POINTER                             
*                                                                               
         OC    0(L'CSHCHKDT,RF),0(RF)  IF THERE IS A DATE                       
         BZ    GBHDYS20                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,0(RF)),(0,DISBDATE) USE FOR DISB DAY              
*                                                                               
GBHDYS20 DS    0H                                                               
*                                                                               
*        CALCULATE DAYS TO DISBURSEMENT                                         
*                                                                               
         CLC   DISBDATE,=6C' '     SKIP IF NO DISBURSED DATE KNOWN              
         BNH   GBHDYSX                                                          
*                                                                               
         CLC   BASEDATE,DISBDATE     IF BASE AFTER DUEDATE                      
         BNH   GBHDYS50                                                         
*                                                                               
         GOTO1 PERVERT,DMCB,DISBDATE,BASEDATE - DAYS BETWEEN                    
*                                                                               
         MVC   HALF,8(R1)                                                       
         LH    R2,HALF                                                          
         BCTR  R2,0         PERVERT RETURNS DAYS INCLUSIVE                      
*                                                                               
         MHI   R2,-1               MAKE IT NEGATIVE                             
*                                                                               
         B     GBHDYS52                                                         
*                                                                               
* - DUE DATE AFTER BASE DATE                                                    
*                                                                               
GBHDYS50 GOTO1 PERVERT,DMCB,BASEDATE,DISBDATE                                   
*                                                                               
         MVC   HALF,8(R1)                                                       
         LH    R2,HALF                                                          
         BCTR  R2,0           PERVERT RETURNS DAYS INCLUSIVE-DROP 1             
*                                                                               
GBHDYS52 DS    0H                                                               
*                                                                               
         MHI   R2,-1               REVERSE CASH DAYS SIGN                       
*                                                                               
         ST    R2,CSHDAYD                                                       
*                                                                               
         CVD   R2,DUB                                                           
*                                                                               
         ZAP   FULL,DUB            FORCE FIELD LENGTH OF 4                      
*                                                                               
         OC    CSHGRS,CSHGRS       SKIP IF NO GROSS                             
         BZ    *+22                                                             
         ZAP   PAKWORK,CSHGRS      GROSS CASH AMOUNT                            
         MP    PAKWORK,FULL                                                     
         ZAP   CSHDALI,PAKWORK     GROSS DAILY BALANCE                          
*                                                                               
         OC    CSHNET,CSHNET       SKIP IF NO NET                               
         BZ    *+22                                                             
         ZAP   PAKWORK,CSHNET      NET   CASH AMOUNT                            
         MP    PAKWORK,FULL                                                     
         ZAP   CSHDALIN,PAKWORK    NET    DAILY BALANCE                         
*                                                                               
GBHDYSX  DS    0H                                                               
*                                                                               
*        CALL DRIVER FOR INPUT                                                  
*                                                                               
GBHCSHGO DS    0H                                                               
         MVI   GLMODE,GLINPUT                                                   
*                                                                               
         ICM   R0,15,AIO                                                        
         MVC   AIO,AMYIO           SET I/OAREA ADDRESS                          
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         STCM  R0,15,AIO           RESTORE BILL HEADER ADDRESS                  
*                                                                               
*        CLEAR BHBLOCK DATA TO BE REPORTED ONLY ONCE                            
*                                                                               
         XC    BHCLSDES,BHCLSDES   CLEAR VENDOR NAME                            
         XC    BHVENDCD,BHVENDCD   CLEAR VENDOR CODE                            
         XC    BHFLTID,BHFLTID     CLEAR FLIGHT ID                              
         XC    BHINVNO,BHINVNO     CLEAR INVOICE NUMBER                         
***      XC    BHQDATE,BHQDATE     INV DATE WON'T REPORT IF CLEARED!            
*                                                                               
         ZAP   BHBGRS,=P'0'        AND DOLLARS                                  
         ZAP   BHBNET,=P'0'                                                     
         ZAP   BHBACT,=P'0'                                                     
*                                                                               
         XC    CSHCHK(CSHLENE-12),CSHCHK   CLEAR OUT BILL DATA                  
*                                                                               
         MVI   NBRDBELS,2        ..SET FLAG SO NEWRIDRIVE SKIPS ACCGEN          
         CLI   SVBGRS,X'FF'        SKIPPED CASHFLOW?                            
         BE    GBHCSHDN             THEN WE'RE DONE                             
*                                                                               
GBHCSHCN DS    0H                                                               
*                                                                               
         USING MBTEL,R5            ESTABLISH AS MEDIA TRANSFER ELEMENT          
*                                                                               
         CLI   MBTELD,0            DONE IF END OF RECORD REACHED                
         BE    GBHCSHDN                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,MBTLLN         BUMP TO NEXT ELEMENT                         
         BZ    GBHCSHDN                                                         
         LA    R5,MBTEL(RF)                                                     
*                                                                               
         B     GBHCSHLP                                                         
*                                                                               
GBHSWTCH DS    CL1                 PROCESSING SWITCH                            
*                                  C'T' - TRANSACTION ELM FOUND                 
*                                  C'D' - HAVE GONE TO DRIVER                   
GBHCSHDN DS    0H                                                               
*                                                                               
         XC    CSMBTELA,CSMBTELA   INIT MEDIA TRANSFER ELEMENT ADDRESS          
         XC    CSTRNELA,CSTRNELA   INIT TRANSACTION    ELEMENT ADDRESS          
         XC    BHQDATE,BHQDATE     CLEAR PRINT BILL DATE                        
*                                                                               
GBHCSHX  DS    0H                                                               
*                                                                               
         XC    SVBHDELA,SVBHDELA   INIT BILL HEADER TABLE ADDRESS               
*                                                                               
         MVC   NBRDBELS,SVRDBELS   RESTORE SWITCH                               
*                                                                               
         CLI   NBRDBELS,0        ..IF BILLING ELEMS READ                        
         BE    GB70                                                             
*                                                                               
         MVI   NBRDBELS,2        ..SET FLAG SO NEWRIDRIVE SKIPS ACCGEN          
*                                                                               
         B     RDB10             ..GET NEXT BILLING ELEM                        
*                                                                               
BBDATA   EQU   (CHQBLOCK-BHDATA)                                                
*                                                                               
GB70     NETGO NVSETSPT,DMCB                                                    
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY(13),BHACTKEY                                                 
*                                                                               
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
GBBHDCN  DS    0H                                                               
*                                                                               
         LA    R4,KEY              RESET                                        
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     GBBHDLP             GET NEXT RECORD                              
*                                                                               
GBBHDDN  DS    0H                                                               
*                                                                               
         MVC   AIO,SVAIO          RESET UNT FILE/RD SEQ/AIO AREA                
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
         CLI   NBRDBELS,0          ..IF COMING FROM UNIT BILL ELEM              
         BE    GBXX                                                             
*                                                                               
         MVC   KEY,NBKEY              ..REREAD UNIT KEY                         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(20),KEYSAVE         MUST FIND IT                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GBXX     B     EXIT                                                             
*                                                                               
         TITLE 'T32082 - READ WR PROFILE - GTWRPRF'                             
*                                                                               
*        READ WR PROFILE                                                        
*                                                                               
GTWRPRF  NTR1                      YES-READ AGENCY'S WR PROFILE                 
*                                                                               
         XC    GTWRKY,GTWRKY       INIT PROFILE KEY                             
         MVC   GTWRSYS(2),=C'S0'   SYSTEM                                       
         MVC   GTWRPID,=C'WR'      PROFILE ID                                   
         MVC   GTWRAGY,AGENCY      AGENCY                                       
         MVI   GTWRMED,C'N'        MEDIA                                        
         MVC   GTWRCLT,NBCLICOD    CLIENT                                       
*                                                                               
         CLC   GTWRKY,GTWRKYS     SKIP IF KEY HASN'T CHANGED                    
         BE    GTWRPRF1                                                         
*                                                                               
         GOTO1 GETPROF,DMCB,GTWRKY,WRPROF,DATAMGR   GET WR PROFILE              
*                                                                               
         MVC   GTWRKYS,GTWRKY     SAVE OFF PROFILE KEY                          
*                                                                               
GTWRPRF1 DS    0H                                                               
*                                                                               
GTWRPRFX DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        KEY FOR WR PROFILE                                                     
*                                                                               
GTWRKY   DS    XL16                PROFILE KEY                                  
         ORG   GTWRKY                                                           
GTWRSYS  DS    CL1                 SYSTEM                                       
         DS    XL1                 NOT USED                                     
GTWRPID  DS    CL2                 PROFILE ID                                   
GTWRAGY  DS    CL2                 AGENCY                                       
GTWRMED  DS    CL1                 MEDIA                                        
GTWRCLT  DS    CL3                 CLIENT                                       
         ORG                                                                    
*                                                                               
GTWRKYS  DS    XL(L'GTWRKY)        PROFILE KEY SAVEAREA                         
*                                                                               
WRPROF   DS    CL16               WR PROFILE                                    
*                                                                               
         TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELMS'                 
*                                                                               
* CALLED FROM BILLING RECORD READ TO RESTORE CLIENT RECORD                      
* TO ANETWS1                                                                    
*                                                                               
RESTCLT  NTR1                                                                   
         L     R1,ANETWS1                                                       
         CLC   CLTRECSV,0(R1)                                                   
         BE    RCLTX                                                            
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEY,CLTRECSV                                                     
         GOTO1 HIGH                                                             
         MVC   AIO,ANETWS1                                                      
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
RCLTX    XIT1                                                                   
*                                                                               
        TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELMS-GETBASE'          
***********************************************************************         
*                                                                     *         
*        BASE DATE IS LAST DAY OF BROADCAST OR CALENDAR MONTH         *         
*          OR DATE BASED ON A PROFILE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETBASE NTR1   LABEL=*                                                          
*                                                                               
* - READ WR PROFILE FOR NBACTDAT ADJUSTMENT                                     
*                                                                               
         BRAS  RE,GTWRPRF                   FIND WR PROFILE                     
*                                                                               
         XC    BASEDATE,BASEDATE   INIT BASEDATE                                
*                                                                               
         CLI   NBRDBELS,0          IF NOT READING BILLING ELEMENTS              
         BNE   GETBASE1               USE BILL DUE DATE                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,BHDUEDT),(0,BASEDATE) - DUE=BASEDATE DATE         
*                                                                               
         B     GETBASE9                                                         
*                                                                               
GETBASE1 DS    0H                                                               
*                                                                               
*        DETERMINE BASE DATE FOR CALCULATIONS - RESULT IN WORK(6)               
*                                                                               
         MVC   WORK+16(16),WRPROF  COPY PROFILE                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(3,FULL) GET AIRDATE YMD BINARY         
         CLI   WORK+19,0           IS PROFILE SET                               
         BE    GBDDFLT             NO.DO DEFAULT                                
*                                                                               
* IF PROF = C00, USE DEFAULT                                                    
*                                                                               
         CLI   WORK+19,C'C'        IF NOT SET                                   
         BNE   GBDPROF                                                          
         CLI   WORK+20,C'0'                                                     
         BH    GBDPROF                                                          
         CLI   WORK+21,C'0'                                                     
         BNH   GBDDFLT             USE DEFAULT                                  
*                                                                               
GBDPROF  BAS   RE,GTPRFDAY         SET/GET PROFILE DAYS                         
*                                                                               
         B     GBD45                                                            
*                                                                               
*        DEFAULT BASE DATE IS LAST DAY OF BROADCAST/CALENDAR MONTH              
*                                                                               
GBDDFLT  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,DUB)  AIR DATE=YYMMDD                
*                                                                               
         XC    WORK,WORK           BASE WILL BE IN WORK(6) - CH                 
*                                                                               
         CLI   NDPEROPT,C'B'         SKIP IF PERIOD IS BROADCAST MONTHS         
         BE    GBD44                                                            
*                                                                               
*        BASE IS LAST DAY OF AIR DATE'S CALENDAR MONTH                          
*                                                                               
         MVC   WORK(6),DUB            COPY AIR DATE                             
*                                                                               
         MVC   WORK+4(2),=C'30'    ASSUME A 30 DAY MONTH                        
*                                                                               
         CLC   WORK+2(2),=C'09'       SEP                                       
         BE    GBD45                                                            
         CLC   WORK+2(2),=C'04'       APRIL                                     
         BE    GBD45                                                            
         CLC   WORK+2(2),=C'06'       JUNE                                      
         BE    GBD45                                                            
         CLC   WORK+2(2),=C'11'       NOV                                       
         BE    GBD45                                                            
*                                                                               
         MVC   WORK+4(2),=C'31'       ASSUME 31 FOR OTHER MONTHS                
*                                                                               
         CLC   WORK+2(2),=C'02'       IF FEB                                    
         BNE   GBD45                                                            
*                                                                               
*        FEBRUARY                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,FULL) - DATE IN BINARY                   
*                                                                               
         MVC   WORK+4(2),=C'28'    ASSUME NORMAL LEAP YEAR                      
*                                                                               
         TM    FULL,X'03'          LEAP YEAR IF YEAR LOW 2 BITS OFF             
         BNZ   *+10                                                             
         MVC   WORK+4(2),=C'29'                                                 
*                                                                               
         B     GBD45                                                            
*                                                                               
* - BROADCAST PERIOD - USE LAST DAY OF BROADCAST MONTH                          
*                                                                               
GBD44    DS    0H                                                               
*                                                                               
* - RETURNS START-END OF BROADCAST IN WORK                                      
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(1,DUB),WORK,GETDAY,ADDAY                      
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(6),WORK+6      MOVE END MOS TO WORK                         
*                                                                               
* - GET NUMBER OF DAYS BETWEEN BASE DATE AND BILL DUEDATE                       
*                                                                               
GBD45    DS    0H                                                               
*                                                                               
         MVC   BASEDATE,WORK       SAVE BASE DATE                               
*                                                                               
GETBASE9 DS    0H                                                               
*                                                                               
         CLI   WRPROF+15,C'Y'      IF TODAY IS BASEDATE                         
         BNE   GETBASEX                                                         
*                                                                               
***      GOTO1 DATCON,DMCB,(5,0),BASEDATE   GET TODAY                           
         GOTO1 DATCON,DMCB,(3,BTODAY),BASEDATE   GET TODAY                      
*                                                                               
GETBASEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
        TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELMS-GTPRFDAY'         
***********************************************************************         
*                                                                     *         
*        FIND BASE DATE DETERMINED BY WR PROFILE                      *         
*                                                                     *         
*         ROUTINE HAS WR PROFILE DAYS IN WORK+19                      *         
*         NBACTDAT IN FULL (YMD)                                      *         
*         CALCULATES NEW DATE IN FULL BASED ON PROFILE                *         
*                                                                     *         
*EXIT    WORK(6)  HAS PROFILE DATE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GTPRFDAY NTR1  LABEL=*                                                          
*                                                                               
         CLI   WORK+19,C'C'        USE CURRENT MONTH                            
         BE    GTPRF50                                                          
*                                                                               
         CLI   WORK+19,C'P'        USE PREVIOUS MONTH                           
         BNE   GTPRF20                                                          
*                                                                               
         ZIC   R1,FULL+1                                                        
         BCTR  R1,0                                                             
         STC   R1,FULL+1                                                        
*                                                                               
         CLI   FULL+1,0                                                         
         BNE   *+8                                                              
         MVI   FULL+1,1                                                         
*                                                                               
         B     GTPRF50                                                          
*                                                                               
GTPRF20  CLI   WORK+19,C'N'        NEXT MONTH                                   
         BNE   GTPRF30                                                          
*                                                                               
         ZIC   R1,FULL+1                                                        
         LA    R1,1(R1)                                                         
*                                                                               
         C     R1,=F'13'                                                        
         BE    GTPRF25                                                          
*                                                                               
         STC   R1,FULL+1                                                        
*                                                                               
         B     GTPRF50                                                          
*                                                                               
GTPRF25  ZIC   R1,FULL                                                          
         LA    R1,1(R1)            BUMP YEAR                                    
         STC   R1,FULL                                                          
         MVI   FULL+1,1            SET MONTH TO JANUARY                         
*                                                                               
         B     GTPRF50                                                          
*                                                                               
GTPRF30  CLI   WORK+19,C'A'        PLUS 2 MONTHS                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,FULL+1                                                        
         LA    R1,2(R1)                                                         
*                                                                               
         C     R1,=F'12'                                                        
         BH    GTPRF40                                                          
*                                                                               
         STC   R1,FULL+1                                                        
*                                                                               
         B     GTPRF50                                                          
*                                                                               
GTPRF40  C     R1,=F'13'           SET MONTH                                    
         BNE   *+12                                                             
         MVI   FULL+1,1            SET JAN                                      
         B     *+8                                                              
         MVI   FULL+1,2            MUST BE FEB                                  
*                                                                               
         ZIC   R1,FULL             NOW BUMP YEAR                                
         LA    R1,1(R1)                                                         
         STC   R1,FULL                                                          
*                                                                               
* - USE PROFILE DAYS AND CONVERT TO YYMMDD                                      
*                                                                               
GTPRF50  PACK  MYDUB,WORK+20(2)                                                 
         CVB   R1,MYDUB                                                         
         STC   R1,FULL+2                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),(0,DUB)                                     
*                                                                               
         MVC   WORK(6),DUB                                                      
*                                                                               
GTPRFDYX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
        TITLE 'T32082 - MODULE TO READ BILL HEADERS/BILL ELMS'                  
* - GET BILL FORMULA AND SET INTO WORK                                          
GETBRFML NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
* - EXPECTS BKEY IN KEY                                                         
CHKCLT   NTR1                                                                   
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       DO WE NEED NEW CLIENT HEADER                 
         BE    CHKCLTX                                                          
         MVC   SVKEY,KEY           YES/SAVE KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY+1        AGY/MED + CLIENT                         
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1             CLIENT REC SITS IN ANETWS1               
         GOTO1 GETREC                                                           
         L     R1,AMYIO                RESET AIO AREA FOR BHREAD                
         ST    R1,AIO                                                           
         MVC   KEY,SVKEY               RESET KEY                                
         MVC   FILENAME,=C'SPTDIR  '   AND SEQ                                  
         GOTO1 HIGH                                                             
CHKCLTX  XIT1                                                                   
         DROP  R4                                                               
                                                                                
* - CALLED FROM RDBELEM                                                         
CHKCLT1  NTR1                                                                   
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       DO WE NEED NEW CLIENT HEADER                 
         BE    CHKCLTX1                                                         
         MVC   SVKEY,KEY           YES/SAVE KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY+1        AGY/MED + CLIENT                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',KEY+14,ANETWS1,MYDM         
         MVI   NBFUNCT,NBFRDHI                                                  
         MVC   KEY,SVKEY                                                        
CHKCLTX1 XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* - READ COMMERCIAL CLASS RECORD FOR VENDOR NAME FOR HOME DEPOT                 
*                                                                               
*          DATA SET NEWRI73    AT LEVEL 123 AS OF 08/15/94                      
*                                                                               
GETVNDNM NTR1                                                                   
         XC    CMMLIDSV,CMMLIDSV                                                
         L     R4,NBAIO            UNIT RECORD                                  
         CLI   0(R4),X'04'             IF NOT UNIT                              
         BNE   GVNXX                                                            
         MVI   ELCODE,X'21'        COMMERCIAL ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   GVNXX                                                            
         MVC   MYKEY,KEY                                                        
         B     *+8                                                              
GVN10    BAS   RE,NEXTEL                                                        
         BNE   GVN20                                                            
         USING NUCMLEL,R4                                                       
         TM    NUCMLFLG,X'C8'      IS CMML ELEM VALID                           
         BNZ   GVN10                                                            
*****    CLC   NBSPLPRN,NBPRD      FIRST PROD                                   
*****    BNE   *+10                                                             
         CLC   NBSPLPR3,NBPR1CL3  FIRST PROD                                    
         BNE   *+10                                                             
         MVC   CMMLIDSV(8),NUCML1                                               
****     CLC   NBSPLPRN,NBPRD2     SECOND PROD                                  
****     BNE   *+10                                                             
         CLC   NBSPLPR3,NBPR2CL3   SECOND PROD                                  
         BNE   *+10                                                             
         MVC   CMMLIDSV(8),NUCML2                                               
         MVI   ELCODE,X'23'        ANY FEEDS                                    
GVN15    BAS   RE,NEXTEL                                                        
         BNE   GVN20                                                            
         USING NUFDCEL,R4                                                       
         TM    NUFDCFL2,X'80'      DELETED FEED                                 
         BO    GVN15                                                            
***      CLC   NBSPLPRN,NBPRD                                                   
***      BNE   *+14                                                             
         CLC   NBSPLPR3,NBPR1CL3                                                
         BNE   *+14                                                             
         MVC   CMMLIDSV(8),NUFDCML1     FIRST PROD FEED                         
         B     GVN20                                                            
***      CLC   NBSPLPRN,NBPRD2                                                  
         CLC   NBSPLPR3,NBPR2CL3                                                
         BNE   GVN20                                                            
         MVC   CMMLIDSV(8),NUFDCML2                                             
         DROP  R4                                                               
GVN20    XC    KEY,KEY             READ CMML PROFILE RECORD FIRST               
         LA    R2,KEY              TO GET COMMERCIAL CLASS                      
         USING CMLRECD,R2                                                       
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,NBACTAM                                                   
         MVC   CMLKCLT,NBACTCLI                                                 
         MVC   CMLKCML,CMMLIDSV                                                 
GVN30    NETGO NVSETSPT,DMCB                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
***      BE    *+6                                                              
***      DC    H'0'                                                             
         BNE   GVNX                LET TRAFFIC SORT IT OUT                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         L     R1,AMYIO                                                         
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,X'10'              GET CMML DATA ELEM FIRST               
         BAS   RE,GETEL                                                         
***      BE    *+6                                                              
***      DC    H'0'                                                             
         BNE   GVNX                                                             
         USING CMLDTAEL,R4                                                      
         CLC   CMLCLASS,=4X'00'    IF THERE                                     
         BE    GVN50                                                            
         MVC   WORK(4),CMLCLASS    USE IT                                       
         B     GVN60                                                            
GVN50    L     R4,AIO                                                           
         MVI   ELCODE,X'21'        ELSE  GET COMMERCIAL CLASS ELEM              
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         BNE   GVNX                                                             
*        DC    H'0'                                                             
         USING CMLCLSEL,R4                                                      
         MVC   WORK(4),CMLCLS      COMMERCIAL CLASS                             
* - READ COMMERCIAL CLASS RECORD FOR CLASS DESCRIPTION(VENDOR)                  
GVN60    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLSKEY,R4                                                        
         MVC   CLSKID,=X'0A44'                                                  
         MVC   CLSKAM,NBACTAM                                                   
         MVC   CLSKCLAS,WORK                                                    
         OC    CLSKCLAS,=X'40404040'    NEED X'40' FILLER                       
         MVC   CLSKCLT,NBACTCLI                                                 
****     MVC   WORK(1),NBSPLPRN    NEED 3 BYTE PROD CODE                        
****     BAS   RE,GETPRD3                                                       
****     MVC   CLSKPROD,WORK     .READ FOR SPECIFIC PRODUCT FIRST               
         MVC   CLSKPROD,NBSPLPR3 .READ FOR SPECIFIC PRODUCT FIRST               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GVN70                                                            
         MVC   KEY(13),KEYSAVE    .IF NOT FOUND                                 
         XC    CLSKPROD,CLSKPROD  .TRY PROD=0                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GVNX                                                             
GVN70    MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,X'10'        CLASS DESCRIPTION ELEM                       
         BAS   RE,GETEL                                                         
         BNE   GVN80               NO ELEM/SKIP CLASS DESC                      
         USING CLSDSCEL,R4                                                      
         MVC   BHCLSDES,CLSDESC                                                 
GVN80    LA    R4,KEY                                                           
         USING CLSKEY,R4                                                        
         MVC   BHVENDCD,CLSKCLAS           SET CMML CLASS CODE                  
         CLI   NBRDBELS,0                                                       
         BNE   *+8                                                              
         MVI   NBRDBELS,1                                                       
GVNX     DS    0H                                                               
         MVC   KEY,MYKEY                                                        
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
GVNXX    XIT1                                                                   
*                                                                               
         EJECT                                                                  
* - EXPECTS R2 TO POINT TO EST/PRDMSK                                           
TESTMASK NTR1                                                                   
*        LA    R2,NBESTMSK                                                      
*        LA    R2,ESTMSK                                                        
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         XIT1                                                                   
***      BE    NO                                                               
***      B     YES                                                              
                                                                                
SETMASK  NTR1                                                                   
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0               R1 HAS NUMBER                                
         SLDL  R0,29               GET BYTE NO. INTO R0                         
         SRL   R1,29                   BIT  NO. INTO R1                         
         AR    R2,R0               R2 HAS (MASK)                                
         LA    R1,BITLIST(R1)                                                   
         OC    0(1,R2),0(R1)                                                    
         XIT1                                                                   
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
BITTEST  DS    CL1                                                              
HOLDEFLT DS    CL1                                                              
FFS      DC    32X'FF'                                                          
ESTMSK   DS    CL32                                                             
*                                                                               
*          DATA SET NENETIO    AT LEVEL 062 AS OF 01/19/06                      
* TEST PROD CODE AGAINST OP PROD AREA                                           
* INPUT  WORK BYTE 1 = BINARY CODE                                              
* INPUT  WORK CL3 = 3 CHAR PROD CODE                                            
* PROD TABEL = (1 BINARY + 3 CHAR) X 200 PRODS                                  
TSTOPMSK NTR1                                                                   
         L     R2,=A(PRDMSK)              ADDRESS OF PROGR GROUP?               
         LA    RE,500              HARD CODED FOR 500 OVERFLOW PRDMSK           
TS20     CLC   1(3,R2),WORK                                                     
         BE    TSX                                                              
         CLI   WORK+1,0            ARE WE DEALING WITH BINARY PROD              
         BNE   TS22                NO                                           
         CLC   0(1,R2),WORK        YES                                          
         BE    TSX                                                              
TS22     LA    R2,4(R2)                                                         
         BCT   RE,TS20                                                          
         LTR   R2,R2               SET CC -> NOT EQUAL                          
TSX      XIT1                                                                   
*                                                                               
*          DATA SET NENETIO    AT LEVEL 062 AS OF 01/19/06                      
* SET 3 CHAR PROD IN OVERFLOW PROD MASK AREA                                    
* INPUT  R1 -       BINARY PROD CODE                                            
* INPUT  WORK -     3 CHAR PROD CODE                                            
*                                                                               
SETOPMSK NTR1                                                                   
         L     R2,=A(PRDMSK)                                                    
         LA    RE,500              FOR 500 PRODS = NEWRI20                      
*                                  NEWRI20 USES NBADRPRG                        
ST20     OC    0(3,R2),0(R2)                                                    
         BZ    ST21                                                             
         CLC   1(3,R2),WORK        DO WE ALREADY HAVE IT?                       
         BE    STX                 YES                                          
         LA    R2,4(R2)                                                         
         BCT   RE,ST20                                                          
         DC    H'0'                EXPAND TABLE                                 
ST21     STC   R1,0(R2)            SET OVERFLOW BINARY PROD                     
         MVC   1(3,R2),WORK        SET OVERFLOW CL3 PRD                         
STX      XIT1                                                                   
         EJECT                                                                  
                                                                                
* INPUT:   WORKINV=MONTH(2BYTES)+INVNO(4BYTES)                                  
*          WORKYMD=YYMMDD OF BILLING RUN DATE                                   
*                                                                               
* OUTPUT:  3BYTE 1(Y/M) + 2(INVNO)                                              
GETBYMN  NTR1                                                                   
         PACK  DUB,WORKINV+2(4)   .GET BINARY VALUE OF INVNO                    
         CVB   R1,DUB                                                           
         STCM  R1,3,WORK+1                                                      
* - CONVERT BILLING RUN MONTH TO BINARY                                         
         PACK  DUB,WORKINV(2)     .GET BINARY VALUE OF MONTH                    
         CVB   R1,DUB                                                           
         STCM  R1,1,WORK                                                        
* GET YEAR INTO ZONE OF MONTH BYTE                                              
         PACK  BYTE,WORKYMD+1(1)   .SWITCH FN TO NF  (YYMMDD)                   
         NI    BYTE,X'F0'          .MAKE IT N0                                  
         OC    WORK(1),BYTE        .SET IT INTO FIRSTHALF OF MONTH BYTE         
         XIT1                                                                   
                                                                                
                                                                                
* - ROUTINE GETS MONTH OF SERVICE OF UNIT DATE                                  
GETMOS   DS    0H                                                               
         LA    RF,PERLIST                                                       
GETM4    DS    0H                                                               
         CLC   NBACTDAT,2(RF)      TEST DATE VS START                           
         BE    GETM8                                                            
         BNL   *+6                                                              
         DC    H'0'                TIME TO LENGTHEN PERLIST                     
         CLC   NBACTDAT,4(RF)                                                   
         BNH   GETM8                                                            
         LA    RF,6(RF)            NEXT ENTRY                                   
         B     GETM4               NOTE- LIST ENDS IN FF                        
GETM8    DS    0H                                                               
         MVC   BYMOS,0(RF)         SET YR/MOS                                   
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
BHFILTRS NTR1                                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         MVI   BHDTYPF,0                                                        
         XC    RDATST(DATLSLEN),RDATST                                          
         LA    R2,SPLOPTH                                                       
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,SCANFIND         SCAN, FIND AND SET BH FILTERS                
         LA    R2,SPLOTHH                                                       
         CLI   5(R2),0                                                          
         BE    BHFX                                                             
         BAS   RE,SCANFIND         SCAN, FIND AND SET BH FILTERS                
BHFX     XIT1                                                                   
         DROP  R5                                                               
                                                                                
*                                                                               
* - FINDS AND SETS ANY FILTERS IN OPTION/OTHER LINE FOR BH                      
SCANFIND NTR1                                                                   
         LA    R4,SCANBLK                                                       
         GOTO1 SCANNER,DMCB,(25,(R2)),(10,SCANBLK),0                            
         ZIC   R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    SCNX                                                             
SCN10    CLC   12(7,R4),=C'BHRDATE'                                             
         BNE   SCN10B                                                           
         OI    BHDTYPF,X'01'       SET RDATE FILTER                             
         BAS   R5,VALDAT                                                        
         MVC   RDATST,WORK                                                      
         MVC   RDATEND,WORK+6                                                   
         B     SCN15                                                            
SCN10B   CLC   12(7,R4),=C'BHPDATE'                                             
         BNE   SCN10C                                                           
         OI    BHDTYPF,X'02'       SET PDATE FILTER                             
         BAS   R5,VALDAT                                                        
         MVC   PDATST,WORK                                                      
         MVC   PDATEND,WORK+6                                                   
         B     SCN15                                                            
SCN10C   CLC   12(7,R4),=C'BHIDATE'                                             
         BNE   SCN10D                                                           
         OI    BHDTYPF,X'04'       SET IDATE FILTER                             
         BAS   R5,VALDAT                                                        
         MVC   IDATST,WORK                                                      
         MVC   IDATEND,WORK+6                                                   
         B     SCN15                                                            
SCN10D   CLC   12(7,R4),=C'BHDDATE'                                             
         BNE   SCN10E                                                           
         OI    BHDTYPF,X'08'       SET DDATE FILTER                             
         BAS   R5,VALDAT                                                        
         MVC   DDATST,WORK                                                      
         MVC   DDATEND,WORK+6                                                   
         B     SCN15                                                            
*                                                                               
SCN10E   CLC   12(7,R4),=C'BHEDATE'                                             
         BNE   SCN20                                                            
         OI    BHDTYPF,X'10'       SET EDATE FILTER                             
         BAS   R5,VALDAT                                                        
         MVC   EDATST,WORK                                                      
         MVC   EDATEND,WORK+6                                                   
         B     SCN15                                                            
                                                                                
SCN15    DS    0H                                                               
*SCN15    GOTO1 DATCON,DMCB,(0,WORK),(0,BHBDATFS)                               
*         GOTO1 DATCON,DMCB,(0,WORK+6),(0,BHBDATFE)                             
         B     SCN100                                                           
*                                                                               
SCN20    CLC   12(6,R4),=C'BHTYPE'                                              
         BNE   SCN100                                                           
         CLC   22(4,R4),=C'-AOR'                                                
         BE    SCN21                                                            
         CLC   22(3,R4),=C'AOR'                                                 
         BNE   SCN22                                                            
SCN21    MVC   BHAOR,22(R4)                                                     
         B     SCN100                                                           
*                                                                               
SCN22    CLI   22(R4),C'M'         MANUAL BILL FILTER                           
         BE    SCN22B                                                           
         CLC   22(2,R4),=C'-M'                                                  
         BNE   SCN23                                                            
         MVI   BHMANFLG,C'-'       SET EXCLUDE MANUAL BILLS                     
         CLI   24(R4),X'40'        ..IF FURTHER FILTER                          
         BNH   SCN100                                                           
         MVI   BHBFLG,C'B'         ..SET IT                                     
         MVC   BHBFLG+1(1),24(R4)                                               
         B     SCN100                                                           
SCN22B   MVI   BHMANFLG,C'Y'       SET MANUAL BILL FLAG                         
         CLI   23(R4),X'40'        ..IF B1 ETC ALSO FILTER                      
         BNH   SCN100                                                           
         MVI   BHBFLG,C'B'         .. SET IT                                    
         MVC   BHBFLG+1(1),23(R4)  SET B1 ETC                                   
         B     SCN100                                                           
*                                                                               
SCN23    CLI   22(R4),C'B'         B1 ETC ONLY FILTER                           
         BNE   SCN100              NO MATCH/SKIP FILTER                         
         MVC   BHBFLG,22(R4)                                                    
         B     SCN100                                                           
*                                                                               
SCN100   LA    R4,47(R4)           BUMP TO NEXT SCANNER ENTRY                   
         BCT   R2,SCN10            25(SCAN INPUT LEN) + (LINE+22) = 47          
SCNX     XIT1                                                                   
                                                                                
                                                                                
* VALIDATE DATES - RETURNS VIA R5 WITH DATES IN WORK AND WORK+6                 
VALDAT   DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,22(R4)),WORK                                      
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'        BAD BUG/NEWRIGEN SHOULD HAVE GOT IT                  
         MVC   WORK+6(6),WORK      ASSUME END = START                           
         LA    R1,22(R1,R4)                                                     
         CLI   0(R1),C' '                                                       
         BE    VALDATX                                                          
         CLI   0(R1),C'-'          CHECK DELIMITER                              
         BE    *+6                                                              
         DC    H'0'                BAD BUG                                      
         LA    R1,1(R1)                                                         
         ST    R1,DMCB                                                          
         GOTO1 DATVAL,DMCB,,WORK+6                                              
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                BAD BUG                                      
         CLC   WORK+6(6),WORK      CHECK END V START                            
         BNL   *+6                                                              
         DC    H'0'                BAD BUG                                      
VALDATX  BR    R5                                                               
*                                                                               
                                                                                
         EJECT                                                                  
* - TESTS BH FILTERS AGAINST BILLING RECORD                                     
*                                                                               
TSTBHFLT NTR1                                                                   
         L     R4,AIO                                                           
         USING BILLREC,R4                                                       
*                                                                               
         TM    NBACCFLT,X'60'      CLEARED/ORDERED FILT ?                       
         BZ    TSTBH00                                                          
         TM    NBACCFLT,X'20'      CLEARED ONLY                                 
         BZ    *+16                                                             
         TM    BILSTAT2,X'80'          BILLED ON CLEARED                        
         BO    TSTBH00                                                          
         B     TSTNO                                                            
         TM    BILSTAT2,X'80'      ASSUME NOT CLEARED ONLY                      
         BO    TSTNO                                                            
         B     TSTBH00                                                          
*                                                                               
TSTBH00  DS    0H                                                               
         ICM   R1,15,NBINVFLT   INVOICE FILTER?                                 
         BZ    TSTINVX                                                          
         OC    0(12,R1),0(R1)                                                   
         BZ    TSTINVX                                                          
         CLI   0(R1),C'*'          START INV NUMBER                             
         BE    *+14                                                             
         CLC   BINVNO,0(R1)                                                     
         BL    TSTNO               NO-LOWER                                     
         CLI   6(R1),C'*'          END INV NUMBER                               
         BE    TSTINVX                                                          
         CLC   BINVNO,6(R1)                                                     
         BH    TSTNO               NO-HIGHER                                    
TSTINVX  EQU   *                                                                
*                                                                               
         CLI   NBSELMFL,0        MEDIA FILTER ?                                 
         BE    ENDNGMFL                                                         
         TM    NBSELMFL,X'40'    NEGATIVE FILTERING                             
         BO    ENDNGMFL                                                         
         MVC   BYTE,NBSELMFL       YES                                          
         OI    BYTE,X'40'                                                       
         CLC   BYTE,BLMED          IF MATCH                                     
         BE    TSTNO               NO GOOD                                      
ENDNGMFL EQU   *                   END NEGATIVE MEDIA FILTER                    
*                                                                               
         CLI   BHSELMED,X'40'      MEDIA                                        
         BNH   *+14                                                             
         CLC   BHSELMED,BLMED                                                   
         BNE   TSTNO                                                            
*                                                                               
         CLI   BHMANFLG,C'Y'       MANUAL BILLS ONLY                            
         BNE   *+12                                                             
         TM    BILSTAT,BSTMANQ     ...ONLY MANUAL BILLS                         
         BNO   TSTNO                                                            
         CLI   BHMANFLG,C'-'       ...EXCLUDE MANUAL BILLS                      
         BNE   *+12                                                             
         TM    BILSTAT,BSTMANQ                                                  
         BO    TSTNO                                                            
*                                                                               
         CLI   BHBFLG,X'40'        B1 EST FILTER                                
         BNH   *+14                                                             
         CLC   BTYPE,BHBFLG        ...IS IT MATCH                               
         BNE   TSTNO               ...NO/SKIP                                   
*                                                                               
         CLI   BHAOR,C'-'          EXCLUDE AOR                                  
         BNE   GB28                                                             
         TM    BILSTAT,BSTTAORQ    ..IF TRUE AOR                                
         BO    TSTNO               ..SKIP                                       
*                                                                               
GB28     CLI   BHAOR,C'A'          ONLY AOR RECORDS                             
         BNE   GB29                                                             
         TM    BILSTAT,BSTTAORQ    ..IF NOT AOR                                 
         BNO   TSTNO               ..SKIP                                       
*                                                                               
GB29     CLI   BHDTYPF,0          BILL DATE FILTER                              
         BE    TSTOK                                                            
         TM    BHDTYPF,X'01'       BHRDATE                                      
         BNO   GB29A                                                            
         CLC   BDATE,RDATST                                                     
         BL    TSTNO                                                            
         CLC   BDATE,RDATEND                                                    
         BH    TSTNO                                                            
                                                                                
GB29A    TM    BHDTYPF,X'02'       POST DATE                                    
         BNO   GB29B                                                            
         GOTO1 DATCON,DMCB,(2,BILPOST),(0,WORK)                                 
         CLC   WORK(6),PDATST                                                   
         BL    TSTNO                                                            
         CLC   WORK(6),PDATEND                                                  
         BH    TSTNO                                                            
                                                                                
GB29B    TM    BHDTYPF,X'04'       INVOICE DATE                                 
         BNO   GB29C                                                            
         CLC   BQDATE,IDATST                                                    
         BL    TSTNO                                                            
         CLC   BQDATE,IDATEND                                                   
         BH    TSTNO                                                            
                                                                                
GB29C    TM    BHDTYPF,X'08'       DUE DATE                                     
         BNO   GB29D                                                            
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(0,WORK)                                
         CLC   WORK(6),DDATST                                                   
         BL    TSTNO                                                            
         CLC   WORK(6),DDATEND                                                  
         BH    TSTNO                                                            
         B     TSTOK                                                            
*                                                                               
GB29D    TM    BHDTYPF,X'10'       EDI DATE                                     
         BNO   TSTOK                                                            
         GOTO1 DATCON,DMCB,(2,BEDIDTE),(0,WORK)                                 
         CLC   WORK(6),EDATST                                                   
         BL    TSTNO                                                            
         CLC   WORK(6),EDATEND                                                  
         BH    TSTNO                                                            
         B     TSTOK                                                            
*                                                                               
                                                                                
TSTOK    SR    RE,RE                                                            
TSTNO    LTR   RE,RE                                                            
         XIT1                                                                   
         DROP  R4                                                               
RDATST   DS    CL6                                                              
RDATEND  DS    CL6                                                              
PDATST   DS    CL6                                                              
PDATEND  DS    CL6                                                              
IDATST   DS    CL6                                                              
IDATEND  DS    CL6                                                              
DDATST   DS    CL6                                                              
DDATEND  DS    CL6                                                              
EDATST   DS    CL6                                                              
EDATEND  DS    CL6                                                              
DATLSLEN EQU   *-RDATST                                                         
         EJECT                                                                  
* - READS SPOT00 PROFILE TO SET UP DATE LIST                                    
GOMOBILE NTR1                                                                   
                                                                                
* - HAVE WE ALREADY SET UP DATE LIST                                            
         CLC   NBCLICOD,PROFILSV                                                
         BE    GOMOBILX                                                         
         MVC   PROFILSV(3),NBCLICOD                                             
                                                                                
* - NEED TO PASS THESE ADDS TO MOBILE                                           
* - NOT ENOUGH ROOM TO LINK THEM IN                                             
         LA    R2,MOBILADS                 ADDRESSES FOR MOBILE                 
         XC    0(4,R2),0(R2)               (GETBROAD) LINKED                    
         MVC   4(4,R2),ADDAY                                                    
         MVC   8(4,R2),GETDAY                                                   
         MVC   12(4,R2),DATCON                                                  
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'B3'           GET B3 PROFILE                        
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..IF FILTERING                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK2,(0,(R2))                           
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..MEDIA FILTER                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK,(0,(R2))                            
* - SET B3 PROFILE VALUES INTO 00 DATE AREAS                                    
         MVC   MYWORK+2(1),MYWORK2                                              
         MVC   MYWORK+6(3),MYWORK2+1                                            
         IC    R0,MYWORK+2       DATE CONTROL                                   
* - SET (NBSELSTR - 1 YEAR)  TO DUB FOR BROAD MOBILE DATELIST                   
         L     R4,=F'-450'                                                      
         GOTO1 ADDAY,DMCB,NBSELSTR,MYWORK2,(R4)                                 
         MVC   MYWORK2+6(6),NBSELEND                                            
                                                                                
* - BUILD LONG LIST OF DATE PAIRS                                               
         L     R2,AMYIO                                                         
         LA    R4,MYWORK         PASS ADDRESS OF 00 PROFILE                     
         GOTO1 =V(MOBILE),DMCB,(208,MYWORK2),((R0),(R2)),MOBILADS,(R4)          
                                                                                
*                                  FIND FIRST PERIOD OF A NEW YEAR              
SETD4    DS    0H                                                               
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD6               YES                                          
         CLI   5(R2),0             IF ZERO WE GET LOOP IN SETD8                 
         BE    SETD6                                                            
         LA    R2,4(R2)                                                         
         B     SETD4                                                            
*                                  BUILD  A LIST OF YM, START-END               
SETD6    DS    0H                                                               
         LA    R3,PERLIST                                                       
SETD7    DS    0H                                                               
         ZIC   R0,2(R2)                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE             YEAR                                         
         SR    R4,R4               FOR PER SEQUENCE WITHIN YR                   
SETD8    DS    0H                                                               
         LA    R4,1(R4)                                                         
         MVC   0(1,R3),BYTE        YEAR                                         
         STC   R4,1(R3)            MONTH                                        
         MVC   2(4,R3),0(R2)       START-END OF PER                             
         LA    R3,6(R3)                                                         
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    SETD12              EOL                                          
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD7               YES                                          
         B     SETD8                                                            
*                                                                               
SETD12   DS    0H                                                               
*                                                                               
SETDATEX DS    0H                                                               
GOMOBILX DS    0H                                                               
         XIT1                                                                   
         SPACE 1                                                                
*                                  FIND START OF NEW YEAR                       
*                                  1) A PERIOD THAT SPANS YEAR CHANGE           
*                                     AND BEGINS NO FURTHER AWAY                
*                                     FROM 12/31 THAN IT ENDS                   
*                             OR   2) A PERIOD THAT STARTS BEFORE 1/14          
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R2)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
*                                                                               
MOBILADS DS    4F                  ADDRESSES PASSED TO MOBILE                   
         EJECT                                                                  
* - CLIENT GROUP FILTERING FOR BILL RECORDS                                     
*   KEY HAS BILLING HEADER RECORD                                               
CGRPFILT NTR1                                                                   
         MVC   MYKEY,KEY           SAVE CURRENT BILL REC KEY                    
         MVI   BYTE,0              CLEAR NEW CLIENT FLAG                        
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       ...DO WE NEED NEW CLIENT HEADER              
         BE    VCL2                                                             
*****    CLC   KEY+2(2),=X'BCDB'                                                
*****    BNE   VCLNO                                                            
         MVI   BYTE,C'Y'                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MYKEY+1     AGY/MED + CLIENT                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1         CLIENT REC SITS IN ANETWS1                   
         GOTO1 GETREC                                                           
         L     R1,AMYIO         RESET AIO AREA FOR BHREAD                       
         ST    R1,AIO                                                           
VCL1     XC    KEY,KEY            RESET SEQ READ FOR BILL HEADER RECORD         
         MVC   KEY,MYKEY                                                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
* - CHECK CLIENT GROUP FILTERING                                                
VCL2     LA    R0,5                                                             
         LA    RF,CGRP1                                                         
         CLC   NBSELCGR(1),0(RF)       CHECK SCHEME LETTER                      
         BE    VCL5                                                             
VCL4     LA    RF,3(RF)                                                         
         BCT   R0,*-14                                                          
         B     VCLNO                                                            
VCL5     UNPK  DUB(5),1(3,RF)      UNPK PWOS                                    
         LA    R3,DUB                                                           
         LA    RE,NBSELCGR+1                                                    
         LA    R1,4                                                             
VCL6     CLI   0(RE),X'C1'         IF LETTER OR NUMBER                          
         BL    VCL7                                                             
         CLC   0(1,RE),0(R3)       MUST MATCH                                   
         BNE   VCL4                IF NO MATCH,TEST AGAINST NXT CGRP            
VCL7     LA    RE,1(RE)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,VCL6                                                          
         MVC   NBACTCGR(2),1(RF)   SET CLIENT GROUP CODE  PWOS                  
*                                                                               
         CLI   BYTE,C'Y'           IS IT NEW CLIENT                             
         BNE   VCLYES                                                           
         BAS   RE,SETPRD           YES/SET PRODUCT                              
         BAS   RE,SETEST           YES/SET ESTIMATES                            
         XC    KEY,KEY            RESET SEQ READ FOR BILL HEADR RECS            
         MVC   KEY,MYKEY                                                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   AIO,AMYIO           AND RESET I/O AREA FOR BILL REC              
*                                                                               
VCLYES   SR    RE,RE               CLIENT PASSED TESTS                          
VCLNO    LTR   RE,RE                                                            
VCLX     XIT1                                                                   
*                                                                               
         EJECT                                                                  
* - CLIENT OFFICE FILTERING FOR BILL RECORDS                                    
*   KEY HAS BILLING HEADER RECORD                                               
COFFFILT NTR1                                                                   
         MVC   MYKEY,KEY           SAVE CURRENT BILL REC KEY                    
         MVI   BYTE,0              CLEAR NEW CLIENT FLAG                        
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       ...DO WE NEED NEW CLIENT HEADER              
         BE    COF2                                                             
         MVI   BYTE,C'Y'                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MYKEY+1     AGY/MED + CLIENT                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1         CLIENT REC SITS IN ANETWS1                   
         GOTO1 GETREC                                                           
         L     R1,AMYIO         RESET AIO AREA FOR BHREAD                       
         ST    R1,AIO                                                           
         XC    KEY,KEY            RESET SEQ READ FOR BILL HEADER RECORD         
         MVC   KEY,MYKEY                                                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
* - CHECK CLIENT OFFICE FILTERING                                               
COF2     DS    0H                                                               
         CLI   NBSELOFF,0          IF OFFICE FILTERING                          
         BE    COFOK                                                            
         TM    NBOFFTYP,X'80'      CHECK OFFICE LIST                            
         BO    OFFLIST                                                          
         TM    NBOFFTYP,X'40'      CHECK NEGATIVE FILTER                        
         BO    NEGOFF                                                           
         CLC   COFFICE,NBSELOFF                                                 
         BNE   OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
OFFLIST  DS    0H                                                               
**       XC    DMCB(8),DMCB                                                     
**       MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
**       GOTO1 NBCALLOV,DMCB                                                    
**       CLI   4(R1),X'FF'                                                      
**       BNE   *+6                                                              
**       DC    H'0'                                                             
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB(8),DUB                                                       
         MVI   OFCSYS,C'S'                                                      
         MVI   OFCAUTH,C'$'                                                     
         MVC   OFCAUTH+1(1),NBSELOFF                                            
         OI    OFCAUTH+1,X'C0'                                                  
         MVC   OFCAGY,NBEFFAGY                                                  
*        MVC   OFCOFC,NBEFFOFF                                                  
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
         SPACE 1                                                                
**       L     RF,DMCB                                                          
**       GOTO1 (RF),DMCB,DUB,NBACOM                                             
         GOTO1 AOFFICER,DMCB,DUB,NBACOM                                         
         TM    NBOFFTYP,X'40'      MAY BE NEGATIVE LIST                         
         BO    NEGLIST                                                          
         CLI   0(R1),0             IS OFFICE IN LIST                            
         BNE   OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
NEGLIST  CLI   0(R1),0             CHECK FOR NEGATIVE LIST                      
         BE    OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
NEGOFF   CLC   NBEFFOFF,NBSELOFF                                                
         BE    OFFNO                                                            
*                                                                               
COFOK    CLI   BYTE,C'Y'           IS IT NEW CLIENT                             
         BNE   OFFYES                                                           
         MVC   NBEFFOFF,COFFICE                                                 
         BAS   RE,SETPRD           YES/SET PRODUCT                              
         BAS   RE,SETEST           YES/SET ESTIMATES                            
         XC    KEY,KEY            RESET SEQ READ FOR BILL HEADR RECS            
         MVC   KEY,MYKEY                                                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   AIO,AMYIO           AND RESET I/O AREA FOR BILL REC              
*                                                                               
OFFYES   SR    RE,RE               CLIENT PASSED TESTS                          
OFFNO    LTR   RE,RE                                                            
OFFX     XIT1                                                                   
*                                                                               
         EJECT                                                                  
* - CALLED WHEN CLIENT CHANGES IN READ BILLING REC ROUTINE                      
* - RESETS ESTIMATE MASK FOR NEW CLIENT                                         
*          DATA SET NENETIO    AT LEVEL 048 AS OF 04/06/94                      
SETEST   NTR1                                                                   
         MVC   AIO,NBAIO           READ ESTIMATE REC INTO NABAIO                
         MVI   NBEFFEST,0          PRESET EFFECTIVE NUMBER                      
         XC    KEY,KEY             SET UP GENERAL KEY                           
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
         L     R1,ANETWS1          GET CLIENT RECORD                            
         MVC   EKEYAM(3),1(R1)     SET A/M AND CLIENT IN KEY                    
         MVC   EKEYPRD,NBSELPRD                                                 
         CLC   EKEYPRD,=C'ALL'     IF NBSELPRD=ALL USE POL IN KEY               
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NBSELEST                                                 
         CLI   NBSELESE,0          IF EST RANGE, GET FIRST                      
         BNE   QVEST2                                                           
         CLI   NBSELEST,0          IF ONE ESTIMATE SELECTED                     
         BNE   GETEST                                                           
         MVI   EKEYEST,1           IF ALL, GET FIRST EST                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLI   EKEYEST+1,0         MAKE SURE THIS IS EST                        
         BNE   BADEST                                                           
         B     PROCEST                                                          
         SPACE 1                                                                
QVEST2   DS    0H                  READ FIRST IF ALL OR RANGE                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    PROCEST                                                          
         B     BADEST                                                           
         SPACE 1                                                                
GETEST   DS    0H                          MUST GET SPECIFIC EST                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE         DID WE GET SPECIFIC EST?                 
         BE    GETEST2                                                          
         XC    ESTMSK,ESTMSK     NO/SET MASK = 00                               
         B     ESTOK               AND GET OUT                                  
GETEST2  MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         BE    PROCEST                                                          
         B     BADEST                                                           
         SPACE 1                                                                
PROCEST  MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         MVC   ESTMSK,FFS        SET UP ESTIMATE MASK                           
         CLI   NBSELEST,0          FOR NO/ALL USE FF'S                          
         BE    ESTFILT                                                          
         SPACE 1                                                                
         XC    ESTMSK,ESTMSK                                                    
         ZIC   R1,NBSELEST                                                      
         LA    R2,ESTMSK                                                        
         BAS   RE,SETMASK          FOR SINGLE ESTIMATE                          
         STC   R1,NBEFFEST                                                      
         ZIC   R3,NBSELESE                                                      
         LTR   R3,R3                                                            
         BZ    ESTFILT                                                          
         SPACE 1                                                                
QI38     LA    R1,1(R1)            SET MASK TO RANGE NBSELEST TO NBSELE         
         CR    R1,R3                                                            
         BH    ESTFILT                                                          
         BAS   RE,SETMASK          FOR ESTIMATE RANGE                           
         B     QI38                                                             
         SPACE 1                                                                
ESTFILT  L     R4,NBAIO                                                         
         OC    NBSELEFL,NBSELEFL   PROCESS FILTERS                              
         BZ    ESTOK                                                            
         SPACE 1                                                                
         XC    ESTMSK,ESTMSK   REDO ESTMSK                                      
         LA    R4,KEY                                                           
         MVI   EKEYEST,0                                                        
         SPACE 1                                                                
QI42     LA    R4,KEY                                                           
         CLI   EKEYEST,255                                                      
         BE    ESTOK               LOOP EXIT                                    
         AI    EKEYEST,1           SKIP TO NEXT ESTIMATE                        
         XC    EKEYEST+1(5),EKEYEST+1                                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ESTOK               LOOP EXIT                                    
         CLI   NBSELESE,0          MAKE SURE ITS WITHIN A RANGE                 
         BE    QI44                                                             
         CLC   EKEYEST,NBSELEST                                                 
         BL    QI42                                                             
         CLC   EKEYEST,NBSELESE                                                 
         BH    QI42                                                             
         SPACE 1                                                                
QI44     L     R4,NBAIO            MEETS FILTER                                 
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         LA    R3,NBSELEFL                                                      
         LA    R5,EPROF                                                         
         LA    R0,3                                                             
         SPACE 1                                                                
QI46     CLI   0(R3),C'*'          WILD                                         
         BE    QI48                                                             
         CLI   0(R3),0                                                          
         BE    QI48                                                             
         TM    0(R3),X'40'                                                      
         BZ    QI47                                                             
         CLC   0(1,R3),0(R5)       FILTER                                       
         BNE   QI42                                                             
         B     QI48                                                             
         SPACE 1                                                                
QI47     MVC   HOLDEFLT,0(R5)                                                   
         NI    HOLDEFLT,X'FF'-X'40'   TURN OFF X'40' BIT                        
         CLC   0(1,R3),HOLDEFLT    NEGATIVE FILTER                              
         BE    QI42                                                             
         SPACE 1                                                                
QI48     LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,QI46                                                          
         ZIC   R1,EKEYEST          PASSED FILTERS                               
         LA    R2,ESTMSK                                                        
         BAS   RE,SETMASK          SO SET MASK                                  
         CLI   NBEFFEST,0          SAVE NUMBER OF FIRST GOOD EST                
         BNE   QI42                                                             
         STC   R1,NBEFFEST                                                      
         B     QI42                                                             
         SPACE 1                                                                
BADEST   DC    H'0'                SHOULD NEVER GET HERE                        
         SPACE 1                                                                
ESTOK    DS    0H                                                               
         MVC   AIO,AMYIO           RESTORE I/OAREA POINTER                      
         XIT1                                                                   
         EJECT                                                                  
* - FILL IN PRODMSK FOR PRODUCT GROUP FILTER                                    
*          DATA SET NENETIO    AT LEVEL 054 AS OF 09/27/94                      
SETPRD   NTR1                                                                   
***      XC    PRDMSK,PRDMSK                                                    
         L     RE,=A(PRDMSK)                                                    
         LA    RF,L'PRDMSK                                                      
         XCEF                                                                   
         MVC   AIO,NBAIO                                                        
         LA    R4,KEY              NO PRODUCT NUMBER GIVEN                      
         XC    KEY,KEY                                                          
         USING PRDHDR,R4                                                        
         L     R1,ANETWS1          CLIENT RECORD                                
         MVC   PKEYAM(3),1(R1)        SET A/M AND CLIENT IN KEY                 
         CLC   =C'ALL',NBSELPRD                                                 
         BE    SETPGRP                                                          
         MVC   PKEYPRD,NBSELPRD                                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         B     SETPX                                                            
*                                                                               
SETPGRP  DS    0H                                                               
         CLI   NBSELPGR,0          IF NO PRODUCT GROUP                          
         BNE   QI21                                                             
******   MVC   NBPRDMSK,FFS        SET PRDMSK AND EXIT                          
         XIT1                                                                   
         SPACE 1                                                                
QI21     LA    R4,KEY                                                           
         XC    PKEYPRD,PKEYPRD                                                  
         SPACE 1                                                                
QI22     LA    R4,KEY              GET NEXT PRODUCT                             
         AI    PKEYPRD+2,1                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    QI23                                                             
         MVC   KEY,KEYSAVE         NEED TO RESORE POL                           
         CLC   NBSELPRD,=C'POL'                                                 
         BNE   SETPX                                                            
         MVC   PKEYPRD,=C'POL'                                                  
         GOTO1 HIGH                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         B     SETPX                                                            
         SPACE 1                                                                
QI23     MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
*****    CLC   =C'CF ',PKEYPRD                                                  
*****    BNE   QI22                                                             
         L     R4,NBAIO                                                         
         LA    R2,PGRP1            SELECT ASSIGNMENT (V,W,X)                    
         CLC   NBSELPGR(1),PGRP1                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23A                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP1                                                       
         BE    QI22                                                             
QI23A    LA    R2,PGRP2                                                         
         CLC   NBSELPGR(1),PGRP2                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23B                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP2                                                       
         BE    QI22                                                             
QI23B    LA    R2,PGRP3                                                         
         CLC   NBSELPGR(1),PGRP3                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23C                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP3                                                       
         BE    QI22                                                             
QI23C    LA    R2,PGRP4                                                         
         CLC   NBSELPGR(1),PGRP4                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23D                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP4                                                       
         BE    QI22                                                             
QI23D    LA    R2,PGRP5                                                         
         CLC   NBSELPGR(1),PGRP5                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23DA              NO / GET NEXT REC                            
         MVC   BYTE,NBSELPGR       YES/CHECK IT                                 
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP5                                                       
         BE    QI22                                                             
QI23DA   LA    R2,PGRP6                                                         
         CLC   NBSELPGR(1),PGRP6                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BO    QI22                NO / GET NEXT REC                            
         MVC   BYTE,NBSELPGR       YES/CHECK IT                                 
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP5                                                       
         BE    QI22                                                             
         SPACE 1                                                                
QI24     UNPK  DUB(5),1(3,R2)      SEE IF THIS PRODUCT IS IN                    
         LA    RF,DUB                                                           
         LA    RE,NBSELPGR+1                                                    
         LA    R0,4                                                             
         SPACE 1                                                                
QI25     CLI   0(RE),C'*'          WILD CARD OK                                 
         BE    QI26                                                             
         CLI   0(RE),X'40'         BLANKS/ZERO OK                               
         BNH   QI26                                                             
         CLC   0(1,RE),0(RF)       ELSE IT MUST MATCH                           
         BE    QI26                                                             
         TM    0(RE),X'40'         IS IT NEGATIVE FILTER                        
         BO    QI22                NO                                           
         MVC   BYTE,0(RE)          YES/CHECK IT                                 
         OI    BYTE,X'40'                                                       
         CLC   BYTE,0(RF)                                                       
         BE    QI22                                                             
         SPACE 1                                                                
QI26     LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,QI25                                                          
         SPACE 1                                                                
         LH    R1,PCODE            PASSED THE TESTS SO...                       
***      BAS   RE,FILLPDET         FILL IN DETAILS                              
****     LA    R2,PRDMSK                                                        
***      BAS   RE,SETMASK          SET THIS PRODUCT AS OK                       
***      B     QI22                                                             
         MVC   WORK(3),PKEYPRD                                                  
         BAS   RE,SETOPMSK         SET THIS PRODUCT AS OK                       
         B     QI22                                                             
SETPX    XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
* - FILTER BILLING HEADER                                                       
FILTBH   NTR1                                                                   
         LA    R4,KEY                                                           
         USING BKEY,R4                                                          
         CLI   BHSELCLI,0                                                       
         BE    *+14                                                             
         CLC   BKEYCLT,BHSELCLI                                                 
         BNE   FBX                                                              
         CLI   BHSELPRD,0                                                       
         BE    *+14                                                             
         CLC   BKEYPRD,BHSELPRD                                                 
         BNE   FBX                                                              
         CLI   BHSELEST,0                                                       
         BE    FB8                                                              
         CLI   BHSELESE,0          IS IT ESTIMATE RANGE                         
         BE    FB7                 NO                                           
         CLC   BKEYEST,BHSELEST    YES                                          
         BL    FBNO                                                             
         CLC   BKEYEST,BHSELESE                                                 
         BH    FBNO                                                             
         B     FBYE                                                             
FB7      CLC   BKEYEST,BHSELEST    NO RANGE/MUST MATCH ESTIMATE                 
         BNE   FBX                                                              
FB8      CLI   BHSELSTB,0          YEAR/MONTH OF SERVICE                        
         BE    FB10                                                             
         CLC   BKEYYSRV(2),BHSELSTB                                             
         BL    FBNO                                                             
FB10     CLI   BHSELENB,0                                                       
         BE    FBYE                                                             
         CLC   BKEYYSRV(2),BHSELENB                                             
         BH    FBNO                                                             
FBYE     SR    R4,R4                                                            
*                                                                               
FBNO     LTR   R4,R4                                                            
*                                                                               
FB20     DS    0H                                                               
*                                                                               
FBX      XIT1                                                                   
         DROP  R3,R4                                                            
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
*                                                                               
GETPRD3  NTR1                  RETURNS 3 BYTE PROD IN WORK                      
         L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
         MVI   WORK+3,0                                                         
GP5      CLC   WORK(1),3(R1)                                                    
         BE    GOTPRD3                                                          
         LA    R1,4(R1)                                                         
         BCT   R2,GP5                                                           
         CLI   WORK+3,2                                                         
         BE    GPNO                                                             
         MVI   WORK+3,2                                                         
         LA    R2,35                                                            
         L     R1,ANETWS1                                                       
         LA    R1,CLIST2                                                        
         B     GP5                                                              
GPNO     MVC   WORK(3),=C'***'                                                  
         B     *+10                                                             
GOTPRD3  MVC   WORK(3),0(R1)                                                    
         XIT1                                                                   
                                                                                
*                                                                               
         USING BHBLOCK,R3                                                       
GETPRD1  NTR1                  USES BHACTPRD AND RETURNS                        
         L     R1,ANETWS1      1 BYTE PROD IN WORK                              
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
         MVI   WORK+10,0                                                        
GTP5     CLC   BHACTPRD,0(R1)                                                   
         BE    GOTPRD                                                           
         LA    R1,4(R1)                                                         
         BCT   R2,GTP5                                                          
         CLI   WORK+10,2           ARE WE DEALING WITH CLIST2?                  
         BE    GTP7                YES/SO NO MATCH                              
         MVI   WORK+10,2                                                        
         LA    R2,35                                                            
         L     R1,ANETWS1      RESET ADDRESS                                    
         LA    R1,CLIST2                                                        
         B     GTP5                                                             
GTP7     MVI   WORK,0                                                           
         B     *+10                                                             
GOTPRD   MVC   WORK(1),3(R1)                                                    
         XIT1                                                                   
         DROP  R1,R3                                                            
         EJECT                                                                  
GETBFRML NTR1                                                                   
* - FOR WILA READ PRODUCT AAA RECORD FOR BILLING FORMULA                        
         CLC   =C'WI',NBSELAGY                                                  
         BNE   SBFX                                                             
* - HAVE WE SET BILL FORMULA FOR THIS CLIENT                                    
         CLC   CLTBFML,NBACTCLI                                                 
         BE    SBFX                                                             
         MVC   CLTBFML,NBACTCLI                                                 
         XC    MYKEY,MYKEY                                                      
         MVC   MYKEY+1(3),NBKEY+1  GET AGY/CLIENT                               
         MVC   MYKEY+4(3),=C'AAA'                                               
         MVC   KEYSAVE(13),MYKEY                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',MYKEY,MYKEY                 
         CLC   MYKEY,KEYSAVE                                                    
         BE    *+8                                                              
         B     SBFX                                                             
         LA    R4,MYIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',MYKEY+14,(R4),MYDM          
         USING PRDHDR,R4                                                        
         MVC   BILLFRML,PBILLBAS   GET BILL FORMULA                             
         DROP  R4                                                               
SBFX     XIT1                                                                   
         EJECT                                                                  
*          DATA SET NEWRI20    AT LEVEL 022 AS OF 03/17/95                      
HOOK     NTR1                                                                   
         CLI   GLHOOK,GLHEAD                                                    
         BNE   HOOK2                                                            
         GOTO1 NDGENHED                                                         
         XIT1                                                                   
         SPACE 1                                                                
HOOK2    CLI   GLHOOK,GLPRINT                                                   
         BNE   HOOKX                                                            
         TM    NDLININD,X'80'                                                   
         BNO   *+8                                                              
         MVI   GLHOOK,GLDONT                                                    
         MVI   NDLININD,0          RESET LINE INDICATORS                        
HOOKX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
MYIO2    DS    CL3000                                                           
*PRDMSK   DS    CL32                                                            
PRDMSK   DS    CL2000        500X(4 BYTE PROD CODE)                             
         EJECT                                                                  
*                                                                               
                                                                                
* - WORK AREA FOR RDBELEM                                                       
WRKAREA  DSECT                                                                  
MYDM     DS    CL96                                                             
ABELEM   DS    A                                                                
ADRIVHK  DS    A                  R5 SAVED FOR DRIVER IN CALLING MODULE         
AMYIO    DS    A                   ADDRESS PASSED BY CALLING MODULE             
SVAIO    DS    A                   SAVE CURRENT A(I/O AREA)                     
         DS    0D                                                               
MYDUB    DS    CL8                                                              
COMPLEN  DS    CL1                                                              
BHMANFLG DS    CL1                 MANUAL BILL ONLY                             
BHBFLG   DS    CL2                 B1,B2,B3,ETC FLAG                            
BHDTYPF  DS    CL1                 DATE TYPE FILTER                             
CLTSAV   DS    CL3                                                              
SKIPFLG  DS    CL1                 IF NOT CALLED FROM NEWRI20 = 1               
FIRST    DS    CL1                 FIRST CALL TO BH                             
                                                                                
MYKEY    DS    CL13                                                             
SVKEY    DS    CL20                                                             
CLTRECSV DS    CL13                                                             
PROFILSV DS    CL10                                                             
BYMOS    DS    CL2                 BILLING YR/MOS FROM GETMOS                   
RUNDATE  DS    0CL6                                                             
RUNDYY   DS    CL2                                                              
RUNDMM   DS    CL2                                                              
RUNDDD   DS    CL2                                                              
INVNUMBR DS    CL6         CURRENT INV NUMBER MONTH(2) + NUMBER                 
WORKINV  DS    CL6                 TEMP INVOICE WORK AREA                       
WORKYMD  DS    CL6                 TEMP YYMMDD WORK AREA                        
CURRBINV DS    CL3                 TEMP CL1(Y/M) + CL2(INVNO)                   
CMMLIDSV DS    CL18                CMMLID(8)+PRD(1) X 2                         
PAKWORK  DS    PL12        WORKAREA FOR PACKED INSTRUCTIONS                     
*                                                                               
PKWORK   DS    0PL16               PACK INSTRUCTIONS WORKAREA                   
PKQUOT   DS    PL8                 QUOTIENT                                     
PKRMDR   DS    PL8                 REMAINDER                                    
*                                                                               
PKDVSR   DS    PL8                 DIVISOR                                      
*                                                                               
CLTBFML  DS    CL2         CLIENT OF BILLFRML                                   
BILLFRML DS    CL5         BILL FORMULA FROM EST/PROD/AAA REC                   
*                                                                               
MYWORK   DS    CL100                                                            
MYWORK2  DS    CL100                                                            
SCANBLK  DS    CL420                                                            
VCASHIER DS    V                   V(CASHIER)                                   
ACSHIERC DS    A                   A(CASHIER CONTROL BLOCK)                     
SVBHDELA DS    A                   A(CASHIER BUFFER ENTRY)                      
SVMBTELA DS    A                   A(MEDIA TRANSFER    ELEMENT)                 
SVTRNELA DS    A                   A(CASH  TRANSACTION ELEMENT)                 
SVLSQN   DS    XL1                 LINE SEQUENCE NUMBER SAVEAREA                
SVBKEY   DS    XL(L'BKEY)          BILL RECORD KEY SAVEAREA                     
BASEDATE DS    CL6                 BASE DATE FOR DAYS TO DISBURSE               
DISBDATE DS    CL6                 DISBURSEMENT DATE                            
SVBGRS   DS    PL8                 BILLED GROSS SAVEAREA                        
SVBNET   DS    PL8                 BILLED NET   SAVEAREA                        
SVRDBELS DS    X                   NBRDBELS SWITCH SAVEAREA                     
*                                                                               
WBILLED  DS    PL8                 BILLED AMOUNT FOR CASH APPLIED               
WPAID    DS    PL8                 PAID   AMOUNT FOR CASH APPLIED               
*                                                                               
B1XPROF  DS    CL25                                                             
B1PROF   DS    CL25                                                             
*                                                                               
PERLIST  DS    XL(15*13*6+1)       15YRS X 13MNTHS X 6                          
*                                                                               
         DS    0D                                                               
SVCSHREC DS    XL8192              CASHIER RECORD SAVEAREA                      
*                                                                               
WRKLEN   EQU   *-WRKAREA                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
BHBLOCKD DSECT                                                                  
       ++INCLUDE BHBLOCKBOB                                                     
*                                                                               
* NEGENINCLS                                                                    
* NEDATELSTD                                                                    
* DRGLOBAL                                                                      
* SPGENPRD                                                                      
* NEGENCOM                                                                      
* NECOMBLOK                                                                     
* SPGENBILL                                                                     
* SPBVALD                                                                       
* NEGBLOCKD                                                                     
* NEGENUNIT                                                                     
* NEGENPACK                                                                     
* SPGENCLT                                                                      
* DDCAHIERD                                                                     
* SPTRCMLCLS                                                                    
* SPTRCMML                                                                      
* DDOFFICED                                                                     
* ACGENFILE                                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLN                                                     
       ++INCLUDE NEDATELSTD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENCOM                                                       
       ++INCLUDE NECOMBLOK                                                      
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPBVALD                                                        
       ++INCLUDE NEGBLOCKD                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDCASHIERD                                                     
       ++INCLUDE SPTRCMLCLS                                                     
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE0D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEWRI82   04/24/17'                                      
         END                                                                    
