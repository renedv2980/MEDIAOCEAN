*          DATA SET SPWRI09    AT LEVEL 031 AS OF 12/15/04                      
*PHASE T20409A,*                                                                
         TITLE 'T20409 - P&&G AGENCY DATA COLLECTION'                           
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI09 (T20409) - SPOT P&G REPORT                       *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 29APR02 29 EFJ -- CHANGE AGY CODE FOR CLT PG FOR STARCOM          *           
* 17APR02 28 EFJ -- ALMOST GOT L27 RIGHT!                           *           
* 31JAN02 27 EFJ -- ESTIMATE HEADER CONVERSION                      *           
* 12FEB02 26 AKAT-- PUT SPG"NAME" (DEFUALT SPGTAPE) ON PRINT QUEUE  *           
* 27SEP01 25 EFJ -- COMMISSION OPTION (BYCST-BYNET)                 *           
* 23AUG01 24 EFJ -- REPORT BUY$ AT NET INSTEAD OF GROSS             *           
*                -- AND FIX SPLITBUY CODE!!!                        *           
* 14JUL01 21 EFJ -- HISTORY LOST                                    *           
*                -- CHANGE SORT ORDER OF F4, F5, AND F6 RECORDS     *           
*                                                                   *           
*********************************************************************           
T20409   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20409,RA                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     OI    SBQSKIP,SBQSKBUY+SBQSKGL+SBQSKBIL                                
         MVI   SBQSEPES,C'Y'       ENSURE EST=ALL                               
         OI    SBQPER,SBQPMN       MONTHS                                       
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         OI    DATAIND2,DIEST      ESTIMATE                                     
         XC    LEVELS,LEVELS                                                    
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
         BZ    INIMASTX            NOT OFF-LINE                                 
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(R1) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         MVC   REMOTDSC(3),=CL3'SPG'       REPORT QUEUE DESCRIPTION             
         CLI   WRINAMH+5,0                                                      
         BNE   *+14                                                             
         MVC   REMOTDSC+3(8),=CL8'TAPE'    DEFAULT (NO NAME GIVEN)              
         B     *+10                                                             
         MVC   REMOTDSC+3(8),WRINAM        NAME FROM SCREEN                     
*                                                                               
INIMASTX DS    0H                                                               
*                                                                               
         MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
*                                                                               
         LA    R2,PNGPCTH          99.3% OPTION                                 
         MVI   PCTOPT,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    INIT2                                                            
         MVC   PCTOPT,8(R2)                                                     
         CLI   8(R2),C'N'                                                       
         BE    INIT2                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   EINV                                                             
*                                                                               
INIT2    LA    R2,PNGCOMH          COMMISSION ONLY OPTION?                      
         MVI   CMMOPT,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    INIT10                                                           
         MVC   CMMOPT,8(R2)                                                     
         CLI   8(R2),C'N'                                                       
         BE    INIT10                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   EINV                                                             
         OI    DATAIND2,DIEFFCST                                                
*                                                                               
INIT10   LA    R2,PNGTITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    CLI   SBQBPRD,X'FF'       TEST PRD=POL REQUEST                         
         BNE   *+12                NO                                           
         MVI   SBQBPRD,0           YES-BREAK OUT THE PRODUCTS                   
         OI    SBQPIND,SBQPOLSP                                                 
         B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLOUTPUT     HOOK BEFORE DRIVER OUTPUT                    
         BE    PGEST                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'IF4     ',A(IF4)                                             
         DC    CL8'OF4     ',A(OF4)                                             
         DC    CL8'IF5     ',A(IF5)                                             
         DC    CL8'OF5     ',A(OF5)                                             
         DC    CL8'IF6     ',A(IF6)                                             
         DC    CL8'OF6     ',A(OF6)                                             
         DC    CL8'IF5PRD  ',A(IF5PRD)                                          
         DC    CL8'IF5EST  ',A(IF5EST)                                          
         DC    CL8'OF5EST  ',A(OF5EST)                                          
         DC    CL8'IERR    ',A(IERR)                                            
         DC    CL8'OERR    ',A(OERR)                                            
         DC    CL8'IDOL    ',A(IDOL)                                            
         DC    CL8'ODOL    ',A(ODOL)                                            
         DC    CL8'HDOL    ',A(HDOL)                                            
         DC    CL8'ISPACES ',A(ISPACES)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVI   GLOPTS,0            DOWNLOAD OPTION                              
         CLI   DOWNOPT,0                                                        
         BE    *+12                                                             
         MVI   GLOPTS,C'D'                                                      
         OI    GLDOWNLD,GLDLNOHD+GLDLNOTR+GLDLALPH                              
         OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   *+8                                                              
         MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
IF4      MVI   0(R2),C'A'          MAINTENANCE CODE                             
         MVC   1(3,R2),CHRGPER     CHARGE PERIOD                                
         MVC   4(2,R2),PGAGY       AGENCY                                       
         MVC   6(6,R2),ACCOUNT     ACCOUNT                                      
         MVC   12(4,R2),BRAND      BRAND                                        
         CLI   NOBRAND,C'Y'                                                     
         BNE   *+10                                                             
         MVC   12(4,R2),BRANDCD                                                 
         MVC   16(4,R2),ESTIMATE   ESTIMATE                                     
         MVC   20(2,R2),=C'F4'                                                  
         MVC   22(2,R2),SPACES     FILLER                                       
         GOTO1 DATCON,DMCB,(0,SBQTODAY),(X'20',DUB)  NON-FUNNY DATE             
         MVC   24(4,R2),DUB+2      ESTIMATE DATE MMDDYY                         
         MVC   28(2,R2),DUB                                                     
         MVC   30(6,R2),EVENTCD    EVENT CODE                                   
         MVC   36(8,R2),SPACES     FILLER                                       
         MVC   44(1,R2),MLTBRND    MULTI-BRAND FLAG                             
         B     XIT                                                              
*                                                                               
OF4      MVC   WORK(45),0(R2)                                                   
         MVC   0(2,R2),WORK+20                                                  
         MVC   2(20,R2),WORK                                                    
         MVC   22(23,R2),WORK+22                                                
         MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
IF5      MVI   0(R2),C'A'          MAINTENANCE CODE                             
         MVC   1(3,R2),CHRGPER     CHARGE PERIOD                                
         MVC   4(2,R2),PGAGY       AGENCY                                       
         MVC   6(6,R2),ACCOUNT     ACCOUNT                                      
         MVC   12(4,R2),BRAND      BRAND                                        
         CLI   NOBRAND,C'Y'                                                     
         BNE   *+10                                                             
         MVC   12(4,R2),BRANDCD                                                 
         MVC   16(4,R2),ESTIMATE   ESTIMATE                                     
         MVC   20(2,R2),=C'F5'                                                  
         MVC   22(2,R2),SPACES     FILLER                                       
         B     XIT                                                              
*                                                                               
OF5      MVC   WORK(24),0(R2)                                                   
         MVC   0(2,R2),WORK+20                                                  
         MVC   2(20,R2),WORK                                                    
         MVC   22(02,R2),WORK+22                                                
         MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
IF6      MVI   0(R2),C'A'          MAINTENANCE CODE                             
         MVC   1(3,R2),CHRGPER     CHARGE PERIOD                                
         MVC   4(2,R2),PGAGY       AGENCY                                       
         MVC   6(6,R2),ACCOUNT     ACCOUNT                                      
         MVC   12(4,R2),BRANDCD    BRAND CODE (FROM TABLE)                      
         MVC   16(4,R2),ESTIMATE   ESTIMATE                                     
         MVC   20(2,R2),=C'F6'                                                  
         MVC   22(2,R2),BRDSUFF    BRAND SUFFIX (DDS PRODUCT CODE)              
         MVC   24(4,R2),BRAND      BRAND                                        
         B     XIT                                                              
*                                                                               
OF6      MVC   WORK(28),0(R2)                                                   
         MVC   0(2,R2),WORK+20                                                  
         MVC   2(20,R2),WORK                                                    
         MVC   22(06,R2),WORK+22                                                
         MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
IF5PRD   MVC   0(3,R2),F5PRD       PRODUCT FOR F5 REPORT                        
         B     XIT                                                              
*                                                                               
IF5EST   MVC   0(1,R2),SBBEST      ESTIMATE FOR F5 REPORT                       
         B     XIT                                                              
*                                                                               
OF5EST   ZIC   RE,0(R2)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         B     XIT                                                              
         SPACE 2                                                                
IDOL     XC    0(4,R2),0(R2)       MONTHLY DOLLARS                              
         CLC   SBPRD,=C'POL'       PASS NOTHING FOR POL                         
         BE    XIT                                                              
         CLI   READMODE,RDBUY      TEST READING BUYS                            
         BNE   IDOL1                                                            
         CLC   SBACURCH(1),GLARGS  YES-DOLLARS COME FROM CHUNKS                 
         BNE   XIT                 (MONTH NUM IS IN SBACURCH)                   
         L     R3,SBACURCH                                                      
         USING SCHUNKD,R3                                                       
         L     RE,SCNET                                                         
         CLI   CMMOPT,C'Y'                                                      
         BNE   IDOL4                                                            
         L     RE,SCEFFCST                                                      
         S     RE,SCNET                                                         
         B     IDOL4                                                            
*                                                                               
IDOL1    CLI   READMODE,RDBILL     TEST READING BILLS                           
         BNE   IDOL2                                                            
         CLC   SBACURCH(1),GLARGS  YES-MATCH MONTH NUMBER                       
         BNE   XIT                                                              
         L     R1,SBACURCH         R1=A(GROSS BILL AMOUNT)                      
         PACK  DUB,0(L'BAMT,R1)                                                 
         CVB   RE,DUB                                                           
         B     IDOL6                                                            
*                                                                               
IDOL2    ZIC   RE,GLARGS           DOLLARS FROM ESTIMATE BUCKETS                
         BCTR  RE,0                                                             
         SLL   RE,1                                                             
         LA    RE,REQMNTHS(RE)                                                  
         OC    0(2,RE),0(RE)                                                    
         BZ    XIT                                                              
         CLC   ESTSTMN,0(RE)                                                    
         BH    XIT                                                              
         CLC   ESTENMN,0(RE)                                                    
         BL    XIT                                                              
         ZIC   RF,1(RE)                                                         
         BCTR  RF,0                                                             
**NOP         SLL   RF,2                                                        
         MHI   RF,6                                                             
         LA    RF,ORDERDOL(RF)                                                  
**NOP         L     RE,0(RF)                                                    
**NOP         A     RE,52(RF)                                                   
         ZAP   DUB,0(6,RF)                                                      
         AP    DUB,78(6,RF)                                                     
         B     IDOL8                                                            
*                                                                               
IDOL4    CLI   PCTOPT,C'Y'         99.3% OPTION                                 
         B     IDOL6                                                            
*&&DO                                                                           
         BNE   IDOL6                                                            
         CVD   RE,DUB                                                           
         ZAP   BIG,DUB                                                          
         MP    BIG,=PL2'993'                                                    
         MP    BIG,=PL1'2'                                                      
         DP    BIG,=PL8'1000'                                                   
         ZAP   DUB,BIG(8)                                                       
         CP    DUB,=P'0'                                                        
         BL    *+10                                                             
         AP    DUB,=P'1'                                                        
         ZAP   BIG,DUB                                                          
         DP    BIG,=PL8'2'                                                      
         ZAP   DUB,BIG(8)                                                       
         CVB   RE,DUB                                                           
*&&                                                                             
*                                                                               
**NOP  IDOL6    ST    RE,0(R2)                                                  
IDOL6    CVD   RE,DUB                                                           
*                                                                               
IDOL8    ZAP   0(8,R2),DUB                                                      
         B     XIT                                                              
*                                                                               
**NOP ODOL     ICM   RE,15,0(R2)                                                
**NOP         CVD   RE,DUB                                                      
ODOL     ZAP   DUB,0(8,R2)                                                      
         OI    DUB+7,X'0F'                                                      
         L     R1,GLADTENT                                                      
         ZIC   RF,DROLEN-DROD(R1)                                               
         LR    RE,RF                                                            
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R3),DUB                                                      
         TM    0(R2),X'80'         TEST NEGATIVE                                
         BZ    XIT                                                              
         SH    RE,=H'3'            YES-OVERPUNCH LAST DOLLAR POSITION           
         LA    RE,0(RE,R3)                                                      
         NI    0(RE),X'DF'                                                      
         B     XIT                                                              
*                                                                               
HDOL     MVC   0(7,R3),=C'DOLLARS'                                              
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,1                                                             
         LA    RE,REQMNTHS(RE)                                                  
         MVC   FULL(2),0(RE)                                                    
         MVI   FULL+2,1                                                         
         LA    R5,198(R3)                                                       
         GOTO1 DATCON,DMCB,(3,FULL),(6,(R5))                                    
         B     XIT                                                              
         SPACE 2                                                                
ISPACES  L     R1,GLADTENT                                                      
         ZIC   RE,DRINLEN-DRIND(R1)                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),SPACES                                                   
         B     XIT                                                              
         SPACE 2                                                                
IERR     MVC   0(1,R2),ERRCD       ERROR CODE                                   
         CLI   GLARGS,4            TEST F4 REPORT AND PRD=POL                   
         BNE   *+18                                                             
         CLC   SBPRD,=C'POL'                                                    
         BE    IERR2                                                            
         B     XIT                                                              
         CLI   GLARGS,5            OR F5 REPORT AND PRD=POL                     
         BNE   XIT                                                              
         CLC   F5PRD,=C'POL'                                                    
         BNE   XIT                                                              
IERR2    NI    0(R2),255-ERRBRN    YES-IGNORE BRAND ERROR                       
         B     XIT                                                              
*                                                                               
OERR     MVC   ERRCD,0(R2)                                                      
         LA    R1,F4ERRMSK                                                      
         CLI   GLARGS,4                                                         
         BE    OERR2                                                            
         LA    R1,F5ERRMSK                                                      
         CLI   GLARGS,5                                                         
         BE    OERR2                                                            
         LA    R1,F6ERRMSK                                                      
         CLI   GLARGS,6                                                         
         BE    OERR2                                                            
         DC    H'0'                                                             
*                                                                               
OERR2    NC    ERRCD,0(R1)                                                      
         LR    R5,R3                                                            
         TM    ERRCD,ERRCHP                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'CHP,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRAGY                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'AGY,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRACC                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'ACC,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRBRN                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'BRN,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERREST                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'EST,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERREVC                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'EVC,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRMBR                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'MBR,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRBRC                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'BRC,'                                                 
         LA    R5,4(R5)                                                         
         CR    R5,R3                                                            
         BE    XIT                                                              
         BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   XIT                                                              
         MVI   0(R5),C' '                                                       
         B     XIT                                                              
         EJECT                                                                  
* HOOK BEFORE DRIVER OUTPUT - READ PG ESTIMATE RECORDS AND CALL DRIVER          
* FOR INPUT                                                                     
*                                                                               
PGEST    CLI   PCTOPT,C'Y'         TEST 99.3% OPTION                            
         BNE   PG1                                                              
         L     R1,SBAOFFBF         YES-NEED SPOT TABLE AND CHUNK AREA           
         L     RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         ST    R1,SBASPTTB                                                      
         SR    RE,RE               L'SPTTAB=2/3 OF AVAILABLE                    
         SLDA  RE,2                                                             
         D     RE,=F'3'                                                         
         LA    RE,1(RF)                                                         
         SRL   RE,1                                                             
         ST    RE,SBLSPTTB                                                      
         AR    R1,RE                                                            
         ST    R1,SBACHUNK                                                      
*                                                                               
PG1      XC    REQMNTHS,REQMNTHS   SET REQUEST MONTHS                           
         LA    R2,REQMNTHS                                                      
         L     R3,AMONTHS                                                       
         ST    R3,SBADATE                                                       
         SR    R5,R5                                                            
         LA    R0,6                                                             
*                                                                               
PG2      CLI   0(R3),0                                                          
         BE    PG3                                                              
         GOTO1 DATCON,DMCB,(2,2(R3)),(3,FULL)                                   
         MVC   0(2,R2),FULL                                                     
         LA    R2,2(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,PG2                                                           
*                                                                               
PG3      ST    R5,SBNDATES                                                      
         L     R5,SBAESTTB                                                      
         LA    R2,1                R2=PRODUCT NUMBER                            
         LA    R7,255              TRY EVERY PRD INCLUDING POL                  
*                                                                               
PG4      CH    R2,=H'254'          SKIP UNALLOCATED                             
         BE    PG24                                                             
         STC   R2,SBBPRD           GET PRODUCT DETAILS                          
         LR    RE,R2                                                            
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH                                                     
         MVC   SBPRDNM,PBNAME                                                   
         DROP  R1                                                               
         XC    PGBRAND,PGBRAND                                                  
         SR    R3,R3               R3=ESTIMATE NUMBER                           
         LA    R6,256              TRY ESTIMATES 0 TO 255                       
         CH    R2,=H'255'          TEST PRD=POL                                 
         BNE   PG5                                                              
         LA    R3,1                YES-DON'T BOTHER WITH ESTIMATE 0             
         BCTR  R6,0                                                             
*                                                                               
PG5      ST    R5,SVREG5                                                        
         LTR   R3,R3               TEST ESTIMATE=0                              
         BNZ   *+18                                                             
         OC    0(255,R5),0(R5)     YES-CHECK THAT THERE ARE ANY                 
         BZ    PG24                    ESTIMATES OPEN FOR THIS PRODUCT          
         B     *+12                                                             
         CLI   0(R5),0             TEST ESTIMATE ACTIVE                         
         BE    PG22                                                             
         STC   R3,SBBEST           YES                                          
         XC    KEY,KEY             READ PG ESTIMATE RECORD                      
         LA    R5,KEY              FOR THIS PRODUCT/ESTIMATE                    
         USING PGESTD,R5                                                        
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         MVC   PGKAM,SBBAGYMD                                                   
         MVC   PGKCLT,SBBCLT                                                    
         MVC   PGKPRD,SBPRD                                                     
         STC   R3,PGKEST                                                        
         GOTO1 HIGH                                                             
         MVI   GLOPTS+2,C'Y'       GENERATE F4 REPORT                           
         CLC   KEY(PGKLENQ),KEYSAVE    TEST RECORD FOUND                        
         BE    PG6                                                              
         CLI   SBBEST,0            NO-GIVE UP FOR ESTIMATE=0                    
         BE    PG22                                                             
         CH    R2,=H'255'          EXCEPT FOR PRDUCT POL,                       
         BE    PG22                                                             
         MVC   KEY,KEYSAVE         TRY POL RECORD                               
         MVC   PGKPRD,=C'POL'                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(PGKLENQ),KEYSAVE    TEST RECORD FOUND                        
         BNE   PG22                    NO-GO ON TO NEXT ESTIMATE                
         MVI   GLOPTS+2,C'N'       SUPPRESS F4 REPORT                           
*                                                                               
PG6      LA    R1,PGDATA                                                        
*                                                                               
PG7      CLI   0(R1),0                 CLEAR ALL P&G DATA FIELDS                
         BE    PG8                                                              
         L     RE,8(R1)                                                         
         ZIC   RF,12(R1)                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         XC    0(0,RE),0(RE)                                                    
         LA    R1,16(R1)                                                        
         B     PG7                                                              
*                                                                               
PG8      MVI   NOBRAND,C'Y'        NOBRAND DEFAULT = Y                          
         L     R5,AIO1                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   F5PRD,PGKPRD        SET PRODUCT FOR F5 REPORT                    
         LA    RF,PGKEDQ(R5)       SCAN ELEMENTS FOR PG DATA                    
         SR    R0,R0                                                            
*                                                                               
PG9      CLI   0(RF),0                                                          
         BE    PG12                                                             
         CLI   0(RF),PGSTEIDQ                                                   
         BNE   PG11                                                             
         USING PGSTELMD,RF                                                      
         LA    R1,PGDATA                                                        
*                                                                               
PG10     CLI   0(R1),0                                                          
         BE    PG11                                                             
         CLC   PGSTNAME,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,16(R1)                                                        
         B     PG10                                                             
         ZIC   RE,12(R1)                                                        
         BCTR  RE,0                                                             
         L     R1,8(R1)                                                         
         OC    PGSTDATA,SPACES                                                  
         EX    RE,*+4                                                           
         MVC   0(0,R1),PGSTDATA                                                 
*                                                                               
PG11     IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     PG9                                                              
*                                                                               
PG12     CLI   SBBEST,0            TEST ESTIMATE 0 RECORD                       
         BNE   *+14                                                             
         MVC   PGBRAND,BRAND       YES-SAVE THE BRAND CODE                      
         B     PG22                                                             
         CLC   PGKPRD,=C'POL'      TEST DIDN'T FIND BRAND RECORD                
         BNE   PG12A                                                            
         XC    BRAND,BRAND         YES-RESET BRAND CODE                         
         OC    PGBRAND,PGBRAND     TEST ESTIMATE 0 BRAND CODE FOUND             
         BZ    PG12A                                                            
         MVC   BRAND,PGBRAND       YES-USE THAT ONE                             
*                                                                               
PG12A    MVI   ERRCD,0             INITIALIZE ERROR CODE                        
         LA    R1,BRANDTAB                                                      
         MVC   BRANDCD,=C'0000'    SET BRAND CODE AND AGENCY CODE               
         MVC   PGAGY,=C'00'                                                     
*                                                                               
PG13     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         OI    ERRCD,ERRBRC+ERRAGY                                              
         B     PG14                                                             
         CLC   SBAGY,0(R1)                                                      
         BNE   PG13A                                                            
         MVC   BRANDCD,2(R1)                                                    
         MVC   PGAGY,4(R1)                                                      
         CLC   SBAGY,=C'H9'        FOR STARCOM CLT PG, OVERRIDE PGAGY           
         BNE   PG14                 TO 02                                       
         CLC   SBCLT,=C'PG '                                                    
         BNE   PG14                                                             
         MVC   PGAGY,=C'02'                                                     
         MVC   BRANDCD+2,=C'02'                                                 
         B     PG14                                                             
*                                                                               
PG13A    LA    R1,6(R1)                                                         
         B     PG13                                                             
*                                                                               
BRANDTAB DC    CL2'NW',CL4'0304'    BRAND CODE TABLE                            
         DC    CL2'DF',CL4'0303'                                                
         DC    CL2'H9',CL4'0301'                                                
         DC    X'00'                                                            
*                                                                               
PG14     LA    R1,PGDATA           SET ERROR CODES                              
*                                                                               
PG15     CLI   0(R1),0                                                          
         BE    PG16                                                             
         L     RE,8(R1)                                                         
         ZIC   RF,12(R1)                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SPACES                                                   
         BH    *+10                                                             
         OC    ERRCD,13(R1)                                                     
         LA    R1,16(R1)                                                        
         B     PG15                                                             
*                                                                               
PG16     MVI   GLOPTS+1,C'Y'       F6 OPTION                                    
         CH    R2,=H'255'          NO F6 FOR PRD=POL                            
         BE    PG17                                                             
         CLI   MLTBRND,C'Y'        OR MULTI-BRAND = Y                           
         BE    PG17                                                             
         CLI   NOBRAND,C'Y'        OR NOBRAND = N                               
         BE    *+8                                                              
PG17     MVI   GLOPTS+1,C'N'                                                    
*                                                                               
         CH    R2,=H'255'          TEST PRD=POL                                 
         BE    PG19                YES-CALL DRIVER ONCE WITH $0                 
         CLC   SRSTART,SPACES      TEST SHORT RATE ESTIMATE                     
         BNH   PG18                                                             
         CLI   ORIGEST,0           YES-CHECK THERE'S AN ORIGINAL EST            
         BE    PG20                                                             
         MVI   READMODE,RDBILL     YES-READ BILLS INSTEAD                       
         BAS   RE,READBILL                                                      
         B     PG20                                                             
*                                                                               
PG18     CLI   PCTOPT,C'Y'         TEST 99.3% OPTION                            
         BNE   *+16                                                             
         MVI   READMODE,RDBUY      YES-READ BUYS INSTEAD                        
         BAS   RE,READBUYS                                                      
         B     PG20                                                             
         MVI   READMODE,RDEST      READ ESTIMATE RECORD                         
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING ESTHDRD,R5                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,SBPRD                                                    
         STC   R3,EKEYEST                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO1                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
*         MVC   ORDERDOL,EORDN      EXTRACT ORDERED DOLLARS                     
         MVC   ORDERDOL(EORDX-EORD),EORD     EXTRACT ORDERED DOLLARS            
*                                                                               
         GOTO1 GETBROAD,DMCB,(1,ESTART),WORK,GETDAY,ADDAY EST START MON         
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,FULL)                                  
         MVC   ESTSTMN,FULL                                                     
         ZIC   RE,ESTSTMN          GET 12TH MONTH OF ESTIMATE                   
         ZIC   RF,ESTSTMN+1                                                     
         CLI   ESTSTMN+1,1                                                      
         BNE   *+12                                                             
         LA    RF,12                                                            
         B     *+10                                                             
         LA    RE,1(RE)                                                         
         BCTR  RF,0                                                             
         STC   RE,ESTENMN                                                       
         STC   RF,ESTENMN+1                                                     
*                                                                               
PG19     MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER         CALL DRIVER FOR INPUT                        
*                                                                               
PG20     L     R5,SVREG5                                                        
*                                                                               
PG22     LA    R3,1(R3)            NEXT ESTIMATE                                
         L     R5,SVREG5                                                        
         CH    R3,=H'1'            TEST ESTIMATE WAS 0                          
         BE    *+8                 YES-R5 ALREADY AT EST=1 POSITION             
         LA    R5,1(R5)                                                         
         BCT   R6,PG5                                                           
*                                                                               
PG24     LR    RE,R2               NEXT PRODUCT                                 
         SLL   RE,8                                                             
         L     R5,SBAESTTB                                                      
         AR    R5,RE                                                            
         LA    R2,1(R2)                                                         
         BCT   R7,PG4                                                           
*                                                                               
PGX      B     XIT                                                              
         SPACE 2                                                                
         DS    0F                                                               
PGDATA   DC    CL8'CHRGPER ',AL4(CHRGPER),AL1(L'CHRGPER,ERRCHP,0,0)             
         DC    CL8'ACCOUNT ',AL4(ACCOUNT),AL1(L'ACCOUNT,ERRACC,0,0)             
         DC    CL8'BRAND   ',AL4(BRAND),AL1(L'BRAND,ERRBRN,0,0)                 
         DC    CL8'ESTIMATE',AL4(ESTIMATE),AL1(L'ESTIMATE,ERREST,0,0)           
         DC    CL8'EVENTCD ',AL4(EVENTCD),AL1(L'EVENTCD,ERREVC,0,0)             
         DC    CL8'MLTBRND ',AL4(MLTBRND),AL1(L'MLTBRND,ERRMBR,0,0)             
         DC    CL8'NOBRAND ',AL4(NOBRAND),AL1(L'NOBRAND,0,0,0)                  
         DC    CL8'FISYREND',AL4(SRSTART),AL1(L'SRSTART,0,0,0)                  
         DC    CL8'ACCEST  ',AL4(ORIGEST),AL1(L'ORIGEST,0,0,0)                  
         DC    CL8'BRDSUFF ',AL4(BRDSUFF),AL1(L'BRDSUFF,0,0,0)                  
         DC    X'00'                                                            
         EJECT                                                                  
* ROUTINE TO READ BILL RECORDS                                                  
*                                                                               
READBILL NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING BILLRECD,R5                                                      
         MVC   BKEYAM,SBBAGYMD                                                  
         MVC   BKEYCLT,SBBCLT                                                   
         MVC   BKEYPRD,SBPRD                                                    
         MVC   BKEYEST,ORIGEST     ORIGINAL ESTIMATE                            
*                                                                               
BL2      GOTO1 HIGH                                                             
         B     BL6                                                              
*                                                                               
BL4      GOTO1 SEQ                                                              
*                                                                               
BL6      CLC   KEY(BKEYYSRV-BKEY),KEYSAVE  TEST DONE WITH ESTIMATE              
         BNE   BLX                                                              
         L     R5,AIO1             GET THE BILL RECORD                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         CLC   BDATE,SRSTART       BILL DATE MUST BE ON OR AFTER SHORT          
         BL    BL4                 RATE START                                   
         GOTO1 DATCON,DMCB,BDATE,(2,HALF)                                       
         LA    R1,BAMT                                                          
         ST    R1,SBACURCH                                                      
         L     R1,AMONTHS          GET THE RELATIVE MONTH NUMBER                
         LA    R0,6                                                             
         LA    RE,1                                                             
*                                                                               
BL8      CLC   HALF,0(R1)                                                       
         BL    *+14                                                             
         CLC   HALF,2(R1)                                                       
         BNH   BL10                                                             
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BL8                                                           
         B     BL4                                                              
*                                                                               
BL10     STC   RE,SBACURCH         SAVE MONTH NUMBER                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER         CALL DRIVER FOR INPUT                        
         MVI   GLOPTS+2,C'N'       ONLY NEED R4 RECORD ONCE                     
         B     BL4                                                              
*                                                                               
BLX      B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO READ BUYS RECORDS FOR AN ESTIMATE                                  
*                                                                               
READBUYS NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING BUYRECD,R5                                                       
         MVC   BUYKAM,SBBAGYMD                                                  
         MVC   BUYKCLT,SBBCLT                                                   
         MVC   BUYKPRD,SBBPRD                                                   
*                                                                               
RB2      GOTO1 HIGH                                                             
         B     RB6                                                              
*                                                                               
RB4      GOTO1 SEQ                                                              
*                                                                               
RB6      CLC   KEY(BUYMSTA-BUYKEY),KEYSAVE  TEST DONE WITH PRODUCT              
         BNE   RBX                                                              
         LA    R5,KEY                                                           
         CLC   BUYKEST,SBBEST      MATCH ESTIMATE NUMBER                        
         BE    RB8                                                              
         BL    *+14                                                             
         MVC   BUYKEST(4),XFF      HIGH-SKIP TO NEXT STATION                    
         B     RB2                                                              
         MVC   BUYKEST,SBBEST      LOW-SKIP TO ESTIMATE                         
         XC    BUYKBUY,BUYKBUY                                                  
         B     RB2                                                              
*                                                                               
RB8      L     R5,AIO1             GET THE BUY RECORD                           
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   SBEPRD,SBBPRD                                                    
         LA    RF,SBLOCK           SPLIT BUYS                                   
         AHI   RF,SBEFLAG2-SBLOCK                                               
         OI    0(RF),SBESPLBY                                                   
*                                                                               
RB9      GOTO1 SPOTBUY,DMCB,SBLOCK       CALL SPOTBUY                           
         L     R3,SBACHUNK                                                      
         USING SCHUNKD,R3          LOOP THROUGH THE CHUNKS                      
*                                                                               
RB10     OC    SCNEXT,SCNEXT       TEST END OF CHUNKS                           
         BZ    RB18                                                             
         CLC   SCPRD1,SBBPRD       CHECK PRODUCT'S CORRECT                      
         BNE   RB16                                                             
         OC    SCNET,SCNET         CHECK THERE'S MONEY                          
         BZ    RB16                                                             
         ST    R3,SBACURCH         SET A(CHUNK)                                 
         L     R1,AMONTHS          GET THE RELATIVE MONTH NUMBER                
         LA    R0,6                                                             
         LA    RE,1                                                             
*                                                                               
RB12     CLC   SCDATE,0(R1)                                                     
         BE    RB14                                                             
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,RB12                                                          
         B     RB16                                                             
*                                                                               
RB14     STC   RE,SBACURCH         SAVE MONTH NUMBER                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER         CALL DRIVER FOR INPUT                        
         MVI   GLOPTS+2,C'N'       ONLY NEED R4 RECORD ONCE                     
*                                                                               
RB16     L     R3,SCNEXT           NEXT CHUNK                                   
         B     RB10                                                             
*                                                                               
RB18     LA    RF,SBLOCK           ANY BUY CONTINUATION?                        
         AHI   RF,SBACONT-SBLOCK                                                
         OC    0(4,RF),0(RF)                                                    
         BNZ   RB9                 YES - GO GET THE REST                        
         B     RB4                 READ NEXT BUY RECORD                         
*                                                                               
RBX      B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD01                                                      
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         DS    0D                                                               
BIG      DS    PL16                                                             
SVREG5   DS    F                                                                
*                                                                               
PCTOPT   DS    CL1                                                              
CMMOPT   DS    CL1                                                              
PGBRAND  DS    CL3                                                              
F5PRD    DS    CL3                                                              
READMODE DS    XL1                                                              
RDEST    EQU   C'E'                                                             
RDBUY    EQU   C'B'                                                             
RDBILL   EQU   C'L'                                                             
*                                                                               
CHRGPER  DS    CL3                 PG DATA FIELDS                               
PGAGY    DS    CL2                                                              
ACCOUNT  DS    CL6                                                              
BRAND    DS    CL4                                                              
ESTIMATE DS    CL4                                                              
EVENTCD  DS    CL6                                                              
MLTBRND  DS    CL1                                                              
NOBRAND  DS    CL1                                                              
BRANDCD  DS    CL4                                                              
SRSTART  DS    CL6                                                              
ORIGEST  DS    XL1                                                              
BRDSUFF  DS    CL2                                                              
*                                                                               
ERRCD    DS    XL1                 ERROR BYTE                                   
ERRCHP   EQU   X'80'               CHARGE PERIOD                                
ERRACC   EQU   X'40'               ACCOUNT                                      
ERRBRN   EQU   X'20'               BRAND                                        
ERREST   EQU   X'10'               ESTIMATE                                     
ERREVC   EQU   X'08'               EVENT CODE                                   
ERRMBR   EQU   X'04'               MULTI BRAND                                  
ERRBRC   EQU   X'02'               'NO BRAND' CODE                              
ERRAGY   EQU   X'01'               AGENCY                                       
*                                                                               
F4ERRMSK DC    AL1(ERRCHP+ERRAGY+ERRACC+ERRBRN+ERREST+ERREVC+ERRMBR)            
F5ERRMSK DC    AL1(ERRCHP+ERRAGY+ERRACC+ERRBRN+ERREST)                          
F6ERRMSK DC    AL1(ERRCHP+ERRAGY+ERRACC+ERRBRN+ERREST+ERRBRC)                   
*                                                                               
REQMNTHS DS    XL12                REQUEST MONTHS, 2 X 6 MONTHS                 
ESTSTMN  DS    XL2                 ESTIMATE START MONTH                         
ESTENMN  DS    XL2                 ESTIMATE END MONTH                           
*ORDERDOL DS    XL104               ORDERED DOLLARS X 13 MONTHS YTD/CUR         
ORDERDOL DS    26PL6               13 ORDERED YTD/13 ORDERED TODAY              
*                                                                               
XFF      DC    XL8'FFFFFFFFFFFFFFFF'                                            
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*SPGENPGEST                                                                     
*SPGENEST                                                                       
*SPGENBUY                                                                       
*SPGENBILL                                                                      
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE SPGENPGEST                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE7D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPWRI09   12/15/04'                                      
         END                                                                    
