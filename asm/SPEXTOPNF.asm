*          DATA SET SPEXTOPNF  AT LEVEL 017 AS OF 05/01/02                      
*PHASE SPEXTOPF,*                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE GETRATE                                                                
*INCLUDE QSORT                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
***********************************************************************         
*                                                                     *         
*        THIS PROGRAM WILL RE-OPEN CLOSED OUT SPOT INFORMATION        *         
*        IT REQUIRES A FILE OF CONTROL INFORMATION (REOPEN)           *         
*        TO DETERMINE WHAT AGENCIES, MEDIA, CLIENT, PRODUCT, AND EST  *         
*                                                                     *         
*        1ST RECORD MUST BE OPEN, LOAD, OR COPY                       *         
*        OPEN TURNS OFF THE CLOSED BITS (KEY+15) ONLY                 *         
*        LOAD CREATES AN EXTRACT TAPE OF ONLY REOPENED RECORDS        *         
*        2 TO 10 RECORDS ARE THE FOLLOWING FORMAT                     *         
*        COPY WILL ONLY COPY SELECTED RECORDS EVEN THOUGH NOT CLOSED  *         
*        COL LEN DATA                                                 *         
*          1  1  AGENCY NUMBER 1 - F                                  *         
*          2  2  AGENCY ALPHA                                         *         
*          4  1  MEDIA - T, R, X, N                                   *         
*          5  3  CLIENT - MAY BE 'ALL'                                *         
*          8  3  PRODUCT                                              *         
*         11  3  ESTIMATE                                             *         
*         14  3  ESTIMATE SERIES                                      *         
*         14  3  ESTIMATE SERIES                                      *         
*         14  3  ESTIMATE SERIES                                      *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
* LEV  3- 4 AUG15/91 ALLOW 100 REQUESTS                               *         
* LEV  5    SEP24/92 MEDIA C REOPENS COMBINED, NET AND TV             *         
* LEV  6    NOV24/93 ADD STATUS, PG EST, CHILD SPOT, AND NWS RECS     *         
* LEV  7    MAR02/94 ADD CLIENT, PRODUCT REOPEN                       *         
* LEV  8    APR12/94 TAKE OUT NWS ALPHA AGENCY CODE CHECK             *         
* LEV  9    JAN18/95 ALLOW 2ND CHAR OF AGY CODE TO BE NUMERIC         *         
* LEV 12    AUG07/97 STOP STATUS REC AGY ALPHA ERROR                  *         
*                    SORT ESTS IN TESTS, AND PRT MORE THAN 28         *         
* LEV 13    APR06/98 STOP EXTRACTING NWS - BUYERS WORK SHEET RECS     *         
* LEV 14    MAR03/99 ONE TIME ONLY BYPASS CLT/PRD/0D65/71 RECS        *         
* LEV 14    MAR03/99 ONE TIME ONLY BYPASS CLT/PRD/0D65/71 RECS        *         
*                                                                     *         
***********************************************************************         
         SPACE 3                                                                
***********************************************************************         
*                                                                     *         
*        REG  USAGE                                                   *         
*         0   WORK                                                    *         
*         1   WORK                                                    *         
*         2   WORK                                                    *         
*         3   POINTER TO INPUT RECS                                   *         
*         4   BASE FOR REOPEN TABLES                                  *         
*         5   WORK                                                    *         
*         6   ELEM POINTER FOR BUYS                                   *         
*         7   POINTER TO REOPEN TABLE                                 *         
*         8   -                                                       *         
*         9   SECOND BASE                                             *         
*         A   PRINT POINTER                                           *         
*         B   FIRST BASE                                              *         
*         C   POINTER TO WORKING STORAGE                              *         
*         E   WORK                                                    *         
*         F   WORK                                                    *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
SPEXTOP  CSECT                                                                  
         NMOD1 20,**SPOP**,R9                                                   
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         LA    R2,REOPEN                                                        
         OPEN ((R2),INPUT)                                                      
         SPACE                                                                  
         LA    R0,DMINITEF                                                      
         STCM  R0,7,REOPEN+33                                                   
         SPACE                                                                  
         GET   (R2),WORK                                                        
         CLC   =C'COPY',WORK                                                    
         BE    DMINIT10                                                         
         CLC   =C'LOAD',WORK                                                    
         BE    DMINIT10                                                         
         CLC   =C'OPEN',WORK                                                    
         BNE   DMINTCTL                                                         
         SPACE                                                                  
DMINIT10 MVC   SELECT,WORK                                                      
         MVC   RTITLE+7(4),WORK                                                 
         GET   (R2),WORK                                                        
         LA    R3,99                                                            
         LA    R7,REOPNTAB                                                      
         USING OPNTABD,R7                                                       
         B     DMINIT22                                                         
DMINIT20 GET   (R2),WORK                                                        
         SPACE                                                                  
* AGENCY NUMBER                                                                 
         SPACE                                                                  
DMINIT22 DS    0H                                                               
         CLI   WORK,C'A'                                                        
         BL    DMINTAGY                                                         
         CLI   WORK,C'F'                                                        
         BNH   DMINIT30                                                         
         CLI   WORK,C'1'                                                        
         BL    DMINTAGY                                                         
         CLI   WORK,C'9'                                                        
         BH    DMINTAGY                                                         
         SPACE                                                                  
* AGENCY POWER CODE                                                             
         SPACE                                                                  
DMINIT30 DS    0H                                                               
         CLI   WORK+1,C'A'                                                      
         BL    DMINTAGY                                                         
         CLI   WORK+1,C'Z'                                                      
         BH    DMINTAGY                                                         
         CLI   WORK+2,C'A'                                                      
         BL    DMINTAGY                                                         
         CLI   WORK+2,C'9'                                                      
         BH    DMINTAGY                                                         
         MVI   BYTE,0                                                           
         MVN   BYTE,WORK                                                        
         ZIC   R1,BYTE                                                          
         CLI   WORK,C'0'                                                        
         BNL   *+8                                                              
         LA    R1,9(,R1)                                                        
         SLL   R1,4                                                             
         STC   R1,BYTE                                                          
         MVZ   TAGYMED,BYTE                                                     
         MVC   TAGY,WORK                                                        
         MVC   TAGYA,WORK+1                                                     
         SPACE                                                                  
* MEDIA VALIDATION                                                              
         SPACE                                                                  
         CLI   WORK+3,C'T'                                                      
         BNE   *+12                                                             
         MVI   BYTE,1                                                           
         B     DMINIT34                                                         
         CLI   WORK+3,C'R'                                                      
         BNE   *+12                                                             
         MVI   BYTE,2                                                           
         B     DMINIT34                                                         
         CLI   WORK+3,C'N'                                                      
         BNE   *+12                                                             
         MVI   BYTE,3                                                           
         B     DMINIT34                                                         
         CLI   WORK+3,C'X'                                                      
         BNE   *+12                                                             
         MVI   BYTE,4                                                           
         B     DMINIT34                                                         
         CLI   WORK+3,C'C'                                                      
         BNE   DMINTMED                                                         
         MVI   BYTE,8                                                           
DMINIT34 MVC   TMED,WORK+3                                                      
         MVN   TAGYMED,BYTE                                                     
         SPACE                                                                  
* CLIENT VALIDATION                                                             
         SPACE                                                                  
         CLI   WORK+4,C'A'                                                      
         BL    DMINTCLT                                                         
         CLI   WORK+4,C'Z'                                                      
         BH    DMINTCLT                                                         
         CLI   WORK+5,C'A'                                                      
         BL    DMINTCLT                                                         
         CLI   WORK+5,C'Z'                                                      
         BH    DMINTCLT                                                         
         CLI   WORK+6,C' '                                                      
         BE    DMINIT36                                                         
         CLI   WORK+6,C'A'                                                      
         BL    DMINTCLT                                                         
         CLI   WORK+6,C'Z'                                                      
         BNH   DMINIT36                                                         
         CLI   WORK+6,C'0'                                                      
         BL    DMINTCLT                                                         
         CLI   WORK+6,C'9'                                                      
         BH    DMINTCLT                                                         
         SPACE                                                                  
DMINIT36 MVC   TCLT,WORK+4                                                      
         CLC   TCLT,=C'ALL'        IF ALL CLIENTS                               
         BNE   *+12                                                             
         MVI   TCLTOK,1                                                         
         B     DMINIT38                                                         
         SPACE                                                                  
         GOTO1 =V(CLPACK),DMCB,TCLT,TBCLT                                       
         SPACE                                                                  
* PRODUCT VALIDATION                                                            
         SPACE                                                                  
DMINIT38 CLC   WORK+7(3),SPACES                                                 
         BE    DMINIT44                                                         
         CLC   TCLT,=C'ALL'        IF ALL CLIENTS                               
         BE    DMINTPRD                                                         
         CLI   WORK+7,C'A'                                                      
         BL    DMINTPRD                                                         
         CLI   WORK+7,C'Z'                                                      
         BH    DMINTPRD                                                         
         CLI   WORK+8,C'A'                                                      
         BL    DMINTPRD                                                         
         CLI   WORK+8,C'Z'                                                      
         BH    DMINTPRD                                                         
         CLI   WORK+9,C' '                                                      
         BE    DMINIT40                                                         
         CLI   WORK+9,C'A'                                                      
         BL    DMINTCLT                                                         
         CLI   WORK+9,C'Z'                                                      
         BNH   DMINIT40                                                         
         CLI   WORK+9,C'0'                                                      
         BL    DMINTPRD                                                         
         CLI   WORK+9,C'9'                                                      
         BH    DMINTPRD                                                         
DMINIT40 MVC   TPROD,WORK+7                                                     
         SPACE                                                                  
* ESTIMATE VALIDATION                                                           
         SPACE                                                                  
DMINIT44 CLC   WORK+10(6),=6C' '                                                
         BE    DMINIT50                                                         
         SPACE                                                                  
         CLI   WORK+10,C' '                                                     
         BE    *+20                                                             
         CLI   WORK+10,C'0'                                                     
         BL    DMINTEST                                                         
         CLI   WORK+10,C'9'                                                     
         BH    DMINTEST                                                         
         CLI   WORK+11,C' '                                                     
         BE    *+20                                                             
         CLI   WORK+11,C'0'                                                     
         BL    DMINTEST                                                         
         CLI   WORK+11,C'9'                                                     
         BH    DMINTEST                                                         
         CLI   WORK+12,C'0'                                                     
         BL    DMINTEST                                                         
         CLI   WORK+12,C'9'                                                     
         BH    DMINTEST                                                         
         PACK  DUB,WORK+10(3)                                                   
         CVB   R0,DUB                                                           
         STC   R0,TBEST                                                         
         UNPK  TEST,DUB                                                         
         SPACE                                                                  
         CLC   WORK+13(3),=6C' '                                                
         BE    DMINIT46                                                         
         CLI   WORK+13,C' '                                                     
         BE    *+20                                                             
         CLI   WORK+13,C'0'                                                     
         BL    DMINTEST                                                         
         CLI   WORK+13,C'9'                                                     
         BH    DMINTEST                                                         
         CLI   WORK+14,C' '                                                     
         BE    *+20                                                             
         CLI   WORK+14,C'0'                                                     
         BL    DMINTEST                                                         
         CLI   WORK+14,C'9'                                                     
         BH    DMINTEST                                                         
         CLI   WORK+15,C'0'                                                     
         BL    DMINTEST                                                         
         CLI   WORK+15,C'9'                                                     
         BH    DMINTEST                                                         
         PACK  DUB,WORK+13(3)                                                   
         CVB   R0,DUB                                                           
         STC   R0,TBEST2                                                        
         UNPK  TEST2,DUB                                                        
         B     DMINIT50                                                         
         SPACE                                                                  
DMINIT46 MVC   TEST2,TEST                                                       
         MVC   TBEST2,TBEST                                                     
         SPACE                                                                  
DMINIT50 DS    0H                                                               
         CLI   WORK+16,C' '                                                     
         BE    DMINIT60                                                         
         CLI   WORK+16,C'A'                                                     
         BL    DMINTAGN                                                         
         CLI   WORK+16,C'F'                                                     
         BNH   DMINIT54                                                         
         CLI   WORK+16,C'1'                                                     
         BL    DMINTAGN                                                         
         CLI   WORK+16,C'9'                                                     
         BH    DMINTAGN                                                         
         SPACE                                                                  
* AGENCY POWER CODE                                                             
         SPACE                                                                  
DMINIT54 DS    0H                                                               
         CLI   WORK+17,C'A'                                                     
         BL    DMINTAGN                                                         
         CLI   WORK+17,C'Z'                                                     
         BH    DMINTAGN                                                         
         CLI   WORK+18,C'A'                                                     
         BL    DMINTAGN                                                         
         CLI   WORK+18,C'9'                                                     
         BH    DMINTAGN                                                         
         MVI   BYTE,0                                                           
         MVN   BYTE,WORK+16                                                     
         ZIC   R1,BYTE                                                          
         CLI   WORK+16,C'0'                                                     
         BNL   *+8                                                              
         LA    R1,9(,R1)                                                        
         SLL   R1,4                                                             
         STC   R1,TNAGY                                                         
         SPACE                                                                  
         MVC   TNAGYN,WORK+16                                                   
         MVC   TNAGYA,WORK+17                                                   
DMINIT60 LA    R7,TNEXT                                                         
         BCT   R3,DMINIT20                                                      
         SPACE                                                                  
         MVC   P(30),=C'MORE REQUESTS THAN TABLE SPACE'                         
         B     DMINITER                                                         
         SPACE                                                                  
DMINITEF CLI   SELECT,0            WAS THERE A SELECT                           
         BE    DMINTSEL                                                         
         CLI   REOPNTAB,0                                                       
         BE    DMINTNON                                                         
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         B     DMXIT                                                            
         EJECT                                                                  
DMINTCTL MVC   P+5(41),=C'1ST CONTROL REC MUST BE OPEN OR LOAD, NOT'            
         MVC   P+42(10),WORK                                                    
         B     DMINITER                                                         
DMINTSEL MVC   P(22),=C'NO CONTROL RECS AT ALL'                                 
         B     DMINITER                                                         
DMINTNON MVC   P(11),=C'NO REQUESTS'                                            
         B     DMINITER                                                         
         SPACE                                                                  
DMINTAGY LA    R0,3                AGENCY ERROR                                 
         LA    R1,P                                                             
         B     DMINTERR                                                         
         SPACE                                                                  
DMINTMED LA    R0,1                MEDIA ERROR                                  
         LA    R1,P+3                                                           
         B     DMINTERR                                                         
         SPACE                                                                  
DMINTCLT LA    R0,3                CLIENT ERROR                                 
         LA    R1,P+4                                                           
         B     DMINTERR                                                         
         SPACE                                                                  
DMINTPRD LA    R0,3                PRODUCT ERROR                                
         LA    R1,P+7                                                           
         B     DMINTERR                                                         
         SPACE                                                                  
DMINTEST LA    R0,6                ESTIMATE ERROR                               
         LA    R1,P+10                                                          
         B     DMINTERR                                                         
         SPACE                                                                  
DMINTAGN LA    R0,3                AGENCY ERROR                                 
         LA    R1,P+16                                                          
         B     DMINTERR                                                         
         SPACE                                                                  
DMINTERR MVC   P(80),WORK                                                       
         GOTO1 VPRINTER                                                         
         MVI   0(R1),C'*'                                                       
         LA    R1,1(,R1)                                                        
         BCT   R0,*-8                                                           
         SPACE                                                                  
DMINITER GOTO1 VPRINTER                                                         
         ABEND 099                                                              
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
         LA    R7,REOPNTAB         POINT TO RE-OPEN TABLE                       
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
*        CLI   0(R3),0             TEMP                                         
*        BNE   TEMPX10             TEMP FOR SECOND COKE PASS                    
*        OC    4(9,R3),4(R3)       CLIENT REC                                   
*        BZ    DMXSEL              TEMP FOR SECOND COKE PASS                    
*        OC    7(6,R3),7(R3)       PROD HDR                                     
*        BZ    DMXSEL              TEMP FOR SECOND COKE PASS                    
*        OC    8(5,R3),8(R3)       EST HDR                                      
*        BNZ   DMXSEL              TEMP FOR SECOND COKE PASS                    
         SPACE                                                                  
* FIND CLIENT HDRS, AND GO FIND AND SAVE BPRD *                                 
         SPACE                                                                  
         CLI   0(R3),0                                                          
         BNE   DMXREC10                                                         
         SPACE                                                                  
DMXREC02 LA    RF,1(,R3)                                                        
         BAS   RE,COMTAGY          GO CHECK AGENCY/MEDIA AND COMBINED           
         BNE   DMXREC04                                                         
         SPACE                                                                  
         CLC   TCLT,=C'ALL'                                                     
         BE    DMXREC10                                                         
         CLC   TBCLT,2(R3)                                                      
         BNE   DMXREC04                                                         
         SPACE                                                                  
         OC    4(9,R3),4(R3)                                                    
         BNZ   DMXREC10                                                         
         SPACE                                                                  
         MVI   TCLTOK,1                                                         
         SPACE                                                                  
         OC    TPROD,TPROD         REQUEST BY PRODUCT                           
         BZ    DMXREC04                                                         
         SPACE                                                                  
         BAS   RE,SETPRD                                                        
         SPACE                                                                  
DMXREC04 DS    0H                                                               
         LA    R7,TNEXT                                                         
         CLI   TAGYMED,0                                                        
         BNE   DMXREC02                                                         
         SPACE                                                                  
DMXREC10 CLI   SELECT,C'C'         IS THIS A COPY?                              
         BNE   DMXREC20                                                         
         TM    15(R3),X'C0'        HAS THIS RECORD BEEN CLOSED-OUT?             
         BNO   DMXREC30                                                         
         AP    TOTCLS,=P'1'                                                     
         B     DMXREC30                                                         
         SPACE                                                                  
DMXREC20 TM    15(R3),X'C0'        HAS THIS RECORD BEEN CLOSED-OUT?             
         BNO   DMXSEL                                                           
         SPACE                                                                  
         AP    TOTCLS,=P'1'                                                     
         SPACE                                                                  
DMXREC30 CLI   0(R3),0             CLTHDR, PRDHDR, ESTHDR, OR BILL              
         BE    STCLT                YES.                                        
         CLI   0(R3),2             GOAL REC?                                    
         BE    STGOAL               YES                                         
         SPACE                                                                  
         CLC   =X'0D49',0(R3)      IS THIS A CHILD SPOT RECORD?                 
         BE    CHSPT                YES.                                        
         SPACE                                                                  
         CLC   =X'0D5D',0(R3)      IS THIS A PG EST RECORD?                     
         BE    PGEST                YES.                                        
         SPACE                                                                  
         CLC   =X'0D65',0(R3)      IS THIS A NWS BUYER RECORD?                  
*        BE    NWSB                 YES.                                        
         SPACE                                                                  
         CLC   =X'0D66',0(R3)      IS THIS A NWS CAMPAIGN RECORD?               
         NOP   NWSC                 YES.                                        
         SPACE                                                                  
         CLC   =X'0D67',0(R3)      IS THIS A NWS HEADER RECORD?                 
         NOP   NWSH                 YES.                                        
         SPACE                                                                  
         CLC   =X'0D68',0(R3)      IS THIS A NWS DETAIL RECORD?                 
         NOP   NWSD                 YES.                                        
         SPACE                                                                  
         CLC   =X'0D71',0(R3)      IS THIS A STATUS RECORD?                     
*        BE    STSTAT               YES.                                        
         SPACE                                                                  
         CLC   =X'0E01',0(R3)      IS THIS A STATION BILL?                      
         BE    STSTAB               YES.                                        
         SPACE                                                                  
*EMPX10  DS    0H                                                               
         CLI   0(R3),X'11'                                                      
         BL    DMXSEL                                                           
         EJECT                                                                  
* BUY RECORD *                                                                  
         SPACE                                                                  
STBUY    AP    TBUYCT,=P'1'                                                     
         SPACE                                                                  
STBUY10  LR    RF,R3                                                            
         BAS   RE,COMTAGY          GO CHECK AGENCY/MEDIA AND COMBINED           
         BNE   STBUY14                                                          
         SPACE                                                                  
         CLC   TAGYA,20(R3)        CK AGENCY ALPHA GIVEN                        
         BE    STBUY11                                                          
         LA    R1,=CL8'BUY'                                                     
         BAS   RE,AGYAERR                                                       
         SPACE                                                                  
STBUY11  DS    0H                                                               
         CLC   TCLT,=C'ALL'        RE-OPEN ALL CLIENTS                          
         BE    STBUY12                                                          
         SPACE                                                                  
         CLC   1(2,R3),TBCLT       RE-OPEN CLIENT                               
         BNE   STBUY14                                                          
         SPACE                                                                  
STBUY12  DS    0H                                                               
         OC    TPROD,TPROD         BY PRODUCT                                   
         BZ    *+14                                                             
         CLC   TBPRD,3(R3)         THIS REQUESTED PROD                          
         BNE   STBUY14                                                          
         SPACE                                                                  
         OC    TEST,TEST           BY ESTIMATE                                  
         BZ    STBUY20                                                          
         CLC   TBEST,9(R3)       THIS REQUESTED EST                             
         BH    STBUY14                                                          
         CLC   TBEST2,9(R3)      THIS REQUESTED EST                             
         BNL   STBUY20                                                          
         SPACE                                                                  
STBUY14  LA    R7,TNEXT                                                         
         CLI   0(R7),0                                                          
         BNE   STBUY10                                                          
         B     DMXSEL                                                           
         SPACE                                                                  
* CHECK ESTIMATE TABLE *                                                        
         SPACE                                                                  
STBUY20  DS    0H                                                               
         LR    RF,R3                                                            
         BAS   RE,COMTAGYM         GO CHECK AGENCY/MEDIA AND COMBINED           
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R0,L'TESTS                                                       
         LA    R2,TESTS                                                         
STBUY24  CLC   0(1,R2),9(R3)                                                    
         BE    BLDP00                                                           
         LA    R2,1(,R2)                                                        
         BCT   R0,STBUY24                                                       
         DC    H'0'                                                             
         SPACE                                                                  
* CALC DOLLARS ORDERED AND PAID *                                               
         SPACE                                                                  
         USING SPGENBUYD,R3                                                     
BLDP00   LA    R6,BDELEM                                                        
         SPACE                                                                  
BLDP10   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BLDPX               END OF RECORD                                
         CLI   0(R6),6                                                          
         BL    BLDP10                                                           
         CLI   0(R6),10                                                         
         BL    BLDP30                                                           
         CLI   0(R6),11                                                         
         BL    BLDP10                                                           
         CLI   0(R6),14                                                         
         BNL   BLDP10                                                           
         SPACE                                                                  
* ELEMENT PROCESSING FOR POL                                                    
         SPACE                                                                  
BLDP20   TM    6(R6),X'04'         TEST HIATUS                                  
         BO    BLDP10              YES-NEXT ELEM                                
         SPACE                                                                  
BLDP30   MVC   THISPRD,3(R3)                                                    
         CLI   THISPRD,X'FF'       THIS POOL                                    
         BNE   BLDP40               NO                                          
         CLI   1(R6),10            ALLOCATED                                    
         BNH   BLDP40               NO                                          
         MVC   THISPRD,10(R6)                                                   
         SPACE                                                                  
BLDP40   DS    0H                                                               
         SPACE                                                                  
         GOTO1 =V(GETRATE),DMCB,(THISPRD,SPOTS),(R3),(R6)                       
         SPACE                                                                  
         LM    R0,R1,GROSS                                                      
         SPACE                                                                  
         CLI   BDXFRAGY,0          TEST CAME FROM ANOTHER SPOT FILE             
         BE    *+8                  NO, USE DOLLARS                             
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SPACE                                                                  
         LM    RE,RF,GRSORD        GROSS ORD/NET ORD                            
         AR    RE,R0                                                            
         AR    RF,R1                                                            
         STM   RE,RF,GRSORD                                                     
         SPACE                                                                  
         LM    RE,RF,TGRSORD       GROSS ORD/NETORD                             
         AR    RE,R0                                                            
         AR    RF,R1                                                            
         STM   RE,RF,TGRSORD                                                    
         EJECT                                                                  
* TEST PAID                                                                     
         SPACE                                                                  
         OC    4(2,R6),4(R6)                                                    
         BZ    BLDP60                                                           
         TM    BDSTAT,X'01'        TEST NETPAK                                  
         BZ    BLDP50              NO                                           
         LR    RE,R6                                                            
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),X'10'         TEST AFFID                                   
         BE    *-10                                                             
         CLI   0(RE),X'11'         TEST INTG ELEM                               
         BNE   *+14                                                             
         OC    6(2,RE),6(RE)       TEST INTG PAID                               
         BNZ   BLDP50              YES                                          
         SPACE                                                                  
* NEED TIME RATES ONLY                                                          
         SPACE                                                                  
         GOTO1 =V(GETRATE),DMCB,(THISPRD,SPOTS),(R3),(C'T',(R6))                
         SPACE                                                                  
         LM    R0,R1,GROSS                                                      
         SPACE                                                                  
         CLI   BDXFRAGY,0          TEST CAME FROM ANOTHER SPOT FILE             
         BE    *+8                  NO, USE DOLLARS                             
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SPACE                                                                  
BLDP50   LM    RE,RF,GRSPAID      GROSS PAID/NET PAID                           
         AR    RE,R0                                                            
         AR    RF,R1                                                            
         STM   RE,RF,GRSPAID                                                    
         SPACE                                                                  
         LM    RE,RF,TGRSPAID      GROSS PAID/NET PAID                          
         AR    RE,R0                                                            
         AR    RF,R1                                                            
         STM   RE,RF,TGRSPAID                                                   
         SPACE                                                                  
BLDP60   B     BLDP10                                                           
         SPACE                                                                  
BLDPX    NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         AP    BUYCTOP,=P'1'                                                    
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPBUYS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPBUYS                                                       
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'BUY  OPEN'                                              
         B     PRT                                                              
         EJECT                                                                  
* CHILD SPOT RECORD (0D49) *                                                    
         SPACE                                                                  
CHSPT    AP    TCHSPTCT,=P'1'                                                   
         SPACE                                                                  
CHSPT10  LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGY          GO CHECK AGENCY/MEDIA AND COMBINED           
         BNE   CHSPT14                                                          
         SPACE                                                                  
         CLC   TAGYA,20(R3)        CK AGENCY ALPHA GIVEN                        
         BE    CHSPT11                                                          
         LA    R1,=CL8'CHD SPOT'                                                
         BAS   RE,AGYAERR                                                       
         SPACE                                                                  
CHSPT11  DS    0H                                                               
         CLC   TCLT,=C'ALL'        RE-OPEN ALL CLIENTS                          
         BE    CHSPT12                                                          
         SPACE                                                                  
         CLC   3(2,R3),TBCLT       RE-OPEN CLIENT                               
         BNE   CHSPT14                                                          
         SPACE                                                                  
CHSPT12  DS    0H                                                               
         OC    TEST,TEST           BY ESTIMATE                                  
         BZ    CHSPT20                                                          
         CLC   TBEST,10(R3)      THIS REQUESTED EST                             
         BL    CHSPT14                                                          
         CLC   TBEST2,10(R3)     THIS REQUESTED EST                             
         BNH   CHSPT20                                                          
         SPACE                                                                  
CHSPT14  LA    R7,TNEXT                                                         
         CLI   0(R7),0                                                          
         BNE   CHSPT10                                                          
         B     DMXSEL                                                           
         SPACE                                                                  
CHSPT20  DS    0H                                                               
         LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGYM         GO CHECK AGENCY/MEDIA AND COMBINED           
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    CHSPCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPCHSPT                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,TOPCHSPT                                                      
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CHILD SPOT'                                             
         B     PRT                                                              
         EJECT                                                                  
* PG ESTIMATE RECORD (0D5D) *                                                   
         SPACE                                                                  
PGEST    AP    TPGESTCT,=P'1'                                                   
         SPACE                                                                  
PGEST10  LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGY          GO CHECK AGENCY/MEDIA AND COMBINED           
         BNE   PGEST14                                                          
         SPACE                                                                  
         CLC   TAGYA,20(R3)        CK AGENCY ALPHA GIVEN                        
         BE    PGEST10C                                                         
         LA    R1,=CL8'PG ESTIM'                                                
         BAS   RE,AGYAERR                                                       
         SPACE                                                                  
PGEST10C DS    0H                                                               
         CLC   TCLT,=C'ALL'        RE-OPEN ALL CLIENTS                          
         BE    PGEST11                                                          
         SPACE                                                                  
         CLC   3(2,R3),TBCLT       RE-OPEN CLIENT                               
         BNE   PGEST14                                                          
         SPACE                                                                  
PGEST11  DS    0H                                                               
         OC    TPROD,TPROD         BY PRODUCT                                   
         BZ    PGEST12                                                          
         CLC   TPROD,=C'POL'       THIS PRD POL                                 
         BE    PGEST12                                                          
         CLC   TPROD,5(R3)         THIS REQUESTED PROD                          
         BNE   PGEST14                                                          
         SPACE                                                                  
PGEST12  OC    TEST,TEST           BY ESTIMATE                                  
         BZ    PGEST20                                                          
         CLC   TBEST,8(R3)       THIS REQUESTED EST                             
         BL    PGEST14                                                          
         CLC   TBEST2,8(R3)      THIS REQUESTED EST                             
         BNH   PGEST20                                                          
         SPACE                                                                  
PGEST14  LA    R7,TNEXT                                                         
         CLI   0(R7),0                                                          
         BNE   PGEST10                                                          
         B     DMXSEL                                                           
         SPACE                                                                  
PGEST20  DS    0H                                                               
         LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGYM         GO CHECK AGENCY/MEDIA AND COMBINED           
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    PGESCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPPGEST                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,TOPPGEST                                                      
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'PG ESTIMATE'                                            
         B     PRT                                                              
         EJECT                                                                  
* NWS NEW BUYERS WORKSHEET (0D65 - 0D66 - 0D67 - 0D68) *                        
*                           BUYER  CAMP   HEADER DETAIL                         
         SPACE                                                                  
NWSB     AP    TNWSBCT,=P'1'                                                    
         B     NWS10                                                            
NWSC     AP    TNWSCCT,=P'1'                                                    
         B     NWS10                                                            
NWSH     AP    TNWSHCT,=P'1'                                                    
         B     NWS10                                                            
NWSD     AP    TNWSDCT,=P'1'                                                    
         SPACE                                                                  
NWS10    LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGY          GO CHECK AGENCY/MEDIA AND COMBINED           
         BNE   NWS14                                                            
         SPACE                                                                  
*        CLC   TAGYA,20(R3)        CK AGENCY ALPHA GIVEN                        
*        BE    *+12                                                             
*        LA    R1,=CL8'NWS     '                                                
*        BAS   RE,AGYAERR                                                       
         SPACE                                                                  
         CLI   1(R3),X'65'         DO ALL BUYER (NO CLT, PRD EST)               
         BE    NWS30                                                            
         CLI   1(R3),X'66'         DO ALL CAMPAIGN (NO CLT, PRD EST)            
         BE    NWS40                                                            
         CLI   1(R3),X'67'         DO ALL HEADER (NO CLT, PRD EST)              
         BE    NWS50                                                            
         CLI   1(R3),X'68'         DO ALL DETAIL (NO CLT, PRD EST)              
         BE    NWS60                                                            
         DC    H'0'                                                             
         SPACE                                                                  
         CLC   TCLT,=C'ALL'        RE-OPEN ALL CLIENTS                          
         BE    NWS20                                                            
         SPACE                                                                  
         LA    R6,24(,R3)                                                       
         CLI   0(R6),01            BETTER BE AN ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLC   BWDELCDQ(2,R6),TBCLT       RE-OPEN CLIENT                        
         BNE   NWS14                                                            
         SPACE                                                                  
         OC    TPROD,TPROD         BY PRODUCT                                   
         BZ    NWS12                                                            
         CLC   TPROD,=C'POL'       THIS PRD POL                                 
         BE    NWS12                                                            
         CLC   TBPRD,5(R3)         THIS REQUESTED PROD                          
         BNE   NWS14                                                            
         SPACE                                                                  
NWS12    OC    TEST,TEST           BY ESTIMATE                                  
         BZ    NWS20                                                            
         CLC   TBEST,7(R3)       THIS REQUESTED EST                             
         BL    NWS14                                                            
         CLC   TBEST2,7(R3)      THIS REQUESTED EST                             
         BNH   NWS20                                                            
         SPACE                                                                  
NWS14    LA    R7,TNEXT                                                         
         CLI   0(R7),0                                                          
         BNE   NWS10                                                            
         B     DMXSEL                                                           
         SPACE                                                                  
NWS20    DS    0H                                                               
         LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGYM         GO CHECK AGENCY/MEDIA AND COMBINED           
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    STATCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPSTUS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPSTUS                                                       
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NWS BUYER OPEN'                                         
         B     PRT                                                              
         SPACE                                                                  
NWS30    AP    NWSBCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPNWSB                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPNWSB                                                       
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NWS BUYER OPEN'                                         
         B     PRT                                                              
         SPACE                                                                  
NWS40    AP    NWSCCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPNWSC                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPNWSC                                                       
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NWS CAMPAIGN OPEN'                                      
         B     PRT                                                              
         SPACE                                                                  
NWS50    AP    NWSHCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPNWSH                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPNWSH                                                       
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NWS HEADER OPEN'                                        
         B     PRT                                                              
         SPACE                                                                  
NWS60    AP    NWSDCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPNWSD                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPNWSD                                                       
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NWS DETAIL OPEN'                                        
         B     PRT                                                              
         EJECT                                                                  
* STATUS RECORD (0D71) *                                                        
         SPACE                                                                  
STSTAT   AP    TSTATUCT,=P'1'                                                   
         SPACE                                                                  
STSTAT10 LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGY          GO CHECK AGENCY/MEDIA AND COMBINED           
         BNE   STSTAT14                                                         
         SPACE                                                                  
* IF AGY ALPHA CODE EVER PUT IN STATUS RECS, RESUME CHECK                       
         SPACE                                                                  
*        CLC   TAGYA,20(R3)        CK AGENCY ALPHA GIVEN                        
*        BE    *+12                                                             
*        LA    R1,=CL8'STATUS  '                                                
*        BAS   RE,AGYAERR                                                       
         SPACE                                                                  
         CLC   TCLT,=C'ALL'        RE-OPEN ALL CLIENTS                          
         BE    STSTAT11                                                         
         SPACE                                                                  
         CLC   3(2,R3),TBCLT       RE-OPEN CLIENT                               
         BNE   STSTAT14                                                         
         SPACE                                                                  
STSTAT11 OC    TEST,TEST           BY ESTIMATE                                  
         OC    TPROD,TPROD         BY PRODUCT                                   
         BZ    STSTAT12                                                         
         CLC   TPROD,=C'POL'       THIS PRD POL                                 
         BE    STSTAT12                                                         
         CLC   TBPRD,5(R3)         THIS REQUESTED PROD                          
         BNE   STSTAT14                                                         
         SPACE                                                                  
STSTAT12 OC    TEST,TEST           BY ESTIMATE                                  
         BZ    STSTAT20                                                         
         CLC   TBEST,7(R3)       THIS REQUESTED EST                             
         BL    STSTAT14                                                         
         CLC   TBEST2,7(R3)      THIS REQUESTED EST                             
         BNH   STSTAT20                                                         
         SPACE                                                                  
STSTAT14 LA    R7,TNEXT                                                         
         CLI   0(R7),0                                                          
         BNE   STSTAT10                                                         
         B     DMXSEL                                                           
         SPACE                                                                  
STSTAT20 DS    0H                                                               
         LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGYM         GO CHECK AGENCY/MEDIA AND COMBINED           
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    STATCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPSTUS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPSTUS                                                       
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STATUS OPEN'                                            
         B     PRT                                                              
         EJECT                                                                  
* STATION BILLING RECORD (0E01) *                                               
         SPACE                                                                  
STSTAB   AP    TSBILLCT,=P'1'                                                   
         SPACE                                                                  
STSTAB10 LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGY          GO CHECK AGENCY/MEDIA AND COMBINED           
         BNE   STSTAB14                                                         
         SPACE                                                                  
         CLC   TAGYA,20(R3)        CK AGENCY ALPHA GIVEN                        
         BE    *+12                                                             
         LA    R1,=CL8'STA BILL'                                                
         BAS   RE,AGYAERR                                                       
         SPACE                                                                  
         CLC   TCLT,=C'ALL'        RE-OPEN ALL CLIENTS                          
         BE    STSTAB11                                                         
         SPACE                                                                  
         CLC   3(2,R3),TBCLT       RE-OPEN CLIENT                               
         BNE   STSTAB14                                                         
         SPACE                                                                  
STSTAB11 DS    0H                                                               
         OC    TPROD,TPROD         BY PRODUCT                                   
         BZ    STSTAB12                                                         
         CLC   TPROD,=C'POL'       THIS PRD POL                                 
         BE    STSTAB12                                                         
         CLC   TBPRD,5(R3)         THIS REQUESTED PROD                          
         BNE   STSTAB14                                                         
         SPACE                                                                  
STSTAB12 OC    TEST,TEST           BY ESTIMATE                                  
         BZ    STSTAB20                                                         
         CLC   TBEST,6(R3)       THIS REQUESTED EST                             
         BL    STSTAB14                                                         
         CLC   TBEST2,6(R3)      THIS REQUESTED EST                             
         BNH   STSTAB20                                                         
         SPACE                                                                  
STSTAB14 LA    R7,TNEXT                                                         
         CLI   0(R7),0                                                          
         BNE   STSTAB10                                                         
         B     DMXSEL                                                           
         SPACE                                                                  
STSTAB20 LA    RF,2(,R3)                                                        
         BAS   RE,COMTAGYM         GO CHECK AGENCY/MEDIA AND COMBINED           
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    SBILCTOP,=P'1'                                                   
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPSTBS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPSTBS                                                       
         NI    15(R3),X'FF'-X'C0'   RE-OPEN RECORD                              
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STATION BILL OPEN'                                      
         B     PRT                                                              
         EJECT                                                                  
* CLIENT, PRODUCT, ESTIMATE, BILL OR GOAL RECORDS *                             
         SPACE                                                                  
STGOAL   AP    TGOLCT,=P'1'                                                     
         B     STCLT10                                                          
         SPACE                                                                  
STCLT    OC    4(9,R3),4(R3)       CLIENT REC                                   
         BNZ   STCLT04                                                          
         AP    TCLTCT,=P'1'                                                     
         B     STCLT10                                                          
STCLT04  DS    0H                                                               
         OC    7(6,R3),7(R3)       PROD HDR                                     
         BNZ   STCLT06                                                          
         AP    TPHDCT,=P'1'                                                     
         B     STCLT10                                                          
STCLT06  DS    0H                                                               
         OC    8(5,R3),8(R3)       EST HDR                                      
         BNZ   *+14                                                             
         AP    TESTCT,=P'1'                                                     
         B     STCLT10                                                          
         AP    TBILCT,=P'1'                                                     
         SPACE                                                                  
STCLT10  DS    0H                                                               
         LA    RF,1(,R3)                                                        
         BAS   RE,COMTAGY          GO CHECK AGENCY/MEDIA AND COMBINED           
         BNE   STCLT14                                                          
         SPACE                                                                  
         CLC   TCLT,=C'ALL'        RE-OPEN ALL CLIENTS                          
         BE    STCLT11                                                          
         SPACE                                                                  
         CLC   2(2,R3),TBCLT       RE-OPEN CLIENT                               
         BNE   STCLT14                                                          
         SPACE                                                                  
STCLT11  DS    0H                                                               
         OC    4(9,R3),4(R3)       CLIENT REC                                   
         BZ    STCLT20                                                          
         SPACE                                                                  
         OC    TPROD,TPROD         BY PRODUCT                                   
         BZ    STCLT12                                                          
         SPACE                                                                  
         CLC   TPROD,=C'POL'       THIS PRD POL                                 
         BE    STCLT12              OPEN ALL PRODUCTS                           
         SPACE                                                                  
         CLI   0(R3),2             THIS A GOAL                                  
         BE    STGOL10                                                          
         SPACE                                                                  
         CLC   TPROD,4(R3)         THIS REQUESTED PROD                          
         BNE   STCLT14                                                          
         B     STCLT12              OPEN ALL PRODUCTS                           
         SPACE                                                                  
STGOL10  DS    0H                                                               
         OC    TPROD,TPROD         BY PRODUCT                                   
         BZ    STCLT12                                                          
         SPACE                                                                  
         CLC   TBPRD,4(R3)         THIS REQUESTED PROD                          
         BNE   STCLT14                                                          
         SPACE                                                                  
STCLT12  OC    7(6,R3),7(R3)       PROD HDR                                     
         BZ    STCLT20                                                          
         SPACE                                                                  
* ONLY ESTIMATES/BILLS/GOALS LEFT, ALL WITH EST IN POSITION+7                   
         SPACE                                                                  
         CLI   TEST,0              BY ESTIMATE                                  
         BE    STCLT20                                                          
         SPACE                                                                  
         CLC   TBEST,7(R3)       THIS REQUESTED EST                             
         BH    STCLT14                                                          
         CLC   TBEST2,7(R3)      THIS REQUESTED EST                             
         BNL   STCLT20                                                          
         SPACE                                                                  
STCLT14  LA    R7,TNEXT                                                         
         CLI   0(R7),0                                                          
         BNE   STCLT10                                                          
         B     DMXSEL                                                           
         SPACE                                                                  
STCLT20  DS    0H                                                               
         LA    RF,1(,R3)                                                        
         BAS   RE,COMTAGYM         GO CHECK AGENCY/MEDIA AND COMBINED           
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLI   0(R3),2             GOAL                                         
         BE    STCLTGOL                                                         
         SPACE                                                                  
         OC    4(9,R3),4(R3)       CLIENT REC                                   
         BNZ   STCLT30              NO                                          
         SPACE                                                                  
         AP    TOPNCT,=P'1'                                                     
         AP    CLTCTOP,=P'1'                                                    
         L     R1,TOPCLTS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPCLTS                                                       
         B     DMXPURGE                                                         
         SPACE                                                                  
         NI    15(R3),X'FF'-X'C0'  RE-OPEN THE RECORD                           
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CLIENT OPEN'                                            
         B     PRT                                                              
         SPACE                                                                  
STCLT30  OC    7(6,R3),7(R3)       PROD HDR                                     
         BNZ   STCLT40              NO                                          
         SPACE                                                                  
         L     R1,TOPPRDS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPPRDS                                                       
         SPACE                                                                  
         AP    TOPNCT,=P'1'                                                     
         AP    PHDCTOP,=P'1'                                                    
         NI    15(R3),X'FF'-X'C0'  RE-OPEN THE RECORD                           
         B     DMXPURGE                                                         
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'PRODUCT OPEN'                                           
         B     PRT                                                              
         SPACE                                                                  
STCLT40  OC    8(5,R3),8(R3)       EST HDR                                      
         BZ    STCLTEHD             YES                                         
         SPACE                                                                  
* BILL                                                                          
         SPACE                                                                  
STCLT50  DS    0H                                                               
         AP    BILCTOP,=P'1'                                                    
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPBILS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPBILS                                                       
         SPACE                                                                  
         CLC   TAGYA,20(R3)        CK AGENCY ALPHA GIVEN                        
         BE    *+12                                                             
         LA    R1,=CL8'BILL'                                                    
         BAS   RE,AGYAERR                                                       
         SPACE                                                                  
         NI    15(R3),X'FF'-X'C0'  RE-OPEN THE RECORD                           
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA BILL OPEN'                                          
         B     PRT                                                              
         SPACE                                                                  
* ESTIMATE HEADER                                                               
         SPACE                                                                  
STCLTEHD AP    ESTCTOP,=P'1'                                                    
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPESTS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPESTS                                                       
         SPACE                                                                  
* BUILD ESTIMATE TABLE *                                                        
         SPACE                                                                  
         LA    R0,L'TESTS                                                       
         LA    R2,TESTS                                                         
STESTA   CLC   0(1,R2),7(R3)                                                    
         BE    STESTB                                                           
         CLI   0(R2),0                                                          
         BE    STESTB                                                           
         LA    R2,1(,R2)                                                        
         BCT   R0,STESTA                                                        
         DC    H'0'                                                             
STESTB   MVC   0(1,R2),7(R3)       STORE EST                                    
         NI    15(R3),X'FF'-X'C0'  RE-OPEN THE RECORD                           
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'EST HDR OPEN'                                           
         B     PRT                                                              
         SPACE 3                                                                
STCLTGOL AP    GOLCTOP,=P'1'                                                    
         AP    TOPNCT,=P'1'                                                     
         L     R1,TOPGOLS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TOPGOLS                                                       
         SPACE                                                                  
* IF AGY ALPHA CODE EVER PUT IN GOAL RECS, RESUME CHECK                         
         SPACE                                                                  
*        CLC   TAGYA,20(R3)        CK AGENCY ALPHA GIVEN                        
*        BE    *+12                                                             
*        LA    R1,=CL8'GOAL'                                                    
*        BAS   RE,AGYAERR                                                       
         SPACE                                                                  
         NI    15(R3),X'FF'-X'C0'  RE-OPEN THE RECORD                           
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'GOAL OPEN'                                              
         SPACE                                                                  
* PRINT HEXADECIMAL DUMP OF RE-OPENED RECORD *                                  
         SPACE                                                                  
PRT      SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE                                                                  
* PRINT HEXADECIMAL DUMP OF KEY OF RECORD WITH AGENCY ALPHA ERROR *             
         SPACE                                                                  
AGYAERR  NTR1                                                                   
         MVC   WORK(8),0(R1)                                                    
         MVC   WORK+8(23),=CL23'AGENCY ALPHA CODE ERROR'                        
         GOTO1 =V(PRNTBL),DMCB,(31,WORK),(R3),C'DUMP',24,=C'0D'                 
         B     DMXIT                                                            
         SPACE                                                                  
*                                                                               
* SELECT KEEP IF OPEN RUN, PURGE IF LOAD, PURGE IF COPY OPTION                  
*                                                                               
DMXSEL   CLI   SELECT,C'C'         COPY                                         
         BE    DMXPURGE                                                         
         CLI   SELECT,C'O'         OPEN                                         
         BE    DMXKEEP                                                          
         CLI   SELECT,C'L'         PURGE                                        
         BE    DMXPURGE                                                         
         DC    H'0'                BUG                                          
         SPACE                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P+1(L'HDG),HDG                                                   
         GOTO1 VPRINTER                                                         
         LA    R7,REOPNTAB                                                      
DMXEOF10 MVC   P+2(1),TAGY                                                      
         MVC   P+4(2),TAGYA                                                     
         MVC   P+9(1),TMED                                                      
         MVC   P+12(3),TCLT                                                     
         MVC   P+16(3),TPROD                                                    
         MVC   P+21(3),TEST                                                     
         CLC   TEST,TEST2                                                       
         BE    *+14                                                             
         MVI   P+24,C'-'                                                        
         MVC   P+25(3),TEST2                                                    
         SPACE                                                                  
         LA    R2,TGRSORD                                                       
         LA    R3,4                                                             
         LA    R4,P+30                                                          
DMXEOF14 L     R0,0(,R2)                                                        
         EDIT  (R0),(17,0(R4)),2,COMMAS=YES,MINUS=YES                           
         LA    R2,4(,R2)                                                        
         LA    R4,20(,R4)                                                       
         BCT   R3,DMXEOF14                                                      
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R2,TOPCLTS                                                       
         LA    R3,7                                                             
         LA    R4,P+30                                                          
         LA    R5,TITLES                                                        
DMXEOF16 L     R0,0(,R2)                                                        
         EDIT  (R0),(5,0(R4))                                                   
         MVI   5(R4),C'='                                                       
         MVC   6(6,R4),0(R5)                                                    
         LA    R2,4(,R2)                                                        
         LA    R4,13(,R4)                                                       
         LA    R5,6(,R5)                                                        
         BCT   R3,DMXEOF16                                                      
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R3,7                                                             
         LA    R4,P+30                                                          
DMXEOF17 L     R0,0(,R2)                                                        
         EDIT  (R0),(5,0(R4))                                                   
         MVI   5(R4),C'='                                                       
         MVC   6(6,R4),0(R5)                                                    
         LA    R2,4(,R2)                                                        
         LA    R4,13(,R4)                                                       
         LA    R5,6(,R5)                                                        
         BCT   R3,DMXEOF17                                                      
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
* COUNT NUMBER OF ESTIMATES & EXPAND TO 2 BYTE ENTRIES (MIN SORT SIZE)          
         SPACE                                                                  
         LA    R0,L'TESTS                                                       
         LA    R1,TESTS                                                         
         LA    RE,SORTESTS                                                      
         SR    RF,RF                                                            
DMXEOF20 CLI   0(R1),0                                                          
         BE    DMXEOF22                                                         
         MVC   0(1,RE),0(R1)                                                    
         MVI   1(RE),0                                                          
         LA    R1,1(,R1)                                                        
         LA    RE,2(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,DMXEOF20                                                      
         SPACE                                                                  
DMXEOF22 MVI   0(RE),0                                                          
         LR    R6,RF                                                            
         GOTO1 =V(QSORT),DMCB,SORTESTS,(R6),2,2,0                               
         SPACE                                                                  
         LA    R2,SORTESTS                                                      
         MVC   P(12),=C'OPENED EST ='                                           
         SPACE                                                                  
DMXEOF24 LA    R3,28                                                            
         LA    R4,P+12                                                          
         B     *+8                                                              
DMXEOF26 MVI   0(R4),C','                                                       
         ZIC   R0,0(R2)                                                         
         EDIT  (R0),(3,1(R4))                                                   
         LA    R2,2(,R2)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R6,*+8                                                           
         B     DMXEOF28                                                         
         SPACE                                                                  
         CLI   0(R2),0                                                          
         BE    DMXEOF28                                                         
         BCT   R3,DMXEOF26                                                      
         SPACE                                                                  
         GOTO1 VPRINTER                                                         
         B     DMXEOF24                                                         
         SPACE                                                                  
DMXEOF28 GOTO1 VPRINTER                                                         
         CLI   TCLTOK,1            WAS CLIENT ON FILE                           
         BE    *+10                                                             
         MVC   P+5(32),=C'CLIENT NOT FOUND ON AGENCY/MEDIA'                     
         GOTO1 VPRINTER                                                         
         LA    R7,TNEXT                                                         
         CLI   0(R7),0                                                          
         BNE   DMXEOF10                                                         
         SPACE                                                                  
         MVC   P+34(L'HDG-33),HDG+33                                            
         GOTO1 VPRINTER                                                         
         LA    R2,GRSORD                                                        
         LA    R3,P+30                                                          
         LA    R4,4                                                             
DMXEOF40 L     R0,0(,R2)                                                        
         EDIT  (R0),(17,0(R3)),2,COMMAS=YES,MINUS=YES                           
         LA    R2,4(,R2)                                                        
         LA    R3,20(,R3)                                                       
         BCT   R4,DMXEOF40                                                      
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R2,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF50 MVC   P+5(28),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+33)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,33(,R3)                                                       
         BCT   R2,DMXEOF50                                                      
         SPACE                                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
SETPRD   LA    RF,CLIST-CLTHDRD(R3)                                             
         SPACE                                                                  
SETPRD10 CLC   TPROD,0(RF)                                                      
         BE    SETPRD20                                                         
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    SETPRD10                                                         
         DC    H'0'                                                             
SETPRD20 MVC   TBPRD,3(RF)                                                      
         BR    RE                                                               
         SPACE 3                                                                
* CK POL BUY FOR 1 PROD                                                         
         SPACE                                                                  
CKPOL    LA    R6,24(,R3)                                                       
         SPACE                                                                  
CKPOL10  CLI   0(R6),11                                                         
         BL    CKPOL20                                                          
         CLI   0(R6),13                                                         
         BH    CKPOL20                                                          
         CLI   1(R6),10            UNALLOCATED                                  
         BNH   CKPOL20                                                          
         CLC   10(1,R6),TBPRD      REQUESTED PROD                               
         BER   RE                                                               
         CLI   1(R6),18            PIGGYBACK                                    
         BNE   CKPOL20                                                          
         CLC   14(1,R6),TBPRD      REQUESTED PROD                               
         BER   RE                                                               
CKPOL20  ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   CKPOL10                                                          
         LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 3                                                                
* CHECK FOR AGENCY MEDIA, ALSO ALLOW TV AND NETWORK IF COMBINED *               
         SPACE                                                                  
COMTAGYM DS    0H                                                               
         CLC   0(1,RF),TAGYMED                                                  
         BE    COMT10                                                           
         CLI   TMED,C'C'                                                        
         BNER  RE                                                               
         XC    WORK(3),WORK                                                     
         MVZ   WORK(1),TAGYMED                                                  
         MVZ   WORK+1(1),0(RF)                                                  
         CLC   WORK(1),WORK+1                                                   
         BNER  RE                                                               
         MVN   WORK+2(1),0(RF)                                                  
         CLI   WORK+2,8            COMBINED                                     
         BE    COMT10                                                           
         CLI   WORK+2,3            NET                                          
         BE    COMT10                                                           
         CLI   WORK+2,1            TV                                           
         BNER  RE                                                               
         SPACE                                                                  
COMT10   DS    0H                                                               
         CLI   TNAGY,0                                                          
         BER   RE                                                               
         NI    0(RF),X'0F'                                                      
         OC    0(1,RF),TNAGY                                                    
         CLI   TNAGYA,0                                                         
         BER   RE                                                               
         MVC   20(2,R3),TNAGYA                                                  
         CR    R0,R0                                                            
         BR    RE                                                               
         SPACE                                                                  
COMTAGY  DS    0H                                                               
         CLC   0(1,RF),TAGYMED                                                  
         BER   RE                                                               
         CLI   TMED,C'C'                                                        
         BNER  RE                                                               
         XC    WORK(3),WORK                                                     
         MVZ   WORK(1),TAGYMED                                                  
         MVZ   WORK+1(1),0(RF)                                                  
         CLC   WORK(1),WORK+1                                                   
         BNER  RE                                                               
         MVN   WORK+2(1),0(RF)                                                  
         CLI   WORK+2,8            COMBINED                                     
         BER   RE                                                               
         CLI   WORK+2,3            NET                                          
         BER   RE                                                               
         CLI   WORK+2,1            TV                                           
         BR    RE                                                               
         EJECT                                                                  
TOTRD    DC    PL5'0',CL28'TOTAL RECS READ'                                     
TOTCLS   DC    PL5'0',CL28'TOTAL CLOSED OUT'                                    
TCLTCT   DC    PL5'0',CL28'TOT CLTS CLOSED OUT'                                 
CLTCTOP  DC    PL5'0',CL28'CLT RECS OPENED'                                     
TPHDCT   DC    PL5'0',CL28'TOT PROD HDRS CLOSED OUT'                            
PHDCTOP  DC    PL5'0',CL28'PRD HDR RECS OPENED'                                 
TESTCT   DC    PL5'0',CL28'TOT ESTS CLOSED OUT'                                 
ESTCTOP  DC    PL5'0',CL28'EST RECS OPENED'                                     
TBUYCT   DC    PL5'0',CL28'TOT BUYS CLOSED OUT'                                 
BUYCTOP  DC    PL5'0',CL28'BUY RECS OPENED'                                     
TBILCT   DC    PL5'0',CL28'TOT BILLS CLOSED OUT'                                
BILCTOP  DC    PL5'0',CL28'BILL RECS OPENED'                                    
TSBILLCT DC    PL5'0',CL28'TOT STA BILLS CLOSED OUT'                            
SBILCTOP DC    PL5'0',CL28'STA BILL RECS OPENED'                                
TCHSPTCT DC    PL5'0',CL28'TOT CHILD SPOT CLOSED OUT'                           
CHSPCTOP DC    PL5'0',CL28'CHILD SPOT RECS OPENED'                              
TPGESTCT DC    PL5'0',CL28'TOT PG ESTIMATES CLOSED OUT'                         
PGESCTOP DC    PL5'0',CL28'PG ESTIMATE RECS OPENED'                             
TNWSBCT  DC    PL5'0',CL28'TOT NWS BUYER CLOSED OUT'                            
NWSBCTOP DC    PL5'0',CL28'NWS BUYER RECS OPENED'                               
TNWSCCT  DC    PL5'0',CL28'TOT NWS CAMPAIGN CLOSED OUT'                         
NWSCCTOP DC    PL5'0',CL28'NWS CAMPAIGN RECS OPENED'                            
TNWSHCT  DC    PL5'0',CL28'TOT NWS HEADER CLOSED OUT'                           
NWSHCTOP DC    PL5'0',CL28'NWS HEADER RECS OPENED'                              
TNWSDCT  DC    PL5'0',CL28'TOT NWS DETAIL CLOSED OUT'                           
NWSDCTOP DC    PL5'0',CL28'NWS DETAIL RECS OPENED'                              
TSTATUCT DC    PL5'0',CL28'TOT STATUS RECS CLOSED OUT'                          
STATCTOP DC    PL5'0',CL28'STATUS RECS OPENED'                                  
TGOLCT   DC    PL5'0',CL28'TOT GOAL CLOSED OUT'                                 
GOLCTOP  DC    PL5'0',CL28'GOAL RECS OPENED'                                    
TOPNCT   DC    PL5'0',CL28'TOTAL RECS OPENED'                                   
TOTCTRS  EQU   (*-TOTRD)/33                                                     
SELECT   DS    CL1                 C = COPY SELECTED RECS ON FILE               
*                                  O = OPEN EXISTING RECS ON FILE               
*                                  L = CREATE LOAD TAPE (OPEN CLOSED,           
*                                      PURGE OTHERS)                            
WORK     DS    CL80                                                             
BYTE     DS    CL1                                                              
         SPACE                                                                  
* GETRATE WORK AREA                                                             
         SPACE                                                                  
SPOTS    DC    F'0'                                                             
GROSS    DC    F'0'                                                             
NET      DC    F'0'                                                             
ADJ      DC    F'0'                                                             
         SPACE                                                                  
GRSORD   DC    F'0'                                                             
NETORD   DC    F'0'                                                             
GRSPAID  DC    F'0'                                                             
NETPAID  DC    F'0'                                                             
THISPRD  DC    X'00'                                                            
RTITLE   DC    CL100'FIX RE-XXXX SPOT FILES FOR THE FOLLOWING ONLY'             
HDG      DC    C'AGENCY MED CLT PRD  EST-EST      GROSS ORDERED        C        
                NET ORDERED          GROSS PAID            NET PAID'            
         LTORG                                                                  
TITLES   DC    CL6'CLTHDR'                                                      
         DC    CL6'PRDHDR'                                                      
         DC    CL6'ESTHDR'                                                      
         DC    CL6'BUYS  '                                                      
         DC    CL6'BILLS '                                                      
         DC    CL6'ST BIL'                                                      
         DC    CL6'GOALS '                                                      
         DC    CL6'CH SPT'                                                      
         DC    CL6'PG EST'                                                      
         DC    CL6'STATUS'                                                      
         DC    CL6'NWS BY'                                                      
         DC    CL6'NWS CM'                                                      
         DC    CL6'NWS HD'                                                      
         DC    CL6'NWS DT'                                                      
         DS    0D                                                               
REOPEN   DCB   DDNAME=REOPEN,DSORG=PS,RECFM=FB,LRECL=80,               C        
               BLKSIZE=3200,MACRF=GM,EODAD=DMINITEF                             
         SPACE                                                                  
SORTESTS DS    CL512                                                            
REOPNTAB DC 200XL176'00'           ACTUALLY 100 ENTRIES                         
         SPACE 2                                                                
ENDOPN   DS    0D                                                               
         SPACE 2                                                                
* DSECT FOR REOPENED DATA                                                       
         SPACE                                                                  
OPNTABD  DSECT                                                                  
*                         START    END                                          
TAGYMED  DS    XL1        1        1                                            
TAGY     DS    CL1        2        2                                            
TAGYA    DS    CL2        3        4                                            
TMED     DS    CL1        5        5                                            
TCLT     DS    CL3        6        8                                            
TBCLT    DS    XL2        9        10                                           
TPROD    DS    CL3        11       13                                           
TBPRD    DS    XL1        14       14                                           
TEST     DS    CL3        15       17                                           
TBEST    DS    XL1        18       18                                           
TEST2    DS    CL3        19       21                                           
TBEST2   DS    XL1        22       22                                           
TCLTOK   DS    XL1        23       23                                           
TNAGY    DS    XL1        24       24 NEW AGENCY BITS 1-4                       
TNAGYN   DS    XL1        24       24                                           
TNAGYA   DS    CL2        25       26 NEW AGENCY ALPHA                          
         DS    CL4        27       31 SPARE                                     
TGRSORD  DS    F                   32                                           
TNETORD  DS    F                   36                                           
TGRSPAID DS    F                   40                                           
TNETPAID DS    F                   44                                           
TOPCLTS  DS    F                   48                                           
TOPPRDS  DS    F                   52                                           
TOPESTS  DS    F                   56                                           
TOPBUYS  DS    F                   60                                           
TOPBILS  DS    F                   64                                           
TOPSTBS  DS    F                   68                                           
TOPGOLS  DS    F                   72                                           
TOPCHSPT DS    F                   76  I                                        
TOPPGEST DS    F                   80  I                                        
TOPSTUS  DS    F                   84  I                                        
TOPNWSB  DS    F                   88  I                                        
TOPNWSC  DS    F                   92  I                                        
TOPNWSH  DS    F                   96  I                                        
TOPNWSD  DS    F                  100  I                                        
TESTS    DS    XL256              324  352+28                                   
TNEXT    EQU   *                                                                
         SPACE                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
SPGENBUYD DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
CLTHDRD   DSECT                                                                 
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPNWSDTL                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPEXTOPNF 05/01/02'                                      
         END                                                                    
