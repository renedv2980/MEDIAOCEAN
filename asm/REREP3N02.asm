*          DATA SET REREP3N02  AT LEVEL 073 AS OF 10/24/05                      
*PHASE RE3N02C,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE QSORT                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
* NEW CONTRACT TYPES FOR SELNY/INTNY                                            
*                                                                               
* KZMCON# IF NOT IN CONTRACT REC MIGHT BE IF CONTRACT HAS PROD      *           
*            NUMBER RCONPRD-IF BLKS, DONE                                       
*         IF NUMBER - RD PRODUCT REC - RPRDNET#                                 
*                                                                               
* KZTOTAL$  1ST MON IN ARRAY                                                    
* KZNYTEAM                                                                      
* KZCHTEAM                                                                      
* KZLATEAM                                                                      
* KZSFTEAM                                                                      
* KZDATEAM                                                                      
* KZATTEAM                                                                      
         TITLE 'RE3N02 - REREP3N02 - LOTUS INTERFACE TAPE'                      
**********************************************************************          
* UNDER MVS, AN UNLABELLED TAPE IS CREATED BY MEANS OF LABEL=(,NL)   *          
* PARAMETER ON THE APPROPRIATE DD STATEMENT FOR THE OUTPUT TAPE.     *          
**********************************************************************          
         SPACE 2                                                                
*********************************************************************           
*                                                                   *           
*   REREP3N02 - RE3N02 - LOTUS ACCOUNTING TAPE                      *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
*   QOPTION1 = 'M'ONTHLY OR 'W'EEKLY                                *           
*   QOPTION2 = 'E'ESTIMATE/ORDERED RECORDS ONLY ON THE WEEKLY TAPE  *           
*                                                                   *           
*                --- ADD PRINTOUT OF TAPE OUTPUT IF REQUESTOR =     *           
*                    'SPEC PRINT'                                   *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* FEB09/05 (BU ) --- INITIAL ENTRY (COPY OF 37 VIA 3H)              *           
* OCT24/05 (BU ) --- CHANGE AGY CONTRACT RATHER THAN SCREEN NAME    *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - WORK REG                                                          
*        R3 - WORK REG                                                          
*        R4 - WORK REG & TAPE DSECT POINTER                                     
*        R5 - WORK REG                                                          
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER                              
*        R7 -                                                                   
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - SECOND BASE REG                                                   
*        RA - POINTER TO WORKD                                                  
*        RB - FIRST BASE                                                        
*        RC - POINTER TO FILED                                                  
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*                                                                               
RE3N02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3N02,R9,RR=R2                                              
         ST    R2,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         ST    RB,SAVEDRB                                                       
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
         BZ    MAIN30              ZERO IS GOOD RETURN                          
         GOTO1 REPORT                                                           
         MVC   P(24),=C'>>> PROCESSING ERROR <<<'                               
         GOTO1 REPORT                                                           
         B     MAINBAD                                                          
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
         B     TESTEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
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
         DC    AL1(RUNFRST),AL3(INIT)       RUN FIRST                           
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
         DC    AL1(STAFRST),AL3(STAINIT)    STATION FIRST                       
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
         DC    AL1(RUNLAST),AL3(RUNDONE)    RUN LAST CLOSE FILES                
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*                                                                               
*        INITIAL --- PROCESS TO OPEN THE TAPE                                   
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         BAS   RE,REPPROFL                                                      
*                                                                               
         OPEN  (INTFILA,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    INITGOOD                                                         
         DC    H'0'                                                             
*                                                                               
INITGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
REPPROFL NTR1                                                                   
         XC    KEY,KEY         READ REP RECORD FOR CONTRACT PROFILE             
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 REP RECORD MUST BE ON FILE                   
         DC    H'0'                                                             
         BAS   RE,GETREP                                                        
         MVC   DAILYFLG,RREPPROF+27                                             
*                                  SAVE DAILY PACING FLAG                       
*                                                                               
         LA    RE,RREPREC          RECORD IS HERE                               
         ZICM  RF,RREPLEN,2                                                     
         AR    RF,RE                                                            
         MVI   0(RF),0             FORCE 0 AT END OF RECORD                     
*                                                                               
         LA    RE,34(RE)           A(1ST ELEMENT)                               
RPRO0020 EQU   *                                                                
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BE    RPRO0100                                                         
*                                                                               
         CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
         BE    RPRO0040                                                         
*                                                                               
         ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     RPRO0020                                                         
RPRO0040 EQU   *                                                                
*- FIND CONTRACT PROGRAM UNIT WITHIN PROGRAM PROFILE ELEMENT                    
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
         DROP  RE                                                               
*                                                                               
RPRO0050 EQU   *                                                                
         CLI   0(RE),RREPQCNT      LOOKING FOR CONTRACT                         
         BE    RPRO0055                                                         
RPRO0052 EQU   *                                                                
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,RPRO0050                                                      
         B     RPRO0100            NO MATCH. USE DEFAULTS                       
RPRO0055 EQU   *                                                                
*                                                                               
         LA    RE,2(RE)            CONTRACT PROFILES FOUND:                     
*                                     BUMP TO PROFILE BITS                      
*                                                                               
*   THE FOLLOWING BIT TEST AGREES WITH THE DOCUMENTATION                        
*        PROVIDED FOR THIS FUNCTION IN PAN BOOK RECNTPROF,                      
*        WHICH DETAILS THE PROFILE BIT SETTINGS FOR THE                         
*        CONTRACT FUNCTION.                                                     
*                                                                               
**********************************************************************          
*   NOTE:  FIELD 'HALF' IS BEING USED FOR FLAGS WITHIN THIS MODULE.  *          
*        PLEASE DO NOT USE THIS FIELD, AS IT WILL DESTROY THE VALUES *          
*        STORED THEREIN.    BILL UHR.  JAN/2002.                     *          
*                                                                    *          
**********************************************************************          
         XC    HALF,HALF           CLEAR FLAG USE                               
         TM    6(RE),X'80'         PAY S/P IN USE?                              
         BNO   RPRO0100            NO                                           
         OI    HALF,X'80'          YES -TURN ON INDICATOR FLAG                  
RPRO0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        STAINIT --- PROCESS FIRST ENCOUNTER OF A STATION                       
*                                                                               
STAINIT  NTR1                                                                   
*                                                                               
         BAS   RE,BLDSTA                                                        
*                                                                               
         XC    KEY,KEY         READ STATION RECORD FOR INTERFACE CODE           
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),RCREPFL                                                
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 HIGH                                                             
         CLI   KEY,X'02'                                                        
         BNE   SINT20                                                           
         CLC   KEY(27),KEYSAVE                                                  
         BNE   SINT20                                                           
         BAS   RE,GETSTAT                                                       
*                                                                               
         MVC   STARANK(1),RSTARANK                                              
         MVC   STAMKTNM(20),RSTAMKT                                             
         OC    STAMKTNM(20),SPACES SET BIN ZERO TO SPACES                       
*                                                                               
         LA    R1,RSTAELEM                                                      
         SR    R0,R0                                                            
SINT10   ICM   R0,1,1(R1)                                                       
         BZ    SINT20                                                           
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    SINT20                                                           
         CLI   0(R1),8                                                          
         BNE   SINT10                                                           
         MVC   STAIFACE,RSTAOSI-RSTAXXEL(R1)   INTERFACE CODE                   
SINT20   EQU   *                                                                
         SPACE                                                                  
         XC    CTLKEY,CTLKEY       CLEAR KEY FOR STA CTL REC                    
*                                                                               
SINTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        POST --- PROCESS THE CURRENT CONTRACT                                  
*                                                                               
POST     NTR1                                                                   
*                                                                               
         L     R1,CONCNT                                                        
         A     R1,=F'1'                                                         
         ST    R1,CONCNT          CONTRACT COUNTER                              
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+12)                           
         GOTO1 GETBROAD,(R1),WORK+12,WORK,GETDAY,ADDAY                          
         GOTO1 DATCON,(R1),(0,WORK+6),(3,CONEND)                                
         MVC   CONENDC(6),WORK+6                                                
         XC    WORK,WORK                                                        
*                                                                               
         LA    R4,WRK                                                           
         USING KZTPRECD,R4                                                      
         MVI   WRK,C' '                 MOVE IN SPACES                          
         MVC   WRK+1(L'WRK-1),WRK       MOVE IN SPACES                          
*                                                                               
         MVI   KZRECTYP,C'1'                                                    
         MVC   DUB(4),RCONKCON                                                  
         MVI   DUB+4,X'0F'                                                      
         UNPK  WORK(9),DUB(5)                                                   
         MVC   KZCON#,WORK          CONTRACT NUMBER                             
*                                                                               
*   NOTE:  THIS PROGRAM IS ONLY USED BY LOTUS.                                  
*        RIGHT NOW, ONLY CLEAR CHANNEL IS MAKING USE OF THE                     
*        COMPENSATION S/P FEATURE.  HARD CODE IS INSTALLED TO                   
*        INSERT THIS INFORMATION INTO THE OUTPUT FORMAT FOR                     
*        CLEAR CHANNEL ONLY.  ALL OTHER REPS ARE PROCESSED                      
*        AS ORIGINALLY DESIGNED.                                                
*                                                                               
         TM    HALF,X'80'          COMP S/P IN USE?                             
         BNO   POST005             NO  - DO MASTER CONTRACT NUMBER              
         LA    R6,RCONREC          YES - FIND COMP S/P                          
         MVI   BYTE,X'1E'                                                       
         BAS   RE,GETEL                                                         
         BNE   POST002                                                          
         USING RCONRFEL,R6                                                      
         CLC   RCONRPSP,SPACES     ANY VALUE IN FIELD?                          
         BNH   POST002             NO  - DEFAULT TO S/P OF RECORD               
         MVC   KZPAYCOD(5),RCONRPSP                                             
*                                  MOVE COMP S/P + OFFICE                       
         B     POST010                                                          
         DROP  R6                                                               
POST002  EQU   *                                                                
         MVC   KZPAYCOD,RCONSAL    INSERT S/P OF RECORD                         
         MVC   KZPAYOFF,RCONKOFF   INSERT S/P OF RECORD OFFICE                  
         B     POST010                                                          
*                                                                               
* IF MASTER CONTRACT # NOT FOUND HERE, WILL CK RCONPRD LATER-READSUB            
*                                                                               
POST005  EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   BYTE,X'74'                                                       
         BAS   RE,GETEL                                                         
         BNE   POST010                                                          
         USING RCONK4EL,R6                                                      
         MVI   KZMCON#,C'0'                                                     
         MVC   KZMCON#+1(7),RCONK4MC                                            
         DROP  R6                                                               
POST010  DS    0H                                                               
         MVC   KZMKTNAM,STAMKTNM                                                
         MVC   KZSTATN,RCONKSTA    INSERT STATION                               
         MVC   KZGRSGRP,RCONKGRP   INSERT GROUP/SUBGRP                          
         SPACE                                                                  
*  BF = B                                                                       
*  KU = K                                                                       
*  KF = H                                                                       
*  EA = E                                                                       
*  CR = C                                                                       
*  K4 = S                                                                       
*  K6 = D                                                                       
*  S3 = M                                                                       
         SPACE                                                                  
         LA    R0,KDIVTABN                                                      
         LA    R1,KDIVTAB                                                       
         SPACE                                                                  
POST020  CLC   RCONKREP,0(R1)                                                   
         BE    POST024                                                          
         LA    R1,L'KDIVTAB(,R1)                                                
         BCT   R0,POST020                                                       
         B     POST026                                                          
POST024  MVC   KZDIVISN,2(R1)               DIV                                 
         SPACE                                                                  
POST026  MVC   KZDDSREP,RCONKREP            DDS REP CODE                        
         SPACE                                                                  
         CLC   RCONSAL,=C'EC '     TEMPORARY FOR CONVERSION ERROR               
         BNE   POST028             NO  - DUMP IT OUT                            
         CLC   RCONKOFF,=X'0000'                                                
         BNE   POST028                                                          
         MVC   RCONKOFF,=C'NY'     INSERT NEW YORK INTO SLOT                    
POST028  EQU   *                                                                
         LA    R1,KATZOFFC                                                      
POST030  CLC   RCONKOFF,3(R1)                                                   
         BE    POST034                                                          
         LA    R1,LKTZOFF(,R1)                                                  
         CLI   0(R1),0                                                          
         BNE   POST030                                                          
         DC    H'0'                                                             
         SPACE                                                                  
POST034  MVC   KZOFFICE,0(R1)                                                   
         SPACE                                                                  
         CLC   RCONKOFF,=C'SE'     THIS SEATTLE                                 
         BNE   POST036                                                          
         CLC   RCONKREP,=C'SZ'                                                  
         BE    POST036                                                          
         CLC   RCONKREP,=C'S2'                                                  
         BE    POST036                                                          
         MVC   KZOFFICE,=C'065'                                                 
         SPACE                                                                  
POST036  MVC   KZDDSOFF,RCONKOFF            DDS OFF CODE                        
         MVC   KZSALESP(L'RCONSAL),RCONSAL  SALES                               
         SPACE                                                                  
         CLC   RCONKOFF,=C'NY'     DDS OFF CODE                                 
         BNE   *+10                                                             
         MVC   KZNYTEAM,RCONTEM+1                                               
         SPACE                                                                  
         CLC   RCONKOFF,=C'CH'     DDS OFF CODE                                 
         BNE   *+10                                                             
         MVC   KZCHTEAM,RCONTEM+1                                               
         SPACE                                                                  
         CLC   RCONKOFF,=C'LA'     DDS OFF CODE                                 
         BNE   *+10                                                             
         MVC   KZLATEAM,RCONTEM+1                                               
         SPACE                                                                  
         CLC   RCONKOFF,=C'SF'     DDS OFF CODE                                 
         BNE   *+10                                                             
         MVC   KZSFTEAM,RCONTEM+1                                               
         SPACE                                                                  
         CLC   RCONKOFF,=C'DA'     DDS OFF CODE                                 
         BNE   *+10                                                             
         MVC   KZDATEAM,RCONTEM+1                                               
         SPACE                                                                  
         CLC   RCONKOFF,=C'AT'     DDS OFF CODE                                 
         BNE   *+10                                                             
         MVC   KZATTEAM,RCONTEM+1                                               
         SPACE                                                                  
         LA    R6,RCONREC                                                       
         MVI   BYTE,X'18'                                                       
         BAS   RE,GETEL                                                         
*                                                                               
         B     POST040                                                          
*                                                                               
*   LOTUS DOES NOT WANT THE DEVEL FIELDS.  THE PROCESSING OF THIS               
*        ELEMENT IS SKIPPED.                                                    
*                                                                               
***>>>   BNE   POST040                                                          
         USING RCONDVEL,R6                                                      
         MVC   KZNETWSP(L'RCONDVSP),RCONDVSP                                    
         OC    KZNETWSP,SPACES     SET SPACES OVER BINARY ZERO                  
         MVC   KZAGYDCT,RCONDVCT               DEV CONT TYPE                    
         DROP  R6                                                               
POST040  MVC   KZADVERT(L'RCONKADV),RCONKADV   ADVERTISER CODE                  
         SPACE                                                                  
         MVC   KZSALESP+L'RCONSAL(L'KZSALESP-L'RCONSAL),SPACES                  
         MVC   KZSALESP+L'RCONKADV(L'KZADVERT-L'RCONKADV),SPACES                
         MVC   KZAGENCY(L'RCONKAGY),RCONKAGY   AGENCY CODE                      
         MVC   KZAGYOFF,RCONKAOF               AGENCY OFFICE CODE               
*                                                                               
* READSUB FILLS IN KZPROD                                                       
*                                                                               
         BAS   RE,READSUB         READ REST OF INFO FOR PUT REC                 
         MVC   KZPRCDTE,QSTART                                                  
         MVC   KZCUSTNO(5),RCONKSTA                                             
         MVI   KZCUSTNO+5,C'M'                                                  
         CLI   RCONKSTA+4,C' '                                                  
         BE    POST045                                                          
         CLI   RCONKSTA+4,C'L'                                                  
         BE    POST045                                                          
         CLI   RCONKSTA+4,C'T'                                                  
         BNE   POST050                                                          
POST045  EQU   *                                                                
         MVI   KZCUSTNO+4,C'-'                                                  
         MVC   KZCUSTNO+5(2),RCREPFL                                            
         MVI   KZRADTV,C'T'                                                     
         B     POST060                                                          
         SPACE                                                                  
POST050  CLI   RCONKSTA+4,C'A'                                                  
         BE    *+12                                                             
         CLI   RCONKSTA+4,C'F'                                                  
         BNE   *+12                                                             
         MVI   KZRADTV,C'R'                                                     
         B     POST060                                                          
         DC    H'0'                                                             
POST060  DS   0H                                                                
         EJECT                                                                  
         SPACE 3                                                                
****** MONTHLY ********                                                         
         SPACE                                                                  
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
POST202  EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST204             FOUND                                        
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     POST202             GO BACK FOR NEXT                             
POST204  EQU   *                                                                
         LR    R8,R2               SET A(BUCKETS WITHIN MONTH)                  
         LA    R8,BUCKDISP(R8)     PASS MONTH CONTROLS                          
         CLC   CONENDC(4),0(R2)    MONTH DATE IN TABLE: YYMM                    
         BL    POST300                                                          
*                                                                               
         L     R5,TOTORD(R8)       TOTAL ORDERED                                
         TM    FLAG6(R2),X'01'     ANY INVOICE $$?                              
         BNO   POST210             NO                                           
         L     R5,CUASATIN(R8)     CURRENT AS AT INVOICED                       
*        A     R5,CUASATIN(R8)     CURRENT AS AT INVOICED                       
POST210  EQU   *                                                                
         LTR   R5,R5                                                            
         BZ    POST300                                                          
*                                                                               
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KZTOTAL$,DUB                                                     
         AP    TDOLLARS,DUB                                                     
         SPACE                                                                  
* CONVERT AND SEND CONTRACT TYPE                                                
         SPACE                                                                  
*        LA    R0,CONTYPTL                                                      
*        LA    R1,CONTYPTB                                                      
*OST250  CLC   RCONTYPE,0(R1)                                                   
*        BE    POST254                                                          
*        LA    R1,2(,R1)                                                        
*        BCT   R0,POST250                                                       
*        B     POST256                                                          
*OST254  MVC   KZCONTYP,1(R1)                                                   
         SPACE                                                                  
*OST256  DS    0H                                                               
         MVC   KZCONTYP,RCONTYPE                                                
         MVC   KZCONPC,RCONPRD                                                  
         BAS   RE,DOPUT                                                         
         BAS   RE,RUNTOT                                                        
         SPACE                                                                  
         B     POST300                                                          
         SPACE                                                                  
         OC    CTLKEY,CTLKEY       READ STATION CTL RECORD                      
         BNZ   POST280              YES, STATION CONTROL REC UPDATED            
         SPACE                                                                  
         L     R2,AIOAREA                                                       
         L     R6,=A(CTLREC)                                                    
         ST    R6,AIOAREA                                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RCTLREC,R4                                                       
         MVI   RCTLKTYP,RCTLKTYQ                                                
         MVC   RCTLKREP,RCREPFL                                                 
         MVC   RCTLKSTA,RCONKSTA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    POST260                                                          
         XC    0(256,R6),0(R6)                                                  
         MVC   0(27,R6),KEYSAVE                                                 
         MVC   27(2,R6),=H'34'                                                  
         B     POST280                                                          
         SPACE                                                                  
POST260  BAS   RE,GETCTL                                                        
         SPACE                                                                  
POST270  MVI   BYTE,RCTLDNEQ                                                    
         BAS   RE,GETEL                                                         
         BNE   POST276                                                          
         USING RCTLDNEL,R6                                                      
POST274  CLC   RCTLDNYM,LASTYRMO                                                
         BNH   POST276                                                          
         L     R0,=A(CTLREC)                                                    
         GOTO1 =V(HELLO),DMCB,(C'D',REPFILE),(R0),(R6),0                        
         CLI   DMCB+12,0           OKAY?                                        
         BE    POST276                                                          
         DC    H'0'                                                             
POST276  BAS   RE,NEXTEL                                                        
         BE    POST274                                                          
POST280  DS   0H                                                                
         XC    WRK,WRK                                                          
         LA    R6,WRK                                                           
         USING RCTLDNEL,R6                                                      
         MVI   RCTLDNEC,RCTLDNEQ                                                
         MVI   RCTLDNLN,RCTLDNX                                                 
         MVC   RCTLDNYM,CURRYRMO                                                
         MVC   RCTLDNDT,TODAYP                                                  
         TIME  DEC                                                              
         STCM  R0,15,RCTLDNTM                                                   
         L     R0,=A(CTLREC)                                                    
         GOTO1 =V(HELLO),DMCB,(C'P',REPFILE),(R0),(R6),0                        
         SPACE                                                                  
         LA    RF,PREC                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+8                                                              
         LA    RF,AREC                                                          
         GOTO1 (RF)                                                             
         MVC   CTLKEY,KEYSAVE                                                   
         ST    R2,AIOAREA          RESTORE AIOAREA                              
         SPACE 3                                                                
POST300  XC    KEY,KEY            RE-SET KEY (CONTRACT) FOR CONTROLLER          
         MVC   KEY(27),RCONKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
*        POST EXIT                                                              
*                                                                               
POSTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        GETAMTS --- GET THE OTHER DOLLAR AMOUNTS                               
*                                                                               
*ETAMTS  NTR1                                                                   
*                                                                               
*        L     R2,ANEWMON          A(NEW MONTH TABLE)                           
*                                                                               
GETAMT02 EQU   *                                                                
*        CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
*        BE    GETAMT05            FOUND                                        
*        LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
*        B     GETAMT02            GO BACK FOR NEXT                             
GETAMT05 EQU   *                                                                
*        CLC   0(4,R2),GROSSDT2                                                 
*        BE    GETAMT10                                                         
*        CLI   0(R2),0                                                          
*        BE    GETAMTX                                                          
*        CLC   0(4,R2),QFAEND      TABLE ENTRY VS REQ END   DATE                
*        BH    GETAMTX                                                          
*        LA    R2,NEXTBUCK(R2)                                                  
*        B     GETAMT05                                                         
*                                                                               
GETAMT10 EQU   *                                                                
*        LR    R4,R2               SET A(BUCKETS IN MONTH)                      
*        LA    R4,BUCKDISP(R4)     SKIP MONTH CONTROLS                          
*                                                                               
*        L     R5,CHAGROS                                                       
*        XC    CHACOMD,CHACOMD                                                  
*        GOTO1 DOCOMM,DMCB,(R2),(R5),CHACOMD,CHACOMR,1                          
*                                                                               
*        CLI   RECALC,3              SPECIAL BLOCK FOR 1 ORDERED EL             
*        BNE   GETAMT20              AND 1 INV EL BOTH IN SAME WEEK             
*        MVC   ABSGROS(4),CHAGROS                                               
*        MVC   ABSCOMD(4),CHACOMD                                               
*        MVC   ABSCOMR(4),CHACOMR                                               
*        B     GETAMT29                                                         
GETAMT20 EQU   *                                                                
*        CLI   RECALC,2                                                         
*        BE    GETAMT22                                                         
*        CLI   RECALC,5                                                         
*        BNE   GETAMT24                                                         
GETAMT22 L     R5,ORDAMT                                                        
*        XC    ORDAMT(4),ORDAMT                                                 
*        B     GETAMT28                                                         
GETAMT24 EQU   *                                                                
*        CLI   RECALC,1                                                         
*        BE    GETAMT25                                                         
*        CLI   RECALC,6                                                         
*        BNE   GETAMT26                                                         
GETAMT25 EQU   *                                                                
*        L     R5,CUASATIN(R4)     CURRENT AS AT INVOICED                       
*        B     GETAMT28                                                         
GETAMT26 EQU   *                                                                
*        L     R5,TOTORD(R4)       TOTAL ORDERED                                
*        TM    FLAG6(R2),X'01'                                                  
*        BO    GETAMT28                                                         
*        A     R5,CUASATIN(R4)                                                  
GETAMT28 EQU   *                                                                
*        ST    R5,ABSGROS                                                       
*        XC    ABSCOMD,ABSCOMD    FOR COMMISSION AMT                            
*        GOTO1 DOCOMM,DMCB,(R2),(R5),ABSCOMD,ABSCOMR,1                          
GETAMT29 EQU   *                                                                
*                                                                               
*        CLI   RECALC,0            RECALC FLAG ON?                              
*        BE    GETAMT30                                                         
*        CLI   RECALC,5            SPECIAL ORDER AND ACT SAME WEEK              
*        BE    GETAMT30             WITH MULTIPLE ORDERED ELEMENTS              
*        CLI   RECALC,2                                                         
*        BNE   GETAMT40            NO                                           
GETAMT30 EQU   *                                                                
*        OC    ABSGROS,ABSGROS     ANY ABSOLUTE GROSS?                          
*        BNZ   GETAMT32            YES - CHECK NO FURTHER                       
*        OC    ABSCOMD,ABSCOMD     ANY ABSOLUTE COMM $?                         
*                                  REALLY SHOULDN'T BE: NO GROSS                
*        BNZ   GETAMT32            YES                                          
*        OC    ABSCOMR,ABSCOMR     ANY ABSOLUTE COMM RATE?                      
*        BNZ   GETAMT32            YES                                          
*        MVC   ABSCOMR,CHACOMR     SET COMM RATES EQUAL                         
GETAMT32 EQU   *                                                                
*        L     R1,CHAGROS                                                       
*        S     R1,ABSGROS                                                       
*        ST    R1,ABSGROS                                                       
*        L     R1,CHACOMD                                                       
*        S     R1,ABSCOMD                                                       
*        ST    R1,ABSCOMD                                                       
*                                                                               
*        L     R1,CHAGROS                                                       
*        L     R2,CHACOMD                                                       
*        L     R3,ABSGROS                                                       
*        L     R4,ABSCOMD                                                       
*        L     R5,ABSCOMR                                                       
*        ST    R1,ABSGROS                                                       
*        ST    R2,ABSCOMD                                                       
*        ST    R3,CHAGROS                                                       
*        ST    R4,CHACOMD                                                       
*        ST    R5,CHACOMR                                                       
GETAMT40 EQU   *                                                                
*        OC    CHACOMR,CHACOMR     ANY COMMISSION RATE?                         
*        BNZ   GETAMT42            YES                                          
*        OC    ABSCOMD,ABSCOMD     ANY ABSOLUTE COMM $?                         
*        BZ    GETAMT42            NO  - NO RATE NEEDED                         
*        MVC   CHACOMR,ABSCOMR     YES - USE CHANGED COMM RATE                  
GETAMT42 EQU   *                                                                
*        GOTO1 EDITOUT,DMCB,ABSGROS,TGROSS,9,0                                  
*        GOTO1 EDITOUT,DMCB,ABSCOMD,TCOMMAT,9,0                                 
*        GOTO1 EDITOUT,DMCB,CHAGROS,TCHGROSS,9,0                                
*        GOTO1 EDITOUT,DMCB,CHACOMD,TCHCOMAT,9,0                                
*        GOTO1 EDITOUT,DMCB,CHACOMR,TCOMMRT,6,1                                 
*                                                                               
GETAMTX  EQU   *                                                                
*        SR    R0,R0                                                            
*        B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        RPTDONE --- FINISH THE REPORT                                          
*                                                                               
RPTDONE  NTR1                                                                   
*                                                                               
         GOTO1 =A(NRPTDONE),DMCB,(RC)                                           
         SR    R0,R0                                                            
         XIT1                                                                   
*                                                                               
*        RPTDONE --- CLOSE FILES, WRITE AGY, ADV, SALES RECS                    
*                                                                               
RUNDONE  NTR1                                                                   
*                                                                               
         SPACE                                                                  
         CLOSE (INTFILA,LEAVE)            CLOSE LOTUS FILE                      
         SPACE                                                                  
         OPEN  (INTFILB,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R4,WRK                                                           
         USING KZTPRECD,R4                                                      
         MVI   WRK,C' '                 MOVE IN SPACES                          
         MVC   WRK+1(L'WRK-1),WRK       MOVE IN SPACES                          
         XC    KEY,KEY            BUILD KEY FOR AGENCY                          
         MVI   KEY,X'0A'                                                        
         GOTO1 HIGH                                                             
         B     RUND20                                                           
         SPACE                                                                  
RUND10   GOTO1 SEQ                                                              
         SPACE                                                                  
RUND20   CLI   KEY,X'0A'                                                        
         BNE   RUND30                                                           
         SPACE                                                                  
         BAS   RE,CKREP            SEE IF MASTER REP NEEDED                     
         BNE   RUND21               NO                                          
         SPACE                                                                  
         CLC   KEY+25(2),MASTREP                                                
         BNE   RUND10                                                           
         B     RUND22                                                           
         SPACE                                                                  
RUND21   CLC   KEY+25(2),RCREPFL                                                
         BNE   RUND10                                                           
         SPACE                                                                  
RUND22   BAS   RE,GETAGY                                                        
         MVI   KZRECTP2,C'2'                                                    
         MVC   KZDDSAGY,KEY+19                                                  
         MVC   KZAGYNAM,RAGYNAM2                                                
         MVC   FULL,=C'0000'       IF KZDDSAGY IS NUMERIC                       
         MVZ   FULL,KZDDSAGY        WE USED ORIGINAL CODE                       
         CLC   FULL,=C'0000'                                                    
         BE    RUND24                                                           
         SPACE                                                                  
         MVI   KEY,X'1A'                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RUND26                                                           
         BAS   RE,GETAGY                                                        
         MVC   KZKZAGEQ,RAGY2EQU    LOTUS EQUIV AGENCY                          
         B     RUND26                                                           
         SPACE                                                                  
RUND24   DS    0H                                                               
         MVC   KZKZAGEQ,KZDDSAGY    LOTUS EQUIV AGENCY                          
RUND26   DS    0H                                                               
         PUT   INTFILB,WRK                                                      
*                                                                               
         CLI   HALF2,C'Y'          'SPEC PRINT' RUN?                            
         BNE   RUND27                                                           
         LA    R4,KZTPREC2         A(LOTUS TAPE RECORD)                         
         LA    RF,144                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
RUND27   EQU   *                                                                
         L     R1,TAGYCTR                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TAGYCTR                                                       
         SPACE                                                                  
         CLC   FULL,=C'0000'       2ND AGY REC READ                             
         BE    RUND10               NO JUST GO TO SEQ                           
         MVI   KEY,X'0A'                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    RUND10                                                           
         DC    H'0'                                                             
         SPACE                                                                  
RUND30   DS    0H                                                               
         MVC   P+5(15),=CL24'TOTAL AGENCY  ='                                   
         ICM   R5,15,TAGYCTR                                                    
         EDIT  (R5),(17,P+20),0,ZERO=NOBLANK,COMMAS=YES                         
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY            BUILD KEY FOR ADVERTISER                      
         MVI   KEY,X'08'                                                        
         GOTO1 HIGH                                                             
         MVI   WRK,C' '                 MOVE IN SPACES                          
         MVC   WRK+1(L'WRK-1),WRK       MOVE IN SPACES                          
         B     RUND50                                                           
         SPACE                                                                  
RUND40   GOTO1 SEQ                                                              
         SPACE                                                                  
RUND50   CLI   KEY,X'08'                                                        
         BNE   RUND60                                                           
         SPACE                                                                  
         BAS   RE,CKREP            SEE IF MASTER REP NEEDED                     
         BNE   RUND52               NO                                          
         SPACE                                                                  
         CLC   KEY+25(2),MASTREP                                                
         BNE   RUND40                                                           
         B     RUND54                                                           
         SPACE                                                                  
RUND52   CLC   KEY+25(2),RCREPFL                                                
         BNE   RUND40                                                           
         SPACE                                                                  
RUND54   BAS   RE,GETADV                                                        
         MVI   KZRECTP3,C'3'                                                    
         MVC   KZDDSADV,KEY+21                                                  
         MVC   KZKZADEQ,RADVKATZ    LOTUS EQUIV AGENCY                          
         MVC   KZADVNAM,RADVNAME                                                
         PUT   INTFILB,WRK                                                      
         L     R1,TADVCTR                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TADVCTR                                                       
         B     RUND40                                                           
         SPACE                                                                  
RUND60   DS    0H                                                               
         MVC   P+5(15),=CL24'TOTAL ADVERT  ='                                   
         ICM   R5,15,TADVCTR                                                    
         EDIT  (R5),(17,P+20),0,ZERO=NOBLANK,COMMAS=YES                         
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY            BUILD KEY FOR SALESP                          
         MVI   KEY,X'06'                                                        
         GOTO1 HIGH                                                             
         MVI   WRK,C' '                 MOVE IN SPACES                          
         MVC   WRK+1(L'WRK-1),WRK       MOVE IN SPACES                          
         B     RUND72                                                           
         SPACE                                                                  
RUND70   GOTO1 SEQ                                                              
         SPACE                                                                  
RUND72   CLI   KEY,X'06'                                                        
         BNE   RUND78                                                           
         SPACE                                                                  
         BAS   RE,CKREP            SEE IF MASTER REP NEEDED                     
         BNE   RUND74               NO                                          
         SPACE                                                                  
         CLC   KEY+22(2),MASTREP                                                
         BNE   RUND70                                                           
         B     RUND76                                                           
         SPACE                                                                  
RUND74   CLC   KEY+22(2),RCREPFL                                                
         BNE   RUND70                                                           
         SPACE                                                                  
RUND76   BAS   RE,GETMAN                                                        
         SR    R0,R0                                                            
         ICM   R0,7,RSALMRG                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   KZRECTP4,C'4'                                                    
         MVC   KZDDSSAL,KEY+24                                                  
         UNPK  KZKZSPEQ,DUB                                                     
         MVC   KZSALNAM,RSALNAME                                                
         MVC   KZSALOFF,RSALOFF                                                 
         MVC   KZSALTEM,RSALTEAM                                                
         MVC   KZSALDIV,RCREPFL                                                 
*        MVC   KZSALDIV,=C'KU'                                                  
         SPACE                                                                  
         PUT   INTFILB,WRK                                                      
         L     R1,TSALCTR                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TSALCTR                                                       
         B     RUND70                                                           
         SPACE                                                                  
RUND78   DS    0H                                                               
         MVC   P+5(15),=CL24'TOTAL SALES   ='                                   
         ICM   R5,15,TSALCTR                                                    
         EDIT  (R5),(17,P+20),0,ZERO=NOBLANK,COMMAS=YES                         
         GOTO1 REPORT                                                           
         SPACE                                                                  
*                                                                               
         XC    KEY,KEY            BUILD KEY FOR PRODUCT                         
         MVI   KEY,X'09'                                                        
         GOTO1 HIGH                                                             
         B     RUND82                                                           
         SPACE                                                                  
RUND80   GOTO1 SEQ                                                              
         SPACE                                                                  
RUND82   CLI   KEY,X'09'                                                        
         BNE   RUND88                                                           
         SPACE                                                                  
         BAS   RE,CKREP            SEE IF MASTER REP NEEDED                     
         BNE   RUND84               NO                                          
         SPACE                                                                  
         CLC   KEY+25(2),MASTREP                                                
         BNE   RUND80                                                           
         B     RUND86                                                           
         SPACE                                                                  
RUND84   CLC   KEY+25(2),RCREPFL                                                
         BNE   RUND80                                                           
         SPACE                                                                  
RUND86   BAS   RE,GETPROD                                                       
         MVI   WRK,C' '                 MOVE IN SPACES                          
         MVC   WRK+1(L'WRK-1),WRK       MOVE IN SPACES                          
         MVI   KZRECTP5,C'5'                                                    
         MVC   KZDDSPC,KEY+22                                                   
         MVC   KZDDSPAD,KEY+18                                                  
         MVC   KZPRDNAM,RPRDNAME                                                
         MVC   KZPRDCON,RPRDNET#                                                
         SPACE                                                                  
         LA    R6,RPRDREC                                                       
         MVI   BYTE,02                                                          
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         SPACE                                                                  
         MVC   KZPRDPTP,RPRDNPNT-RPRDNELM(R6)                                   
         SPACE                                                                  
         LA    R6,RPRDREC                                                       
         MVI   BYTE,04                                                          
         BAS   RE,GETEL                                                         
         BNE   RUND87                                                           
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,RPRDAGDF-RPRDAGFL(R6)),(5,KZPRDFLT)               
         MVI   KZPRDFLT+8,C'-'                                                  
         GOTO1 (RF),(R1),(3,RPRDAGDT-RPRDAGFL(R6)),(5,KZPRDFLT+9)               
         SPACE                                                                  
         OC    RPRDAGAG-RPRDAGFL(L'RPRDAGAG,R6),RPRDAGAG-RPRDAGFL(R6)           
         BZ    RUND87                                                           
         MVC   KZPRDAGY,RPRDAGAG-RPRDAGFL(R6)                                   
         MVC   KZPRDOFF,RPRDAGAO-RPRDAGFL(R6)                                   
         SPACE                                                                  
         MVC   WORK,KEY                                                         
         XC    KEY,KEY            BUILD KEY FOR AGENCY                          
         LA    R6,KEY                                                           
         USING RAGYKEY,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,KZPRDAGY                                                
         MVC   RAGYKAOF,KZPRDOFF                                                
         MVC   RAGYKREP,RCREPFL                                                 
         OC    MASTREP,MASTREP                                                  
         BZ    *+10                                                             
         MVC   RAGYKREP,MASTREP                                                 
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(27),KEYSAVE                                                  
         BE    RUND86A                                                          
         MVC   KZPRDAGN(20),=C'CODE NOT ON FILE    '                            
         B     RUND86B                                                          
RUND86A  EQU   *                                                                
         BAS   RE,GETAGY                                                        
         MVC   KZPRDAGN,RAGYNAM2                                                
RUND86B  EQU   *                                                                
         SPACE                                                                  
         MVC   KEY,WORK            RESTORE PRODUCT KEY                          
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
RUND87   DS    0H                                                               
         PUT   INTFILB,WRK                                                      
         L     R1,TPRDCTR                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,TPRDCTR                                                       
         B     RUND80                                                           
         DROP  R4                                                               
RUND88   DS    0H                                                               
         MVC   P+5(15),=CL24'TOTAL PRODUCTS='                                   
         ICM   R5,15,TPRDCTR                                                    
         EDIT  (R5),(17,P+20),0,ZERO=NOBLANK,COMMAS=YES                         
         GOTO1 REPORT                                                           
         SPACE                                                                  
         CLOSE (INTFILB,)            CLOSE LOTUS FILE                           
         SPACE                                                                  
         GOTO1 REPORT                                                           
         MVC   P+1(18),=CL18'*** END OF RUN ***'                                
         GOTO1 REPORT                                                           
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
GBEXT    XIT1                                                                   
         SPACE                                                                  
CKREP    DS    0H                                                               
         MVC   MASTREP,=C'K3'                                                   
         LA    R0,K3REPTCT                                                      
         LA    R1,K3REPTAB                                                      
CKREP10  CLC   RCREPFL,0(R1)                                                    
         BER   RE                                                               
         LA    R1,2(,R1)                                                        
         BCT   R0,CKREP10                                                       
         SPACE                                                                  
         MVC   MASTREP,=C'MR'                                                   
         LA    R0,MRREPTCT                                                      
         LA    R1,MRREPTAB                                                      
CKREP20  CLC   RCREPFL,0(R1)                                                    
         BER   RE                                                               
         LA    R1,2(,R1)                                                        
         BCT   R0,CKREP20                                                       
         XC    MASTREP,MASTREP                                                  
         LTR   RB,RB                                                            
         BR    RE                                                               
K3REPTAB DC    C'BF',C'CR',C'EA',C'KF',C'KU',C'K4',C'K6',C'S3'                  
         DC    C'QD',C'J0',C'WC',C'G8'                                          
K3REPTCT EQU   (*-K3REPTAB)/2                                                   
MRREPTAB DC    C'AM',C'CQ',C'NK'                                                
MRREPTCT EQU   (*-MRREPTAB)/2                                                   
MASTREP  DS    CL2                                                              
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*        CHKINVEL --- IF WE HAVE AN ORDERED ELEMENT X'03' FOR A MONTH,          
*                      DO WE HAVE AN INVOICE ELEMENT X'04' FOR THE SAME         
*                      PERIOD?  IF WE HAVE A X'04' EL, LOOK FOR A X'03'         
*                                                                               
*        R3  ->  CURRENT ELEMENT                                                
*                                                                               
CHKINVEL NTR1                                                                   
*                                                                               
         LA    R4,RCONELEM                                                      
         ZIC   R5,0(R3)                                                         
         CLI   0(R3),3             HAVE A 3, LOOK FOR A 4                       
         BE    CIEL10              HAVE A 4, LOOK FOR A 3                       
         BCTR  R5,0                                                             
         B     CIEL11                                                           
CIEL10   EQU   *                                                                
         LA    R5,1(R5)                                                         
CIEL11   EQU   *                                                                
         SR    R6,R6               USE R6 AS ORDERED EL COUNT                   
         SR    R7,R7               USE R7 AS ORDERED DOL ACCUM                  
*                                                                               
CIEL50   EQU   *                                                                
         ZICM  R0,1(R4),1                                                       
         BZ    CIEL70              ZERO IS END OF REC W/O A FIND                
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    CIEL70                                                           
         ZIC   R0,0(R4)                                                         
         CR    R0,R5                                                            
         BNE   CIEL50                                                           
*                                                                               
         CLC   2(2,R4),2(R3)       CHECK MONTH-OF-SERVICE                       
         BNE   CIEL50                                                           
*                                                                               
         CLC   4(2,R4),COMPDT      FOR A HISTORICAL RUN CHECK ACTIVITY          
         BNL   CIEL60                                                           
         LA    R6,1(R6)            BUMP COUNT                                   
         CLI   0(R4),X'03'         ACCUM ORDERED DOLLARS ONLY                   
         BNE   CIEL50                                                           
         A     R7,6(R4)            AND ACCUM DOLLARS                            
         B     CIEL50                                                           
CIEL60   EQU   *                                                                
         CLI   0(R4),X'04'         AN ACTUAL BUCKET FOUND RETURNS FOUND         
         BNE   CIEL70              IF IN THE SAME WEEK                          
         CLC   4(2,R4),COMPDT                                                   
         BE    CIELBAD                                                          
CIEL70   EQU   *                                                                
         LTR   R6,R6                                                            
         BNZ   CIELBAD                                                          
*                                                                               
CIELGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     CIELEXIT                                                         
CIELBAD  EQU   *                                                                
         LA    R0,1                                                             
CIELEXIT EQU   *                                                                
         STC   R6,ORDCNT                                                        
         ST    R7,ORDAMT                                                        
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        CHKSAMEW --- IF WE HAVE AN INVOICE ELEMENT X'04' FOR A MONTH,          
*                      DO WE HAVE AN ORDERED ELEMENT X'03' FOR THE SAME         
*                      PERIOD ON THE SAME WEEK WITH NO PREVIOUS X'03'S          
*                                                                               
*        R3  ->  CURRENT ELEMENT                                                
*                                                                               
CHKSAMEW NTR1                                                                   
*                                                                               
         LA    R4,RCONELEM                                                      
         CLI   0(R3),3             HAVE A 3, GET OUT                            
         BE    CWKGOOD                                                          
         SR    R6,R6               USE R6 AS ORDERED EL COUNT                   
         SR    R7,R7               USE R7 AS ORDERED DOL ACCUM                  
*                                                                               
CWK50    EQU   *                                                                
         ZICM  R0,1(R4),1                                                       
         BZ    CWKGOOD             ZERO IS END OF REC W/O A FIND                
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    CWKGOOD                                                          
         CLI   0(R4),X'03'                                                      
         BNE   CWK50                                                            
         CLC   2(2,R4),2(R3)       CHECK MONTH-OF-SERVICE                       
         BNE   CWK50                                                            
         LA    R6,1(R6)                                                         
*                                                                               
         CLC   4(2,R4),4(R3)       CHECK ACTIVITY WEEK                          
         BE    CWK60               EQUAL IS GET OUT WITH ERROR                  
         A     R7,6(R4)                                                         
         B     CWK50               NOT EQUAL IS GET NEXT                        
*                                                                               
CWK60    EQU   *                                                                
         CLI   RECALC,0                                                         
         BE    CWK70                                                            
         A     R7,6(R4)                                                         
*                                                                               
CWK70    EQU   *                                                                
         STC   R6,ORDCNT                                                        
         ST    R7,ORDAMT                                                        
         B     CWKBAD                                                           
*                                                                               
CWKGOOD  EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
CWKBAD   EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        LOCAL EDIT ROUTINES TO PRODUCED ZERO FILLED SIGN OVERPUNCHED           
*         NUMBERS FOR THE TAPE                                                  
*                                                                               
*                                                                               
*        P1   =  A(INPUT)                                                       
*        P2   =  A(OUTPUT)                                                      
*        P3   =  (DATA WIDTH, 6 OR 9)                                           
*        P4   =  (DO OVERPUCH FLAG, 0 = YES)                                    
*                                                                               
EDITOUT  NTR1                                                                   
*                                                                               
         L     R2,0(R1)            GET INPUT ADDR                               
         L     R3,4(R1)            GET OUTPUT ADDR                              
         L     R4,8(R1)            GET OUTPUT WIDTH                             
         L     R8,12(R1)           GET OVERPUCH FLAG                            
         SR    R5,R5               USE R5 AS A NEG FLAG                         
*                                                                               
         ICM   R6,15,0(R2)         INPUT NEG?                                   
         BNM   EOUT10              NO, MOVE ON                                  
*                                                                               
         SR    R6,R6               YES, WORK WITH POS NUMBER AND                
         BCTR  R6,0                 SET FLAG                                    
         X     R6,0(R2)                                                         
         LA    RF,1                ADD 1 TO POSITIVE NUMBER                     
         AR    R6,RF               NOTE:  DO 'AR' INSTRUCTION BECAUSE           
*                                     LARGE NEGATIVE NUMBERS LOSE               
*                                     HIGH-ORDER BYTE IN FORMAT                 
*                                     LA RX,1(RX)                               
         LA    R5,1                                                             
EOUT10   EQU   *                                                                
*                                                                               
         LA    R7,9                                                             
         CR    R4,R7                                                            
         BE    EOUT20                                                           
         EDIT  (R6),(6,(R3)),ZERO=NOBLANK                                       
         B     EOUT30                                                           
EOUT20   EQU   *                                                                
         EDIT  (R6),(9,(R3)),ZERO=NOBLANK                                       
EOUT30   EQU   *                                                                
         LR    R1,R4               COPY INPUT LEN FOR ZERO FILLING              
         LTR   R8,R8               CHECK OVERPUCH FLAG                          
         BNZ   EOUT50              NON-ZERO IS DON'T DO                         
         AR    R4,R3               POINT TO THE LAST CHARACTER                  
         BCTR  R4,0                                                             
         LTR   R5,R5                                                            
         BZ    EOUT50                                                           
         NI    0(R4),X'DF'         ONLY DO NEG OVERPUCH = DF                    
EOUT50   EQU   *                                                                
         CLI   0(R3),C' '                                                       
         BNE   EOUT60                                                           
         MVI   0(R3),C'0'                                                       
         LA    R3,1(R3)                                                         
         BCT   R1,EOUT50                                                        
EOUT60   EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        RUNTOT --- KEEP RUNNING TOTAL DOLLARS                                  
*                                                                               
RUNTOT   NTR1                                                                   
*                                                                               
         LA    R1,RTOTLIST                                                      
         LA    R2,4                DO FOUR TIMES                                
*                                                                               
RTOT10   EQU   *                                                                
         L     R3,0(R1)            A(THIS CONTRACT AMOUNT)                      
         L     R4,4(R1)            A(RUNNING TOTAL AMOUNT)                      
         L     R5,0(R3)            THIS CONTRACT AMOUNT                         
         A     R5,4(R4)            ADD IN RUNNING TOTAL                         
         BNO   RTOT20              CHECK FOR OVERFLOW                           
         L     R6,0(R4)                                                         
         A     R6,=F'1'                                                         
         ST    R6,0(R4)                                                         
RTOT20   EQU   *                                                                
         ST    R5,4(R4)                                                         
*                                                                               
         XC    0(4,R3),0(R3)                                                    
*                                                                               
         LA    R1,8(R1)                                                         
         BCT   R2,RTOT10                                                        
*                                                                               
*        XC    TGROSS,TGROSS                                                    
*        XC    TCOMMAT,TCOMMAT                                                  
*        XC    TCOMMRT,TCOMMRT                                                  
*        XC    TCHGROSS,TCHGROSS                                                
*        XC    TCHCOMAT,TCHCOMAT                                                
*                                                                               
RTOTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
*                                                                               
RTOTLIST DC    AL4(ABSGROS),AL4(RTOTABGR)                                       
         DC    AL4(ABSCOMD),AL4(RTOTABCM)                                       
         DC    AL4(CHAGROS),AL4(RTOTCHGR)                                       
         DC    AL4(CHACOMD),AL4(RTOTCHCM)                                       
         EJECT                                                                  
*                                                                               
*        DOPUT --- LOGICAL OR IN SPACES TO TAKE CARE OR NULLS                   
*                   AND PUT THE RECORD                                          
*                                                                               
DOPUT    NTR1                                                                   
*                                                                               
         OC    000(130,R6),SPACES                                               
         OC    130(130,R6),SPACES                                               
         OC    260(040,R6),SPACES                                               
*                                                                               
*                                                                               
         XC    ABSGROS,ABSGROS                                                  
         XC    ABSCOMD,ABSCOMD                                                  
         XC    CHAGROS,CHAGROS                                                  
         XC    CHACOMD,CHACOMD                                                  
*        B     DPUTGOOD                                                         
*                                                                               
DPUT20   EQU   *                                                                
*                                                                               
         PUT   INTFILA,WRK                                                      
*                                                                               
         L     R1,PUTCNT                                                        
         LA    R1,1(,R1)                                                        
         ST    R1,PUTCNT                                                        
*                                                                               
*    IF REQUESTOR = 'SPEC PRINT' CALL MODULE TO DISPLAY TAPE                    
*       RECORD ON PRINTOUT                                                      
*                                                                               
         CLC   =C'SPEC PRINT',QUESTOR                                           
         BNE   DPUTGOOD                                                         
         GOTO1 =A(DUMPTAPE),DMCB,(RC)                                           
****>>   LA    R4,KZTPREC          A(LOTUS TAPE RECORD)                         
         LA    RF,144                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
                                                                                
*                                                                               
DPUTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        READ ADVERTISER/AGENCY/SALES/PRODUCT/STATION FOR OUTPUT REC            
*                                                                               
         USING KZTPRECD,R4                                                      
READSUB  NTR1                                                                   
*                                                                               
READS18  LA    R2,AGYTABCT                                                      
         L     R3,=A(AGYTABLE)                                                  
READS20  CLC   KZAGENCY(4),0(R3)                                                
         BE    READS26                                                          
         OC    0(4,R3),0(R3)                                                    
         BZ    READS24                                                          
         LA    R3,L'AGYTABLE(,R3)                                               
         BCT   R2,READS20                                                       
         SPACE                                                                  
         L     RE,=A(AGYTABLE)                                                  
         L     RF,=A(L'AGYTABLE*AGYTABCT)                                       
         XCEF                                                                   
         B     READS18                                                          
         SPACE                                                                  
READS24  MVC   0(4,R3),KZAGENCY                                                 
         MVC   4(2,R3),RCONKAOF                                                 
         MVC   10(20,R3),=CL20'NAME NOT FOUND'     DEFAULT                      
         MVC   30(13,R3),SPACES                                                 
         SPACE                                                                  
         XC    KEY,KEY            BUILD KEY FOR AGENCY                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),KZAGENCY                                               
         MVC   KEY+23(2),RCONKAOF  AGENCY CITY CODE                             
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'0A'                                                        
         BNE   READS26                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   READS26                                                          
         BAS   RE,GETAGY                                                        
         MVC   10(33,R3),RAGYNAM2                                               
         SPACE                                                                  
READS26  XC    KEY,KEY            BUILD KEY FOR AGENCY                          
         MVI   KEY,X'1A'                                                        
         MVC   KEY+19(4),KZAGENCY                                               
         MVC   KEY+23(2),RCONKAOF  AGENCY CITY CODE                             
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'1A'                                                        
         BNE   READS28                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   READS28                                                          
         BAS   RE,GETAGY                                                        
         MVC   6(4,R3),RAGY2EQU    LOTUS EQUIV AGENCY                           
READS28  DS    0H                                                               
         LA    R2,SALTABCT                                                      
         L     R3,=A(SALTABLE)                                                  
READS30  CLC   KZSALESP(3),0(R3)                                                
         BE    READS36                                                          
         OC    0(3,R3),0(R3)                                                    
         BZ    READS34                                                          
         LA    R3,L'SALTABLE(,R3)                                               
         BCT   R2,READS30                                                       
         SPACE                                                                  
         L     RE,=A(SALTABLE)                                                  
         L     RF,=A(L'SALTABLE*SALTABCT)                                       
         XCEF                                                                   
         B     READS28                                                          
         SPACE                                                                  
READS34  MVC   0(3,R3),KZSALESP                                                 
         MVC   8(20,R3),=CL20'NAME NOT FOUND'     DEFAULT                       
         SPACE                                                                  
READS36  DS    0H                                                               
         SPACE                                                                  
         CLC   RCONPRD,SPACES      IS THERE A PROD CODE?                        
         BNE   RDSUB35             YES--- GO READ PROD RECORD                   
*                                                                               
         LA    R1,RCONELEM                                                      
         SR    R0,R0                                                            
RDSUB30  ICM   R0,1,1(R1)                                                       
         BZ    RDSUBOK                                                          
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    RDSUBOK                                                          
         CLI   0(R1),5            FIND PRODUCT NAME ELEMENT                     
         BNE   RDSUB30                                                          
         MVC   KZPROD(20),RCONEXPR-RCONEXEL(R1)     PRODUCT NAME                
         B     RDSUBOK                                                          
*                                                                               
RDSUB35  EQU   *                                                                
         MVC   KZPRDCOD,RCONPRD    INSERT REP PRODUCT CODE                      
         XC    KEY,KEY             READ PRODUCT RECORD                          
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),RCONKADV                                               
         MVC   KEY+22(3),RCONPRD                                                
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         CLI   KEY,X'09'                                                        
         BNE   RDSUBOK                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDSUBOK                                                          
         BAS   RE,GETPROD                                                       
         MVC   KZPROD(20),RPRDNAME           PRODUCT DESCRIPTION                
         SPACE                                                                  
*                                                                               
*   DON'T CHANGE KZMCON# FIELD IF CLEAR CHANNEL.                                
*                                                                               
         TM    HALF,X'80'          COMP S/P IN USE?                             
         BO    RDSUB50             YES - LEAVE FIELD AS IS.                     
*                                                                               
         CLC   KZMCON#,SPACES      ALREADY HAVE MASTER CONTRACT #               
         BNE   RDSUB50                                                          
         MVC   KZMCON#,RPRDNET#                                                 
         OC    KZMCON#,SPACES      SET SPACES OVER BINARY ZERO                  
         SPACE                                                                  
RDSUB50  EQU   *                                                                
         LA    R6,RPRDREC                                                       
         MVI   BYTE,X'03'                                                       
         BAS   RE,GETEL                                                         
         BNE   RDSUBOK                                                          
         USING RPRDSPOT,R6                                                      
         MVC   KZCLICOD,RPRDSPCL   INSERT SPOTPAK CLIENT CODE                   
         MVC   KZSPRDCD,RPRDSPP1   INSERT SPOTPAK PRODUCT CODE                  
         EDIT  RPRDSPES,(3,KZESTNUM),FILL=0                                     
****     MVC   KZESTNUM,RPRDSPES   INSERT SPOTPAK ESTIMATE NUMBER               
         DROP  R6                                                               
RDSUBOK  EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        DATA MANAGER INTERFACE (FILE GETS)                                     
*                                                                               
         SPACE                                                                  
GETCTL   L     RF,=A(CTLREC)                                                    
         B     LINKFILE                                                         
         SPACE                                                                  
GETAGY   LA    RF,RAGYREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETADV   LA    RF,RADVREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETMAN   LA    RF,RSALREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETTEAM  LA    RF,RTEMREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETOFF   LA    RF,ROFFREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETSTAT  LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETREP   LA    RF,RREPREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETPROD  LA    RF,RPRDREC                                                       
         B     LINKFILE                                                         
*                                                                               
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
*                                                                               
         SPACE                                                                  
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION                
         BZ    DM020               NO - ERROR                                   
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     GBEXT                                                            
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    GBEXT                                                            
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR*************'           
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'            BLOW UP                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO TRACE DATA MANAGER CALLS                                    
*                                                                               
         SPACE                                                                  
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,MTRACKEY                                                      
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
         B     GBEXT                                                            
         SPACE                                                                  
         GETEL R6,34,BYTE                                                       
         SPACE                                                                  
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE RECOMRTN                                                       
         EJECT                                                                  
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*  TAPE FOR LOTUS DATA                                                          
         SPACE                                                                  
INTFILA  DCB   DDNAME=INTFILA,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00144,                                            X        
               BLKSIZE=05760,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
INTFILB  DCB   DDNAME=INTFILB,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00144,                                            X        
               BLKSIZE=05760,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE RELOTOFF                                                       
         SPACE                                                                  
KDIVTAB  DS   0CL3                                                              
         DC    C'BFB'                                                           
         DC    C'KUK'                                                           
         DC    C'KFH'                                                           
         DC    C'EAE'                                                           
         DC    C'CRC'                                                           
         DC    C'K4S'                                                           
         DC    C'K6D'                                                           
         DC    C'S3M'                                                           
         DC    C'S2I'                                                           
         DC    C'SZS'                                                           
KDIVTABN EQU   (*-KDIVTAB)/3                                                    
         SPACE                                                                  
CONTYPTB DC    CL2' S'             SPOT                                         
         DC    CL2'SA'             SPORTS                                       
         DC    CL2'FF'             FARM                                         
         DC    CL2'KN'             NETWORK                                      
         DC    CL2'RR'             RELIGION                                     
CONTYPTL EQU   (*-CONTYPTB)/2                                                   
         EJECT                                                                  
RELO     DS    F                                                                
SAVEDRB  DS    F                                                                
ORDAMT   DS    F                   ORDERED DOLLARS UP TO ACT WEEK               
*                                                                               
TAGYCTR  DC    F'0'                                                             
TADVCTR  DC    F'0'                                                             
TSALCTR  DC    F'0'                                                             
TPRDCTR  DC    F'0'                                                             
CHAGROS  DS    F                   CHANGED GROSS                                
CHACOMD  DS    F                   CHANGED COMM DOLLARS                         
CHACOMR  DS    F                   CHANGED COMM RATE                            
ABSGROS  DS    F                   ABSOLUTE GROSS                               
ABSCOMD  DS    F                   ABSOLUTE COMM DOLLARS                        
ABSCOMR  DS    F                   ABSOLUTE COMM RATE                           
*                                                                               
RTOTCHGR DS    D                   RUN TOTAL $ - CHANGED GROSS                  
RTOTCHCM DS    D                   RUN TOTAL $ - CHANGED COMMISSION             
RTOTABGR DS    D                   RUN TOTAL $ - ABSOLUTE GROSS                 
RTOTABCM DS    D                   RUN TOTAL $ - ABSOLUTE COMMISSION            
*                                                                               
PUTCNT   DS    F                                                                
CONCNT   DS    F                                                                
TDOLLARS DC    PL6'0'                                                           
*                                                                               
RECALC   DS    CL1                 RECALC FLAG, 0=NO                            
ORDCNT   DS    CL1                                                              
COMPDT   DS    CL2                                                              
ENDDT    DS    CL3                                                              
GROSSDT  DS    CL3                                                              
GROSSDT2 DS    CL6                                                              
TEMPDATE DS    CL6                                                              
TEMPEND  DS    CL6                                                              
CONEND   DS    CL3                END OF BROADCAST MONTH FOR THIS CON           
CONENDC  DS    CL6                CHARACTER - END OF BC  FOR THIS CON           
         SPACE                                                                  
CURRYRMO DS    XL2                 THIS YEAR/MO FROM REQUEST CD                 
LASTYRMO DS    XL2                                                              
TODAYP   DS    XL3                 TODAY PACKED                                 
COMMAND  DS    CL8                                                              
STAIFACE DS    CL10               INTERFACE CODE FROM STATION RECORD            
STARANK  DS    CL1                STATION RANK                                  
STAMKTNM DS    CL20               STATION MARKET RANK                           
MTRACKEY DS    CL32                                                             
ELCODE   DS    CL1                                                              
DAILYFLG DS    CL1                                                              
CTLKEY   DS    CL27                                                             
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
WORK2    DS    CL50               ANOTHER WORK AREA FOR DEMO NAMES              
WRK      DS    CL144              LENGTH OF RECORD TO BE WRITTEN                
         DC    C'*AGYTAB*'                                                      
AGYTABLE DC    500XL43'00'                                                      
AGYTABCT EQU   500                                                              
         DC    C'*SALTAB*'                                                      
SALTABLE DC    500XL28'00'                                                      
SALTABCT EQU   500                                                              
         SPACE                                                                  
CTLREC   DS    CL1024                                                           
         SPACE 3                                                                
         DROP  RB,RC                                                            
*                                                                               
*        INITIAL --- PROCESS TO INIT WORK AREAS - N O T E                       
*                                                 DO NOT USE R9, OR BAS         
*                                                 TO ANY RTNS IN BASE           
*                                                                               
INITIAL  NMOD1 0,*INITIAL                                                       
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    CONCNT,CONCNT                                                    
         XC    PUTCNT,PUTCNT                                                    
*                                                                               
         XC    RTOTCHGR,RTOTCHGR                                                
         XC    RTOTCHCM,RTOTCHCM                                                
         XC    RTOTABGR,RTOTABGR                                                
         XC    RTOTABCM,RTOTABCM                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
*                                                                               
         CLC   QASAT,SPACES        WAS AS AT DATE ENTERED                       
         BNE   INIT06               YES                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,QASAT)                                      
*                                                                               
INIT06   MVC   TEMPDATE,QASAT                                                   
         GOTO1 GETDAY,DMCB,QASAT,FULL                                           
         CLI   DAILYFLG,C'Y'       DAILY PACING IN USE?                         
         BNE   INIT08              NO  - ADJUST DATE                            
         CLC   =C'SPEC PRINT',QUESTOR                                           
         BNE   INIT10                                                           
         MVC   P+1(19),=C'DAILY PACING IN USE'                                  
         GOTO1 REPORT                                                           
         B     INIT10              YES - USE DATE AS ENTERED                    
INIT08   EQU   *                                                                
         CLI   DMCB,1             ARE WE MONDAY?                                
         BE    INIT10             GOOD -                                        
*                                 NEED TO GET MONDAY PREV                       
         ZIC   R1,DMCB                                                          
         S     R1,=F'1'                                                         
         LNR   R2,R1                                                            
         GOTO1 ADDAY,DMCB,QASAT,TEMPDATE,(R2)                                   
         MVC   QASAT,TEMPDATE                                                   
INIT10   EQU   *                                                                
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   CURRYRMO,WORK+6                                                  
         GOTO1 ADDAY,(R1),WORK,WORK+6,=F'-365'                                  
         GOTO1 DATCON,(R1),(0,WORK+6),(1,WORK+12)                               
         MVC   LASTYRMO,WORK+12                                                 
         GOTO1 DATCON,(R1),(5,0),(3,TODAYP)                                     
*                                                                               
*    SET UP DATE TABLE USED IN FILCON'S SETTING UP OF MONTABLE                  
*                                                                               
         L     R3,AMONARCH                                                      
         USING MONARCHD,R3                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,QASAT),(2,MONACT+2)                               
*                                                                               
INIT20   EQU   *                                                                
         L     R6,AMONFORC                                                      
         MVC   0(2,R6),MONACT+2                                                 
*                                     FILCON NEEDS LAST YEAR                    
         MVC   MONACT+4(4),MONACT     EVEN THOUGH THESE DATES AREN'T            
         MVC   QSTART+4(2),=C'01'     (THE ABOVE COMMENT MAY MEAN               
         MVC   QEND+4(2),=C'01'       SOMETHING TO SOMEONE)                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(3,ENDDT)                                   
         GOTO1 (RF),(R1),(0,QASAT),(2,COMPDT)                                   
*                                                                               
*   'RUNDONE' TO OUTPUT THE SUPPORT RECORDS IS DONE AT THE END OF               
*        ALL REQUESTS.  AS SUCH, THE REQUEST CARD IS GONE, SO THE               
*        SPEC PRINT OPTION MUST BE FLAGGED IF PRESENT                           
*                                                                               
         MVI   HALF2,C'N'          SET 'SPEC PRINT' = NO                        
         CLC   =C'SPEC PRINT',QUESTOR                                           
         BNE   INIT0040                                                         
         MVI   HALF2,C'Y'          SET 'SPEC PRINT' = YES                       
INIT0040 EQU   *                                                                
         SR    R0,R0                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
NRPTDONE NMOD1 0,*RPTDON*                                                       
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         MVC   P+1(22),=CL22'CONTRACTS PROCESSED ='                             
         EDIT  CONCNT,(7,P+24),ZERO=NOBLANK                                     
         GOTO1 REPORT                                                           
         MVC   P+1(40),=CL40'INTERFACE RECORDS WRITTEN TO THE TAPE ='           
         EDIT  PUTCNT,(5,P+42),ZERO=NOBLANK                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+5(15),=CL24'TOTAL DOLLARS ='                                   
         EDIT  (P6,TDOLLARS),(17,P+20),ZERO=NOBLANK,COMMAS=YES                  
         GOTO1 REPORT                                                           
*                                                                               
RPTDGOOD EQU   *                                                                
         SR    R0,R0                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECOMTAB                                                       
       ++INCLUDE RECOMIO                                                        
         LTORG                                                                  
         EJECT                                                                  
         USING KZTPRECD,R4                                                      
       ++INCLUDE REREP3N03                                                      
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REGENCTL                                                       
       ++INCLUDE REREPRGEQA                                                     
*                                                                               
*  REMONARCHD                                                                   
*  REGENALL                                                                     
*  REREPWORKD                                                                   
*  REREPMODES                                                                   
*  REGENCOM                                                                     
*  DEDBLOCK                                                                     
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE REMONARCHD                                                     
       ++INCLUDE REGENALL1A                                                     
         ORG   RPRDELMX            DO FREAK OUT REPORTER SPACEEND               
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
COMDSCT  DSECT                                                                  
       ++INCLUDE REGENCOM                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DSECT TO COVER TAPE RECORDS                                                   
         SPACE                                                                  
KZTPRECD DSECT                                                                  
       ++INCLUDE REGENKTPA                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073REREP3N02 10/24/05'                                      
         END                                                                    
