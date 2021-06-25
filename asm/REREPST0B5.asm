*          DATA SET REREPST0B5 AT LEVEL 051 AS OF 05/01/02                      
*PHASE REST02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPST02 (REST02) - FULL FILE STRIP FOR UTS'                   
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPST02 -- EXTRACT UTS DATA - FULL FILE                 *            
*                     CODE NEWREP FROM REP CODE OLDREP             *            
*                                                                  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* MAY17/01 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
* JUN04/01 (BU ) --- EOP RECS NOT GEN'D IN THIS VERSION            *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     =  STATION DISPLAY                               *            
*     QUESTOR+1   =  OFFICE DISPLAY                                *            
*     QUESTOR+2   =  AGENCY DISPLAY                                *            
*     QUESTOR+3   =  ADVERTISER DISPLAY                            *            
*     QUESTOR+4   =  SALESPERSON DISPLAY                           *            
*     QUESTOR+5   =  PRODUCT DISPLAY                               *            
*     QUESTOR+6   =  CONTRACT DISPLAY                              *            
*     QUESTOR+7   =  BUY DISPLAY                                   *            
*     QUESTOR+8   =  GROUP DISPLAY                                 *            
*     QUESTOR+9   =  OWNER DISPLAY                                 *            
*     QUESTOR+10  =  MARKET DISPLAY                                *            
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
         NMOD1 0,**ST02**,R7,R8,R9,RR=R5                                        
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         GOTO1 =A(LOADTABS),DMCB   LOAD ALL TABLE INFORMATION                   
*                                                                               
         GOTO1 =A(VALITABS),DMCB   VALIDATE TABLE INFORMATION                   
         GOTO1 UTSSIDE             RETURN TO UTS FILE                           
*                                                                               
*                                                                               
***>>>   GOTO1 REPRPROC,DMCB,(RC)  PROCESS REP 01 RECORDS                       
*                                                                               
***>>>   GOTO1 STATPROC,DMCB,(RC)  PROCESS STATION 02 RECORDS                   
*                                                                               
***>>>   GOTO1 REGNPROC,DMCB,(RC)  PROCESS REGION 03 RECORDS                    
*                                                                               
****>>>  GOTO1 OFFCPROC,DMCB,(RC)  PROCESS OFFICE 04 RECORDS                    
*                                                                               
****>>>  GOTO1 TEAMPROC,DMCB,(RC)  PROCESS TEAM 05 RECORDS                      
*                                                                               
         GOTO1 SALPROC,DMCB,(RC)   PROCESS SALESPERSON 06 RECORDS               
*                                                                               
*   TEST                                                                        
***      MVC   P+1(24),=C'EXIT AFTER SALPROC  CALL'                             
***      GOTO1 REPORT                                                           
***      B     TEST0040                                                         
*   TEST END                                                                    
*                                                                               
*                                                                               
****>>>  GOTO1 GRUPPROC,DMCB,(RC)  PROCESS GROUP 07 RECORDS                     
*                                                                               
         GOTO1 ADVPROC,DMCB,(RC)   PROCESS ADVERTISER 08 RECORDS                
*                                                                               
         GOTO1 PRDPROC,DMCB,(RC)   PROCESS PRODUCT 09 RECORDS                   
*                                                                               
         GOTO1 AGYPROC,DMCB,(RC)   PROCESS AGENCY 0A/1A RECORDS                 
*                                                                               
         GOTO1 =A(CONTPROC),DMCB,(RC),(R9),RR=Y                                 
*                                  PROCESS CONTRACT 0C+0B RECORDS               
*                                                                               
         GOTO1 BUYRPROC,DMCB,(RC)  PROCESS BUY 0B RECORDS ALONE                 
*                                                                               
*        CLASS AND CATEGORY CODES WILL REQUIRE REVIEW OF CONVERTED              
*              DATA TO DETERMINE FUNCTIONAL ACCURACY                            
*                                                                               
*                                                                               
****>>>  GOTO1 CLSPROC,DMCB,(RC)   PROCESS CLASS 0D RECORDS                     
*                                                                               
****>>>  GOTO1 CATPROC,DMCB,(RC)   PROCESS CATEGORY 0F RECORDS                  
*                                                                               
         GOTO1 MKGDPROC,DMCB,(RC)  PROCESS M/G 11 RECORDS                       
*                                                                               
*        INVENTORY RECORDS:  THERE IS OVERLAP OF INVENTORY DATA FOR             
*              STATION WNYW.  LYNN HENDY TO DECIDE PROCESSING                   
*                                                                               
         GOTO1 INVPROC,DMCB,(RC)   PROCESS INVENT 12 RECORDS                    
*                                                                               
         GOTO1 BUDPROC,DMCB,(RC)   PROCESS BUDGET 13 RECORDS                    
*                                                                               
         GOTO1 AVALPROC,DMCB,(RC)  PROCESS AVAIL  14 RECORDS                    
*                                                                               
****>>>  GOTO1 IBKLPROC,DMCB,(RC)  PROCESS IBKLIST 1503 RECORDS                 
*                                                                               
*        PROPOSAL RECORDS ARE NOT BEING BROUGHT OVER                            
*                                                                               
****>>>  GOTO1 PROPPROC,DMCB,(RC)  PROCESS PROPOSAL 16 RECORDS                  
*                                                                               
****>>>  GOTO1 EOMPROC,DMCB,(RC)   PROCESS EOM 18 RECORDS                       
*                                                                               
*   EOP RECORDS WILL NOT BE GENERATED IN THIS VERSION                           
*                                                                               
*                                                                               
****>>>  GOTO1 EOP1PROC,DMCB,(RC)  PROCESS EOP 1B RECORDS                       
*                                                                               
****>>>  GOTO1 EOP2PROC,DMCB,(RC)  PROCESS EOP 1C RECORDS                       
*                                                                               
****>>>  GOTO1 EOP3PROC,DMCB,(RC)  PROCESS EOP 1D RECORDS                       
*                                                                               
****>>>  GOTO1 EOP4PROC,DMCB,(RC)  PROCESS EOP 1E RECORDS                       
*                                                                               
         GOTO1 ACALPROC,DMCB,(RC)  PROCESS ALT CAL 20 RECORDS                   
*                                                                               
****>>>  GOTO1 DEMOPROC,DMCB,(RC)  PROCESS DEMO MENU 23 RECORDS                 
*                                                                               
****>>>  GOTO1 DYPTPROC,DMCB,(RC)  PROCESS DAYPART 24 RECORDS                   
*                                                                               
****>>>  GOTO1 SDDPROC,DMCB,(RC)   PROCESS SDD 26 RECORDS                       
*                                                                               
****>>>  GOTO1 COMMPROC,DMCB,(RC)  PROCESS COMMISSION 29 RECORDS                
*                                                                               
****>>>  GOTO1 OWNRPROC,DMCB,(RC)  PROCESS OWNER 2A RECORDS                     
*                                                                               
****>>>  GOTO1 MRKTPROC,DMCB,(RC)  PROCESS MARKET 2B RECORDS                    
*                                                                               
****>>>  GOTO1 SCOMPROC,DMCB,(RC)  PROCESS STD COMMT 2E RECORDS                 
*                                                                               
****>>>  GOTO1 TYPEPROC,DMCB,(RC)  PROCESS TYPE 32 RECORDS                      
*                                                                               
****>>>  GOTO1 RADRPROC,DMCB,(RC)  PROCESS RADAR 33 RECORDS                     
*                                                                               
****>>>  GOTO1 OCOMPROC,DMCB,(RC)  PROCESS OFFICE COMMT 34 RECORDS              
*                                                                               
****>>>  GOTO1 SETRPROC,DMCB,(RC)  PROCESS SET 38 RECORDS                       
*                                                                               
****>>>  GOTO1 RDPTPROC,DMCB,(RC)  PROCESS RSCH DPT 3C RECORDS                  
*                                                                               
         GOTO1 RARTPROC,DMCB,(RC)  PROCESS AVAIL RATE 3E RECORDS                
*                                                                               
         GOTO1 =A(PROCFOUR),DMCB,(RC),(R9),RR=Y  PROCESS FOUR TYPES             
*                                                                               
*   TEST                                                                        
***      MVC   P+1(24),=C'EXIT AFTER PROCFOUR CALL'                             
***      GOTO1 REPORT                                                           
***      B     TEST0040                                                         
*   TEST END                                                                    
*                                                                               
*                                                                               
TEST0040 EQU   *                                                                
         GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         B     EXIT                EXIT                                         
         EJECT                                                                  
*                                                                               
*   SAMPLE CODE:  BINARY SEARCH                                                 
*                                                                               
*        MVC   DUB(2),RSTAKREP     SET UP SEARCH ARGUMENT                       
*        MVC   DUB+2(5),RSTAKSTA                                                
*                                                                               
*        L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
*        GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,7,(R4)                         
*                                                                               
*        CLI   DMCB,0              RECORD FOUND?                                
*        BE    STAT0030            YES                                          
*                                                                               
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
******************************************************************              
INITIAL  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',2000000,2000000                                 
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVI   RCSUBPRG,2          SET HEADING FLAG                             
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   ASTAAREA,P2         A(STATION TABLE)                             
         MVC   ANEXTSTA,P2         A(STATION SLOT)                              
*                                                                               
*   STATION ENTRIES WILL BE DEVELOPED FROM THE FSS FILES                        
*        100 ENTRIES OF 16 CHARACTERS EACH:                                     
*        BYTES 1  -  5  = STATION CALL LETTERS                                  
*        BYTES 6  -  7  = GROUP/SUBGROUP                                        
*        BYTE  8  -     = FLAG BYTE                                             
*        BYTES 9  - 16  = SPARE                                                 
*                                                                               
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         LA    RF,1600(RF)         LEAVE STATION SPACE                          
*                                                                               
         ST    RF,ASALAREA         ESTABLISH SALESPERSON AREA                   
         ST    RF,ANEXTSAL         A(NEXT S/P SLOT)                             
*                                                                               
*   S/P     ENTRIES WILL BE DEVELOPED FROM THE UTS FILES                        
*        500 ENTRIES OF  8 CHARACTERS EACH:                                     
*        BYTES 1  -  3  = UTS SALESPERSON CODE                                  
*        BYTES 4  -  6  = EQUIVALENT FSS  CODE                                  
*        BYTE  7  -     = FLAG BYTE                                             
*                         X'80'  - COPY S/P FROM OLD TO NEW REP                 
*                         X'40'  - OLD  S/P ALREADY ON  NEW REP                 
*        BYTE  8  -     = SPARE                                                 
*                                                                               
         A     RF,=F'4000'         LEAVE ROOM FOR 500 ENTRIES                   
*                                     8 CHARS * 500 SLOTS                       
*                                                                               
         ST    RF,AAGYAREA         A(AGENCY TRANSLATION AREA)                   
         ST    RF,ANEXTAGY         A(AGENCY SLOT)                               
*                                                                               
*   AGENCY  ENTRIES WILL BE DEVELOPED FROM THE UTS FILES                        
*        4000 ENTRIES OF 16 CHARACTERS EACH:                                    
*        BYTES 1  -  6  = UTS AGENCY/OFFC CODE                                  
*        BYTES 7  - 12  = EQUIVALENT FSS  CODE                                  
*        BYTE  13 -     = FLAG BYTE                                             
*        BYTES 14 - 16  = SPARE                                                 
*                                                                               
         A     RF,=F'64000'        LEAVE ROOM FOR 4000 ENTRIES                  
*                                     16 CHARS * 4000 SLOTS                     
         ST    RF,AADVAREA         A(ADVERT TRANSLATION AREA)                   
         ST    RF,ANEXTADV         A(ADVERT SLOT)                               
*                                                                               
*   ADVERT  ENTRIES WILL BE DEVELOPED FROM THE UTS FILES                        
*        9000 ENTRIES OF 12 CHARACTERS EACH:                                    
*        BYTES 1  -  4  = UTS ADVERT CODE                                       
*        BYTES 5  -  8  = EQUIVALENT FSS  CODE                                  
*        BYTE  9  -     = FLAG BYTE                                             
*        BYTE  10 - 11  = DATE OF LAST CHANGE (COMPRESSED)                      
*        BYTES 12       = SPARE                                                 
*                                                                               
         A     RF,=F'108000'        LEAVE ROOM FOR 9000 ENTRIES                 
*                                     12 CHARS * 9000 SLOTS                     
         ST    RF,APRDAREA         A(PRODUCT TABLE)                             
         ST    RF,ANEXTPRD         A(PRODUCT SLOT)                              
*                                                                               
*   PRODUCT ENTRIES WILL BE DEVELOPED FROM THE UTS FILES                        
*        1000 ENTRIES OF 16 CHARACTERS EACH:                                    
*        BYTES 1  -  4  = UTS ADVERT CODE                                       
*        BYTES 5  -  7  = UTS PRODUCT CODE                                      
*        BYTES 8  - 11  = EQUIVALENT FSS ADVERT CODE                            
*        BYTES 12 - 14  = FSS PRODUCT CODE                                      
*        BYTE  15 -     = FLAG BYTE                                             
*        BYTES 16       = SPARE                                                 
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
                                                                                
         XC    CONCTR,CONCTR       CLEAR COUNTERS                               
         XC    CONCTR2,CONCTR2     CLEAR COUNTERS                               
         XC    BUYCTR,BUYCTR                                                    
         XC    BUYCTR2,BUYCTR2                                                  
         XC    OTHERCTR,OTHERCTR                                                
         XC    ADVCTR,ADVCTR                                                    
         XC    AGYCTR,AGYCTR                                                    
         XC    SALCTR,SALCTR                                                    
         XC    PRDCTR,PRDCTR                                                    
         MVC   OLDREP,QRECORD+36   'FROM' REP                                   
         MVC   NEWREP,QRECORD+38   'TO' REP OR 'TEST TO REP'                    
         MVC   NEWREP2,QRECORD+40  'TO' REP                                     
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  UTSSIDE : RETURN TO UTS FILE                                  *              
******************************************************************              
*                                                                               
UTSSIDE  NTR1                                                                   
*                                                                               
*   DATA RETURN SECTION:                                                        
*                                                                               
*                                     WITH REP UTL CODE                         
         L     R1,ADCONLST                                                      
         L     RE,VUTL-ADCONSD(R1)                                              
         MVC   4(1,RE),SAVEVUTL    INSERT ORIGINAL UTL NUMBER                   
*                                                                               
*   TEST DISPLAY UTL                                                            
         MVC   P+1(12),=C'ORIG/NEW UTL'                                         
         MVC   P+16(1),SAVEVUTL                                                 
         MVI   P+17,C'/'                                                        
         MVC   P+18(1),NEWUTL                                                   
         GOTO1 REPORT                                                           
*   TEST DISPLAY REP END                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',FLIST,REC                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,1                                                            
         GOTO1 HIGHDIR                                                          
         MVC   P+1(15),=C'1ST REC ON UTS:'                                      
         MVC   P+20(27),KEY                                                     
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
******************************************************************              
*  REPRPROC: EXTRACT REP RECS FOR OLDREP                         *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
REPRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RREPKEY,R6          SET RECORD DEFINITION                        
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,OLDREP                                                  
         DROP  R6                                                               
         SPACE                                                                  
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    REPRPR10                                                         
         DC    H'0'                MUST BE THERE                                
REPRPR10 DS   0H                                                                
                                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RREPREC,R6                                                       
         MVC   REC-4(2),RREPLEN    INSERT LENGTH FOR PUT                        
         MVC   RREPKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,REPCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,REPCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR,C'Y'                                                     
         BNE   REPRPRX                                                          
         MVC   P+1(8),=C'REP REC:'                                              
         MVC   P+10(2),RREPKREP                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
REPRPRX  XIT1                                                                   
         EJECT                                                                  
                                                                                
******************************************************************              
*  STATPROC: EXTRACT SELTL STATION RECS AS SPECIFIED BY TABLE AND*              
*           WRITE THEM OUT WITH NEW REP CODE CT                  *              
******************************************************************              
STATPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
STAT10   DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAKEY,R6          SET RECORD DEFINITION                        
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,OLDREP                                                  
         GOTO1 HIGHDIR                                                          
         B     STAT12                                                           
STAT11   EQU   *                                                                
         LA    R6,KEY                                                           
         GOTO1 SEQDIR                                                           
STAT12   EQU   *                                                                
         CLC   KEY(22),KEYSAVE     COMPARE THRU REP                             
         BNE   STATX               FINISHED                                     
*                                                                               
**       GOTO1 REPORT                                                           
**       MVC   P+1(8),=C'STA KEY:'                                              
**       MVC   P+10(27),KEY                                                     
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
STAT16   EQU   *                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     NEW REP CODE                                 
         L     RF,STACTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,STACTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR,C'Y'                                                     
         BNE   STAT20                                                           
         MVC   P+1(8),=C'STA REC:'                                              
         MVC   P+10(5),RSTAKSTA                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
                                                                                
STAT20   DS    0H                                                               
         B     STAT11                                                           
                                                                                
STATX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  REGNPROC:  WILL BRING ALL OLDREP     REGN   RECORDS           *              
******************************************************************              
REGNPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         MVI   KEYTYPE,X'03'                                                    
                                                                                
REGN0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
REGN0040 EQU   *                                                                
         MVC   KEY+23(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     REGN0080                                                         
REGN0060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
REGN0080 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     COMPARE THROUGH REP                          
         BNE   REGN0100            NOT REGION RECORD OR RIGHT REP               
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RREGREC,R6                                                       
         MVC   REC-4(2),RREGLEN    INSERT LENGTH FOR PUT                        
         MVC   RREGKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,RGNCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RGNCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   REGN0060                                                         
         MVC   P+1(8),=C'RGN REC:'                                              
         MVC   P+10(2),=C'03'                                                   
         MVC   P+20(2),RREGKREG                                                 
         GOTO1 REPORT                                                           
         B     REGN0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
REGN0100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  OFFCPROC:  WILL BRING ALL OLDREP     OFFICE RECORDS           *              
******************************************************************              
OFFCPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         MVI   KEYTYPE,X'04'                                                    
                                                                                
OFFC0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
OFFC0040 EQU   *                                                                
         MVC   KEY+23(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     OFFC0080                                                         
OFFC0060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
OFFC0080 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     COMPARE THROUGH REP                          
         BNE   OFFC0100            NOT OFFICE RECORD OR RIGHT REP               
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING ROFFREC,R6                                                       
         MVC   REC-4(2),ROFFLEN    INSERT LENGTH FOR PUT                        
         MVC   ROFFKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,OFFCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,OFFCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   OFFC0060                                                         
         MVC   P+1(8),=C'OFF REC:'                                              
         MVC   P+10(2),=C'04'                                                   
         CLI   KEYTYPE,X'04'                                                    
         BE    OFFC0090                                                         
         MVC   P+10(2),=C'44'                                                   
OFFC0090 EQU   *                                                                
         MVC   P+20(2),ROFFKOFF                                                 
         GOTO1 REPORT                                                           
         B     OFFC0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
OFFC0100 DS    0H                                                               
         CLI   KEYTYPE,X'44'                                                    
         BE    OFFC0120                                                         
         XC    KEY,KEY             CLEAR KEY TO START 44'S                      
         MVI   KEYTYPE,X'44'       INSERT 44 KEY TYPE                           
         MVI   KEY,X'44'           INSERT INTO KEY ALSO                         
         B     OFFC0040            GO BACK AND START 44'S                       
                                                                                
OFFC0120 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  TEAMPROC:  WILL BRING ALL OLDREP     TEAM   RECORDS           *              
******************************************************************              
TEAMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         MVI   KEYTYPE,X'05'                                                    
                                                                                
TEAM0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
TEAM0040 EQU   *                                                                
         MVC   KEY+23(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     TEAM0080                                                         
TEAM0060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
TEAM0080 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     COMPARE THROUGH REP                          
         BNE   TEAM0100            NOT OFFICE RECORD OR RIGHT REP               
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RTEMREC,R6                                                       
         MVC   REC-4(2),RTEMLEN    INSERT LENGTH FOR PUT                        
         MVC   RTEMKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,TEMCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TEMCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   TEAM0060                                                         
         MVC   P+1(8),=C'TEM REC:'                                              
         MVC   P+10(2),=C'05'                                                   
         MVC   P+20(2),RTEMKTEM                                                 
         GOTO1 REPORT                                                           
         B     TEAM0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
TEAM0100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  AGYPROC: EXTRACT ALL OLDREP AGENCY RECS IN TABLE         AND  *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
AGYPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         MVI   KEYTYPE,X'0A'                                                    
                                                                                
AGYP0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
AGYP0040 EQU   *                                                                
         GOTO1 HIGHDIR                                                          
AGYP0060 EQU   *                                                                
         CLC   KEY(1),KEYTYPE      KEYTYPE FOUND?                               
         BNE   AGYP0200            NO  - AGENCY RECORDS FINISHED                
         CLC   KEY+25(2),OLDREP    CORRECT REP?                                 
         BE    AGYP0100            YES                                          
*****************************************************************               
*    MASTER/SUBSIDIARY EXTRACT CODING                                           
***      CLC   OLDREP,=C'AM'       CORRECT REP?                                 
***      BE    AGYP0070            YES                                          
***      CLC   OLDREP,=C'CQ'       CORRECT REP?                                 
***      BE    AGYP0070            YES                                          
***      CLC   OLDREP,=C'NK'       CORRECT REP?                                 
***      BNE   AGYP0080            YES                                          
AGYP0070 EQU   *                                                                
***      CLC   KEY+25(2),=C'MR'    CORRECT REP?                                 
***      BE    AGYP0100            YES                                          
*****************************************************************               
AGYP0080 EQU   *                                                                
         GOTO1 SEQDIR              NO  - GET NEXT RECORD                        
         B     AGYP0060            GO BACK AND CHECK IT                         
AGYP0100 EQU   *                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RAGYREC,R6                                                       
*** BINARY SEARCH CODE                                                          
*                                                                               
         MVC   DUB(6),RAGYKAGY     SET UP TABLE KEY ARGUMENT                    
*                                                                               
         L     R2,AAGYAREA         SET A(AGENCY TABLE)                          
         L     R4,AGYTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,6,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    AGYP0120            YES                                          
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7                
         MVC   P+01(37),=C'UTS AGY "XXXXXX" NOT IN TABLE:  ERROR'               
         MVC   P+10(6),RAGYKAGY                                                 
         MVI   ERRORFLG,1          SET ERROR FLAG ON                            
         GOTO1 REPORT                                                           
         L     RF,NFAGY                                                         
         LA    RF,1(RF)                                                         
         ST    RF,NFAGY                                                         
         B     AGYP0080            GO BACK FOR NEXT                             
AGYP0120 EQU   *                                                                
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         TM    12(RF),X'40'        OLD  AGY ALREADY ON NEW REP?                 
         BO    AGYP0140            YES                                          
         TM    12(RF),X'80'        COPY AGY AS O/P TO TAPE?                     
         BO    AGYP0160            YES                                          
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1            
         MVC   P+01(41),=C'UTS AGY "XXXXXX" EQUIVALENCED: NOT COPIED'           
         MVC   P+10(6),RAGYKAGY                                                 
         MVC   P+44(16),0(RF)      INSERT TABLE ENTRY                           
         GOTO1 REPORT                                                           
         L     RF,AGEQUIV                                                       
         LA    RF,1(RF)                                                         
         ST    RF,AGEQUIV                                                       
         B     AGYP0080            GO BACK FOR NEXT                             
AGYP0140 EQU   *                                                                
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9'             
         MVC   P+01(36),=C'UTS AGY "XXXXXX" ALREADY ON FSS FILE'                
         MVC   P+10(6),RAGYKAGY                                                 
         MVC   P+40(12),0(RF)      INSERT TABLE ENTRY                           
         GOTO1 REPORT                                                           
         L     RF,AGONFILE                                                      
         LA    RF,1(RF)                                                         
         ST    RF,AGONFILE                                                      
         B     AGYP0080            GO BACK FOR NEXT                             
AGYP0160 EQU   *                                                                
*                                                                               
*** END BINARY SEARCH CODE                                                      
         MVC   REC-4(2),RAGYLEN    INSERT LENGTH FOR PUT                        
         MVC   RAGYKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,AGYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+2,C'Y'                                                   
         BNE   AGYP0080                                                         
         CLC   AGYCTR,=F'50'       DISPLAY FIRST N RECORDS                      
         BH    AGYP0080                                                         
*                                                                               
         MVC   P+1(8),=C'AGY REC:'                                              
         MVC   P+10(2),=C'0A'                                                   
         CLI   KEYTYPE,X'0A'                                                    
         BE    AGYP0180                                                         
         MVC   P+10(2),=C'1A'                                                   
AGYP0180 EQU   *                                                                
         MVC   P+20(4),RAGYKAGY                                                 
         MVI   P+24,C'-'                                                        
         MVC   P+25(2),RAGYKAOF                                                 
         GOTO1 REPORT                                                           
         B     AGYP0080            GO BACK FOR NEXT                             
         DROP  R6                                                               
AGYP0200 DS    0H                                                               
         CLI   KEYTYPE,X'1A'                                                    
         BE    AGYP0220                                                         
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEYTYPE,X'1A'       INSERT KEY TYPE                              
         MVI   KEY,X'1A'           INSERT INTO KEY ALSO                         
         B     AGYP0040                                                         
                                                                                
AGYP0220 DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*  ADVPROC:                                                      *              
*                                                                *              
******************************************************************              
ADVPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
                                                                                
ADVP0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         DROP  R6                                                               
         GOTO1 HIGHDIR                                                          
         B     ADVP0060                                                         
ADVP0040 EQU   *                                                                
         GOTO1 SEQDIR                                                           
ADVP0060 EQU   *                                                                
         CLI   KEY,8               ADVERTISER RECORD?                           
         BNE   ADVP0200             NO  - FINISHED                              
         CLC   KEY+25(2),OLDREP    YES - SAME REP?                              
         BNE   ADVP0040             NO  - SKIP IT                               
*                                                                               
*****************************************************************               
*    MASTER/SUBSIDIARY EXTRACT CODING                                           
***      CLC   OLDREP,=C'AM '       CORRECT REP?                                
***      BE    ADVP0080            YES                                          
***      CLC   OLDREP,=C'CQ'       CORRECT REP?                                 
***      BE    ADVP0080            YES                                          
***      CLC   OLDREP,=C'NK'       CORRECT REP?                                 
***      BNE   ADVP0040             YES                                         
ADVP0080 EQU   *                                                                
***      CLC   KEY+25(2),=C'MR'    CORRECT REP?                                 
***      BNE   ADVP0040             NO  - SKIP IT                               
*****************************************************************               
*                                                                               
         SPACE                                                                  
ADVP0100 EQU   *                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RADVREC,R6                                                       
*** BINARY SEARCH CODE                                                          
*                                                                               
         MVC   DUB(4),RADVKADV     SET UP TABLE KEY ARGUMENT                    
*                                                                               
         L     R2,AADVAREA         SET A(ADVERT TABLE)                          
         L     R4,ADVTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),12,4,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    ADVP0120            YES                                          
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5                  
         MVC   P+01(35),=C'UTS ADV "XXXX" NOT IN TABLE:  ERROR'                 
         MVC   P+10(4),RADVKADV                                                 
         MVI   ERRORFLG,1          SET ERROR FLAG ON                            
         GOTO1 REPORT                                                           
         L     RF,NFADV                                                         
         LA    RF,1(RF)                                                         
         ST    RF,NFADV                                                         
         B     ADVP0040            GO BACK FOR NEXT                             
ADVP0120 EQU   *                                                                
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         TM    8(RF),X'40'         OLD  ADV ALREADY ON NEW REP?                 
         BO    ADVP0140            YES                                          
         TM    8(RF),X'80'         COPY ADV AS O/P TO TAPE?                     
         BO    ADVP0160            YES                                          
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9'             
         MVC   P+01(39),=C'UTS ADV "XXXX" EQUIVALENCED: NOT COPIED'             
         MVC   P+10(4),RADVKADV                                                 
         MVC   P+42(12),0(RF)      INSERT TABLE ENTRY                           
         GOTO1 REPORT                                                           
         L     RF,ADEQUIV                                                       
         LA    RF,1(RF)                                                         
         ST    RF,ADEQUIV                                                       
         B     ADVP0040            GO BACK FOR NEXT                             
ADVP0140 EQU   *                                                                
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9'             
         MVC   P+01(34),=C'UTS ADV "XXXX" ALREADY ON FSS FILE'                  
         MVC   P+10(4),RADVKADV                                                 
         MVC   P+40(8),0(RF)       INSERT TABLE ENTRY                           
         GOTO1 REPORT                                                           
         L     RF,ADONFILE                                                      
         LA    RF,1(RF)                                                         
         ST    RF,ADONFILE                                                      
         B     ADVP0040            GO BACK FOR NEXT                             
ADVP0160 EQU   *                                                                
*                                                                               
*** END BINARY SEARCH CODE                                                      
         MVC   REC-4(2),RADVLEN    INSERT LENGTH FOR PUT                        
         MVC   RADVKREP,NEWREP     NEW REP CODE                                 
         DROP  R6                                                               
                                                                                
         L     RF,ADVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+3,C'Y'                                                   
         BNE   ADVP0180                                                         
         CLC   ADVCTR,=F'50'       DISPLAY FIRST N RECORDS                      
         BH    ADVP0180                                                         
*                                                                               
         MVC   P+1(8),=C'ADV REC:'                                              
         MVC   P+10(2),=C'08'                                                   
         MVC   P+14(4),21(R6)                                                   
         MVC   P+20(2),25(R6)                                                   
         GOTO1 REPORT                                                           
ADVP0180 EQU   *                                                                
         B     ADVP0040            GO BACK FOR NEXT                             
                                                                                
ADVP0200 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  SALPROC: EXTRACT ALL OLDREP SALESPERSON RECORDS       AND     *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
SALPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   KEYTYPE,X'06'                                                    
SALP0020 EQU   *                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         MVC   RSALKTYP,KEYTYPE                                                 
         MVC   RSALKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     SALP0060                                                         
SALP0040 EQU   *                                                                
         GOTO1 SEQDIR                                                           
SALP0060 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     COMPARE THRU REP CODE                        
         BNE   SALP0220                                                         
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RSALREC,R6                                                       
*** BINARY SEARCH CODE                                                          
*                                                                               
         MVC   DUB(3),RSALKSAL     SET UP TABLE KEY ARGUMENT                    
*                                                                               
         L     R2,ASALAREA         SET A(SALESPERSON TABLE)                     
         L     R4,SALTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),08,3,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SALP0100            YES                                          
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5                  
         MVC   P+01(34),=C'UTS S/P "XXX" NOT IN TABLE:  ERROR'                  
         MVC   P+10(3),RSALKSAL                                                 
         CLI   KEYTYPE,6           MAJOR OR SECONDARY KEY?                      
         BE    SALP0080            MAJOR                                        
         MVC   P+40(4),=C'*46*'                                                 
SALP0080 EQU   *                                                                
         MVI   ERRORFLG,1          SET ERROR FLAG ON                            
         GOTO1 REPORT                                                           
         L     RF,NFSALP                                                        
         LA    RF,1(RF)                                                         
         ST    RF,NFSALP                                                        
         B     SALP0040            GO BACK FOR NEXT                             
SALP0100 EQU   *                                                                
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         TM    6(RF),X'40'         OLD  S/P ALREADY ON NEW REP?                 
         BO    SALP0140            YES                                          
         TM    6(RF),X'80'         COPY S/P AS O/P TO TAPE?                     
         BO    SALP0180            YES                                          
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9'             
         MVC   P+01(38),=C'UTS S/P "XXX" EQUIVALENCED: NOT COPIED'              
         MVC   P+10(3),RSALKSAL                                                 
         MVC   P+40(8),0(RF)       INSERT TABLE ENTRY                           
         CLI   KEYTYPE,6           MAJOR OR SECONDARY KEY?                      
         BE    SALP0120            MAJOR                                        
         MVC   P+40(4),=C'*46*'                                                 
SALP0120 EQU   *                                                                
         GOTO1 REPORT                                                           
         L     RF,SPEQUIV                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SPEQUIV                                                       
         B     SALP0040            GO BACK FOR NEXT                             
SALP0140 EQU   *                                                                
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9'             
         MVC   P+01(33),=C'UTS S/P "XXX" ALREADY ON FSS FILE'                   
         MVC   P+10(3),RSALKSAL                                                 
         MVC   P+40(8),0(RF)       INSERT TABLE ENTRY                           
         CLI   KEYTYPE,6           MAJOR OR SECONDARY KEY?                      
         BE    SALP0160            MAJOR                                        
         MVC   P+40(4),=C'*46*'                                                 
SALP0160 EQU   *                                                                
         GOTO1 REPORT                                                           
         L     RF,SPONFILE                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SPONFILE                                                      
         B     SALP0040            GO BACK FOR NEXT                             
SALP0180 EQU   *                                                                
*                                                                               
*** END BINARY SEARCH CODE                                                      
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,SALCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SALCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+4,C'Y'                                                   
         BNE   SALP0040                                                         
         MVC   P+1(8),=C'SAL REC:'                                              
         MVC   P+10(27),RSALREC                                                 
         CLI   KEYTYPE,6           MAJOR OR SECONDARY KEY?                      
         BE    SALP0200            MAJOR                                        
         MVC   P+40(4),=C'*46*'                                                 
SALP0200 EQU   *                                                                
         GOTO1 REPORT                                                           
         B     SALP0040            GO BACK FOR NEXT SALESPERSON                 
                                                                                
SALP0220 DS    0H                                                               
         CLI   KEYTYPE,X'46'                                                    
         BE    SALP0240                                                         
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEYTYPE,X'46'       INSERT KEY TYPE                              
         MVI   KEY,X'46'           INSERT INTO KEY ALSO                         
         B     SALP0020                                                         
SALP0240 EQU   *                                                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  PRDPROC: EXTRACT OLDREP PRD RECS                      AND     *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
PRDPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     PRD20                                                            
PRD10    EQU   *                                                                
         GOTO1 SEQDIR                                                           
PRD20    EQU   *                                                                
         CLI   KEY,X'09'           PRODUCT RECORD?                              
         BNE   PRDX                NO  - FINISHED                               
         CLC   KEY+25(2),OLDREP                                                 
         BNE   PRD10               NO  - GO BACK FOR NEXT                       
*                                                                               
PRD30    EQU   *                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RPRDREC,R6                                                       
         MVC   REC-4(2),RPRDLEN    INSERT LENGTH FOR PUT                        
         MVC   RPRDKREP,NEWREP     NEW REP CODE                                 
         MVC   DUB(7),RPRDKADV     INSERT ADVERTISER/PRODUCT                    
*                                                                               
         L     R2,APRDAREA         SET A(PRODUCT TABLE)                         
         L     R4,PRDTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,7,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    PRD40               YES                                          
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9              
         MVC   P+01(24),=C'PRODUCT CODE CONVERSION:'                            
         GOTO1 REPORT                                                           
         MVC   P+01(38),=C'UTS PRD "XXXXXXX" NOT IN TABLE:  ERROR'              
         MVC   P+10(7),DUB                                                      
         MVI   ERRORFLG,1          SET ERROR FLAG ON                            
         GOTO1 REPORT                                                           
         B     PRDX                                                             
PRD40    EQU   *                                                                
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         MVC   RPRDKADV(7),7(RF)   INSERT NEW CODE                              
                                                                                
         L     RF,PRDCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PRDCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+5,C'Y'                                                   
         BNE   PRD10                                                            
         MVC   P+1(8),=C'PRD REC:'                                              
         MVC   P+10(27),RPRDREC                                                 
         GOTO1 REPORT                                                           
         B     PRD10                                                            
                                                                                
PRDX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  BUYRPROC: EXTRACT ALL BUY RECORDS                             *              
*           WRITE THEM OUT WITH NEW REP CODE                     *              
******************************************************************              
BUYRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         L     R6,BUYCTR                                                        
*                                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,OLDREP                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGHDIR                                                          
         B     BUYR12                                                           
BUYR11   EQU   *                                                                
         GOTO1 SEQDIR                                                           
BUYR12   EQU   *                                                                
         CLC   KEY(18),KEYSAVE     COMPARE THRU REP CODE                        
         BNE   BUYRX                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RBUYREC,R6                                                       
         MVC   REC-4(2),RBUYLEN    INSERT LENGTH FOR PUT                        
         MVC   RBUYKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,BUYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
         L     RF,BUYCTR2          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR2                                                       
         CLC   BUYCTR2,=F'1000'    DISPLAY EVERY N RECORDS                      
         BNE   BUYR40                                                           
         XC    BUYCTR2,BUYCTR2                                                  
         MVC   P+1(21),=C'PROCESSING BUYS:     '                                
         EDIT  BUYCTR,(7,P+24)                                                  
         GOTO1 REPORT                                                           
BUYR40   EQU   *                                                                
*                                                                               
*   TEST                                                                        
***      CLC   BUYCTR,=F'2000'     PROCESS FIRST N RECORDS                      
***      BH    BUYRX                                                            
*   TEST END                                                                    
*                                                                               
         ZICM  RF,RBUYLEN,2        ADD ALL BUY LENGTHS                          
         CVD   RF,DUB                                                           
         AP    BUYBYTES(8),DUB(8)                                               
*                                                                               
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+7,C'Y'                                                   
         BNE   BUYR11                                                           
         CLC   BUYCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    BUYR11                                                           
         MVC   P+1(8),=C'BUY REC:'                                              
         MVC   P+10(1),RBUYKTYP                                                 
         MVC   P+12(11),RBUYKREP                                                
         GOTO1 REPORT                                                           
         B     BUYR11              GO BACK FOR NEXT SALESPERSON                 
                                                                                
BUYRX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  BUYPROC:   RETRIEVE ALL BUY RECORDS ATTACHED TO CONTRACT EXTRACTED           
******************************************************************              
BUYPROC  NTR1                                                                   
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RCONKCON CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5) GET 9'S COMPLEMENT                          
         MVO   WORK(5),WORK+5(5)   CHANGE TO PWOS                               
         DROP  R6                                                               
         MVC   NINECON,WORK        SAVE 9'S COMP CON#                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP BUY RECD KEY                          
         MVC   KEY+16(2),OLDREP    INSERT REP CODE                              
         PACK  KEY+18(1),WORK+3(1) REVERSED 9'COMP OF K NUM                     
         PACK  KEY+19(1),WORK+2(1)                                              
         PACK  KEY+20(1),WORK+1(1)                                              
         PACK  KEY+21(1),WORK(1)                                                
*                                                                               
         MVC   REVCON(4),KEY+18    SAVE REVERSED 9'S COMP KEY                   
*                                                                               
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     BUY20                                                            
                                                                                
BUY10    DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
                                                                                
BUY20    DS    0H                                                               
         CLC   KEY(22),KEYSAVE     SAME CONTRACT?                               
         BNE   BUYX                NO  - FINISHED                               
                                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD            YES - RETRIEVE RECORD                        
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         LA    R6,REC                                                           
         USING RBUYREC,R6                                                       
         MVC   REC-4(2),RBUYLEN                                                 
         MVC   RBUYKREP,NEWREP                                                  
                                                                                
         L     RF,BUYCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
                                                                                
         ZICM  RF,RBUYLEN,2        ADD ALL BUY LENGTHS                          
         CVD   RF,DUB                                                           
         AP    BUYBYTES(8),DUB(8)                                               
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+7,C'Y'                                                   
         BNE   BUY30                                                            
**                                                                              
         CLC   BUYCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    BUY30                                                            
**                                                                              
         MVC   P+1(8),=C'BUY LNE:'                                              
         EDIT  RBUYKMLN,(3,P+10),ALIGN=LEFT                                     
         EDIT  RBUYKLIN,(3,P+16),ALIGN=LEFT                                     
         GOTO1 HEXOUT,DMCB,RBUYKCON,P+24,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
BUY30    DS    0H                                                               
         B     BUY10                                                            
                                                                                
BUYX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CLSPROC: EXTRACT ALL OLDREP CLS RECS AND                      *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
CLSPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCLSKEY,R6                                                       
         MVI   RCLSKTYP,X'0D'                                                   
         MVC   RCLSKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     CLS20                                                            
                                                                                
CLS10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
CLS20    DS    0H                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   CLSX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCLSREC,R6                                                       
         MVC   REC-4(2),RCLSLEN    INSERT LENGTH FOR PUT                        
         MVC   RCLSKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,CLSCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CLSCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+8,C'Y'                                                   
         BNE   CLS10                                                            
         MVC   P+1(8),=C'CLS REC:'                                              
         MVC   P+10(2),=C'0D'                                                   
         MVC   P+20(27),RCLSREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CLS10                                                            
                                                                                
CLSX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CATPROC: EXTRACT ALL OLDREP CAT RECS AND                      *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
CATPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCTGKEY,R6                                                       
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     CTG20                                                            
                                                                                
CTG10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
CTG20    DS    0H                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   CTGX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCTGREC,R6                                                       
         MVC   REC-4(2),RCTGLEN    INSERT LENGTH FOR PUT                        
         MVC   RCTGKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,CTGCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CTGCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+9,C'Y'                                                   
         BNE   CTG10                                                            
         MVC   P+1(8),=C'CTG REC:'                                              
         MVC   P+10(2),=C'0F'                                                   
         MVC   P+20(27),RCTGREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CTG10                                                            
                                                                                
CTGX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  TYPEPROC: EXTRACT ALL OLDREP TYPE RECS AND                    *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
TYPEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCTYKEY,R6                                                       
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     CTY20                                                            
                                                                                
CTY10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
CTY20    DS    0H                                                               
         CLC   KEY(26),KEYSAVE                                                  
         BNE   CTYX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCTYREC,R6                                                       
         MVC   REC-4(2),RCTYLEN    INSERT LENGTH FOR PUT                        
         MVC   RCTYKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,CTYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CTYCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   CTY10                                                            
         MVC   P+1(8),=C'CTY REC:'                                              
         MVC   P+10(2),=C'32'                                                   
         MVC   P+20(27),RCTYREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CTY10                                                            
                                                                                
CTYX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  GRUPPROC: EXTRACT ALL OLDREP GROUP RECS AND                   *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
GRUPPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RGRPKEY,R6                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     GRP20                                                            
                                                                                
GRP10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
GRP20    DS    0H                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GRPX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RGRPREC,R6                                                       
         MVC   REC-4(2),RGRPLEN    INSERT LENGTH FOR PUT                        
         MVC   RGRPKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,GRPCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,GRPCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+08,C'Y'                                                  
         BNE   GRP10                                                            
         MVC   P+1(8),=C'GRP REC:'                                              
         MVC   P+10(2),=C'07'                                                   
         MVC   P+20(27),RGRPREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     GRP10                                                            
                                                                                
GRPX     DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*  OWNRPROC: EXTRACT ALL OLDREP OWNR RECS AND                    *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
OWNRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ROWNKEY,R6                                                       
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     OWN20                                                            
                                                                                
OWN10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
OWN20    DS    0H                                                               
         CLC   KEY(24),KEYSAVE                                                  
         BNE   OWNX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING ROWNREC,R6                                                       
         MVC   REC-4(2),ROWNLEN    INSERT LENGTH FOR PUT                        
         MVC   ROWNKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,OWNCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,OWNCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+09,C'Y'                                                  
         BNE   OWN10                                                            
         MVC   P+1(8),=C'OWN REC:'                                              
         MVC   P+10(2),=C'2A'                                                   
         MVC   P+20(27),ROWNREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     OWN10                                                            
                                                                                
OWNX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  MRKTPROC: EXTRACT ALL OLDREP MRKT RECS AND                    *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
MRKTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RMKTKEY,R6                                                       
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     MKT20                                                            
                                                                                
MKT10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
MKT20    DS    0H                                                               
         CLC   KEY(23),KEYSAVE                                                  
         BNE   MKTX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RMKTREC,R6                                                       
         MVC   REC-4(2),RMKTLEN    INSERT LENGTH FOR PUT                        
         MVC   RMKTKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,MKTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,MKTCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   MKT10                                                            
         MVC   P+1(8),=C'MKT REC:'                                              
         MVC   P+10(2),=C'2B'                                                   
         MVC   P+20(27),RMKTREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     MKT10                                                            
                                                                                
MKTX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  MKGDPROC: EXTRACT ALL OLDREP M/G  RECS AND                    *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
MKGDPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         MVI   RMKGKTYP,X'11'                                                   
         MVC   RMKGKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     MKGD0040                                                         
                                                                                
MKGD0020 DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
MKGD0040 DS    0H                                                               
         CLC   KEY(08),KEYSAVE                                                  
         BNE   MKGD0060                                                         
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RMKGREC,R6                                                       
         MVC   REC-4(2),RMKGLEN    INSERT LENGTH FOR PUT                        
         MVC   RMKGKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,MKGCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,MKGCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         ZICM  RF,RMKGLEN,2        ADD ALL M/G LENGTHS                          
         CVD   RF,DUB                                                           
         AP    MKGBYTES(8),DUB(8)                                               
*                                                                               
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   MKGD0020                                                         
         CLC   MKGCTR,=F'50'       DISPLAY FIRST N RECORDS                      
         BH    MKGD0020                                                         
*                                                                               
         MVC   P+1(8),=C'MGD REC:'                                              
         MVC   P+10(2),=C'11'                                                   
         MVC   P+20(27),RMKGKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     MKGD0020                                                         
                                                                                
MKGD0060 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*   INVPROC: EXTRACT ALL OLDREP INV  RECS AND                    *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
INVPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     INV0040                                                          
                                                                                
INV0020  DS     0H                                                              
         GOTO1 SEQDIR                                                           
                                                                                
INV0040  DS     0H                                                              
         CLC   KEY(12),KEYSAVE                                                  
         BNE   INV0060                                                          
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
         CLC   =C'WNYW',RINVKSTA   UTS WNYW DATA NOT UNIQUE:                    
         BE    INV0020                SKIP IT                                   
                                                                                
         DROP  R6                                                               
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RINVREC,R6                                                       
         MVC   REC-4(2),RINVLEN    INSERT LENGTH FOR PUT                        
         MVC   RINVKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,INVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,INVCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         ZICM  RF,RINVLEN,2        ADD ALL INVENT LENGTHS                       
         CVD   RF,DUB                                                           
         AP    INVBYTES(8),DUB(8)                                               
*                                                                               
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   INV0020                                                          
         CLC   INVCTR,=F'150'                                                   
         BH    INV0020                                                          
*                                                                               
         MVC   P+1(8),=C'INV REC:'                                              
         MVC   P+10(27),RINVKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     INV0020                                                          
                                                                                
INV0060  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*   BUDPROC: EXTRACT ALL BUDREP BUD  RECS AND                    *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
BUDPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUDKEY,R6                                                       
         MVI   RBUDKTYP,X'13'                                                   
         MVC   RBUDKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     BUD0040                                                          
                                                                                
BUD0020  DS     0H                                                              
         GOTO1 SEQDIR                                                           
                                                                                
BUD0040  DS     0H                                                              
         CLC   KEY(18),KEYSAVE                                                  
         BNE   BUD0060                                                          
         CLC   KEY+18(2),=C'99'                                                 
         BNH   BUD0020             SKIP PRE-2000 BUDGETS                        
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RBUDREC,R6                                                       
         MVC   REC-4(2),RBUDLEN    INSERT LENGTH FOR PUT                        
         MVC   RBUDKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,BUDCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUDCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   BUD0020                                                          
         MVC   P+1(8),=C'BUD REC:'                                              
         MVC   P+10(27),RBUDKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     BUD0020                                                          
                                                                                
BUD0060  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*   IBKLPROC: EXTRACT ALL IBKLIST RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
IBKLPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RIBLKEY,R6                                                       
         MVC   RIBLKTYP(2),=X'1503'                                             
         MVC   RIBLKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     IBL0040                                                          
                                                                                
IBL0020  DS     0H                                                              
         GOTO1 SEQDIR                                                           
                                                                                
IBL0040  DS     0H                                                              
         CLC   KEY(17),KEYSAVE                                                  
         BNE   IBL0060                                                          
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RIBLREC,R6                                                       
         MVC   REC-4(2),RIBLLEN    INSERT LENGTH FOR PUT                        
         MVC   RIBLKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,IBLCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,IBLCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   IBL0020                                                          
         MVC   P+1(8),=C'IBL REC:'                                              
         MVC   P+10(27),RIBLKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     IBL0020                                                          
                                                                                
IBL0060  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*   EOMPROC:  EXTRACT ALL EOM     RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
EOMPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REOMKEY,R6                                                       
         MVI   REOMKTYP,X'18'                                                   
         MVC   REOMKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     EOM0040                                                          
                                                                                
EOM0020  DS     0H                                                              
         GOTO1 SEQDIR                                                           
                                                                                
EOM0040  DS     0H                                                              
         CLC   KEY(20),KEYSAVE                                                  
         BNE   EOM0060                                                          
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING REOMREC,R6                                                       
         MVC   REC-4(2),REOMLEN    INSERT LENGTH FOR PUT                        
         MVC   REOMKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,EOMCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EOMCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   EOM0020                                                          
         MVC   P+1(8),=C'EOM REC:'                                              
         MVC   P+10(27),REOMKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     EOM0020                                                          
                                                                                
EOM0060  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*   SETRPROC: EXTRACT ALL SETR    RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
SETRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSETKEY,R6                                                       
         MVI   RSETKTYP,X'38'                                                   
         MVC   RSETKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     SETR0040                                                         
                                                                                
SETR0020 DS     0H                                                              
         GOTO1 SEQDIR                                                           
                                                                                
SETR0040 DS     0H                                                              
         CLC   KEY(21),KEYSAVE                                                  
         BNE   SETR0060                                                         
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RSETREC,R6                                                       
         MVC   REC-4(2),RSETLEN    INSERT LENGTH FOR PUT                        
         MVC   RSETKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,SETRCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SETRCTR                                                       
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   SETR0020                                                         
         MVC   P+1(8),=C'SETR REC:'                                             
         MVC   P+10(27),RSETKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     SETR0020                                                         
                                                                                
SETR0060 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*   RDPTPROC: EXTRACT ALL RDPT    RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
RDPTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RRDPKEY,R6                                                       
         MVI   RRDPKTYP,X'3C'                                                   
         MVC   RRDPKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     RDPT0040                                                         
                                                                                
RDPT0020 DS     0H                                                              
         GOTO1 SEQDIR                                                           
                                                                                
RDPT0040 DS     0H                                                              
*                                                                               
*   TEST                                                                        
         MVC   P+1(08),=C'3C RECS:'                                             
         MVC   P+10(27),KEY                                                     
         GOTO1 REPORT                                                           
*   TEST                                                                        
*                                                                               
         CLC   KEY(26),KEYSAVE                                                  
         BNE   RDPT0060                                                         
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RRDPREC,R6                                                       
         MVC   REC-4(2),RRDPLEN    INSERT LENGTH FOR PUT                        
         MVC   RRDPKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,RDPCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RDPCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   RDPT0020                                                         
         MVC   P+1(8),=C'RDPT REC:'                                             
         MVC   P+10(27),RRDPKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     RDPT0020                                                         
                                                                                
RDPT0060 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*   RARTPROC: EXTRACT ALL RART    RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE NEWREP              *              
******************************************************************              
RARTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RARTKEY,R6                                                       
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     RART0040                                                         
                                                                                
RART0020 DS     0H                                                              
         GOTO1 SEQDIR                                                           
                                                                                
RART0040 DS     0H                                                              
         CLC   KEY(12),KEYSAVE                                                  
         BNE   RART0060                                                         
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RARTREC,R6                                                       
         MVC   REC-4(2),RARTLEN    INSERT LENGTH FOR PUT                        
         MVC   RARTKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,RARTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,RARTCTR                                                       
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   RART0020                                                         
         MVC   P+1(8),=C'RART REC:'                                             
         MVC   P+10(27),RARTKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     RART0020                                                         
                                                                                
RART0060 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  AVALPROC:  WILL BRING ALL OLDREP     AVAL   RECORDS           *              
******************************************************************              
AVALPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'14'                                                    
                                                                                
AVAL0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
AVAL0040 EQU   *                                                                
         MVC   KEY+17(2),OLDREP                                                 
***      MVC   KEY+19(4),REVCON    INSERT 9'S COMP REV CON#                     
         GOTO1 HIGHDIR                                                          
         B     AVAL0080                                                         
AVAL0060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
AVAL0080 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     COMPARE THROUGH REP CODE                     
         BNE   AVAL0100            NOT RIGHT REC, REP, OR CON#                  
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RAVLREC,R6                                                       
         MVC   REC-4(2),RAVLLEN    INSERT LENGTH FOR PUT                        
         MVC   RAVLKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,AVLCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AVLCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   AVAL0060                                                         
         CLC   AVLCTR,=F'50'       DISPLAY FIRST N RECORDS                      
         BH    AVAL0060                                                         
*                                                                               
         MVC   P+1(8),=C'AVL REC:'                                              
         MVC   P+10(2),=C'14'                                                   
         MVC   P+20(8),RAVLKCON    DISPLAY CON#/AV#/SRC/DET#                    
         GOTO1 REPORT                                                           
         B     AVAL0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
AVAL0100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PROPPROC:  WILL BRING ALL OLDREP     PROP   RECORDS           *              
******************************************************************              
PROPPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'16'                                                    
                                                                                
PROP0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
PROP0040 EQU   *                                                                
         MVC   KEY+14(2),OLDREP                                                 
***      MVC   KEY+16(4),REVCON    INSERT 9'S COMP REV CON#                     
*                                                                               
         GOTO1 HIGHDIR                                                          
         B     PROP0080                                                         
PROP0060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
PROP0080 EQU   *                                                                
         CLC   KEY(16),KEYSAVE     COMPARE THROUGH REP CODE                     
         BNE   PROP0100            NOT RIGHT REC, REP, OR CON#                  
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RPRPREC,R6                                                       
         MVC   REC-4(2),RPRPLEN    INSERT LENGTH FOR PUT                        
         MVC   RPRPKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,PRPCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PRPCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   PROP0060                                                         
         MVC   P+1(8),=C'PRP REC:'                                              
         MVC   P+10(2),=C'16'                                                   
         MVC   P+20(27),RPRPREC                                                 
         GOTO1 REPORT                                                           
         B     PROP0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
PROP0100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  EOP1PROC:  WILL BRING ALL OLDREP     EOP1   RECORDS           *              
******************************************************************              
EOP1PROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'1B'                                                    
                                                                                
EOP10020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
EOP10040 EQU   *                                                                
         MVC   KEY+15(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     EOP10080                                                         
EOP10060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
EOP10080 EQU   *                                                                
         CLC   KEY(17),KEYSAVE     COMPARE THROUGH REP                          
         BNE   EOP10100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING REOPREC,R6                                                       
         MVC   REC-4(2),REOPLEN    INSERT LENGTH FOR PUT                        
         MVC   REOPKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,EO1CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EO1CTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   EOP10060                                                         
*                                                                               
         CLC   EO1CTR,=F'50'      DISPLAY FIRST N RECORDS                       
         BH    EOP10060                                                         
*                                                                               
         MVC   P+1(8),=C'EO1 REC:'                                              
         MVC   P+10(2),=C'1B'                                                   
         MVC   P+20(27),REOPREC                                                 
         GOTO1 REPORT                                                           
         B     EOP10060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
EOP10100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  EOP2PROC:  WILL BRING ALL OLDREP     EOP2   RECORDS           *              
******************************************************************              
EOP2PROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'1C'                                                    
                                                                                
EOP20020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
EOP20040 EQU   *                                                                
         MVC   KEY+13(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     EOP20080                                                         
EOP20060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
EOP20080 EQU   *                                                                
         CLC   KEY(15),KEYSAVE     COMPARE THROUGH REP                          
         BNE   EOP20100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING REOPREC,R6                                                       
         MVC   REC-4(2),REOPLEN    INSERT LENGTH FOR PUT                        
         MVC   REO2KREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,EO2CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EO2CTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   EOP20060                                                         
*                                                                               
         CLC   EO2CTR,=F'50'      DISPLAY FIRST N RECORDS                       
         BH    EOP20060                                                         
*                                                                               
         MVC   P+1(8),=C'EO2 REC:'                                              
         MVC   P+10(2),=C'1C'                                                   
         MVC   P+20(27),REOPREC                                                 
         GOTO1 REPORT                                                           
         B     EOP20060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
EOP20100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  EOP3PROC:  WILL BRING ALL OLDREP     EOP3   RECORDS           *              
******************************************************************              
EOP3PROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'1D'                                                    
                                                                                
EOP30020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
EOP30040 EQU   *                                                                
         MVC   KEY+17(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     EOP30080                                                         
EOP30060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
EOP30080 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     COMPARE THROUGH REP                          
         BNE   EOP30100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING REOPREC,R6                                                       
         MVC   REC-4(2),REOPLEN    INSERT LENGTH FOR PUT                        
         MVC   REO3KREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,EO3CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EO3CTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   EOP30060                                                         
*                                                                               
         CLC   EO3CTR,=F'50'      DISPLAY FIRST N RECORDS                       
         BH    EOP30060                                                         
*                                                                               
         MVC   P+1(8),=C'EO3 REC:'                                              
         MVC   P+10(2),=C'1D'                                                   
         MVC   P+20(27),REOPREC                                                 
         GOTO1 REPORT                                                           
         B     EOP30060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
EOP30100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  EOP4PROC:  WILL BRING ALL OLDREP     EOP4   RECORDS           *              
******************************************************************              
EOP4PROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'1E'                                                    
                                                                                
EOP40020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
EOP40040 EQU   *                                                                
         MVC   KEY+16(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     EOP40080                                                         
EOP40060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
EOP40080 EQU   *                                                                
         CLC   KEY(18),KEYSAVE     COMPARE THROUGH REP                          
         BNE   EOP40100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING REOPREC,R6                                                       
         MVC   REC-4(2),REOPLEN    INSERT LENGTH FOR PUT                        
         MVC   REO4KREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,EO4CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EO4CTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   EOP40060                                                         
*                                                                               
         CLC   EO4CTR,=F'50'      DISPLAY FIRST N RECORDS                       
         BH    EOP40060                                                         
*                                                                               
         MVC   P+1(8),=C'EO4 REC:'                                              
         MVC   P+10(2),=C'1E'                                                   
         MVC   P+20(27),REOPREC                                                 
         GOTO1 REPORT                                                           
         B     EOP40060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
EOP40100 DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*  ACALPROC:  WILL BRING ALL OLDREP ALT CALENDAR RECORDS         *              
******************************************************************              
ACALPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'20'                                                    
                                                                                
ACAL0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
ACAL0040 EQU   *                                                                
         MVC   KEY+01(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     ACAL0080                                                         
ACAL0060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
ACAL0080 EQU   *                                                                
         CLC   KEY(03),KEYSAVE     COMPARE THROUGH REP                          
         BNE   ACAL0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RACLREC,R6                                                       
         MVC   REC-4(2),RACLLEN    INSERT LENGTH FOR PUT                        
         MVC   RACLKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,ACLCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ACLCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   ACAL0060                                                         
         MVC   P+1(8),=C'ACL REC:'                                              
         MVC   P+10(2),=C'20'                                                   
         MVC   P+20(27),RACLREC                                                 
         GOTO1 REPORT                                                           
         B     ACAL0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
ACAL0100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  DEMOPROC:  WILL BRING ALL OLDREP     DEMO   RECORDS           *              
******************************************************************              
DEMOPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'23'                                                    
                                                                                
DEMO0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
DEMO0040 EQU   *                                                                
         MVC   KEY+23(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     DEMO0080                                                         
DEMO0060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
DEMO0080 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     COMPARE THROUGH REP                          
         BNE   DEMO0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RDEMREC,R6                                                       
         MVC   REC-4(2),RDEMLEN    INSERT LENGTH FOR PUT                        
         MVC   RDEMKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,DEMCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,DEMCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   DEMO0060                                                         
         MVC   P+1(8),=C'DEM REC:'                                              
         MVC   P+10(2),=C'23'                                                   
         MVC   P+20(27),RDEMREC                                                 
         GOTO1 REPORT                                                           
         B     DEMO0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
DEMO0100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  DYPTPROC:  WILL BRING ALL OLDREP     DYPT   RECORDS           *              
******************************************************************              
DYPTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'24'                                                    
                                                                                
DYPT0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
DYPT0040 EQU   *                                                                
         MVC   KEY+24(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     DYPT0080                                                         
DYPT0060 EQU   *                                                                
         GOTO1 SEQDIR                                                           
DYPT0080 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     COMPARE THROUGH REP                          
         BNE   DYPT0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RDPTREC,R6                                                       
         MVC   REC-4(2),RDPTLEN    INSERT LENGTH FOR PUT                        
         MVC   RDPTKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,DPTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,DPTCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   DYPT0060                                                         
         MVC   P+1(8),=C'DPT REC:'                                              
         MVC   P+10(2),=C'24'                                                   
         MVC   P+20(27),RDPTREC                                                 
         GOTO1 REPORT                                                           
         B     DYPT0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
DYPT0100 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  SDDPROC:   WILL BRING ALL OLDREP     SDD    RECORDS           *              
******************************************************************              
SDDPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'26'                                                    
                                                                                
SDD0020  DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
SDD0040  EQU    *                                                               
         MVC   KEY+20(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     SDD0080                                                          
SDD0060  EQU    *                                                               
         GOTO1 SEQDIR                                                           
SDD0080  EQU    *                                                               
         CLC   KEY(22),KEYSAVE     COMPARE THROUGH REP                          
         BNE   SDD0100             NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RSDDREC,R6                                                       
         MVC   REC-4(2),RSDDLEN    INSERT LENGTH FOR PUT                        
         MVC   RSDDKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,SDDCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SDDCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   SDD0060                                                          
         MVC   P+1(8),=C'SDD REC:'                                              
         MVC   P+10(2),=C'26'                                                   
         MVC   P+20(27),RSDDREC                                                 
         GOTO1 REPORT                                                           
         B     SDD0060             GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
SDD0100  DS     0H                                                              
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  COMMPROC:  WILL BRING ALL OLDREP     COMM   RECORDS           *              
******************************************************************              
COMMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'29'                                                    
                                                                                
COMM0020 DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
COMM0040 EQU    *                                                               
         MVC   KEY+11(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     COMM0080                                                         
COMM0060 EQU    *                                                               
         GOTO1 SEQDIR                                                           
COMM0080 EQU    *                                                               
         CLC   KEY(13),KEYSAVE     COMPARE THROUGH REP                          
         BNE   COMM0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCOMREC,R6                                                       
         MVC   REC-4(2),RCOMLEN    INSERT LENGTH FOR PUT                        
         MVC   RCOMKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,COMCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,COMCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   COMM0060                                                         
         MVC   P+1(8),=C'COM REC:'                                              
         MVC   P+10(2),=C'29'                                                   
         MVC   P+20(27),RCOMREC                                                 
         GOTO1 REPORT                                                           
         B     COMM0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
COMM0100 DS     0H                                                              
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  SCOMPROC:  WILL BRING ALL OLDREP     SCOM   RECORDS           *              
******************************************************************              
SCOMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'2E'                                                    
                                                                                
SCOM0020 DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
SCOM0040 EQU    *                                                               
         MVC   KEY+15(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     SCOM0080                                                         
SCOM0060 EQU    *                                                               
         GOTO1 SEQDIR                                                           
SCOM0080 EQU    *                                                               
         CLC   KEY(17),KEYSAVE     COMPARE THROUGH REP                          
         BNE   SCOM0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCMTREC,R6                                                       
         MVC   REC-4(2),RCMTLEN    INSERT LENGTH FOR PUT                        
         MVC   RCMTKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,CMTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CMTCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   SCOM0060                                                         
         MVC   P+1(8),=C'CMT REC:'                                              
         MVC   P+10(2),=C'2E'                                                   
         MVC   P+20(27),RCMTREC                                                 
         GOTO1 REPORT                                                           
         B     SCOM0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
SCOM0100 DS     0H                                                              
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  RADRPROC:  WILL BRING ALL OLDREP     RADR   RECORDS           *              
******************************************************************              
RADRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'33'                                                    
                                                                                
RADR0020 DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
RADR0040 EQU    *                                                               
         MVC   KEY+17(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     RADR0080                                                         
RADR0060 EQU    *                                                               
         GOTO1 SEQDIR                                                           
RADR0080 EQU    *                                                               
         CLC   KEY(19),KEYSAVE     COMPARE THROUGH REP                          
         BNE   RADR0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RRDAREC,R6                                                       
         MVC   REC-4(2),RRDALEN    INSERT LENGTH FOR PUT                        
         MVC   RRDAKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,RDRCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RDRCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   RADR0060                                                         
         MVC   P+1(8),=C'RDR REC:'                                              
         MVC   P+10(2),=C'33'                                                   
         MVC   P+20(27),RRDAREC                                                 
         GOTO1 REPORT                                                           
         B     RADR0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
RADR0100 DS     0H                                                              
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  OCOMPROC:  WILL BRING ALL OLDREP     OCOM   RECORDS           *              
******************************************************************              
OCOMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'34'                                                    
                                                                                
OCOM0020 DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
OCOM0040 EQU    *                                                               
         MVC   KEY+20(2),OLDREP                                                 
         GOTO1 HIGHDIR                                                          
         B     OCOM0080                                                         
OCOM0060 EQU    *                                                               
         GOTO1 SEQDIR                                                           
OCOM0080 EQU    *                                                               
         CLC   KEY(22),KEYSAVE     COMPARE THROUGH REP                          
         BNE   OCOM0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING ROCMREC,R6                                                       
         MVC   REC-4(2),ROCMLEN    INSERT LENGTH FOR PUT                        
         MVC   ROCMKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,OCMCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,OCMCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   OCOM0060                                                         
         MVC   P+1(8),=C'OCM REC:'                                              
         MVC   P+10(2),=C'34'                                                   
         MVC   P+20(27),ROCMREC                                                 
         GOTO1 REPORT                                                           
         B     OCOM0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
OCOM0100 DS     0H                                                              
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  P431PROC:  WILL BRING ALL OLDREP     P431   RECORDS           *              
*                                                                               
*       NEED MORE INFORMATION ABOUT THESE RECORDS                               
*                                                                               
******************************************************************              
*P431PROC NTR1                                                                  
*        L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
*                                                                               
P4310020 DS     0H                                                              
*        XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
*        XC    KEY,KEY                                                          
*        MVC   KEY(2),=X'4301'     INSERT RECORD TYPE                           
P4310040 EQU    *                                                               
*        MVC   KEY+07(2),OLDREP                                                 
***      MVC   KEY+9(4),NINECON    INSERT 9'S COMP KEY                          
*        GOTO1 HIGHDIR                                                          
*        B     P4310080                                                         
P4310060 EQU    *                                                               
*        GOTO1 SEQDIR                                                           
P4310080 EQU    *                                                               
*        CLC   KEY(09),KEYSAVE     COMPARE THROUGH CON #                        
*        BNE   P4310100            NOT RIGHT REC, REP, OR CON#                  
*                                                                               
*        GOTO1 GETRECRD                                                         
*                                                                               
*        LA    R6,REC                                                           
*        USING RPROKEY,R6                                                       
*        MVC   REC-4(2),RPRORLEN   INSERT LENGTH FOR PUT                        
*        MVC   RPROKRCD,NEWREP     NEW REP CODE                                 
*                                                                               
*        L     RF,X41CTR                                                        
*        LA    RF,1(RF)                                                         
*        ST    RF,X41CTR                                                        
*                                                                               
*        BAS   RE,PUTRECS                                                       
*                                                                               
*        ZICM  RF,RPRORLEN,2       ADD ALL PROP LENGTHS                         
*        CVD   RF,DUB                                                           
*        AP    D43BYTES(8),DUB(8)                                               
*                                                                               
*        CLI   QUESTOR+1,C'Y'                                                   
*        BNE   P4310060                                                         
*        CLC   X41CTR,=F'50'                                                    
*        BH    P4310060                                                         
*        MVC   P+1(8),=C'PRO REC:'                                              
*        MVC   P+10(4),=C'4301'                                                 
*        MVC   P+20(27),RPROKEY                                                 
*        GOTO1 REPORT                                                           
*        B     P4310060            GO BACK FOR NEXT REQ                         
*        DROP  R6                                                               
*                                                                               
P4310100 DS     0H                                                              
*        B     EXIT                                                             
*        EJECT                                                                  
******************************************************************              
*  P432PROC:  WILL BRING ALL OLDREP     P432   RECORDS           *              
*                                                                               
*       NEED MORE INFORMATION ABOUT THESE RECORDS                               
*                                                                               
******************************************************************              
*P432PROC NTR1                                                                  
*        L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
*                                                                               
P4320020 DS     0H                                                              
*        XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
*        XC    KEY,KEY                                                          
*        MVC   KEY(2),=X'4302'     INSERT RECORD TYPE                           
P4320040 EQU    *                                                               
*        MVC   KEY+07(15),OLDREP                                                
****     MVC   KEY+17(4),NINECON   INSERT 9'S COMP KEY                          
*        GOTO1 HIGHDIR                                                          
*        B     P4320080                                                         
P4320060 EQU    *                                                               
*        GOTO1 SEQDIR                                                           
P4320080 EQU    *                                                               
*        CLC   KEY(17),KEYSAVE     COMPARE THROUGH REP CODE                     
*        BNE   P4320100            NOT RIGHT REC, REP, OR CON#                  
*                                                                               
*        GOTO1 GETRECRD                                                         
*                                                                               
*        LA    R6,REC                                                           
*        USING RSLWKEY,R6                                                       
*        MVC   REC-4(2),RSLWRLEN   INSERT LENGTH FOR PUT                        
*        MVC   RSLWKREP,NEWREP     NEW REP CODE                                 
*                                                                               
*        L     RF,X42CTR                                                        
*        LA    RF,1(RF)                                                         
*        ST    RF,X42CTR                                                        
*                                                                               
*        BAS   RE,PUTRECS                                                       
*                                                                               
*        CLI   QUESTOR+1,C'Y'                                                   
*        BNE   P4320060                                                         
*        CLC   X42CTR,=F'50'                                                    
*        BH    P4320060                                                         
*        MVC   P+1(8),=C'SLW REC:'                                              
*        MVC   P+10(4),=C'4302'                                                 
*        MVC   P+20(27),RSLWKEY                                                 
*        GOTO1 REPORT                                                           
*        B     P4320060            GO BACK FOR NEXT REQ                         
*        DROP  R6                                                               
*                                                                               
P4320100 DS     0H                                                              
*        B     EXIT                                                             
*        EJECT                                                                  
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
LOCALDUB DS    D                   LOCAL 'DUB'                                  
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'CONTRACTS     PROCESSED:'                             
         EDIT  CONCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUYS          PROCESSED:'                             
         EDIT  BUYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AGY+AGY1 RECS OUTPUT   :'                             
         EDIT  AGYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AGY ALREADY ON O/P FILE:'                             
         EDIT  AGONFILE,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AGY EQUIVED: NOT COPIED:'                             
         EDIT  AGEQUIV,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AGY NOT IN TABLE       :'                             
         EDIT  NFAGY,(12,P+30),COMMAS=YES                                       
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADVERTISERS   OUTPUT   :'                             
         EDIT  ADVCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADV ALREADY ON O/P FILE:'                             
         EDIT  ADONFILE,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADV EQUIVED: NOT COPIED:'                             
         EDIT  ADEQUIV,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADV NOT IN TABLE       :'                             
         EDIT  NFADV,(12,P+30),COMMAS=YES                                       
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SALESPERSONS  OUTPUT   :'                             
         EDIT  SALCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'S/P ALREADY ON O/P FILE:'                             
         EDIT  SPONFILE,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'S/P EQUIVED: NOT COPIED:'                             
         EDIT  SPEQUIV,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'S/P NOT IN TABLE       :'                             
         EDIT  NFSALP,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'PRODUCTS      PROCESSED:'                             
         EDIT  PRDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REP RECORDS   PROCESSED:'                             
         EDIT  REPCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STATIONS      PROCESSED:'                             
         EDIT  STACTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OFFICES       PROCESSED:'                             
         EDIT  OFFCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TEAM          PROCESSED:'                             
         EDIT  TEMCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CLASSES       PROCESSED:'                             
         EDIT  CLSCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CATEGORIES    PROCESSED:'                             
         EDIT  CTGCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CON TYPES     PROCESSED:'                             
         EDIT  CTYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'GROUP RECS    PROCESSED:'                             
         EDIT  GRPCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OWNER RECS    PROCESSED:'                             
         EDIT  OWNCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MARKET RECS   PROCESSED:'                             
         EDIT  MKTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'M/G    RECS   PROCESSED:'                             
         EDIT  MKGCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'INVENTORY REC PROCESSED:'                             
         EDIT  INVCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUDGET   RECS PROCESSED:'                             
         EDIT  BUDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'IBKLIST  RECS PROCESSED:'                             
         EDIT  IBLCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'EOM      RECS PROCESSED:'                             
         EDIT  EOMCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SET      RECS PROCESSED:'                             
         EDIT  SETRCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'RSCH DPT RECS PROCESSED:'                             
         EDIT  RDPCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AVAIL RATES   PROCESSED:'                             
         EDIT  RARTCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REGION RECS   PROCESSED:'                             
         EDIT  RGNCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AVAIL  RECS   PROCESSED:'                             
         EDIT  AVLCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'PROPOS RECS   PROCESSED:'                             
         EDIT  PRPCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'EOP 1B RECS   PROCESSED:'                             
         EDIT  EO1CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'EOP 1C RECS   PROCESSED:'                             
         EDIT  EO2CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'EOP 1D RECS   PROCESSED:'                             
         EDIT  EO3CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'EOP 1E RECS   PROCESSED:'                             
         EDIT  EO4CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DALT CAL RECS PROCESSED:'                             
         EDIT  ACLCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DEMENU RECS   PROCESSED:'                             
         EDIT  DEMCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DAYPT  RECS   PROCESSED:'                             
         EDIT  DPTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SDD    RECS   PROCESSED:'                             
         EDIT  SDDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'COMMIS RECS   PROCESSED:'                             
         EDIT  COMCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STD COMMENTS  PROCESSED:'                             
         EDIT  CMTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'RADAR  RECS   PROCESSED:'                             
         EDIT  RDRCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OFF CMT RECS  PROCESSED:'                             
         EDIT  OCMCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DAR 41 RECS   PROCESSED:'                             
         EDIT  D41CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DAR 51 RECS   PROCESSED:'                             
         EDIT  D51CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'4301   RECS   PROCESSED:'                             
         EDIT  X41CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'4302   RECS   PROCESSED:'                             
         EDIT  X42CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CFC    RECS   PROCESSED:'                             
         EDIT  CFCCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ACTIV  RECS   PROCESSED:'                             
         EDIT  ACTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
***      MVC   P+1(24),=C'MISCELLANEOUS PROCESSED:'                             
***      EDIT  OTHERCTR,(12,P+30),COMMAS=YES                                    
***      GOTO1 REPORT                                                           
*                                  CALCULATE AVERAGE RECORD SIZE                
         L     RF,CONCTR                                                        
         LTR   RF,RF               ANY DIVISOR?                                 
         BZ    NODIV001            NO                                           
         CVD   RF,DUB              YES - CONVERT TO PACKED FOR DIVIDE           
         MVC   P+1(24),=C'AVERAGE CONTRACT RECORD:'                             
         EDIT  (P8,CONBYTES),(12,P+48),COMMAS=YES,DUB=LOCALDUB                  
         DP    CONBYTES(8),DUB+4(4)     NUM BYTES / # CONTRACTS                 
         EDIT  (P4,CONBYTES),(12,P+30),COMMAS=YES,DUB=LOCALDUB                  
         GOTO1 REPORT                                                           
NODIV001 EQU   *                                                                
         L     RF,BUYCTR           CALCULATE AVERAGE RECORD SIZE                
         LTR   RF,RF               ANY DIVISOR?                                 
         BZ    NODIV002            NO                                           
         CVD   RF,DUB              YES - CONVERT TO PACKED FOR DIVIDE           
         MVC   P+1(19),=C'AVERAGE BUY RECORD:'                                  
         EDIT  (P8,BUYBYTES),(12,P+48),COMMAS=YES,DUB=LOCALDUB                  
         DP    BUYBYTES(8),DUB+4(4)     NUM BYTES / # BUYS                      
         EDIT  (P4,BUYBYTES),(12,P+30),COMMAS=YES,DUB=LOCALDUB                  
         GOTO1 REPORT                                                           
NODIV002 EQU   *                                                                
         L     RF,MKGCTR           CALCULATE AVERAGE RECORD SIZE                
         LTR   RF,RF               ANY DIVISOR?                                 
         BZ    NODIV003            NO                                           
         CVD   RF,DUB              YES - CONVERT TO PACKED FOR DIVIDE           
         MVC   P+1(19),=C'AVERAGE M/G RECORD:'                                  
         EDIT  (P8,MKGBYTES),(12,P+48),COMMAS=YES,DUB=LOCALDUB                  
         DP    MKGBYTES(8),DUB+4(4)     NUM BYTES / # M/GS                      
         EDIT  (P4,MKGBYTES),(12,P+30),COMMAS=YES,DUB=LOCALDUB                  
         GOTO1 REPORT                                                           
NODIV003 EQU   *                                                                
         L     RF,INVCTR           CALCULATE AVERAGE RECORD SIZE                
         LTR   RF,RF               ANY DIVISOR?                                 
         BZ    NODIV004            NO                                           
         CVD   RF,DUB              YES - CONVERT TO PACKED FOR DIVIDE           
         MVC   P+1(19),=C'AVERAGE INV RECORD:'                                  
         EDIT  (P8,INVBYTES),(12,P+48),COMMAS=YES,DUB=LOCALDUB                  
         DP    INVBYTES(8),DUB+4(4)     NUM BYTES / # INVS                      
         EDIT  (P4,INVBYTES),(12,P+30),COMMAS=YES,DUB=LOCALDUB                  
         GOTO1 REPORT                                                           
NODIV004 EQU   *                                                                
         L     RF,D41CTR           CALCULATE AVERAGE RECORD SIZE                
         LTR   RF,RF               ANY DIVISOR?                                 
         BZ    NODIV005            NO                                           
         CVD   RF,DUB              YES - CONVERT TO PACKED FOR DIVIDE           
         MVC   P+1(20),=C'AVERAGE DR41 RECORD:'                                 
         EDIT  (P8,D41BYTES),(12,P+48),COMMAS=YES,DUB=LOCALDUB                  
         DP    D41BYTES(8),DUB+4(4)     NUM BYTES / # DARE 41'S                 
         EDIT  (P4,D41BYTES),(12,P+30),COMMAS=YES,DUB=LOCALDUB                  
         GOTO1 REPORT                                                           
NODIV005 EQU   *                                                                
         L     RF,D51CTR           CALCULATE AVERAGE RECORD SIZE                
         LTR   RF,RF               ANY DIVISOR?                                 
         BZ    NODIV006            NO                                           
         CVD   RF,DUB              YES - CONVERT TO PACKED FOR DIVIDE           
         MVC   P+1(20),=C'AVERAGE DR51 RECORD:'                                 
         EDIT  (P8,D51BYTES),(12,P+48),COMMAS=YES,DUB=LOCALDUB                  
         DP    D51BYTES(8),DUB+4(4)     NUM BYTES / # DARE 51'S                 
         EDIT  (P4,D51BYTES),(12,P+30),COMMAS=YES,DUB=LOCALDUB                  
         GOTO1 REPORT                                                           
NODIV006 EQU   *                                                                
         L     RF,X41CTR           CALCULATE AVERAGE RECORD SIZE                
         LTR   RF,RF               ANY DIVISOR?                                 
         BZ    NODIV007            NO                                           
         CVD   RF,DUB              YES - CONVERT TO PACKED FOR DIVIDE           
         MVC   P+1(20),=C'AVERAGE 4301 RECORD:'                                 
         EDIT  (P8,D43BYTES),(12,P+48),COMMAS=YES,DUB=LOCALDUB                  
         DP    D43BYTES(8),DUB+4(4)     NUM BYTES / # 4301 RECORDS              
         EDIT  (P4,D43BYTES),(12,P+30),COMMAS=YES,DUB=LOCALDUB                  
         GOTO1 REPORT                                                           
NODIV007 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         B     EXIT                                                             
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
         B     EXIT                                                             
         EJECT                                                                  
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
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
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
         B     EXIT                                                             
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     EXIT                                                             
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
         B     EXIT                                                             
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ABLDAREA DS    A                                                                
AGRPAREA DS    A                                                                
ANEXTGRP DS    A                                                                
ASTAAREA DS    A                                                                
ANEXTSTA DS    A                                                                
LSTASLOT EQU   16                                                               
STATABCT DS    F                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
LSALSLOT EQU   8                                                                
LSALCODE EQU   6                                                                
SALTABCT DS    F                                                                
AAGYAREA DS    A                                                                
ANEXTAGY DS    A                                                                
LAGYSLOT EQU   16                                                               
LAGYCODE EQU   12                                                               
AGYTABCT DS    F                                                                
AADVAREA DS    A                                                                
ANEXTADV DS    A                                                                
LADVSLOT EQU   12                                                               
LADVCODE EQU   8                                                                
ADVTABCT DS    F                                                                
APRDAREA DS    A                                                                
ANEXTPRD DS    A                                                                
LPRDSLOT EQU   16                                                               
LPRDCODE EQU   14                                                               
PRDTABCT DS    F                                                                
ACOMAREA DS    A                                                                
ANEXTCOM DS    A                                                                
ASTNAREA DS    A                                                                
LBLDAREA DS    F                                                                
NEXTAREA DS    A                   NEXT OPEN SLOT                               
STRTSRCH DS    A                   A(START OF SEARCH)                           
NUMBLD   DS    F                                                                
CONCTR   DS    F                                                                
CONCTR2  DS    F                                                                
REPCTR   DS    F                                                                
STACTR   DS    F                                                                
OFFCTR   DS    F                                                                
TEMCTR   DS    F                                                                
CTYCTR   DS    F                                                                
GRPCTR   DS    F                                                                
OWNCTR   DS    F                                                                
MKTCTR   DS    F                                                                
MKGCTR   DS    F                                                                
INVCTR   DS    F                                                                
BUDCTR   DS    F                                                                
IBLCTR   DS    F                                                                
EOMCTR   DS    F                                                                
OWNRCTR  DS    F                                                                
SETRCTR  DS    F                                                                
RDPTCTR  DS    F                                                                
RARTCTR  DS    F                                                                
CTGCTR   DS    F                                                                
CLSCTR   DS    F                                                                
BUYCTR   DS    F                                                                
BUYCTR2  DS    F                                                                
ADVCTR   DS    F                                                                
SALCTR   DS    F                                                                
SPONFILE DS    F                                                                
ADONFILE DS    F                                                                
AGONFILE DS    F                                                                
SPEQUIV  DS    F                                                                
ADEQUIV  DS    F                                                                
AGEQUIV  DS    F                                                                
NFSALP   DS    F                                                                
NFADV    DS    F                                                                
NFAGY    DS    F                                                                
PRDCTR   DS    F                                                                
RGNCTR   DS    F                                                                
AVLCTR   DS    F                                                                
PRPCTR   DS    F                                                                
EO1CTR   DS    F                                                                
EO2CTR   DS    F                                                                
EO3CTR   DS    F                                                                
EO4CTR   DS    F                                                                
ACLCTR   DS    F                                                                
DEMCTR   DS    F                                                                
DPTCTR   DS    F                                                                
SDDCTR   DS    F                                                                
COMCTR   DS    F                                                                
SENDCTR  DS    F                                                                
SENDCTR2 DS    F                                                                
CMTCTR   DS    F                                                                
CMMCTR   DS    F                                                                
CMMCTR2  DS    F                                                                
RDRCTR   DS    F                                                                
OCMCTR   DS    F                                                                
RDPCTR   DS    F                                                                
D41CTR   DS    F                                                                
D51CTR   DS    F                                                                
X41CTR   DS    F                                                                
X42CTR   DS    F                                                                
CFCCTR   DS    F                                                                
ACTCTR   DS    F                                                                
OTHERCTR DS    F                                                                
AGYCTR   DS    F                                                                
CONBYTES DC    PL8'0'                                                           
BUYBYTES DC    PL8'0'                                                           
D41BYTES DC    PL8'0'                                                           
D43BYTES DC    PL8'0'                                                           
D51BYTES DC    PL8'0'                                                           
INVBYTES DC    PL8'0'                                                           
MKGBYTES DC    PL8'0'                                                           
REVCON   DS    XL4                                                              
NINECON  DS    XL4                                                              
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
OLDADV   DS    CL4                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
NEWREP   DS    CL2                 NEW REP CODE                                 
NEWREP2  DS    CL2                 NEW REP CODE # 2                             
OLDREP   DS    CL2                 OLD REP CODE                                 
FSSREP   DS    CL2                 REP FROM CONTROL FILE                        
REPCODE  DS    CL2                                                              
KEYTYPE  DS    CL1                                                              
DBLSPACE DS    CL1                                                              
DATEWORK DS    CL24                DATE WORK AREA                               
SAVEKEY  DS    CL(L'KEY)                                                        
SAVEVUTL DS    CL1                 ORIGINAL UTL NUMBER                          
NEWUTL   DS    CL1                 NEW UTL NUMBER                               
NEEDDEL  DS    CL1                 NEED DELETE FLAG                             
*                                  0  =  NO DELETE NEEDED                       
*                                  1  =  DELETE NEEDED                          
KEYSAVE2 DS    CL(L'KEY)                                                        
ERRORFLG DC    XL1'00'                                                          
*                                  X'01' = TABLE ERROR HAS OCCURRED             
*                                  X'80' = CONTRACT/AGENCY ERROR                
*                                  X'40' = CONTRACT/ADVERT ERROR                
*                                  X'20' = CONTRACT/SALESP ERROR                
*                                  X'10' = CONTRACT/GROUP  ERROR                
FLIST    DS    0H                                                               
         DC    CL8'NREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL4100              AREA FOR RECORD                              
         DS    0D                                                               
*  INCLUDE REGENCOM                COMMISSION RECORD                            
*  INCLUDE REGENREG                REGION RECORD                                
*  INCLUDE REGENOFF                OFFICE RECORD                                
*  INCLUDE REGENTEM                TEAM   RECORD                                
*  INCLUDE REGENEOM                EOM RECORD                                   
*  INCLUDE REGENDPT                DAYPART RECORD                               
*  INCLUDE REGENBUD                BUDGET RECORD                                
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION RECORD                               
*  INCLUDE REGENSDD                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL4100                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENREPA         REP RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCOM          COMMISSION RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREG          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOFF          OFFICE     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTEM          TEAM       RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOM          END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDPTA         END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUD          BUDGET RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY2         AGENCY2 RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCLS          CLASS RECORD                                 
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTG          CATEGORY RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTY          K TYPE RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENGRP          K GRP  RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOWN          K OWNR RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKT          K MRKT RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKG                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENINV                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSET                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAVLN                                                      
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENACT                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCFC                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOCM                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENRDA                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCMT                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRPN                                                      
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOP                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENACL                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDEM                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENARTE                                                      
         EJECT                                                                  
       ++INCLUDE REGENIBKL                                                      
         EJECT                                                                  
       ++INCLUDE REGENRDP                                                       
         EJECT                                                                  
       ++INCLUDE REGENPRO                                                       
         EJECT                                                                  
       ++INCLUDE REGENSLW                                                       
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
NEWCSECT CSECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
*        AT THIS POINT, WE HAVE RETURNED TO THE UTS FILE.                       
*        EJECT                                                                  
*                                                                               
*   LOADTABS:  S/P, AGENCY, ADVERTISER CONVERSION TABLES ARE LOADED             
*        FROM THE UTS RECORDS.                                                  
*        AFTER LOADING, THE EQUIVALENCY CODES IN THE TABLES ARE                 
*        VALIDATED BY SWITCHING TO THE FSS FILE, AND AN ERROR                   
*        LISTING IS PRODUCED FOR EACH CODE TYPE.                                
*        STATION INFORMATION IS ALL LOADED FROM THE FSS FILE.  THIS             
*        PERMITS THE DEVELOPMENT OF THE CORRECT GROUP/SUBGROUP DATA             
*                                                                               
LOADTABS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*   TEST                                                                        
**       MVC   P+1(10),=C'ENTER L/T:'                                           
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         L     R2,ASALAREA         SET A(FIRST SLOT IN S/P TABLE)               
         XC    KEY,KEY                                                          
         MVI   KEY,6               SET SALESPERSON KEY TYPE                     
         MVC   KEY+22(2),=C'UT'    SET UTS INTO KEY                             
         GOTO1 HIGHDIR2            READ FIRST KEY ON FILE                       
         B     LTAB0020                                                         
LTAB0010 EQU   *                                                                
         GOTO1 SEQDIR2                                                          
LTAB0020 EQU   *                                                                
*                                                                               
*   TEST                                                                        
***      MVC   P+1(10),=C'KEY FOUND:'                                           
***      MVC   P+12(27),KEY                                                     
***      MVC   P+42(27),KEYSAVE                                                 
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   KEY(24),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   LTAB0030            NO  - S/P FINISHED                           
         GOTO1 GETREC2             YES - RETRIEVE RECORD                        
         LA    R6,REC                                                           
         USING RSALREC,R6                                                       
*                                                                               
*   TEST                                                                        
***      MVC   P+1(10),=C'S/P FOUND:'                                           
***      MVC   P+12(27),RSALREC                                                 
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   0(3,R2),RSALKSAL    INSERT UTS S/P INTO TABLE                    
***      CLC   RSALEQU(3),=C'000'  ANY VALUE IN FIELD?                          
***      BE    LTAB0028            NO                                           
***      MVC   3(3,R2),RSALEQU     YES - INSERT FSS S/P INTO TABLE              
         CLC   RSALMRG,SPACES      VALUE IN FIELD?                              
         BNH   LTAB0028            NO                                           
         MVC   3(3,R2),RSALMRG     YES - INSERT FSS S/P INTO TABLE              
LTAB0028 EQU   *                                                                
         OC    0(LSALCODE,R2),SPACES                                            
*                                  OR CODE SECTION TO SPACES                    
         DROP  R6                                                               
*                                                                               
         LA    R2,LSALSLOT(R2)     BUMP TO NEXT SLOT                            
         ST    R2,ANEXTSAL         SAVE A(NEXT SLOT)                            
         L     RF,SALTABCT         INCREMENT TABLE COUNT                        
         LA    RF,1(RF)                                                         
         ST    RF,SALTABCT                                                      
         B     LTAB0010            GO BACK FOR NEXT                             
LTAB0030 EQU   *                                                                
         CLI   QUESTOR+4,C'Y'      DISPLAY S/P TABLE?                           
         BNE   LTAB0050            NO                                           
         MVC   P+1(15),=C'TOTAL S/P     :'                                      
         EDIT  SALTABCT,(5,P+20)                                                
         GOTO1 REPORT                                                           
         L     R2,ASALAREA         YES                                          
LTAB0040 EQU   *                                                                
         CLC   0(3,R2),SPACES      ANY ENTRY IN TABLE?                          
         BNH   LTAB0050            NO  - S/P FINISHED                           
         MVC   P+1(04),=C'S/P:'                                                 
         MVC   P+8(3),0(R2)                                                     
         MVI   P+11,C'/'                                                        
         MVC   P+12(3),3(R2)                                                    
         GOTO1 REPORT                                                           
         LA    R2,LSALSLOT(R2)     BUMP TO NEXT SLOT                            
         B     LTAB0040            GO BACK FOR NEXT                             
LTAB0050 EQU   *                                                                
**AGENCY                                                                        
         L     R2,AAGYAREA         SET A(FIRST SLOT IN AGY TABLE)               
         XC    KEY,KEY                                                          
*                                                                               
*   USES THE AGENCY2 RECORD                                                     
*                                                                               
         MVI   KEY,X'1A'           SET AGENCY2     KEY TYPE                     
         GOTO1 HIGHDIR2            READ FIRST KEY ON FILE                       
         B     LTAB0120                                                         
LTAB0110 EQU   *                                                                
         GOTO1 SEQDIR2                                                          
LTAB0120 EQU   *                                                                
         CLI   KEY,X'1A'           STILL AGENCY KEY?                            
         BNE   LTAB0130            NO  - AGY FINISHED                           
         CLC   KEY+25(2),=C'UT'    UTS AGENCY CODE?                             
         BNE   LTAB0110            NO  - SKIP IT                                
         GOTO1 GETREC2             YES - RETRIEVE RECORD                        
         LA    R6,REC                                                           
         USING RAGY2REC,R6                                                      
         MVC   0(6,R2),RAGK2AGY    INSERT UTS AGY/OFFC INTO TABLE               
         MVC   6(4,R2),RAGY2EQU    INSERT FSS AGY INTO TABLE                    
         MVC   10(2,R2),RAGY2EQO   INSERT FSS AGY-OFFC INTO TABLE               
         OC    0(LAGYCODE,R2),SPACES                                            
*                                  OR CODE SECTION TO SPACES                    
         DROP  R6                                                               
*                                                                               
         LA    R2,LAGYSLOT(R2)     BUMP TO NEXT SLOT                            
         ST    R2,ANEXTAGY         SAVE A(NEXT SLOT)                            
         L     RF,AGYTABCT         INCREMENT TABLE COUNT                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYTABCT                                                      
         B     LTAB0110            GO BACK FOR NEXT                             
LTAB0130 EQU   *                                                                
         CLI   QUESTOR+2,C'Y'      DISPLAY AGY TABLE?                           
         BNE   LTAB0150            NO                                           
         MVC   P+1(15),=C'TOTAL AGENCIES:'                                      
         EDIT  AGYTABCT,(5,P+20)                                                
         GOTO1 REPORT                                                           
         L     R2,AAGYAREA         YES                                          
LTAB0140 EQU   *                                                                
         CLC   0(6,R2),SPACES      ANY ENTRY IN TABLE?                          
         BNH   LTAB0150            NO  - S/P FINISHED                           
         MVC   P+1(04),=C'AGY:'                                                 
         MVC   P+8(6),0(R2)                                                     
         MVI   P+14,C'/'                                                        
         MVC   P+15(6),6(R2)                                                    
         GOTO1 REPORT                                                           
         LA    R2,LAGYSLOT(R2)     BUMP TO NEXT SLOT                            
         B     LTAB0140            GO BACK FOR NEXT                             
LTAB0150 EQU   *                                                                
**AGENCY                                                                        
**ADVERT                                                                        
         L     R2,AADVAREA         SET A(FIRST SLOT IN ADV TABLE)               
         XC    KEY,KEY                                                          
         MVI   KEY,8               SET ADVERT KEY TYPE                          
         GOTO1 HIGHDIR2            READ FIRST KEY ON FILE                       
         B     LTAB0220                                                         
LTAB0210 EQU   *                                                                
         GOTO1 SEQDIR2                                                          
LTAB0220 EQU   *                                                                
         CLI   KEY,8               SAME KEY THROUGH REP?                        
         BNE   LTAB0230            NO  - ADV FINISHED                           
         CLC   KEY+25(2),=C'UT'    UTS ADVERT CODE?                             
         BNE   LTAB0210            NO  - SKIP                                   
         GOTO1 GETREC2             YES - RETRIEVE RECORD                        
         LA    R6,REC                                                           
         USING RADVREC,R6                                                       
         MVC   0(4,R2),RADVKADV    INSERT UTS ADV INTO TABLE                    
         CLC   RADVLCD,=X'CB01'    PRE AUG1/01?                                 
         BL    LTAB0228            YES - NO CODE                                
         MVC   4(4,R2),RADVKATZ    INSERT FSS ADV INTO TABLE                    
LTAB0228 EQU   *                                                                
         MVC   9(2,R2),RADVLCD     INSERT LAST CHANGED DATE (COMP)              
         OC    0(LADVCODE,R2),SPACES                                            
*                                  OR CODE SECTION TO SPACES                    
         DROP  R6                                                               
*                                                                               
         LA    R2,LADVSLOT(R2)     BUMP TO NEXT SLOT                            
         ST    R2,ANEXTADV         SAVE A(NEXT SLOT)                            
         L     RF,ADVTABCT         INCREMENT TABLE COUNT                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVTABCT                                                      
         B     LTAB0210            GO BACK FOR NEXT                             
LTAB0230 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY ADV TABLE?                           
         BNE   LTAB0250            NO                                           
         MVC   P+1(15),=C'TOTAL ADVERTS :'                                      
         EDIT  ADVTABCT,(5,P+20)                                                
         GOTO1 REPORT                                                           
         L     R2,AADVAREA         YES                                          
LTAB0240 EQU   *                                                                
         CLC   0(4,R2),SPACES      ANY ENTRY IN TABLE?                          
         BNH   LTAB0250            NO  - ADV FINISHED                           
         MVC   P+1(04),=C'ADV:'                                                 
         MVC   P+8(4),0(R2)                                                     
         MVI   P+12,C'/'                                                        
         MVC   P+13(4),4(R2)                                                    
         GOTO1 DATCON,DMCB,(2,9(R2)),(5,P+20)                                   
         GOTO1 REPORT                                                           
         LA    R2,LADVSLOT(R2)     BUMP TO NEXT SLOT                            
         B     LTAB0240            GO BACK FOR NEXT                             
LTAB0250 EQU   *                                                                
**ADVERT                                                                        
**PRODUCT                                                                       
         L     R2,APRDAREA         SET A(FIRST SLOT IN PRD TABLE)               
         XC    KEY,KEY                                                          
         MVI   KEY,9               SET PRODUCT KEY TYPE                         
         GOTO1 HIGHDIR2            READ FIRST KEY ON FILE                       
         B     LTAB0320                                                         
LTAB0310 EQU   *                                                                
         GOTO1 SEQDIR2                                                          
LTAB0320 EQU   *                                                                
         CLI   KEY,9               SAME KEY THROUGH REP?                        
         BNE   LTAB0340            NO  - PRD FINISHED                           
         CLC   KEY+25(2),=C'UT'    UTS PRODUCT CODE?                            
         BNE   LTAB0310            NO  - SKIP                                   
         GOTO1 GETREC2             YES - RETRIEVE RECORD                        
         LA    R6,REC                                                           
         USING RPRDREC,R6                                                       
         MVC   0(7,R2),RPRDKADV    INSERT UTS ADV/PRODUCT INTO TABLE            
*                                                                               
         MVC   7(7,R2),RPRDKADV    SET INITIAL EQUIV TO SAME THING              
*                                                                               
*   FIND THIS ADVERTISER IN ADVERT TABLE                                        
*                                                                               
         MVC   DUB(4),RPRDKADV     SET UP TABLE KEY ARGUMENT                    
*                                                                               
         L     R3,AADVAREA         SET A(ADVERT TABLE)                          
         L     R4,ADVTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R3),(R4),12,4,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                PRODUCT ADVERTISER MUST BE THERE             
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         CLC   4(4,RF),SPACES      ANYTHING IN FSS EQUIV FIELD?                 
         BNH   LTAB0330            NO                                           
         MVC   7(4,R2),4(RF)       YES - INSERT FSS EQUIV INTO TABLE            
LTAB0330 EQU   *                                                                
         LA    RF,PRODTABL         SET A(PRODUCT EQUIV TABLE)                   
LTAB0332 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    LTAB0336            YES - CODE NOT IN TABLE                      
         CLC   11(3,R2),0(RF)      PROD CODE IN PROD  TABLE VS                  
*                                  PROD CODE IN EQUIV TABLE                     
         BE    LTAB0334            FOUND: REPLACE PROD TABLE CODE               
         LA    RF,LPRDEQIV(RF)     NOT FOUND:  BUMP TO NEXT SLOT                
         B     LTAB0332            GO BACK FOR NEXT                             
LTAB0334 EQU   *                                                                
         MVC   11(3,R2),3(RF)      REPLACE PROD CODE WITH EQUIV                 
LTAB0336 EQU   *                                                                
         OC    0(LPRDCODE,R2),SPACES                                            
*                                  OR CODE SECTION TO SPACES                    
         DROP  R6                                                               
*                                                                               
         LA    R2,LPRDSLOT(R2)     BUMP TO NEXT SLOT                            
         ST    R2,ANEXTPRD         SAVE A(NEXT SLOT)                            
         L     RF,PRDTABCT         INCREMENT TABLE COUNT                        
         LA    RF,1(RF)                                                         
         ST    RF,PRDTABCT                                                      
         B     LTAB0310            GO BACK FOR NEXT                             
LTAB0340 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY ADV/PRODUCT TABLE?                   
         BNE   LTAB0360            NO                                           
         MVC   P+1(15),=C'TOTAL PRODUCTS :'                                     
         EDIT  PRDTABCT,(5,P+20)                                                
         GOTO1 REPORT                                                           
         L     R2,APRDAREA         YES                                          
LTAB0350 EQU   *                                                                
         CLC   0(7,R2),SPACES      ANY ENTRY IN TABLE?                          
         BNH   LTAB0360            NO  - PRD FINISHED                           
         MVC   P+1(04),=C'PRD:'                                                 
         MVC   P+8(7),0(R2)                                                     
         MVI   P+15,C'/'                                                        
         MVC   P+16(7),7(R2)                                                    
         GOTO1 REPORT                                                           
         LA    R2,LPRDSLOT(R2)     BUMP TO NEXT SLOT                            
         B     LTAB0350            GO BACK FOR NEXT                             
*                                                                               
*   PRODTABL:  PRODUCT CODE EQUIVALENTS.                                        
*        BYTES  1  -  3  =  UTS PRODUCT CODE                                    
*        BYTES  4  -  6  =  FSS PRODUCT CODE                                    
*                                                                               
PRODTABL EQU   *                                                                
         DC    C'CF CKN'                                                        
LPRDEQIV EQU   *-PRODTABL          L (TABLE ENTRY)                              
         DC    C'FA FAG'                                                        
         DC    C'PP PPC'                                                        
         DC    C'VS VLA'                                                        
         DC    C'VARVR1'                                                        
         DC    C'V8 VJU'                                                        
         DC    C'FF FRO'                                                        
         DC    C'VR VR1'                                                        
         DC    C'PT SWP'                                                        
         DC    X'0000'                                                          
LTAB0360 DS    0H                                                               
**PRODUCT                                                                       
**STATION                                                                       
         GOTO1 FSSSIDE             SWITCH TO THE FSS FILE                       
*                                                                               
         L     R2,ASTAAREA         SET A(FIRST SLOT IN STA TABLE)               
         XC    KEY,KEY                                                          
         MVI   KEY,2               SET SALESPERSON KEY TYPE                     
         MVC   KEY+20(2),=C'FB'    SET FSS INTO KEY                             
         GOTO1 HIGHDIR2            READ FIRST KEY ON FILE                       
         B     LTAB0420                                                         
LTAB0410 EQU   *                                                                
         GOTO1 SEQDIR2                                                          
LTAB0420 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   LTAB0430            NO  - S/P FINISHED                           
         GOTO1 GETREC2             YES - RETRIEVE RECORD                        
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
         MVC   0(5,R2),RSTAKSTA    INSERT FSS STA INTO TABLE                    
         MVC   5(2,R2),RSTAGRUP    INSERT FSS GRP INTO TABLE                    
         OC    0(LSTASLOT,R2),SPACES                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R2,LSTASLOT(R2)     BUMP TO NEXT SLOT                            
         ST    R2,ANEXTSTA         SAVE A(NEXT SLOT)                            
         L     RF,STATABCT         INCREMENT TABLE COUNT                        
         LA    RF,1(RF)                                                         
         ST    RF,STATABCT                                                      
         B     LTAB0410            GO BACK FOR NEXT                             
LTAB0430 EQU   *                                                                
         CLI   QUESTOR+0,C'Y'      DISPLAY STA TABLE?                           
         BNE   LTAB0450            NO                                           
         MVC   P+1(15),=C'TOTAL STATIONS:'                                      
         EDIT  STATABCT,(5,P+20)                                                
         GOTO1 REPORT                                                           
         L     R2,ASTAAREA         YES                                          
LTAB0440 EQU   *                                                                
         CLC   0(5,R2),SPACES      ANY ENTRY IN TABLE?                          
         BNH   LTAB0450            NO  - S/P FINISHED                           
         MVC   P+1(04),=C'STA:'                                                 
         MVC   P+8(5),0(R2)                                                     
         MVI   P+13,C'/'                                                        
         MVC   P+14(2),5(R2)       DISPLAY GROUP/SUBGROUP                       
         GOTO1 REPORT                                                           
         LA    R2,LSTASLOT(R2)     BUMP TO NEXT SLOT                            
         B     LTAB0440            GO BACK FOR NEXT                             
LTAB0450 EQU   *                                                                
**STATION                                                                       
*                                                                               
*   TEST DUMP AFTER TABLE LOADING                                               
***      MVC   P+1(19),=C'TEST DUMP: LTAB0350'                                  
***      GOTO1 REPORT                                                           
***      DC    H'0'                                                             
*   END TEST                                                                    
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
FSSSIDE  NTR1                                                                   
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',REC,0                                              
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
*                                                                               
         MVC   WORK+15(06),=C'FSSNY '                                           
*                                  SET ID TO FSS SIDE                           
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,REC                      
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         LA    R1,REC                                                           
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
FSS0010  EQU    *                                                               
         CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BNE   FSS0015             NO                                           
         MVC   FSSREP,2(R1)        YES - SAVE 2-CHAR REP ID                     
*                                                                               
*   TEST DISPLAY REP                                                            
         MVC   P+1(08),=C'SRC REP:'                                             
         MVC   P+10(2),FSSREP                                                   
         GOTO1 REPORT                                                           
*   TEST DISPLAY REP END                                                        
*                                                                               
         B     FSS0020             BUMP TO NEXT ELEMENT                         
FSS0015  EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   FSS0020             NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    FSS0030             YES                                          
FSS0020  EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   FSS0010             NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
FSS0030  EQU   *                                                                
         MVC   NEWUTL,3(R1)        OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
         L     R1,ADCONLST                                                      
         L     RE,VUTL-ADCONSD(R1)                                              
         MVC   SAVEVUTL,4(RE)      SAVE ORIGINAL SYSTEM NUMBER                  
         MVC   4(1,RE),NEWUTL      INSERT NEW UTL NUMBER                        
*                                                                               
*   TEST DISPLAY UTL                                                            
         MVC   P+1(12),=C'ORIG/NEW UTL'                                         
         MVC   P+16(1),SAVEVUTL                                                 
         MVI   P+17,C'/'                                                        
         MVC   P+18(1),NEWUTL                                                   
         GOTO1 REPORT                                                           
*   TEST DISPLAY REP END                                                        
*                                                                               
*   OPEN REP FILE IF BOTH NOT ON SAME FILE                                      
*                                                                               
         CLC   SAVEVUTL,NEWUTL     SAME UTL NUMBERS?                            
         BE    FSS0210             YES - DON'T OPEN AGAIN                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',FLIST,REC                        
*                                                                               
FSS0210  EQU   *                                                                
         XIT1                                                                   
*                                                                               
*    AT THIS POINT, THE PROGRAMMING HAS SWITCHED TO THE FSS FILE.               
*        THE UPDATE TO THE VARIOUS TABLES MUST BE ACCOMPLISHED.                 
*        AT THAT POINT, THE FILES MAY BE SWITCHED BACK TO THE                   
*        UTS FILE FOR COMPLETION OF THE PROCESSING.                             
*                                                                               
         EJECT                                                                  
HIGHDIR2 NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKY2,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK2                                                         
         SPACE 2                                                                
SEQDIR2  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK2                                                         
         SPACE 3                                                                
GETREC2  LA    R6,GETREC                                                        
         B     LINKFIL2                                                         
         SPACE 2                                                                
LINKFIL2 NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK2                                                         
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK2 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT2                                                           
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT2                                                           
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACEX                                                      
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT2   CR    RB,RB                                                            
         B     EXIT2                                                            
         SPACE 1                                                                
NEXIT2   LTR   RB,RB                                                            
         B     EXIT2                                                            
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACEX NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM9,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE3                                                         
         LA    R4,TRACEKY2                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE5                                                         
         SPACE 1                                                                
DMTRACE3 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE5 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM9,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXIT2                                                            
         SPACE 2                                                                
TRACEDM9 DS    C                                                                
TRACEKY2 DS    CL32                                                             
EXIT2    XIT1                                                                   
         LTORG                                                                  
                                                                                
*                                                                               
*                                                                               
*   VALITABS:  S/P, AGENCY, ADVERTISER CONVERSION TABLES HAVE BEEN              
*        LOADED FROM THE APPORPRIATE RECORDS.                                   
*        THIS ROUTINE VALIDATES THE FSS CODES IN THE TABLES.                    
*                                                                               
VALITABS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*   VALIDATE SALESPERSON CODES ENTERED                                          
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         L     R2,ASALAREA                                                      
VALI0010 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,6               SET SALESPERSON KEY TYPE                     
         MVC   KEY+22(2),NEWREP2   INSERT NEW REP CODE INTO KEY                 
         CLC   0(LSALSLOT,R2),SPACES                                            
*                                  ANYTHING IN SLOT?                            
         BNH   VALI0100            NO  - FINISHED WITH S/P                      
         CLC   3(3,R2),SPACES      ANYTHING IN FSS PORTION?                     
         BNH   VALI0030            NO  - SEE IF UTS IS ON FSS                   
         MVC   KEY+24(3),3(R2)     YES - INSERT FSS CODE INTO KEY               
         GOTO1 HIGHDIR3            READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     COMPARE FOR WHOLE KEY                        
         BE    VALI0020            FOUND - PROCEED                              
*                         1.3.5.7.9.1.3.5.7.9.1                                 
         MVC   P+1(21),=C'VS/P: UTS=XXX FSS=YYY'                                
         MVC   P+30(28),=C'FSS CODE NOT ON FILE - ERROR'                        
         MVC   P+11(3),0(R2)       INSERT UTS CODE                              
         MVC   P+19(3),3(R2)       INSERT FSS CODE                              
         GOTO1 REPORT                                                           
         MVI   ERRORFLG,1                                                       
VALI0020 EQU   *                                                                
         LA    R2,LSALSLOT(R2)     BUMP TO NEXT SLOT                            
         B     VALI0010            GO BACK FOR NEXT                             
VALI0030 EQU   *                                                                
*                                                                               
*   NO FSS CODE SPECIFIED:  SEE IF UTS CODE ALREADY EXISTS ON FSS               
*        FILE.  IF SO, THIS IS AN ERROR.  IF NOT, INDICATE THAT                 
*        UTS S/P SHOULD BE COPIED OVER                                          
*                                                                               
         MVC   KEY+24(3),0(R2)     INSERT UTS CODE INTO KEY                     
         GOTO1 HIGHDIR3            READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     COMPARE FOR WHOLE KEY                        
         BNE   VALI0040            NOT FOUND - MUST BE COPIED OVER              
*                         1.3.5.7.9.1.3.5.7.9.1                                 
         MVC   P+1(21),=C'VS/P: UTS=XXX FSS=   '                                
         MVC   P+30(30),=C'UTS CODE ON FSS FILE - WARNING'                      
         MVC   P+11(3),0(R2)       INSERT UTS CODE                              
         GOTO1 REPORT                                                           
**       MVI   ERRORFLG,1                                                       
         OI    6(R2),X'40'         OLD REP'S S/P ON NEW REP:                    
*                                     SET ERROR FLAG ON                         
         B     VALI0020            GO BACK FOR NEXT CODE                        
VALI0040 EQU   *                                                                
         OI    6(R2),X'80'         OLD REP'S S/P NOT ON NEW REP:                
*                                     SET COPY FLAG ON                          
*                         1.3.5.7.9.1.3.5.7.9.1       .                         
         MVC   P+1(21),=C'VS/P: UTS=XXX FSS=   '                                
         MVC   P+30(35),=C'UTS CODE WILL BE COPIED TO FSS FILE'                 
         MVC   P+11(3),0(R2)       INSERT UTS CODE                              
         MVC   P+70(8),0(R2)       INSERT TABLE ENTRY                           
         GOTO1 REPORT                                                           
         B     VALI0020            GO BACK FOR NEXT CODE                        
***VALIDATE AGENCY                                                              
VALI0100 EQU   *                                                                
*                                                                               
*   VALIDATE AGENCY      CODES ENTERED                                          
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         L     R2,AAGYAREA                                                      
VALI0110 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,10              SET AGENCY      KEY TYPE                     
         MVC   KEY+25(2),NEWREP2   INSERT NEW REP CODE INTO KEY                 
         CLC   0(LAGYSLOT,R2),SPACES                                            
*                                  ANYTHING IN SLOT?                            
         BNH   VALI0200            NO  - FINISHED WITH AGENCY                   
         CLC   6(6,R2),SPACES      ANYTHING IN FSS PORTION?                     
         BNH   VALI0130            NO  - SEE IF UTS IS ON FSS                   
         MVC   KEY+19(6),6(R2)     YES - INSERT FSS CODE INTO KEY               
         GOTO1 HIGHDIR3            READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     COMPARE FOR WHOLE KEY                        
         BE    VALI0120            FOUND - PROCEED                              
*                         1.3.5.7.9.1.3.5.7.9.1.3.5.7.9                         
         MVC   P+1(27),=C'VAGY: UTS=XXXXXX FSS=YYYYYY'                          
         MVC   P+30(28),=C'FSS CODE NOT ON FILE - ERROR'                        
         MVC   P+11(6),0(R2)       INSERT UTS CODE                              
         MVC   P+22(6),6(R2)       INSERT FSS CODE                              
         GOTO1 REPORT                                                           
         MVI   ERRORFLG,1                                                       
         B     VALI0128                                                         
VALI0120 EQU   *                                                                
         CLC   10(2,R2),SPACES     ANY OFFICE IN FSS CODE?                      
         BH    VALI0128            YES -                                        
         MVC   KEYSAVE2,KEY        NO  - DOES IT REQUIRE OFFICE?                
VALI0122 EQU   *                                                                
         GOTO1 SEQDIR3             READ FOR NEXT KEY                            
         CLC   KEYSAVE2(23),KEY    SAME KEY THROUGH AGENCY CODE?                
         BNE   VALI0128            NO  - FINISHED                               
         CLC   KEY+25(2),NEWREP2   CODE FOR NEW REP?                            
         BNE   VALI0122            NO  - GO BACK FOR NEXT                       
*                                                                               
*   NEXT TEST IS REDUNDANT BUT HERE ANYWAY                                      
*                                                                               
         CLC   KEY+23(2),SPACES    YES - AGENCY OFFICE CODE?                    
         BNH   VALI0122            NO  - GO BACK FOR NEXT                       
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3                    
         MVC   P+01(27),=C'VAGY: UTS=XXXXXX FSS=YYYYYY'                         
         MVC   P+30(32),=C'FSS CODE REQUIRES OFFICE - ERROR'                    
         MVC   P+11(6),0(R2)       INSERT UTS CODE                              
         MVC   P+22(6),6(R2)       INSERT FSS CODE                              
         GOTO1 REPORT                                                           
         MVI   ERRORFLG,1                                                       
         B     VALI0128                                                         
VALI0128 EQU   *                                                                
         LA    R2,LAGYSLOT(R2)     BUMP TO NEXT SLOT                            
         B     VALI0110            GO BACK FOR NEXT                             
VALI0130 EQU   *                                                                
*                                                                               
*   NO FSS CODE SPECIFIED:  SEE IF UTS CODE ALREADY EXISTS ON FSS               
*        FILE.  IF SO, THIS IS AN ERROR.  IF NOT, INDICATE THAT                 
*        UTS S/P SHOULD BE COPIED OVER                                          
*                                                                               
         MVC   KEY+19(6),0(R2)     INSERT UTS CODE INTO KEY                     
         MVC   KEY+25(2),NEWREP2   INSERT NEW REP INTO KEY                      
         GOTO1 HIGHDIR3            READ FOR KEY                                 
*                                                                               
*   TEST                                                                        
**       CLC   =C'ACC',0(R2)                                                    
**       BNE   *+6                                                              
**       DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         CLC   KEY(27),KEYSAVE     COMPARE FOR WHOLE KEY                        
         BNE   VALI0140            NOT FOUND -                                  
*                         1.3.5.7.9.1.3.5.7.9.1                                 
         MVC   P+1(27),=C'VAGY: UTS=XXXXXX FSS=      '                          
         MVC   P+30(28),=C'UTS CODE ON FSS FILE - ERROR'                        
         MVC   P+11(6),0(R2)       INSERT UTS CODE                              
         GOTO1 REPORT                                                           
         MVI   ERRORFLG,1                                                       
         OI    12(R2),X'40'        OLD REP'S AGY ON NEW REP:                    
*                                     SET ERROR FLAG ON                         
         B     VALI0128            GO BACK FOR NEXT CODE                        
VALI0140 EQU   *                                                                
         OI    12(R2),X'80'         OLD REP'S AGY NOT ON NEW REP:               
*                                     SET COPY FLAG ON                          
*                         1.3.5.7.9.1.3.5.7.9.1       .                         
**       MVC   P+1(27),=C'VAGY: UTS=XXXXXX FSS=      '                          
**       MVC   P+30(35),=C'UTS CODE WILL BE COPIED TO FSS FILE'                 
**       MVC   P+11(6),0(R2)       INSERT UTS CODE                              
**       MVC   P+70(16),0(R2)      INSERT TABLE ENTRY                           
**       GOTO1 REPORT                                                           
         B     VALI0128            GO BACK FOR NEXT CODE                        
***VALIDATE AGENCY                                                              
***VALIDATE ADVERT                                                              
VALI0200 EQU   *                                                                
*                                                                               
*   VALIDATE ADVERT      CODES ENTERED                                          
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         L     R2,AADVAREA                                                      
VALI0210 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,8               SET ADVERT      KEY TYPE                     
         MVC   KEY+25(2),NEWREP2   INSERT NEW REP CODE INTO KEY                 
         CLC   0(LADVSLOT,R2),SPACES                                            
*                                  ANYTHING IN SLOT?                            
         BNH   VALI0300            NO  - FINISHED WITH ADVERT                   
*                                                                               
*   TEST DISPLAY                                                                
***      MVC   P+1(16),=C'ADVERTISER SLOT='                                     
***      MVC   P+17(LADVSLOT),0(R2)                                             
***      GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         CLC   4(4,R2),SPACES      ANYTHING IN FSS PORTION?                     
         BNH   VALI0230            NO  - SEE IF UTS IS ON FSS                   
         MVC   KEY+21(4),4(R2)     YES - INSERT FSS CODE INTO KEY               
         GOTO1 HIGHDIR3            READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     COMPARE FOR WHOLE KEY                        
         BE    VALI0220            FOUND - PROCEED                              
*                         1.3.5.7.9.1.3.5.7.9.1.3.5.7.9                         
         MVC   P+1(23),=C'VADV: UTS=XXXX FSS=YYYY'                              
         MVC   P+30(28),=C'FSS CODE NOT ON FILE - ERROR'                        
         MVC   P+11(4),0(R2)       INSERT UTS CODE                              
         MVC   P+20(4),4(R2)       INSERT FSS CODE                              
         GOTO1 REPORT                                                           
         MVI   ERRORFLG,1                                                       
VALI0220 EQU   *                                                                
         LA    R2,LADVSLOT(R2)     BUMP TO NEXT SLOT                            
         B     VALI0210            GO BACK FOR NEXT                             
VALI0230 EQU   *                                                                
*                                                                               
*   NO FSS CODE SPECIFIED:  SEE IF UTS CODE ALREADY EXISTS ON FSS               
*        FILE.  IF SO, THIS IS AN ERROR.  IF NOT, INDICATE THAT                 
*        UTS S/P SHOULD BE COPIED OVER                                          
*                                                                               
         MVC   KEY+21(4),0(R2)     INSERT UTS CODE INTO KEY                     
         MVC   KEY+25(2),NEWREP2   INSERT NEW REP INTO KEY                      
         GOTO1 HIGHDIR3            READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     COMPARE FOR WHOLE KEY                        
         BNE   VALI0240            FOUND - PROCEED                              
*                         1.3.5.7.9.1.3.5.7.9.1                                 
         MVC   P+1(23),=C'VADV: UTS=XXXX FSS=    '                              
         MVC   P+30(30),=C'UTS CODE ON FSS FILE - WARNING'                      
         MVC   P+11(4),0(R2)       INSERT UTS CODE                              
         GOTO1 REPORT                                                           
**       MVI   ERRORFLG,1                                                       
         OI    8(R2),X'40'         OLD REP'S ADV ON NEW REP:                    
*                                     SET ERROR FLAG ON                         
         B     VALI0220            GO BACK FOR NEXT CODE                        
VALI0240 EQU   *                                                                
         OI    08(R2),X'80'         OLD REP'S AGY NOT ON NEW REP:               
*                                     SET COPY FLAG ON                          
*                         1.3.5.7.9.1.3.5.7.9.1       .                         
**       MVC   P+1(24),=C'VADV: UTS=XXXX FSS=    '                              
**       MVC   P+30(35),=C'UTS CODE WILL BE COPIED TO FSS FILE'                 
**       MVC   P+11(4),0(R2)       INSERT UTS CODE                              
**       MVC   P+70(12),0(R2)      INSERT TABLE ENTRY                           
**       GOTO1 REPORT                                                           
         B     VALI0220            GO BACK FOR NEXT CODE                        
***VALIDATE ADVERT                                                              
***VALIDATE PRODUCT                                                             
VALI0300 EQU   *                                                                
*                                                                               
*   VALIDATE PRODUCT     CODES ENTERED                                          
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         L     R2,APRDAREA                                                      
VALI0310 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,9               SET PRODUCT     KEY TYPE                     
         MVC   KEY+25(2),NEWREP2   INSERT NEW REP CODE INTO KEY                 
         CLC   0(LPRDSLOT,R2),SPACES                                            
*                                  ANYTHING IN SLOT?                            
         BNH   VALI0400            NO  - FINISHED WITH PRODUCT                  
*                                                                               
*   TEST DISPLAY                                                                
         MVC   P+1(16),=C'PRODUCT    SLOT='                                     
         MVC   P+17(LPRDSLOT),0(R2)                                             
         GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         CLC   7(7,R2),SPACES      ANYTHING IN FSS PORTION?                     
         BNH   VALI0330            NO  - SEE IF UTS IS ON FSS                   
         MVC   KEY+18(7),7(R2)     YES - INSERT FSS CODE INTO KEY               
         GOTO1 HIGHDIR3            READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     COMPARE FOR WHOLE KEY                        
         BE    VALI0320            FOUND - PROCEED                              
*                         1.3.5.7.9.1.3.5.7.9.1.3.5.7.9                         
         MVC   P+1(29),=C'VPRD: UTS=XXXXXXX FSS=YYYYXXX'                        
         MVC   P+34(28),=C'PRODUCT CODE WILL BE COPIED '                        
         MVC   P+11(7),0(R2)       INSERT UTS CODE                              
         MVC   P+23(7),7(R2)       INSERT FSS CODE                              
         GOTO1 REPORT                                                           
         OI    14(R2),X'80'         OLD REP'S PROD NOT ON NEW REP:              
VALI0320 EQU   *                                                                
         LA    R2,LPRDSLOT(R2)     BUMP TO NEXT SLOT                            
         B     VALI0310            GO BACK FOR NEXT                             
VALI0330 EQU   *                                                                
*                                                                               
*   NO FSS CODE SPECIFIED:  SEE IF UTS CODE ALREADY EXISTS ON FSS               
*        FILE.  IF SO, THIS IS AN ERROR.  IF NOT, INDICATE THAT                 
*        UTS S/P SHOULD BE COPIED OVER                                          
*                                                                               
         MVC   KEY+18(7),0(R2)     INSERT UTS CODE INTO KEY                     
         MVC   KEY+25(2),NEWREP2   INSERT NEW REP INTO KEY                      
         GOTO1 HIGHDIR3            READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     COMPARE FOR WHOLE KEY                        
         BNE   VALI0340            FOUND - PROCEED                              
*                         1.3.5.7.9.1.3.5.7.9.1                                 
         MVC   P+1(29),=C'VPRD: UTS=XXXXXXX FSS=       '                        
         MVC   P+34(30),=C'UTS CODE ON FSS FILE - WARNING'                      
         MVC   P+11(7),0(R2)       INSERT UTS CODE                              
         GOTO1 REPORT                                                           
**       MVI   ERRORFLG,1                                                       
         OI    14(R2),X'40'        OLD REP'S ADV ON NEW REP:                    
*                                     SET ERROR FLAG ON                         
         B     VALI0320            GO BACK FOR NEXT CODE                        
VALI0340 EQU   *                                                                
         OI    14(R2),X'80'         OLD REP'S AGY NOT ON NEW REP:               
*                                     SET COPY FLAG ON                          
*                         1.3.5.7.9.1.3.5.7.9.1       .                         
**       MVC   P+1(30),=C'VPRD: UTS=XXXXXXX FSS=       '                        
**       MVC   P+34(35),=C'UTS CODE WILL BE COPIED TO FSS FILE'                 
**       MVC   P+11(7),0(R2)       INSERT UTS CODE                              
**       MVC   P+72(16),0(R2)      INSERT TABLE ENTRY                           
**       GOTO1 REPORT                                                           
         B     VALI0320            GO BACK FOR NEXT CODE                        
***VALIDATE PRODUCT                                                             
***VALIDATE STATION                                                             
VALI0400 EQU   *                                                                
*        STATION RECORDS REQUIRE NO VALIDATION.  CONTRACT DATA                  
*        WILL ENSURE THAT ALL UTS STATIONS ARE PROPERLY REPRESENTED             
*        ON THE FSS FILE.                                                       
***VALIDATE STATION                                                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
HIGHDIR3 NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKY3,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK3                                                         
         SPACE 2                                                                
SEQDIR3  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK3                                                         
         SPACE 3                                                                
GETREC3  LA    R6,GETREC                                                        
         B     LINKFIL3                                                         
         SPACE 2                                                                
LINKFIL3 NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK3                                                         
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK3 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT3                                                           
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT3                                                           
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRAC3X                                                      
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT3   CR    RB,RB                                                            
         B     EXIT3                                                            
         SPACE 1                                                                
NEXIT3   LTR   RB,RB                                                            
         B     EXIT3                                                            
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRAC3X NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM3,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRAC3Y                                                         
         LA    R4,TRACEKY3                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRAC3Z                                                         
         SPACE 1                                                                
DMTRAC3Y GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRAC3Z DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM3,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXIT3                                                            
         SPACE 2                                                                
TRACEDM3 DS    C                                                                
TRACEKY3 DS    CL32                                                             
EXIT3    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*   PROCFOUR:  MAKE IO CODE LOCAL TO SEVERAL ROUTINES                           
*                                                                               
PROCFOUR NMOD1 0,*PROCFOUR*                                                     
*                                                                               
         GOTO1 D41SPROC,DMCB,(RC),(R9)  PROCESS DARE 41 RECS                    
*                                                                               
***      GOTO1 P431PROC,DMCB,(RC)  PROCESS PROPOSAL 4301 RECORDS                
*                                                                               
***      GOTO1 P432PROC,DMCB,(RC)  PROCESS PROPOSAL 4302 RECORDS                
*                                                                               
         GOTO1 CFCSPROC,DMCB,(RC),(R9)  PROCESS CFC 47 RECORDS                  
*                                                                               
         GOTO1 ACTVPROC,DMCB,(RC),(R9)  PROCESS ACTY REPT 4A                    
*                                                                               
         GOTO1 D51SPROC,DMCB,(RC),(R9)  PROCESS DARE 51 RECS                    
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  D41SPROC:  WILL BRING ALL OLDREP     D41S   RECORDS           *              
******************************************************************              
D41SPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'41'                                                    
                                                                                
D41S0020 DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
D41S0040 EQU    *                                                               
         MVC   KEY+07(2),OLDREP                                                 
         GOTO1 HIGHFOUR                                                         
         B     D41S0080                                                         
D41S0060 EQU    *                                                               
         GOTO1 SEQFOUR                                                          
D41S0080 EQU    *                                                               
         CLC   KEY(09),KEYSAVE     COMPARE THROUGH REP                          
         BNE   D41S0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRFOUR                                                         
                                                                                
         LA    R6,REC                                                           
         USING RDARREC,R6                                                       
         MVC   REC-4(2),RDARLEN    INSERT LENGTH FOR PUT                        
         MVC   RDARKREP,NEWREP     NEW REP CODE                                 
         CLI   RDARKRT,X'10'       RECORD TYPE = HEADER?                        
         BNE   D41S0090            NO                                           
*                                                                               
*   NEED TO FIND OUT WHAT TO SET DARE CODE(S) TO                                
*                                                                               
         CLC   =C'UTS',RDARRCVR    CHANGE RECEIVING ID                          
         BNE   D41S0090               IF FOX TELEVISION                         
         MVC   RDARRCVR(3),=C'FSS'    TO FOX STATION SALES                      
*                                                                               
D41S0090 EQU   *                                                                
                                                                                
         L     RF,D41CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,D41CTR                                                        
                                                                                
         BAS   RE,PUTRFOUR                                                      
*                                                                               
         ZICM  RF,RDARLEN,2        ADD ALL DARE LENGTHS                         
         CVD   RF,DUB                                                           
         AP    D41BYTES(8),DUB(8)                                               
*                                                                               
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   D41S0060                                                         
         CLC   D41CTR,=F'25'       DISPLAY FIRST N RECORDS                      
         BH    D41S0060                                                         
*                                                                               
         MVC   P+1(8),=C'DAR REC:'                                              
         MVC   P+10(2),=C'41'                                                   
         MVC   P+20(27),RDARREC                                                 
         MVC   P+60(10),RDARRCVR                                                
         GOTO1 REPORT                                                           
         B     D41S0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
D41S0100 DS     0H                                                              
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  CFCSPROC:  WILL BRING ALL OLDREP     CFCS   RECORDS           *              
******************************************************************              
CFCSPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
****     XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'47'                                                    
                                                                                
CFCS0020 DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
CFCS0040 EQU    *                                                               
         MVC   KEY+21(2),OLDREP                                                 
***      MVC   KEY+23(4),SAVEKEY+23                                             
*                                  INSERT CONTRACT NUMBER                       
         GOTO1 HIGHFOUR                                                         
         B     CFCS0080                                                         
CFCS0060 EQU    *                                                               
         GOTO1 SEQFOUR                                                          
CFCS0080 EQU    *                                                               
         CLC   KEY(23),KEYSAVE     COMPARE THRU REP CODE                        
         BNE   CFCS0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRFOUR                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCFCREC,R6                                                       
         MVC   REC-4(2),RCFCLEN    INSERT LENGTH FOR PUT                        
         MVC   RCFCKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,CFCCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CFCCTR                                                        
                                                                                
         BAS   RE,PUTRFOUR                                                      
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   CFCS0060                                                         
         CLC   CFCCTR,=F'50'                                                    
         BH    CFCS0060                                                         
*                                                                               
         MVC   P+1(8),=C'CFC REC:'                                              
         MVC   P+10(2),=C'47'                                                   
         MVC   P+20(27),RCFCREC                                                 
         GOTO1 REPORT                                                           
***      B     CFCS0060            GO BACK FOR NEXT REQ                         
*                                                                               
*   ONLY ONE RECORD PER ORDER                                                   
*                                                                               
         DROP  R6                                                               
                                                                                
CFCS0100 DS     0H                                                              
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  ACTVPROC:  WILL BRING ALL OLDREP     ACTV   RECORDS           *              
******************************************************************              
ACTVPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'4A'                                                    
                                                                                
ACTV0020 DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
ACTV0040 EQU    *                                                               
         MVC   KEY+17(2),OLDREP                                                 
         GOTO1 HIGHFOUR                                                         
         B     ACTV0080                                                         
ACTV0060 EQU    *                                                               
         GOTO1 SEQFOUR                                                          
ACTV0080 EQU    *                                                               
         CLC   KEY(19),KEYSAVE     COMPARE THROUGH REP                          
         BNE   ACTV0100            NOT RIGHT RECORD OR RIGHT REP                
         CLC   =C'DDSTEST',KEY+19  SKIP THESE RECORDS                           
         BE    ACTV0060                                                         
         CLC   =C'DON',KEY+19                                                   
         BE    ACTV0060                                                         
         CLC   =C'TEST',KEY+19                                                  
         BE    ACTV0060                                                         
                                                                                
         GOTO1 GETRFOUR                                                         
                                                                                
         LA    R6,REC                                                           
         USING RACTREC,R6                                                       
         MVC   REC-4(2),RACTLEN    INSERT LENGTH FOR PUT                        
         MVC   RACTREP,NEWREP      NEW REP CODE                                 
                                                                                
         L     RF,ACTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ACTCTR                                                        
                                                                                
         BAS   RE,PUTRFOUR                                                      
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   ACTV0060                                                         
         MVC   P+1(8),=C'ACT REC:'                                              
         MVC   P+10(2),=C'4A'                                                   
         MVC   P+20(27),RACTREC                                                 
         GOTO1 REPORT                                                           
         B     ACTV0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
ACTV0100 DS     0H                                                              
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  D51SPROC:  WILL BRING ALL OLDREP     D51S   RECORDS           *              
******************************************************************              
D51SPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TEST EXIT                                                                   
***      XIT1                                                                   
*   TEST EXIT END                                                               
*                                                                               
         MVI   KEYTYPE,X'51'                                                    
                                                                                
D51S0020 DS     0H                                                              
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
D51S0040 EQU    *                                                               
         MVC   KEY+07(2),OLDREP                                                 
         GOTO1 HIGHFOUR                                                         
         B     D51S0080                                                         
D51S0060 EQU    *                                                               
         GOTO1 SEQFOUR                                                          
D51S0080 EQU    *                                                               
         CLC   KEY(09),KEYSAVE     COMPARE THROUGH REP                          
         BNE   D51S0100            NOT RIGHT RECORD OR RIGHT REP                
                                                                                
         GOTO1 GETRFOUR                                                         
                                                                                
         LA    R6,REC                                                           
         USING RDARREC,R6                                                       
         MVC   REC-4(2),RDARLEN    INSERT LENGTH FOR PUT                        
         MVC   RDARKREP,NEWREP     NEW REP CODE                                 
         CLI   RDARKRT,X'10'       RECORD TYPE = HEADER?                        
         BNE   D51S0090            NO                                           
*                                                                               
*   NEED TO FIND OUT WHAT TO SET DARE CODE(S) TO                                
*                                                                               
         CLC   =C'UTS',RDARRCVR    CHANGE RECEIVING ID                          
         BNE   D51S0090               IF FOX TELEVISION                         
         MVC   RDARRCVR(3),=C'FSS'    TO FOX STATION SALES                      
*                                                                               
D51S0090 EQU    *                                                               
                                                                                
         L     RF,D51CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,D51CTR                                                        
                                                                                
         BAS   RE,PUTRFOUR                                                      
*                                                                               
         ZICM  RF,RDARLEN,2        ADD ALL DARE LENGTHS                         
         CVD   RF,DUB                                                           
         AP    D51BYTES(8),DUB(8)                                               
*                                                                               
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   D51S0060                                                         
         CLC   D51CTR,=F'25'       DISPLAY FIRST N RECORDS                      
         BH    D51S0060                                                         
*                                                                               
         MVC   P+1(8),=C'DAR REC:'                                              
         MVC   P+10(2),=C'51'                                                   
         MVC   P+20(27),RDARREC                                                 
         MVC   P+60(10),RDARRCVR                                                
         GOTO1 REPORT                                                           
         B     D51S0060            GO BACK FOR NEXT REQ                         
         DROP  R6                                                               
                                                                                
D51S0100 DS     0H                                                              
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  PUTRFOUR:  GENERATE OUTFILE ENTRIES                           *              
******************************************************************              
PUTRFOUR NTR1                                                                   
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
HIGHFOUR NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRKYFOUR,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCKFOUR                                                         
         SPACE 2                                                                
SEQFOUR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCKFOUR                                                         
         SPACE 3                                                                
GETRFOUR LA    R6,GETREC                                                        
         B     LINKFOUR                                                         
         SPACE 2                                                                
LINKFOUR NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCKFOUR                                                         
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCKFOUR TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIFOUR                                                         
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIFOUR                                                         
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRFOUR                                                      
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIFOUR CR    RB,RB                                                            
         B     EXT3FOUR                                                         
         SPACE 1                                                                
NEXIFOUR LTR   RB,RB                                                            
         B     EXT3FOUR                                                         
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRFOUR NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACFOUR,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMT2FOUR                                                         
         LA    R4,TRKYFOUR                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMT3FOUR                                                         
         SPACE 1                                                                
DMT2FOUR GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMT3FOUR DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACFOUR,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXT3FOUR                                                         
         SPACE 2                                                                
TRACFOUR DS    C                                                                
TRKYFOUR DS    CL32                                                             
EXT3FOUR XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.                     *              
******************************************************************              
CONTPROC NMOD1 0,*CONTR*                                                        
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
*                                                                               
CONP0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6          SET RECORD DEFINITION                        
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,OLDREP                                                  
         GOTO1 HIGHCONT                                                         
         B     CONP0060                                                         
                                                                                
CONP0040 DS    0H                                                               
         LA    R6,KEY              RESET A(USING)                               
         GOTO1 SEQCONT                                                          
                                                                                
CONP0060 DS    0H                                                               
         CLC   KEY(04),KEYSAVE     COMPARE CODE/REP                             
         BNE   CONP0380                                                         
*                                                                               
CONP0080 DS    0H                                                               
         MVI   NEEDDEL,0           CLEAR 'NEED DELETE' FLAG                     
         GOTO1 GETRECON                                                         
*                                                                               
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
*                                                                               
*                                                                               
**       MVC   P+1(8),=C'PRE CON:'                                              
***      MVC   P+10(27),RCONREC                                                 
**       GOTO1 HEXOUT,DMCB,RCONKCON,P+40,4,=C'TOG'                              
***      EDIT  CONCTR,(7,P+50)                                                  
**       EDIT  CONCTR2,(7,P+60)                                                 
***      GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUT                        
         MVC   RCONKREP,NEWREP     NEW REP CODE                                 
*                                                                               
*                                                                               
**       MVC   P+1(14),=C'MAJOR CHANGES:'                                       
***      MVC   P+15(27),RCONREC                                                 
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         GOTO1 =A(NEWAGY),DMCB,(RC),(R9),RR=Y                                   
         MVC   OLDADV,RCONKADV     SAVE ORIG ADV CODE                           
         GOTO1 =A(NEWADV),DMCB,(RC),(R9),RR=Y                                   
         GOTO1 =A(NEWSAL),DMCB,(RC),(R9),RR=Y                                   
         GOTO1 =A(NEWGRP),DMCB,(RC),(R9),RR=Y                                   
*                                                                               
*                                                                               
**       MVC   P+1(14),=C'END   CHANGES:'                                       
***      MVC   P+15(27),RCONREC                                                 
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         CLI   RCONTYPE,C' '       ANY CONTRACT TYPE?                           
         BNH   CONP0140                                                         
         LA    RF,CONTABLE         SET A(CONTRACT TYPE TABLE)                   
CONP0100 EQU   *                                                                
         CLC   RCONTYPE,0(RF)      CONTRACT IN TABLE?                           
         BE    CONP0120            YES - REPLACE                                
         LA    RF,LCONTAB(RF)      NO  - BUMP TO NEXT ENTRY                     
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BNE   CONP0100            NO  - GO BACK AND CHECK                      
         DC    H'0'                CODES MUST BE IN TABLE                       
CONP0120 EQU   *                                                                
         MVC   RCONTYPE,1(RF)      REPLACE OLD REP CTYP WITH NEW                
         B     CONP0140                                                         
CONTABLE DC    C'AA'                                                            
LCONTAB  EQU   *-CONTABLE                                                       
         DC    C'DR'                                                            
         DC    C'NN'                                                            
         DC    C'PP'                                                            
         DC    C'SS'                                                            
         DC    C'TT'                                                            
         DC    C'VO'                                                            
         DC    C'XX'                                                            
         DC    X'0000'                                                          
         DS    0H                                                               
CONP0140 EQU   *                                                                
*                                                                               
         GOTO1 =A(CHKPROD),DMCB,(RC),(R9),RR=Y                                  
*                                                                               
*                                                                               
*                                                                               
**       MVC   P+1(14),=C'END  CHKPROD :'                                       
***      MVC   P+15(27),RCONREC                                                 
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
*                                  FIND SENDING INFO ID ELT                     
         LA    R5,RCONELEM                                                      
CONP0160 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    CONP0180            YES - NO SENDING ID ELT                      
         CLI   0(R5),X'20'         SENDING INFO ELT?                            
         BE    CONP0170            YES - RESET TO ZERO                          
         ZIC   RF,1(R5)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R5,RF                                                            
         B     CONP0160            GO BACK FOR NEXT                             
CONP0170 EQU   *                                                                
         TM    4(R5),X'80'         REP LAST SENT (ON STA SIDE)?                 
         BNO   CONP0179            NO  - STA LAST SENT (ON REP SIDE)            
*                                  YES - MUST RESET SEND ID BY TABLE            
         MVC   SENDLOOK(4),RCONKSTA                                             
         MVC   SENDLOOK+4(2),RCONKOFF                                           
         LA    RF,SENDTABL                                                      
CONP0172 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BE    CONP0179            YES - CLEAR FIELD                            
         CLC   SENDLOOK,0(RF)      STATION/OFF IN TABLE?                        
         BE    CONP0175            YES - INSERT NEW VALUE                       
         LA    RF,LSENDTAB(RF)     NO  - BUMP TO NEXT ENTRY                     
         B     CONP0172            GO BACK FOR NEXT                             
CONP0175 EQU   *                                                                
         MVC   2(2,R5),6(RF)       INSERT NEW SENDING ID                        
*                                                                               
         L     RF,SENDCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SENDCTR                                                       
*                                                                               
         CLC   SENDCTR,=F'100'     DISPLAY 1ST N ITEMS                          
         BH    CONP0180                                                         
         MVC   P+1(14),=C'SEND ID CHNGE:'                                       
         MVC   P+15(27),RCONREC                                                 
         MVC   P+45(20),0(R5)                                                   
         GOTO1 REPORT                                                           
         B     CONP0180                                                         
*                                                                               
SENDLOOK DS    CL6                 STATION/OFF COMBO FOR LOOKUP                 
*                                                                               
SENDTABL EQU   *                                                                
*   CHICAGO/FSSCHB  =  ID = 9653/25B5 (DEC/HEX)                                 
         DC    C'KPTVCH',X'25B5'                                                
LSENDTAB EQU   *-SENDTABL          L(SENDTABL ENTRY)                            
         DC    C'KBHKCH',X'25B5'                                                
*   CHICAGO/FSSCHO  =  ID = 9652/25B4 (DEC/HEX)                                 
         DC    C'KCOPCH',X'25B4'                                                
         DC    C'WWORCH',X'25B4'                                                
         DC    C'KUTPCH',X'25B4'                                                
*   NEW YORK/FSSNYB  =  ID = 9649/25B1 (DEC/HEX)                                
         DC    C'KMSPNY',X'25B1'                                                
         DC    C'KPTVNY',X'25B1'                                                
*   NEW YORK/FSSNYG  =  ID = 9651/25B3 (DEC/HEX)                                
         DC    C'WUTBNY',X'25B3'                                                
         DC    C'KUTPNY',X'25B3'                                                
         DC    C'KBHKNY',X'25B3'                                                
*   NEW YORK/FSSNYR  =  ID = 9648/25B0 (DEC/HEX)                                
***      DC    C'****NY',X'25B0'   NO STATIONS FOR NYR                          
*   NEW YORK/FSSNYY  =  ID = 9650/25B2 (DEC/HEX)                                
         DC    C'KCOPNY',X'25B2'                                                
*   DALLAS  /FSSDAM  =  ID = 9654/25B6 (DEC/HEX)                                
         DC    C'WUTBDA',X'25B6'                                                
         DC    C'KBHKDA',X'25B6'                                                
         DC    C'KMSPDA',X'25B6'                                                
         DC    C'WWORDA',X'25B6'                                                
         DC    C'KPTVDA',X'25B6'                                                
*   DALLAS  /FSSDAP  =  ID = 9655/25B7 (DEC/HEX)                                
         DC    C'KCOPDA',X'25B7'                                                
*   LOS ANGELES/FSSLAE  =  ID = 9657/25B9 (DEC/HEX)                             
         DC    C'KUTPLA',X'25B9'                                                
*   LOS ANGELES/FSSLAS  =  ID = 9656/25B8 (DEC/HEX)                             
         DC    C'WUTBLA',X'25B8'                                                
         DC    C'KCOPLA',X'25B8'                                                
         DC    C'WWORLA',X'25B8'                                                
*   LOS ANGELES/FSSLAT  =  ID = 9842/2672 (DEC/HEX)                             
         DC    C'KMSPLA',X'2672'                                                
         DC    C'KPTVLA',X'2672'                                                
         DC    C'KBHKLA',X'2672'                                                
         DC    X'0000'                                                          
         DS    0H                                                               
CONP0179 EQU   *                                                                
         XC    2(2,R5),2(R5)       ON REP SIDE: SET SEND ID TO ZERO             
         L     RF,SENDCTR2                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SENDCTR2                                                      
*                                                                               
         CLC   SENDCTR2,=F'100'    DISPLAY 1ST N ITEMS                          
         BH    CONP0180                                                         
         MVC   P+1(14),=C'SEND ID NOCHG:'                                       
         MVC   P+15(27),RCONREC                                                 
         MVC   P+45(20),0(R5)                                                   
         GOTO1 REPORT                                                           
CONP0180 EQU   *                                                                
*                                                                               
*                                                                               
**       MVC   P+1(14),=C'END  SENDINFO:'                                       
***      MVC   P+15(27),RCONREC                                                 
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         LA    R5,RCONELEM                                                      
CONP0200 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    CONP0260            YES - FINISHED                               
         CLI   0(R5),X'02'         COMMENT ELEMENT?                             
         BE    CONP0220            YES - RESET TO ZERO                          
         BH    CONP0260            PAST 02'S - FINISHED                         
CONP0210 EQU   *                                                                
         ZIC   RF,1(R5)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R5,RF                                                            
         B     CONP0200            GO BACK FOR NEXT                             
CONP0220 EQU   *                                                                
         CLC   2(2,R5),=C'C='        STANDARD COMMENT?                          
         BE    CONP0240            YES - DISPLAY                                
         CLC   2(3,R5),=C'SC='     NO  - OFFICE COMMENT?                        
         BNE   CONP0210            NO  - GO BACK FOR NEXT                       
CONP0240 EQU   *                                                                
         MVI   NEEDDEL,1           SET 'NEED DELETE' FLAG                       
         L     RF,CMMCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CMMCTR                                                        
         L     RF,CMMCTR2          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CMMCTR2                                                       
         CLC   CMMCTR2,=F'0050'    DISPLAY EVERY N RECORDS                      
         BNE   CONP0250                                                         
         XC    CMMCTR2,CMMCTR2                                                  
         MVC   P+1(21),=C'COMMENTS FOUND:      '                                
         EDIT  CMMCTR,(7,P+24)                                                  
         MVC   P+32(27),RCONREC                                                 
         GOTO1 REPORT                                                           
CONP0250 EQU   *                                                                
         MVI   0(R5),X'FF'         SET ELEMENT TO DELETE                        
         B     CONP0210            GO BACK FOR NEXT                             
CONP0260 EQU   *                                                                
*                                                                               
*                                                                               
***      MVC   P+1(14),=C'9F    CHANGES:'                                       
**       MVC   P+15(27),RCONREC                                                 
***      GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         LA    R5,RCONELEM                                                      
CONP0280 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    CONP0320            YES - FINISHED                               
         CLI   0(R5),X'9F'         EXTRA DESCRIPTION ELEMENT?                   
         BE    CONP0300            YES - CLEAR STATION ADV/AGY CODES            
***      BH    CONP0320            PAST 9F'S - FINISHED                         
         ZIC   RF,1(R5)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R5,RF                                                            
         B     CONP0280            GO BACK FOR NEXT                             
CONP0300 EQU   *                                                                
         XC    RCONXADV-RCONXXEL(20,R5),RCONXADV-RCONXXEL(R5)                   
*                                  CLEAR OUT BOTH CODES                         
CONP0320 EQU   *                                                                
*                                                                               
*                                                                               
**       MVC   P+1(14),=C'END 9F CHANGE:'                                       
***      MVC   P+15(27),RCONREC                                                 
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         CLI   NEEDDEL,0           NEED DELETE?                                 
         BE    CONP0330            NO                                           
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0            
CONP0330 EQU   *                                                                
         LA    R6,REC              RESET A(RCONREC)                             
*                                                                               
         BAS   RE,PUTRECON                                                      
                                                                                
         L     RF,CONCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         L     RF,CONCTR2          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR2                                                       
         CLC   CONCTR2,=F'0250'    DISPLAY EVERY N RECORDS                      
         BNE   CONP0340                                                         
         XC    CONCTR2,CONCTR2                                                  
         MVC   P+1(21),=C'PROCESSING CONTRACTS:'                                
         EDIT  CONCTR,(7,P+24)                                                  
         MVC   P+35(27),KEY                                                     
         GOTO1 REPORT                                                           
CONP0340 EQU   *                                                                
*                                                                               
*  TEST                                                                         
***      CLC   CONCTR,=F'500'      PROCESS FIRST N RECORDS                      
***      BH    CONP0380                                                         
*  TEST END                                                                     
*                                                                               
                                                                                
         ZICM  RF,RCONLEN,2        ADD ALL CONTRACT LENGTHS                     
         CVD   RF,DUB                                                           
         AP    CONBYTES(8),DUB(8)                                               
                                                                                
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONP0360                                                         
         CLC   CONCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    CONP0360                                                         
         MVC   P+1(8),=C'CON REC:'                                              
         MVC   P+10(27),RCONREC                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+40,4,=C'TOG'                              
         EDIT  CONCTR,(7,P+50)                                                  
         EDIT  CONCTR2,(7,P+60)                                                 
         GOTO1 REPORT                                                           
                                                                                
CONP0360 DS    0H                                                               
*                                                                               
***      BAS   RE,BUYPROC          PROCESS BUY RECORDS                          
***      BAS   RE,AVALPROC         PROCESS AVAIL RECORDS                        
***      BAS   RE,PROPPROC         PROCESS PROPOSAL RECORDS                     
*                                     KEY SET BY BUYPROC IN REVCON              
***      BAS   RE,P431PROC         PROCESS PROPOSAL 4301 RECORDS                
*                                     KEY SET BY BUYPROC IN NINECON             
***      BAS   RE,P432PROC         PROCESS PROPOSAL 4302 RECORDS                
*                                     KEY SET BY BUYPROC IN NINECON             
***      BAS   RE,CFCSPROC         PROCESS CFC RECORDS                          
*                                                                               
         MVC   KEY,SAVEKEY         CONTINUE WITH NEXT CONTRACT REC              
*                                                                               
         OI    DMINBTS,X'08'       GET DELETED KEY ALSO                         
         GOTO1 HIGHCONT            RESTABLISH SEQ ORDER                         
                                                                                
         B     CONP0040                                                         
                                                                                
CONP0380 DS    0H                                                               
         XIT1                                                                   
SCONKSTA DS    CL5                                                              
SCONKGRP DS    CL2                                                              
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  PUTRECON: GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
PUTRECON NTR1                                                                   
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
HIGHCONT NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACKYCN,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHKCON                                                         
         SPACE 2                                                                
SEQCONT  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHKCON                                                         
         SPACE 3                                                                
GETRECON LA    R6,GETREC                                                        
         B     LINKCONT                                                         
         SPACE 2                                                                
LINKCONT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHKCON                                                         
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHKCON TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXITCON                                                         
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXITCON                                                         
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACON                                                      
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXITCON CR    RB,RB                                                            
         B     EXITCON                                                          
         SPACE 1                                                                
NEXITCON LTR   RB,RB                                                            
         B     EXITCON                                                          
EXITCON  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACON NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACECN8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACN2                                                         
         LA    R4,TRACKYCN                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACN4                                                         
         SPACE 1                                                                
DMTRACN2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACN4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACECN8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXITCON                                                          
         SPACE 2                                                                
TRACECN8 DS    C                                                                
TRACKYCN DS    CL32                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NEWAGY:  LOCATE ORDER'S AGENCY IN TABLE, INSERT EQUIVALENCY IF              
*        NEEDED.                                                                
*                                                                               
NEWAGY   NMOD1 0,*NEWAGY*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
*                                                                               
**       MVC   P+1(09),=C'CONTRACT='                                            
**       MVC   P+10(27),RCONREC                                                 
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
*** BINARY SEARCH CODE: AGENCY FROM CONTRACT RECORD                             
*                                                                               
         MVC   DUB(6),RCONKAGY     SET UP TABLE KEY ARGUMENT                    
*                                                                               
         L     R2,AAGYAREA         SET A(AGENCY TABLE)                          
         L     R4,AGYTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,6,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                NOW MUST BE IN TABLE                         
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         TM    12(RF),X'80'        THIS AGENCY COPIED TO NEW REP FILE?          
         BO    NAGY0120            YES - NO NEED TO CONVERT CODE                
         TM    12(RF),X'40'        THIS AGENCY NOT EQUIV'D, BUT                 
*                                     IS ALREADY ON NEW REP?                    
         BNO   NAGY0020            NO                                           
         OI    ERRORFLG,X'80'      YES - SET CONTRACT/AGY ERROR                 
         B     NAGY0120                                                         
NAGY0020 EQU   *                                                                
         MVC   RCONKAGY(6),6(RF)   OLD AGY -> NEW AGY                           
NAGY0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NEWADV:  LOCATE ORDER'S ADVERT IN TABLE, INSERT EQUIVALENCY IF              
*        NEEDED.                                                                
*                                                                               
NEWADV   NMOD1 0,*NEWADV*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
*** BINARY SEARCH CODE: ADVERT FROM CONTRACT RECORD                             
*                                                                               
         MVC   DUB(4),RCONKADV     SET UP TABLE KEY ARGUMENT                    
*                                                                               
         L     R2,AADVAREA         SET A(ADVERT TABLE)                          
         L     R4,ADVTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),12,4,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                NOW MUST BE IN TABLE                         
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         TM    08(RF),X'80'        THIS ADVERT COPIED TO NEW REP FILE?          
         BO    NADV0120            YES - NO NEED TO CONVERT CODE                
         TM    08(RF),X'40'        THIS ADVERT NOT EQUIV'D, BUT                 
*                                     IS ALREADY ON NEW REP?                    
         BNO   NADV0020            NO                                           
         OI    ERRORFLG,X'40'      YES - SET CONTRACT/ADV ERROR                 
         B     NADV0120                                                         
NADV0020 EQU   *                                                                
         MVC   RCONKADV(4),4(RF)   OLD ADV -> NEW ADV                           
*                                                                               
NADV0120 EQU   *                                                                
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NEWSAL:  LOCATE ORDER'S S/P    IN TABLE, INSERT EQUIVALENCY IF              
*        NEEDED.                                                                
*                                                                               
NEWSAL   NMOD1 0,*NEWSAL*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
*                                                                               
*** BINARY SEARCH CODE: S/P    FROM CONTRACT RECORD                             
*                                                                               
         MVC   DUB(3),RCONSAL      SET UP TABLE KEY ARGUMENT                    
*                                                                               
         L     R2,ASALAREA         SET A(S/P    TABLE)                          
         L     R4,SALTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),08,3,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                NOW MUST BE IN TABLE                         
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         TM    06(RF),X'80'        THIS S/P    COPIED TO NEW REP FILE?          
         BO    NSAL0120            YES - NO NEED TO CONVERT CODE                
         TM    06(RF),X'40'        THIS S/P    NOT EQUIV'D, BUT                 
*                                     IS ALREADY ON NEW REP?                    
         BNO   NSAL0020            NO                                           
         OI    ERRORFLG,X'40'      YES - SET CONTRACT/S/P ERROR                 
         B     NSAL0120                                                         
NSAL0020 EQU   *                                                                
         MVC   RCONSAL(3),3(RF)    OLD S/P -> NEW S/P                           
*                                                                               
NSAL0120 EQU   *                                                                
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NEWGRP:  LOCATE ORDER'S STATION IN TABLE, INSERT GROUP/SUBGROUP             
*        NEEDED.                                                                
*                                                                               
NEWGRP   NMOD1 0,*NEWGRP*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
*** BINARY SEARCH CODE: S/P    FROM CONTRACT RECORD                             
*                                                                               
         MVC   DUB(5),RCONKSTA     SET UP TABLE KEY ARGUMENT                    
*                                                                               
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STATABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,5,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   NGRP0020            NO                                           
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         MVC   RCONKGRP(2),5(RF)   INSERT NEW GROUP/SUBGROUP                    
         B     NGRP0040                                                         
NGRP0020 EQU   *                                                                
         MVC   RCONKGRP(2),=C'T '  INSERT DUMMY                                 
**       TM    ERRORFLG,X'10'      GROUP ERROR PREVIOUSLY ENCOUNTERED?          
**       BO    NGRP0040            YES                                          
         CLC   STATMISS,RCONKSTA   SAME STATION?                                
         BE    NGRP0040            YES                                          
         MVC   P+1(28),=C'STATION NOT ON FSS FILE:    '                         
         MVC   P+20(5),RCONKSTA                                                 
         GOTO1 REPORT                                                           
         MVC   STATMISS,RCONKSTA                                                
         OI    ERRORFLG,X'10'      SET FLAG ON                                  
NGRP0040 EQU   *                                                                
*                                                                               
         DROP  R6                                                               
         XIT1                                                                   
STATMISS DS    CL5                                                              
         DS    0H                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHKPROD:  LOOK FOR PRODUCT CODE USAGE                                       
*                                                                               
CHKPROD  NMOD1 0,*CHKPRD*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R9,4(R1)            RESET A(WORKSPACE)                           
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE USED?                       
         BNH   CHKP0120            NO  - EXIT                                   
         MVC   P+1(20),=C'PRODUCT CODE IN USE:'                                 
         MVC   P+22(4),OLDADV      USE ORIGINAL ADVERTISER                      
         MVI   P+26,C'/'                                                        
         MVC   P+27(3),RCONPRD                                                  
         MVC   P+36(27),RCONREC    SHOW CONTRACT KEY                            
         GOTO1 REPORT                                                           
         MVC   DUB(4),OLDADV       INSERT ORIGINAL ADVERTISER                   
         MVC   DUB+4(3),RCONPRD                                                 
*                                                                               
         L     R2,APRDAREA         SET A(PRODUCT TABLE)                         
         L     R4,PRDTABCT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,7,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    CHKP0020            YES                                          
*                          1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9              
         MVC   P+01(38),=C'UTS PRD "XXXXXXX" NOT IN TABLE:  ERROR'              
         MVC   P+10(7),DUB                                                      
         MVI   ERRORFLG,1          SET ERROR FLAG ON                            
         GOTO1 REPORT                                                           
         B     CHKP0120                                                         
CHKP0020 EQU   *                                                                
         L     RF,DMCB             SET A(TABLE ENTRY FOUND)                     
         MVC   RCONPRD,11(RF)      INSERT CONVERT PRODUCT CODE                  
         MVC   P+1(20),=C'PRODUCT CODE CHANGED'                                 
         MVC   P+22(4),RCONKADV                                                 
         MVI   P+26,C'/'                                                        
         MVC   P+27(3),RCONPRD                                                  
         MVC   P+36(27),RCONREC    SHOW CONTRACT KEY                            
         GOTO1 REPORT                                                           
CHKP0120 EQU   *                                                                
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051REREPST0B505/01/02'                                      
         END                                                                    
