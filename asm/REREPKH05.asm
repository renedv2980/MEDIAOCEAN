*          DATA SET REREPKH05  AT LEVEL 028 AS OF 05/01/02                      
*PHASE REKH02F,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPKH05 (REKH02F) - PULL F1/F2/F3 OFFICES/STATIONS'           
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPKH02 --                                              *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* MAR12/97 (BU ) --- ORIGINAL ENTRY                                *            
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
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REKH02   CSECT                                                                  
         NMOD1 0,**RES2**,R9,RR=R5                                              
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
         GOTO1 REPRPROC,DMCB,(RC)  PROCESS REP RECORD                           
*                                                                               
         GOTO1 GRUPPROC,DMCB,(RC)  PROCESS GROUP RECORDS                        
*                                                                               
*        GOTO1 TEAMPROC,DMCB,(RC)  PROCESS TEAM  RECORDS                        
*                                                                               
         GOTO1 STATPROC,DMCB,(RC)  PROCESS STATION RECORDS                      
*                                                                               
         GOTO1 OFFCPROC,DMCB,(RC)  PROCESS OFFICE RECORDS                       
*                                                                               
*        GOTO1 AGYPROC,DMCB,(RC)   PROCESS AGENCY RECORDS                       
*                                                                               
*        GOTO1 CONTPROC,DMCB,(RC)  PROCESS CONTRACT RECORDS                     
*                                                                               
*        GOTO1 BUYPROC,DMCB,(RC)   PROCESS BUY RECORDS                          
*                                                                               
*        GOTO1 ADVPROC,DMCB,(RC)   PROCESS ADVERTISER RECORDS                   
*                                                                               
*        GOTO1 SALPROC,DMCB,(RC)   PROCESS SALESPERSON RECORDS                  
*                                                                               
*        GOTO1 PRDPROC,DMCB,(RC)   PROCESS PRODUCT RECORDS                      
*                                                                               
*        GOTO1 CLSPROC,DMCB,(RC)   PROCESS CLASS RECORDS                        
*                                                                               
*        GOTO1 CATPROC,DMCB,(RC)   PROCESS CATEGORY RECORDS                     
*                                                                               
*        GOTO1 TYPEPROC,DMCB,(RC)  PROCESS TYPE RECORDS                         
*                                                                               
         GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         B     EXIT                EXIT                                         
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
         GOTO1 (RF),DMCB,C'GET',1800,4000                                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVI   RCSUBPRG,2          SET HEADING FLAG                             
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   AGRPAREA,P2         A(GROUP DIFF TABLE)                          
         MVC   ANEXTGRP,P2         A(NEXT GROUP DIFF SLOT)                      
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         LA    RF,200(RF)                                                       
         ST    RF,ASALAREA         ESTABLISH SALESPERSON AREA                   
         ST    RF,ANEXTSAL         A(NEXT S/P SLOT)                             
         A     RF,=F'1200'         LEAVE ROOM FOR 200 ENTRIES                   
*                                     6 CHARS * 200 SLOTS                       
         ST    RF,ACOMAREA         A(COMMISSION/BUDGET TABLE)                   
         ST    RF,ANEXTCOM         A(NEXT COMM/BUDGET SLOT)                     
         A     RF,=F'500'          LEAVE ROOM FOR 70 ENTRIES                    
*                                     7 CHARS * 70 SLOTS + SPARE                
         ST    RF,ASTNAREA         A(STATION TABLE)                             
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
                                                                                
         XCEF  SALBUF,450          CLEAR SAL BUFFER                             
         XCEF  PRDBUF,700          CLEAR PRD BUFFER                             
         XCEF  ADVBUF,2000         CLEAR ADV BUFFER                             
         XC    CONCTR,CONCTR       CLEAR COUNTERS                               
         XC    BUYCTR,BUYCTR                                                    
         XC    OTHERCTR,OTHERCTR                                                
         XC    ADVCTR,ADVCTR                                                    
         XC    SALCTR,SALCTR                                                    
         XC    PRDCTR,PRDCTR                                                    
         XC    CONBYTES,CONBYTES                                                
         XC    BUYBYTES,BUYBYTES                                                
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
REPTABLE DC    C'F1'                                                            
         DC    C'F2'                                                            
         DC    C'F3'                                                            
         DC    X'FFFF'                                                          
******************************************************************              
*  REPRPROC: EXTRACT OLD REP RECORD                              *              
******************************************************************              
REPRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R1,REPTABLE                                                      
REPR0010 EQU   *                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RREPKEY,R6          SET RECORD DEFINITION                        
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,0(R1)                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BNE   REPR20                                                           
                                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RREPREC,R6                                                       
         MVC   REC-4(2),RREPLEN    INSERT LENGTH FOR PUT                        
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR,C'Y'                                                     
         BNE   REPR20                                                           
         MVC   P+1(8),=C'REP REC:'                                              
         MVC   P+10(27),RREPKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
REPR20   DS    0H                                                               
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BNE   REPR0010            NO  - GO BACK FOR NEXT                       
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  GRUPPROC: EXTRACT OLDREP GROUP   RECS                         *              
******************************************************************              
GRUPPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R1,REPTABLE                                                      
GRUP10   DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RGRPKEY,R6          SET RECORD DEFINITION                        
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,0(R1)                                                   
                                                                                
         GOTO1 HIGHDIR                                                          
         B     GRUP15                                                           
GRUP12   EQU   *                                                                
         GOTO1 SEQDIR                                                           
GRUP15   EQU   *                                                                
         CLC   KEY(25),KEYSAVE     SAME RECORD TYPE/REP?                        
         BNE   GRUP20              NO  - FINISHED                               
                                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RGRPREC,R6                                                       
         MVC   REC-4(2),RGRPLEN    INSERT LENGTH FOR PUT                        
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR,C'Y'                                                     
         BNE   GRUP20                                                           
         MVC   P+1(8),=C'GRP REC:'                                              
         MVC   P+10(27),RGRPKEY                                                 
         GOTO1 REPORT                                                           
         B     GRUP12              GO BACK FOR NEXT GROUP                       
         DROP  R6                                                               
                                                                                
GRUP20   DS    0H                                                               
         LA    R1,2(R1)            BUMP TO NEXT REP                             
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BNE   GRUP10              NO  - GO BACK FOR NEXT REP                   
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  TEAMPROC: EXTRACT OLDREP TEAM    RECS                         *              
******************************************************************              
TEAMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
TEAM10   DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RGRPKEY,R6          SET RECORD DEFINITION                        
         MVI   RGRPKTYP,X'05'                                                   
         MVC   RGRPKREP,OLDREP                                                  
                                                                                
         GOTO1 HIGHDIR                                                          
         B     TEAM15                                                           
TEAM12   EQU   *                                                                
         GOTO1 SEQDIR                                                           
TEAM15   EQU   *                                                                
         CLC   KEY(25),KEYSAVE     SAME RECORD TYPE/REP?                        
         BNE   TEAM20              NO  - FINISHED                               
                                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RTEMREC,R6                                                       
         MVC   REC-4(2),RTEMLEN    INSERT LENGTH FOR PUT                        
         MVC   RTEMKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR,C'Y'                                                     
         BNE   TEAM20                                                           
         MVC   P+1(8),=C'TEM REC:'                                              
         MVC   P+10(27),RTEMKEY                                                 
         GOTO1 REPORT                                                           
         B     TEAM12              GO BACK FOR NEXT TEAM                        
         DROP  R6                                                               
                                                                                
TEAM20   DS    0H                                                               
                                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  STATPROC: EXTRACT OLDREP STATION RECS                         *              
******************************************************************              
STATPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R1,REPTABLE                                                      
STAT10   DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAKEY,R6          SET RECORD DEFINITION                        
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,0(R1)                                                   
                                                                                
         GOTO1 HIGHDIR                                                          
         B     STAT15                                                           
STAT12   EQU   *                                                                
         GOTO1 SEQDIR                                                           
STAT15   EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME RECORD TYPE/REP?                        
         BNE   STAT20              NO  - FINISHED                               
                                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR,C'Y'                                                     
         BNE   STAT20                                                           
         MVC   P+1(8),=C'STA REC:'                                              
         MVC   P+10(27),RSTAKEY                                                 
         GOTO1 REPORT                                                           
         B     STAT12              GO BACK FOR NEXT STATION                     
                                                                                
         DROP  R6                                                               
STAT20   DS    0H                                                               
         LA    R1,2(R1)            BUMP TO NEXT REP                             
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BNE   STAT10              NO  - GO BACK FOR NEXT                       
                                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  OFFCPROC:  COPY ALL OFFICE RECORDS                            *              
******************************************************************              
OFFCPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R1,REPTABLE                                                      
OFF0005  EQU   *                                                                
         MVI   KEYTYPE,X'04'                                                    
OFF10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
         MVC   KEY+23(2),0(R1)                                                  
                                                                                
         GOTO1 HIGHDIR                                                          
         B     OFF15                                                            
OFF12    EQU   *                                                                
         GOTO1 SEQDIR                                                           
OFF15    EQU   *                                                                
         CLC   KEY(25),KEYSAVE     SAME RECORD TYPE/REP?                        
         BNE   OFF20               NO  - CHECK FOR SECONDARY RECS               
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING ROFFREC,R6                                                       
         MVC   REC-4(2),ROFFLEN    INSERT LENGTH FOR PUT                        
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   OFF20                                                            
         MVC   P+1(8),=C'XX  OFF:'                                              
         MVC   P+1(3),=C'44 '                                                   
         CLI   KEYTYPE,X'44'                                                    
         BE    OFF18                                                            
         MVC   P+1(3),=C'04 '                                                   
OFF18    EQU   *                                                                
         MVC   P+10(27),ROFFKEY                                                 
         GOTO1 REPORT                                                           
         B     OFF12               GO BACK FOR NEXT                             
                                                                                
         DROP  R6                                                               
OFF20    DS    0H                                                               
         CLI   KEYTYPE,X'44'                                                    
         BE    OFF30                                                            
         MVI   KEYTYPE,X'44'                                                    
         B     OFF10                                                            
                                                                                
OFF30    DS    0H                                                               
         LA    R1,2(R1)            BUMP TO NEXT REP                             
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BNE   OFF0005             NO  - GO BACK FOR NEXT REP                   
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  AGYPROC: EXTRACT AGENCY RECORDS                               *              
******************************************************************              
AGYPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   KEYTYPE,X'0A'                                                    
AGY10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
                                                                                
         GOTO1 HIGHDIR                                                          
         B     AGY15                                                            
AGY12    EQU   *                                                                
         GOTO1 SEQDIR                                                           
AGY15    EQU   *                                                                
         CLC   KEY(19),KEYSAVE     SAME RECORD TYPE?                            
         BNE   AGY20                                                            
         CLC   KEY+25(2),OLDREP    SAME REP?                                    
         BNE   AGY12               NO  - GO BACK FOR NEXT RECORD                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RAGYREC,R6                                                       
         MVC   REC-4(2),RAGYLEN    INSERT LENGTH FOR PUT                        
         MVC   RAGYKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,AGYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+2,C'Y'                                                   
         BNE   AGY20                                                            
         MVC   P+1(8),=C'XX  AGY:'                                              
         MVC   P+1(2),=C'0A'                                                    
         CLI   KEYTYPE,X'0A'                                                    
         BE    AGY18                                                            
         MVC   P+1(2),=C'1A'                                                    
AGY18    EQU   *                                                                
         MVC   P+10(27),RAGYKEY                                                 
         GOTO1 REPORT                                                           
         B     AGY12               GO BACK FOR NEXT                             
                                                                                
         DROP  R6                                                               
AGY20    DS    0H                                                               
         CLI   KEYTYPE,X'1A'                                                    
         BE    AGY30                                                            
         MVI   KEYTYPE,X'1A'                                                    
         B     AGY10                                                            
                                                                                
AGY30    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  ADVPROC: EXTRACT ADVERTISER RECORDS                           *              
******************************************************************              
ADVPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
ADV10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     ADV15                                                            
ADV12    EQU   *                                                                
         GOTO1 SEQDIR                                                           
ADV15    EQU   *                                                                
         CLC   KEY(21),KEYSAVE     SAME RECORD TYPE?                            
         BNE   ADV20               NO                                           
         CLC   KEY+25(2),OLDREP    SAME REP CODE?                               
         BNE   ADV12               NO  - GO BACK FOR NEXT RECORD                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RADVREC,R6                                                       
         MVC   REC-4(2),RADVLEN    INSERT LENGTH FOR PUT                        
         MVC   RADVKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,ADVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+3,C'Y'                                                   
         BNE   ADV20                                                            
         MVC   P+1(8),=C'ADV REC:'                                              
         MVC   P+10(27),RADVKEY                                                 
         GOTO1 REPORT                                                           
         B     ADV12               GO BACK FOR NEXT RECORD                      
                                                                                
         DROP  R6                                                               
ADV20    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  SALPROC: EXTRACT OLDREP SALESPERSON RECORDS                   *              
******************************************************************              
SALPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
SAL10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,OLDREP                                                  
                                                                                
         GOTO1 HIGHDIR                                                          
         B     SAL15                                                            
SAL12    EQU   *                                                                
         GOTO1 SEQDIR                                                           
SAL15    EQU   *                                                                
         CLC   KEY(24),KEYSAVE     SAME RECORD TYPE/REP?                        
         BNE   SAL20                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RSALREC,R6                                                       
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     NEW REP CODE                                 
***>>>   MVC   RSALTEAM,=C'T '     DEFAULT TEAM CODE                            
                                                                                
         L     RF,SALCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SALCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+4,C'Y'                                                   
         BNE   SAL20                                                            
         MVC   P+1(8),=C'SAL REC:'                                              
         MVC   P+10(27),RSALKEY                                                 
         GOTO1 REPORT                                                           
         B     SAL12                                                            
         DROP  R6                                                               
SAL20    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  PRDPROC: EXTRACT PRODUCT RECORDS                              *              
******************************************************************              
PRDPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
PRD10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     PRD15                                                            
PRD12    EQU   *                                                                
         GOTO1 SEQDIR                                                           
PRD15    EQU   *                                                                
         CLC   KEY(18),KEYSAVE     SAME RECORD TYPE?                            
         BNE   PRD20               NO  - FINISHED                               
         CLC   KEY+25(2),OLDREP    SAME REP?                                    
         BNE   PRD12               NO  - GO BACK FOR NEXT                       
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RPRDREC,R6                                                       
         MVC   REC-4(2),RPRDLEN    INSERT LENGTH FOR PUT                        
         MVC   RPRDKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,PRDCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PRDCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+5,C'Y'                                                   
         BNE   PRD20                                                            
         MVC   P+1(8),=C'PRD REC:'                                              
         MVC   P+10(27),RPRDKEY                                                 
         GOTO1 REPORT                                                           
         B     PRD12               GO BACK FOR NEXT RECORD                      
         DROP  R6                                                               
PRD20    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.                     *              
******************************************************************              
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
CONT10   DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6          SET RECORD DEFINITION                        
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         OI    DMINBTS,X'08'       GET DELETED KEY ALSO                         
         GOTO1 HIGHDIR                                                          
         B     CONT20                                                           
                                                                                
CONT15   DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED KEY ALSO                         
         GOTO1 SEQDIR                                                           
                                                                                
CONT20   DS    0H                                                               
         CLC   KEY(04),KEYSAVE     SAME RECORD TYPE/REP?                        
         BNE   CONTX               NO  - FINISHED                               
                                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
                                                                                
*                                  RANGE TEST DEACTIVATED                       
                                                                                
**       CLC   RCONDATE+3(3),=X'5E0C1A'                                         
**       BL    CONT15                                                           
**       CLC   RCONDATE(3),=X'5F081B'                                           
**       BH    CONT15                                                           
                                                                                
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUT                        
         MVC   RCONKREP,NEWREP     NEW REP CODE                                 
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT33                                                           
         MVC   P+1(8),=C'CON REC:'                                              
         MVC   P+10(27),RCONKEY                                                 
         GOTO1 REPORT                                                           
                                                                                
CONT33   DS    0H                                                               
         L     RF,CONCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
                                                                                
         ZICM  RF,RCONLEN,2        ADD ALL CONTRACT LENGTHS                     
         A     RF,CONBYTES                                                      
         ST    RF,CONBYTES                                                      
                                                                                
         DROP  R6                                                               
                                                                                
         B     CONT15              GO BACK FOR NEXT                             
                                                                                
CONTX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  BUYPROC:   RETRIEVE ALL BUY RECORDS                                          
******************************************************************              
BUYPROC  NTR1                                                                   
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP BUY RECD KEY                          
         MVC   KEY+16(2),OLDREP    INSERT REP CODE                              
                                                                                
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     BUY20                                                            
                                                                                
BUY10    DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
                                                                                
BUY20    DS    0H                                                               
         CLC   KEY(18),KEYSAVE     SAME RECORD TYPE/REP?                        
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
         A     RF,BUYBYTES                                                      
         ST    RF,BUYBYTES                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+7,C'Y'                                                   
         BNE   BUY30                                                            
         MVC   P+1(8),=C'BUY LNE:'                                              
         MVC   P+10(27),RBUYKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
BUY30    DS    0H                                                               
         B     BUY10                                                            
                                                                                
BUYX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  CLSPROC: EXTRACT ALL CLASS RECORDS                            *              
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
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+8,C'Y'                                                   
         BNE   CLS10                                                            
         MVC   P+1(8),=C'CLS REC:'                                              
         MVC   P+10(27),RCLSKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CLS10                                                            
                                                                                
CLSX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  CATPROC: EXTRACT ALL CATEGORY RECS                            *              
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
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+9,C'Y'                                                   
         BNE   CTG10                                                            
         MVC   P+1(8),=C'CTG REC:'                                              
         MVC   P+10(27),RCTGKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CTG10                                                            
                                                                                
CTGX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  TYPEPROC: EXTRACT ALL CONTRACT TYPE RECORDS                   *              
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
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+10,C'Y'                                                  
         BNE   CTY10                                                            
         MVC   P+1(8),=C'CTY REC:'                                              
         MVC   P+10(27),RCTYKEY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CTY10                                                            
                                                                                
CTYX     DS    0H                                                               
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
         MVC   P+1(24),=C'BUYS          PROCESSED:'                             
         EDIT  BUYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADVS          PROCESSED:'                             
         EDIT  ADVCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AGENCIES      PROCESSED:'                             
         EDIT  AGYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SALS          PROCESSED:'                             
         EDIT  SALCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'PRDS          PROCESSED:'                             
         EDIT  PRDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MISCELLANEOUS PROCESSED:'                             
         EDIT  OTHERCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
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
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ABLDAREA DS    A                                                                
AGRPAREA DS    A                                                                
ANEXTGRP DS    A                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
ACOMAREA DS    A                                                                
ANEXTCOM DS    A                                                                
ASTNAREA DS    A                                                                
LBLDAREA DS    F                                                                
NEXTAREA DS    A                   NEXT OPEN SLOT                               
STRTSRCH DS    A                   A(START OF SEARCH)                           
NUMBLD   DS    F                                                                
NUMCONS  DS    F                                                                
NUMBUYS  DS    F                                                                
CONBYTES DS    F                                                                
BUYBYTES DS    F                                                                
CONCTR   DS    F                                                                
BUYCTR   DS    F                                                                
ADVCTR   DS    F                                                                
AGYCTR   DS    F                                                                
SALCTR   DS    F                                                                
PRDCTR   DS    F                                                                
OTHERCTR DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
OLDREP   DC    CL2'S2'             OLD REP CODE                                 
NEWREP   DC    CL2'V5'             NEW REP CODE                                 
FLAGBYTS DS    0CL12               FLAGS                                        
BOTHOPEN DS    CL1                 NEITHER STATION LEFT                         
HNOPEN   DS    CL1                                                              
DIOPEN   DS    CL1                                                              
BOTHLEFT DS    CL1                                                              
         DS    CL8                 SPARE                                        
REPCODE  DS    CL2                                                              
KEYTYPE  DS    CL1                                                              
DBLSPACE DS    CL1                                                              
DATEWORK DS    CL24                DATE WORK AREA                               
SAVEKEY  DS    CL(L'KEY)                                                        
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
         DS    0D                                                               
SALBUF   DS    CL450               AREA FOR SALESPERSON BUFFER                  
         DS    0D                                                               
PRDBUF   DS    CL700               AREA FOR PRODUCT BUFFER                      
         DS    0D                                                               
ADVBUF   DS    CL2000              AREA FOR ADVERTISER BUFFER                   
*  INCLUDE REGENCOM                COMMISSION RECORD                            
*  INCLUDE REGENREG                REGION RECORD                                
*  INCLUDE REGENOFF                OFFICE RECORD                                
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
RECORD   DS    CL1008                                                           
         PRINT OFF                                                              
         ORG   RECORD                                                           
       ++INCLUDE REGENCOM          COMMISSION RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREG          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENGRP          GROUP      RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTEM          TEAM       RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREPA         REP        RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOFF          OFFICE     RECORD                            
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
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028REREPKH05 05/01/02'                                      
         END                                                                    
