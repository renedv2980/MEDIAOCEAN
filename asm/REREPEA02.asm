*          DATA SET REREPEA02  AT LEVEL 042 AS OF 05/01/02                      
*PHASE REEA02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPEA02 (REEA02) NEW REP EA FILE EXTRACT/CREATION'            
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPEA02 -- EXTRACTS AND CREATS A NEW FILE WITH REP      *            
*                      FROM THE EA FILE                           *             
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* DEC11/95 (SKU) --- ORIGINAL ENTRY                                *            
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
RESZ02   CSECT                                                                  
         NMOD1 0,**RESW**,R9,RR=R5                                              
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
         GOTO1 AGYPROC,DMCB,(RC)   PROCESS AGENCY RECORDS                       
         B     MAINX                                                            
*                                                                               
         GOTO1 STATPROC,DMCB,(RC)   PROCESS REGION RECORDS                      
*                                                                               
         GOTO1 REGPROC,DMCB,(RC)   PROCESS REGION RECORDS                       
*                                                                               
         GOTO1 ADVPROC,DMCB,(RC)   PROCESS REGION RECORDS                       
*                                                                               
         GOTO1 OFFCPROC,DMCB,(RC)  PROCESS OFFICE RECORDS                       
*                                                                               
         GOTO1 TEAMPROC,DMCB,(RC)  PROCESS TEAM RECORDS                         
*                                                                               
         GOTO1 SALPROC,DMCB,(RC)  PROCESS TEAM RECORDS                          
*                                                                               
         GOTO1 GRPPROC,DMCB,(RC)   PROCESS GROUP RECORDS                        
*                                                                               
         GOTO1 PRDPROC,DMCB,(RC)   PROCESS PRODUCT RECORDS                      
*                                                                               
         GOTO1 AGYPROC,DMCB,(RC)   PROCESS AGENCY RECORDS                       
*                                                                               
         GOTO1 CLSPROC,DMCB,(RC)   PROCESS CLASS RECORDS                        
*                                                                               
         GOTO1 CONTPROC,DMCB,(RC)   PROCESS CONTRACT RECORDS                    
*                                                                               
         GOTO1 CATPROC,DMCB,(RC)   PROCESS CATEGORY RECORDS                     
*                                                                               
         GOTO1 BUDPROC,DMCB,(RC)   PROCESS BUDGET RECORDS                       
*                                                                               
         GOTO1 DPTPROC,DMCB,(RC)   PROCESS DAYPART RECORDS                      
*                                                                               
         GOTO1 MKTPROC,DMCB,(RC)   PROCESS MARKET RECORDS                       
*                                                                               
         GOTO1 PTPPROC,DMCB,(RC)   PROCESS MARKET RECORDS                       
*                                                                               
         GOTO1 SCPROC,DMCB,(RC)   PROCESS STANDARD COMMENT RECORDS              
*                                                                               
         GOTO1 CTYPROC,DMCB,(RC)  PROCESS CTYPE RECORDS                         
*                                                                               
         GOTO1 OCMPROC,DMCB,(RC)  PROCESS CTYPE RECORDS                         
*                                                                               
         GOTO1 DVSPROC,DMCB,(RC)  PROCESS CTYPE RECORDS                         
*                                                                               
         GOTO1 DCTPROC,DMCB,(RC)  PROCESS CTYPE RECORDS                         
*                                                                               
         GOTO1 TYPEPROC,DMCB,(RC)  PROCESS TYPE RECORDS                         
*                                                                               
MAINX    DS    0H                                                               
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
         GOTO1 (RF),DMCB,C'GET',1800000,4000000                                 
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
                                                                                
         XC    OTHERCTR,OTHERCTR                                                
         XC    REGCTR,REGCTR                                                    
         XC    OFFCTR,OFFCTR                                                    
         XC    OFF2CTR,OFF2CTR                                                  
         XC    TEAMCTR,TEAMCTR                                                  
         XC    GRPCTR,GRPCTR                                                    
         XC    PRDCTR,PRDCTR                                                    
         XC    CLSCTR,CLSCTR                                                    
         XC    CATCTR,CATCTR                                                    
         XC    BUDCTR,BUDCTR                                                    
         XC    MKTCTR,MKTCTR                                                    
         XC    AGYCTR,AGYCTR                                                    
         XC    OCMCTR,OCMCTR                                                    
         XC    DVSCTR,DVSCTR                                                    
         XC    STACTR,STACTR                                                    
         XC    PTPCTR,PTPCTR                                                    
         XC    DPTCTR,DPTCTR                                                    
         XC    DCTCTR,DCTCTR                                                    
         XC    SCCTR,SCCTR                                                      
         XC    TYPECTR,TYPECTR                                                  
         XC    CTYPECTR,CTYPECTR                                                
         XC    CONBYTES,CONBYTES                                                
         XC    BUYBYTES,BUYBYTES                                                
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  STATPROC: EXTRACT OLDREP STATION RECS AS SPECIFIED BY TABLE AND*             
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
STATPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAKEY,R6          SET RECORD DEFINITION                        
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     STAT20                                                           
                                                                                
STAT10   DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
STAT20   DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   STATX                                                            
                                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,STACTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,STACTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'STA REC:'                                              
         MVC   P+10(5),RSTAKSTA                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     STAT10                                                           
                                                                                
STATX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  REGPROC:  WILL BRING SELECTED SELNY REGION RECORDS           *               
******************************************************************              
REGPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RREGKEY,R6                                                       
         MVI   RREGKTYP,X'03'                                                   
         MVC   RREGKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     REG20                                                            
                                                                                
REG10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
REG20    DS    0H                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   REGX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RREGREC,R6                                                       
         MVC   REC-4(2),RREGLEN    INSERT LENGTH FOR PUT                        
         MVC   RREGKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,REGCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,REGCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'REG REC:'                                              
         MVC   P+10(2),RREGKREG                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     REG10                                                            
                                                                                
REGX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  ADVPROC:  WILL BRING SELECTED SELNY ADV RECORDS           *                  
******************************************************************              
ADVPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
                                                                                
         GOTO1 HIGHDIR                                                          
         B     ADV20                                                            
                                                                                
ADV10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
ADV20    DS    0H                                                               
         CLI   KEY,X'08'                                                        
         BNE   ADVX                                                             
         CLC   =C'K3',KEY+25                                                    
         BNE   ADV10                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RADVREC,R6                                                       
         MVC   REC-4(2),RADVLEN    INSERT LENGTH FOR PUT                        
         MVC   RADVKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,ADVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'ADV REC:'                                              
         MVC   P+10(2),RADVKADV                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     ADV10                                                            
                                                                                
ADVX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  OFFCPROC:  WILL BRING SELECTED SELNY OFFICE RECORDS           *              
******************************************************************              
OFFCPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         MVI   KEYTYPE,X'04'                                                    
                                                                                
OFF03    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
         GOTO1 HIGHDIR                                                          
         B     OFF10                                                            
*                                                                               
OFF05    DS    0H                                                               
         GOTO1 SEQDIR                                                           
         CLC   KEY(1),KEYTYPE                                                   
         BNE   OFF20                                                            
                                                                                
OFF10    DS    0H                                                               
         CLC   KEY+23(2),OLDREP                                                 
         BNE   OFF05                                                            
*                                                                               
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING ROFFREC,R6                                                       
         MVC   REC-4(2),ROFFLEN    INSERT LENGTH FOR PUT                        
         MVC   ROFFKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,OFFCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,OFFCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'OFF REC:'                                              
         CLI   KEY,X'44'                                                        
         BNE   *+8                                                              
         MVI   P+3,C'2'                                                         
*                                                                               
         MVC   P+10(2),ROFFKOFF                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     OFF05                                                            
                                                                                
OFF20    DS    0H                                                               
         CLI   KEYTYPE,X'04'                                                    
         BNE   OFFX                                                             
         MVI   KEYTYPE,X'44'                                                    
         B     OFF03                                                            
                                                                                
OFFX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  TEAMPROC:  WILL BRING SELECTED SELNY TEAM RECORDS           *                
******************************************************************              
TEAMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RTEMKEY,R6                                                       
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     TEAM20                                                           
                                                                                
TEAM10   DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
TEAM20   DS    0H                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TEAMX                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RTEMREC,R6                                                       
         MVC   REC-4(2),RTEMLEN    INSERT LENGTH FOR PUT                        
         MVC   RTEMKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,TEAMCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,TEAMCTR                                                       
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(9),=C'TEAM REC:'                                             
         MVC   P+10(2),RTEMKTEM                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     TEAM10                                                           
                                                                                
TEAMX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  SALPROC: EXTRACT OLDREP SAL RECS                                             
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
SALPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     SAL10                                                            
                                                                                
SAL10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
SAL20    DS    0H                                                               
         CLC   KEY(24),KEYSAVE                                                  
         BNE   SALX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RSALREC,R6                                                       
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,SALCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SALCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'SAL REC:'                                              
         MVC   P+10(3),RSALKSAL                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     SAL10                                                            
                                                                                
SALX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  GRPPROC:  WILL BRING SELECTED SELNY GROUP RECORDS           *                
******************************************************************              
GRPPROC  NTR1                                                                   
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
                                                                                
         MVC   P+1(9),=C'GRP REC:'                                              
         MVC   P+10(2),RGRPKGRP                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     GRP10                                                            
                                                                                
GRPX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PRDPROC: EXTRACT SELNY PRD RECS AS SPECIFIED BY TABLE AND     *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
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
                                                                                
PRD10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
PRD20    DS    0H                                                               
         CLI   KEY,X'09'                                                        
         BNE   PRDX                                                             
         CLC   =C'K3',KEY+25                                                    
         BNE   PRD10                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RPRDREC,R6                                                       
         MVC   REC-4(2),RPRDLEN    INSERT LENGTH FOR PUT                        
         MVC   RPRDKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,PRDCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PRDCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'PRD REC:'                                              
         MVC   P+10(7),RPRDKADV                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     PRD10                                                            
                                                                                
PRDX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  AGYPROC: EXTRACT BLAIR AGENCY RECS AS SPECIFIED BY TABLE AND  *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
AGYPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGYKEY,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     AGY20                                                            
                                                                                
AGY10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
AGY20    DS    0H                                                               
         CLI   KEY,X'0A'                                                        
         BNE   AGYX                                                             
         CLC   =C'BL',KEY+25                                                    
         BNE   AGY10                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RAGYREC,R6                                                       
         MVC   REC-4(2),RAGYLEN    INSERT LENGTH FOR PUT                        
         MVC   RAGYKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,AGYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'AGY REC:'                                              
         MVC   P+10(4),RAGYKAGY                                                 
         MVI   P+14,C'-'                                                        
         MVC   P+15(2),RAGYKAOF                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     AGY10                                                            
                                                                                
AGYX     DS    0H                                                               
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGY2KEY,R6                                                      
         MVI   RAGK2TYP,X'1A'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     AG220                                                            
                                                                                
AG210    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
AG220    DS    0H                                                               
         CLI   KEY,X'1A'                                                        
         BNE   AG2X                                                             
         CLC   =C'BL',KEY+25                                                    
         BNE   AG210                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RAGY2REC,R6                                                      
         MVC   REC-4(2),RAGY2LEN   INSERT LENGTH FOR PUT                        
         MVC   RAGK2REP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,AG2CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AG2CTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'AG2 REC:'                                              
         MVC   P+10(4),RAGK2AGY                                                 
         MVI   P+14,C'-'                                                        
         MVC   P+15(2),RAGK2AOF                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     AG210                                                            
                                                                                
AG2X     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CLSPROC: EXTRACT ALL SELNY CLS RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE                     *              
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
                                                                                
         MVC   P+1(8),=C'CLS REC:'                                              
         MVC   P+10(2),RCLSKCLS                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CLS10                                                            
                                                                                
CLSX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.  ADJUST CON # OF    *              
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
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CONTX                                                            
                                                                                
CONT30   DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
                                                                                
         MVC   P+1(8),=C'CON REC:'                                              
         MVC   P+10(7),RCONKGRP                                                 
         MVC   P+18(2),RCONKOFF                                                 
         MVC   P+21(6),RCONKAGY                                                 
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,P+28),ALIGN=LEFT                                    
         GOTO1 REPORT                                                           
                                                                                
CONT33   DS    0H                                                               
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUT                        
         MVC   RCONKREP,NEWREP     NEW REP CODE                                 
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         L     RF,CONCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
                                                                                
         ZICM  RF,RCONLEN,2        ADD ALL CONTRACT LENGTHS                     
         A     RF,CONBYTES                                                      
         ST    RF,CONBYTES                                                      
                                                                                
         B     CONT15                                                           
                                                                                
CONTX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CATPROC: EXTRACT ALL SELNY CAT RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
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
                                                                                
         L     RF,CATCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CATCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(8),=C'CTG REC:'                                              
         MVC   P+10(2),RCTGKCTG                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CTG10                                                            
                                                                                
CTGX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  TYPEPROC: EXTRACT ALL SELNY TYPE RECS AND                     *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
TYPEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RTYPKEY,R6                                                       
         MVI   RTYPKTYP,X'30'                                                   
         MVC   RTYPKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     TYP20                                                            
                                                                                
TYP10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
TYP20    DS    0H                                                               
         CLC   KEY(19),KEYSAVE                                                  
         BNE   TYPX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RTYPREC,R6                                                       
         MVC   REC-4(2),RTYPLEN    INSERT LENGTH FOR PUT                        
         MVC   RTYPKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,TYPECTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,TYPECTR                                                       
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(9),=C'TYPE REC:'                                             
         MVC   P+10(8),RTYPKREC                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     TYP10                                                            
                                                                                
TYPX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CTYPPROC: EXTRACT ALL SELNY TYPE RECS AND                     *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
CTYPROC  NTR1                                                                   
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
                                                                                
         L     RF,CTYPECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,CTYPECTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         MVC   P+1(9),=C'CTYP REC:'                                             
         MVC   P+10(1),RCTYKCTY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CTY10                                                            
                                                                                
CTYX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  BUDPROC: EXTRACT ALL SELNY TYPE RECS AND                     *               
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
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
         B     BUD20                                                            
                                                                                
BUD10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
BUD20    DS    0H                                                               
         CLC   KEY(18),KEYSAVE                                                  
         BNE   BUDX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RBUDREC,R6                                                       
         MVC   REC-4(2),RBUDLEN    INSERT LENGTH FOR PUT                        
         MVC   RBUDKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,BUDCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUDCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         MVC   P+1(8),=C'BUD REC:'                                              
         MVC   P+10(9),RBUDKYR                                                  
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
                                                                                
         B     BUD10                                                            
                                                                                
BUDX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  DPTPROC: EXTRACT ALL SELNY DPT  RECS AND                     *               
*           WRITE THEM OUT WITH NEW REP CODE                     *              
******************************************************************              
DPTPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDPTKEY,R6                                                       
         MVI   RDPTKTYP,X'24'                                                   
         MVC   RDPTKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     DPT20                                                            
                                                                                
DPT10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
DPT20    DS    0H                                                               
         CLC   KEY(26),KEYSAVE                                                  
         BNE   DPTX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RDPTREC,R6                                                       
         MVC   REC-4(2),RDPTLEN    INSERT LENGTH FOR PUT                        
         MVC   RDPTKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,DPTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,DPTCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         MVC   P+1(8),=C'DPT REC:'                                              
         MVC   P+10(1),RDPTKDPT                                                 
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
                                                                                
         B     DPT10                                                            
                                                                                
DPTX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  MKTPROC: EXTRACT ALL SELNY MKT RECS AND                     *                
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
MKTPROC  NTR1                                                                   
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
*                                                                               
         MVC   P+1(8),=C'MKT REC:'                                              
         MVC   P+10(4),RMKTKMKT                                                 
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
                                                                                
         B     MKT10                                                            
                                                                                
MKTX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CMTPROC: EXTRACT ALL SELNY MKT RECS AND                     *                
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
OCMPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ROCMKEY,R6                                                       
         MVI   ROCMKTYP,X'34'                                                   
         MVC   ROCMKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     OCM20                                                            
                                                                                
OCM10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
OCM20    DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   OCMX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING ROCMREC,R6                                                       
         MVC   REC-4(2),ROCMLEN    INSERT LENGTH FOR PUT                        
         MVC   ROCMKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,OCMCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,OCMCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         MVC   P+1(8),=C'OCM REC:'                                              
         MVC   P+10(5),ROCMKOFC                                                 
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
                                                                                
         B     OCM10                                                            
                                                                                
OCMX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  DVSPROC: EXTRACT ALL SELNY MKT RECS AND                     *                
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
DVSPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDSPKEY,R6                                                       
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     DVS20                                                            
                                                                                
DVS10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
DVS20    DS    0H                                                               
         CLC   KEY(24),KEYSAVE                                                  
         BNE   DVSX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RDSPREC,R6                                                       
         MVC   REC-4(2),RDSPLEN    INSERT LENGTH FOR PUT                        
         MVC   RDSPKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,DVSCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,DVSCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         MVC   P+1(8),=C'DVS REC:'                                              
         MVC   P+10(3),RDSPKSAL                                                 
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
                                                                                
         B     DVS10                                                            
                                                                                
DVSX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  DCTPROC: EXTRACT ALL SELNY MKT RECS AND                     *                
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
DCTPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDCTKEY,R6                                                       
         MVI   RDCTKTYP,X'3B'                                                   
         MVC   RDCTKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     DCT20                                                            
                                                                                
DCT10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
DCT20    DS    0H                                                               
         CLC   KEY(24),KEYSAVE                                                  
         BNE   DCTX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RDCTREC,R6                                                       
         MVC   REC-4(2),RDCTLEN    INSERT LENGTH FOR PUT                        
         MVC   RDCTKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,DCTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,DCTCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         MVC   P+1(8),=C'DCT REC:'                                              
         MVC   P+10(2),RDCTKCTY                                                 
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
                                                                                
         B     DCT10                                                            
                                                                                
DCTX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PTPPROC: EXTRACT ALL SELNY MKT RECS AND                     *                
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
PTPPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPTPKEY,R6                                                       
         MVI   RPTPKTYP,X'31'                                                   
         MVC   RPTPKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     PTP20                                                            
                                                                                
PTP10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
PTP20    DS    0H                                                               
         CLC   KEY(24),KEYSAVE                                                  
         BNE   PTPX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RPTPREC,R6                                                       
         MVC   REC-4(2),RPTPLEN    INSERT LENGTH FOR PUT                        
         MVC   RPTPKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,PTPCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PTPCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         MVC   P+1(8),=C'PTP REC:'                                              
         MVC   P+10(3),RPTPKREC                                                 
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
                                                                                
         B     PTP10                                                            
                                                                                
PTPX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  SCPROC: EXTRACT ALL SELNY CMT RECS AND                     *                 
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
SCPROC  NTR1                                                                    
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCMTKEY,R6                                                       
         MVI   RCMTKTYP,X'2E'                                                   
         MVC   RCMTKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     CMT20                                                            
                                                                                
CMT10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
CMT20    DS    0H                                                               
         CLC   KEY(17),KEYSAVE                                                  
         BNE   CMTX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCMTREC,R6                                                       
         MVC   REC-4(2),RCMTLEN    INSERT LENGTH FOR PUT                        
         MVC   RCMTKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,SCCTR                                                         
         LA    RF,1(RF)                                                         
         ST    RF,SCCTR                                                         
                                                                                
         BAS   RE,PUTRECS                                                       
*                                                                               
         MVC   P+1(8),=C'SC  REC:'                                              
         MVC   P+10(10),RCMTKOFF                                                
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
                                                                                
         B     CMT10                                                            
                                                                                
CMTX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'AGY           PROCESSED:'                             
         EDIT  AGYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AGY2          PROCESSED:'                             
         EDIT  AG2CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
         MVC   P+1(24),=C'STAS          PROCESSED:'                             
         EDIT  STACTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REGS          PROCESSED:'                             
         EDIT  REGCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OFFS          PROCESSED:'                             
         EDIT  OFFCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OFF2          PROCESSED:'                             
         EDIT  OFF2CTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TEMS          PROCESSED:'                             
         EDIT  TEAMCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SALS          PROCESSED:'                             
         EDIT  SALCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'GRPS          PROCESSED:'                             
         EDIT  GRPCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADVS          PROCESSED:'                             
         EDIT  ADVCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'PRDS          PROCESSED:'                             
         EDIT  PRDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AGY           PROCESSED:'                             
         EDIT  AGYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'AGY2          PROCESSED:'                             
         EDIT  AG2CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONT          PROCESSED:'                             
         EDIT  CONCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CLS           PROCESSED:'                             
         EDIT  CLSCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CATS          PROCESSED:'                             
         EDIT  CATCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUDS          PROCESSED:'                             
         EDIT  BUDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DAYPART       PROCESSED:'                             
         EDIT  DPTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OCM           PROCESSED:'                             
         EDIT  OCMCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DEV SALES     PROCESSED:'                             
         EDIT  DVSCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'DEV CTYPE     PROCESSED:'                             
         EDIT  DCTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MKTS          PROCESSED:'                             
         EDIT  MKTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SCMT          PROCESSED:'                             
         EDIT  SCCTR,(12,P+30),COMMAS=YES                                       
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TYPS          PROCESSED:'                             
         EDIT  TYPECTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CTYP          PROCESSED:'                             
         EDIT  CTYPECTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MISCELLANEOUS PROCESSED:'                             
         EDIT  OTHERCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,CONBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         D     RE,CONCTR           NUM BYTES / # CONTRACTS                      
         MVC   P+1(24),=C'AVERAGE CONTRACT RECORD:'                             
         EDIT  (RF),(12,P+30),COMMAS=YES                                        
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
STACTR   DS    F                                                                
PTPCTR   DS    F                                                                
REGCTR   DS    F                                                                
OFFCTR   DS    F                                                                
OFF2CTR  DS    F                                                                
TEAMCTR  DS    F                                                                
GRPCTR   DS    F                                                                
PRDCTR   DS    F                                                                
CLSCTR   DS    F                                                                
CATCTR   DS    F                                                                
BUDCTR   DS    F                                                                
MKTCTR   DS    F                                                                
CONCTR   DS    F                                                                
AGYCTR   DS    F                                                                
AG2CTR   DS    F                                                                
OCMCTR   DS    F                                                                
DPTCTR   DS    F                                                                
DCTCTR   DS    F                                                                
DVSCTR   DS    F                                                                
SCCTR    DS    F                                                                
TYPECTR  DS    F                                                                
CTYPECTR DS    F                                                                
ADVCTR   DS    F                                                                
SALCTR   DS    F                                                                
OTHERCTR DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
OLDREP   DC    CL2'EA'             NEW REP CODE                                 
NEWREP   DC    CL2'PV'             NEW REP CODE                                 
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
       ++INCLUDE REGENTEM          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENGRP          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTYP          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKT          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCMT          REGION     RECORD                            
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
       ++INCLUDE REGENAGY2         AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPTP          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOCM          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDSP          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDCT          AGENCY RECORD                                
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
**PAN#1  DC    CL21'042REREPEA02 05/01/02'                                      
         END                                                                    
