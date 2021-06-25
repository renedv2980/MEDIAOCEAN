*          DATA SET REREPKH02  AT LEVEL 017 AS OF 05/01/02                      
*PHASE REKH02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPKH02 (REKH02) - NEW REP KH FILE EXTRACT/CREATION'          
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPKH02 -- EXTRACTS AND CREATS A NEW FILE WITH REP      *            
*                      CODE KH FROM THE BLAIR FILE                 *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* OCT04/94 (SKU) --- ORIGINAL ENTRY                                *            
*                                                                  *            
* OCT30/95 (SKU) --- ADDITIONAL RECORD EXTRACTION                  *            
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
         GOTO1 STATPROC,DMCB,(RC)  PROCESS STATION RECORDS                      
*                                                                               
         GOTO1 OFFCPROC,DMCB,(RC)  PROCESS OFFICE RECORDS                       
*                                                                               
*        GOTO1 AGYPROC,DMCB,(RC)   PROCESS AGENCY RECORDS                       
*                                                                               
         GOTO1 CONTPROC,DMCB,(RC)  PROCESS CONTRACT RECORDS                     
*                                                                               
         GOTO1 ADVPROC,DMCB,(RC)   PROCESS ADVERTISER RECORDS                   
*                                                                               
         GOTO1 SALPROC,DMCB,(RC)   PROCESS SALESPERSON RECORDS                  
*                                                                               
         GOTO1 PRDPROC,DMCB,(RC)   PROCESS PRODUCT RECORDS                      
*                                                                               
         GOTO1 CLSPROC,DMCB,(RC)   PROCESS CLASS RECORDS                        
*                                                                               
         GOTO1 CATPROC,DMCB,(RC)   PROCESS CATEGORY RECORDS                     
*                                                                               
         GOTO1 TYPEPROC,DMCB,(RC)  PROCESS TYPE RECORDS                         
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
******************************************************************              
*  STATPROC: EXTRACT BLAIR STATION RECS AS SPECIFIED BY TABLE AND*              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
STATPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         LA    R5,EXSTATAB                                                      
                                                                                
STAT10   DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAKEY,R6          SET RECORD DEFINITION                        
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,=C'BL'                                                  
         MVC   RSTAKSTA,2(R5)                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   STAT20                                                           
                                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKSTA,9(R5)      NEW STATION CALL LETTER                      
         MVC   RSTAKREP,NEWREP     NEW REP CODE                                 
         MVC   RSTAGRUP,7(R5)      NEW GROUP/SUBGROUP                           
         DROP  R6                                                               
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR,C'Y'                                                     
         BNE   STAT20                                                           
         MVC   P+1(8),=C'STA REC:'                                              
         MVC   P+10(5),9(R5)                                                    
         GOTO1 REPORT                                                           
                                                                                
STAT20   DS    0H                                                               
         LA    R5,L'EXSTATAB(R5)   NEXT STATION                                 
         CLI   0(R5),X'FF'                                                      
         BNE   STAT10                                                           
                                                                                
STATX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  OFFCPROC:  WILL BRING SELECTED BLAIR OFFICE RECORDS           *              
******************************************************************              
OFFCPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         LA    R5,EXOFFTAB                                                      
         MVI   KEYTYPE,X'04'                                                    
                                                                                
OFF10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
         MVC   KEY+23(2),=C'BL'                                                 
         MVC   KEY+25(2),0(R5)                                                  
                                                                                
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'ROFFKEY),KEYSAVE                                           
         BNE   OFF20                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING ROFFREC,R6                                                       
         MVC   REC-4(2),ROFFLEN    INSERT LENGTH FOR PUT                        
         MVC   ROFFKREP,NEWREP     NEW REP CODE                                 
         DROP  R6                                                               
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   OFF20                                                            
         MVC   P+1(8),=C'OFF REC:'                                              
         MVC   P+10(2),0(R5)                                                    
         GOTO1 REPORT                                                           
                                                                                
OFF20    DS    0H                                                               
         CLI   KEYTYPE,X'44'                                                    
         BE    OFF30                                                            
         MVI   KEYTYPE,X'44'                                                    
         B     OFF10                                                            
                                                                                
OFF30    DS    0H                                                               
         MVI   KEYTYPE,X'04'                                                    
         LA    R5,L'EXOFFTAB(R5)   NEXT OFFICE                                  
         CLI   0(R5),X'FF'                                                      
         BNE   OFF10                                                            
                                                                                
OFFX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  AGYPROC: EXTRACT BLAIR AGENCY RECS AS SPECIFIED BY TABLE AND  *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
AGYPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         LA    R5,EXAGYTAB                                                      
         MVI   KEYTYPE,X'0A'                                                    
                                                                                
AGY10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
         MVC   KEY+19(6),0(R5)                                                  
         MVC   KEY+25(2),=C'BL'                                                 
                                                                                
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   AGY20                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RAGYREC,R6                                                       
         MVC   REC-4(2),RAGYLEN    INSERT LENGTH FOR PUT                        
         MVC   RAGYKREP,NEWREP     NEW REP CODE                                 
         DROP  R6                                                               
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+2,C'Y'                                                   
         BNE   AGY20                                                            
         MVC   P+1(8),=C'AGY REC:'                                              
         MVC   P+10(4),0(R5)                                                    
         MVI   P+14,C'-'                                                        
         MVC   P+15(2),4(R5)                                                    
         GOTO1 REPORT                                                           
                                                                                
AGY20    DS    0H                                                               
         CLI   KEYTYPE,X'1A'                                                    
         BE    AGY30                                                            
         MVI   KEYTYPE,X'1A'                                                    
         B     AGY10                                                            
                                                                                
AGY30    DS    0H                                                               
         MVI   KEYTYPE,X'0A'                                                    
         LA    R5,L'EXAGYTAB(R5)   NEXT AGENCY                                  
         CLI   0(R5),X'FF'                                                      
         BNE   AGY10                                                            
                                                                                
AGYX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  ADVPROC: EXTRACT BLAIR ADV RECS AS SPECIFIED BY TABLE AND     *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
ADVPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         L     R6,ADVCTR                                                        
         GOTO1 =V(XSORT),DMCB,ADVBUF,(R6),4,4,0,RR=YES                          
         LA    R5,ADVBUF                                                        
                                                                                
ADV10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,0(R5)                                                   
         MVC   RADVKREP,=C'BL'                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   ADV20                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RADVREC,R6                                                       
         MVC   REC-4(2),RADVLEN    INSERT LENGTH FOR PUT                        
         MVC   RADVKREP,NEWREP     NEW REP CODE                                 
         DROP  R6                                                               
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+3,C'Y'                                                   
         BNE   ADV20                                                            
         MVC   P+1(8),=C'ADV REC:'                                              
         MVC   P+10(4),0(R5)                                                    
         GOTO1 REPORT                                                           
                                                                                
ADV20    DS    0H                                                               
         LA    R5,4(R5)            NEXT ADV                                     
         CLI   0(R5),0                                                          
         BNE   ADV10                                                            
                                                                                
ADVX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  SALPROC: EXTRACT BLAIR SAL RECS AS SPECIFIED BY TABLE AND     *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
SALPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         L     R6,SALCTR                                                        
         GOTO1 =V(XSORT),DMCB,SALBUF,(R6),3,3,0,RR=YES                          
         LA    R5,SALBUF                                                        
                                                                                
SAL10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKSAL,0(R5)                                                   
         MVC   RSALKREP,=C'BL'                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RSALKEY),KEYSAVE                                           
         BNE   SAL20                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RSALREC,R6                                                       
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     NEW REP CODE                                 
         MVC   RSALTEAM,=C'T '     DEFAULT TEAM CODE                            
         DROP  R6                                                               
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+4,C'Y'                                                   
         BNE   SAL20                                                            
         MVC   P+1(8),=C'SAL REC:'                                              
         MVC   P+10(3),0(R5)                                                    
         GOTO1 REPORT                                                           
                                                                                
SAL20    DS    0H                                                               
         LA    R5,3(R5)            NEXT SAL                                     
         CLI   0(R5),0                                                          
         BNE   SAL10                                                            
                                                                                
SALX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PRDPROC: EXTRACT BLAIR PRD RECS AS SPECIFIED BY TABLE AND     *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
PRDPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         L     R6,PRDCTR                                                        
         LA    R4,PRDBUF                                                        
         LR    R5,R4                                                            
         GOTO1 =V(XSORT),DMCB,(R4),(R6),7,7,0,RR=YES                            
                                                                                
PRD10    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV(7),0(R5)                                                
         MVC   RPRDKREP,=C'BL'                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BNE   PRD20                                                            
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RPRDREC,R6                                                       
         MVC   REC-4(2),RPRDLEN    INSERT LENGTH FOR PUT                        
         MVC   RPRDKREP,NEWREP     NEW REP CODE                                 
         DROP  R6                                                               
                                                                                
         L     RF,OTHERCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+5,C'Y'                                                   
         BNE   PRD20                                                            
         MVC   P+1(8),=C'PRD REC:'                                              
         MVC   P+10(7),0(R5)                                                    
         GOTO1 REPORT                                                           
                                                                                
PRD20    DS    0H                                                               
         LA    R5,7(R5)            NEXT PRD                                     
         CLI   0(R5),0                                                          
         BNE   PRD10                                                            
                                                                                
PRDX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.  ADJUST CON # OF    *              
*     HNNY RECORDS BY 100,000.  REPLACE SALESPERSON CODES FROM   *              
*     TABLE.  REPLACE GROUP/SUBGROUP AS APPROPRIATE.  ADJUST     *              
*     CONT #S IN COMBO CONTROL ELEMENT ALSO.                     *              
******************************************************************              
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         LA    R3,EXAGYTAB                                                      
         LA    R4,EXOFFTAB                                                      
         LA    R5,EXSTATAB                                                      
                                                                                
CONT10   DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6          SET RECORD DEFINITION                        
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,=C'BL'                                                  
         MVC   RCONKGRP,0(R5)                                                   
         MVC   RCONKSTA,2(R5)                                                   
         MVC   RCONKOFF,0(R4)                                                   
         MVC   RCONKAGY(6),0(R3)                                                
         DROP  R6                                                               
                                                                                
         OI    DMINBTS,X'08'       GET DELETED KEY ALSO                         
         GOTO1 HIGHDIR                                                          
         B     CONT20                                                           
                                                                                
CONT15   DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED KEY ALSO                         
         GOTO1 SEQDIR                                                           
                                                                                
CONT20   DS    0H                                                               
         CLC   KEY(19),KEYSAVE                                                  
         BE    CONT30                                                           
         LA    R3,L'EXAGYTAB(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   CONT10                                                           
         LA    R3,EXAGYTAB                                                      
         LA    R4,L'EXOFFTAB(R4)                                                
         CLI   0(R4),X'FF'                                                      
         BNE   CONT10                                                           
         LA    R4,EXOFFTAB                                                      
         LA    R5,L'EXSTATAB(R5)                                                
         CLI   0(R5),X'FF'                                                      
         BNE   CONT10                                                           
         B     CONTX                                                            
                                                                                
CONT30   DS    0H                                                               
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
                                                                                
* CONTRACTS MUST FALL WITHIN 12/26/94-08/27/95                                  
                                                                                
         CLC   RCONDATE+3(3),=X'5E0C1A'                                         
         BL    CONT15                                                           
         CLC   RCONDATE(3),=X'5F081B'                                           
         BH    CONT15                                                           
                                                                                
         MVC   SAVEKEY,KEY                                                      
                                                                                
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT33                                                           
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
         MVC   RCONKGRP,7(R5)      NEW GROUP/SUBGROUP                           
         MVC   RCONKSTA,9(R5)      NEW STATION CALL LETTER                      
         MVC   RCONKREP,NEWREP     NEW REP CODE                                 
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         L     RF,CONCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
                                                                                
         ZICM  RF,RCONLEN,2        ADD ALL CONTRACT LENGTHS                     
         A     RF,CONBYTES                                                      
         ST    RF,CONBYTES                                                      
                                                                                
         LA    RE,ADVBUF                                                        
CONT40   CLC   RCONKADV,0(RE)      SAVE OFF ADV CODE IN BUFFER                  
         BE    CONT50                                                           
         CLI   0(RE),0                                                          
         BE    CONT45                                                           
         LA    RE,4(RE)                                                         
         B     CONT40                                                           
                                                                                
CONT45   MVC   0(4,RE),RCONKADV                                                 
         L     RF,ADVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
                                                                                
CONT50   DS    0H                  SAVE OFF SAL CODE IN BUFFER                  
         LA    RE,SALBUF                                                        
CONT53   CLC   RCONSAL,0(RE)                                                    
         BE    CONT60                                                           
         CLI   0(RE),0                                                          
         BE    CONT55                                                           
         LA    RE,3(RE)                                                         
         B     CONT53                                                           
                                                                                
CONT55   MVC   0(3,RE),RCONSAL                                                  
         L     RF,SALCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SALCTR                                                        
                                                                                
CONT60   DS    0H                                                               
         OC    RCONPRD,RCONPRD                                                  
         BZ    CONT100                                                          
         CLC   RCONPRD,SPACES                                                   
         BE    CONT100                                                          
                                                                                
         LA    RE,PRDBUF           POINT TO PRODUCT BUFFER                      
CONT63   DS    0H                                                               
         CLI   0(RE),0                                                          
         BE    CONT65                                                           
         CLC   RCONKADV,0(RE)                                                   
         BNE   CONT64                                                           
         CLC   RCONPRD,4(RE)                                                    
         BE    CONT100                                                          
CONT64   LA    RE,7(RE)                                                         
         B     CONT63                                                           
                                                                                
CONT65   MVC   0(4,RE),RCONKADV                                                 
         MVC   4(3,RE),RCONPRD                                                  
         L     RF,PRDCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PRDCTR                                                        
         DROP  R6                                                               
                                                                                
CONT100  DS    0H                                                               
         BAS   RE,BUYPROC          PROCESS BUY RECORDS                          
                                                                                
         MVC   KEY,SAVEKEY         CONTINUE WITH NEXT CONTRACT REC              
         OI    DMINBTS,X'08'       GET DELETED KEY ALSO                         
         GOTO1 HIGHDIR             RESTABLISH SEQ ORDER                         
                                                                                
*** TEST                                                                        
*CONT110  DS    0H                                                              
*         CLC   CONCTR,=F'100'                                                  
*         BH    CONTX                                                           
*** TEST                                                                        
                                                                                
         B     CONT15                                                           
                                                                                
CONTX    DS    0H                                                               
         B     EXIT                                                             
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
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP BUY RECD KEY                          
         MVC   KEY+16(2),=C'BL'    INSERT REP CODE                              
         PACK  KEY+18(1),WORK+3(1) REVERSED 9'COMP OF K NUM                     
         PACK  KEY+19(1),WORK+2(1)                                              
         PACK  KEY+20(1),WORK+1(1)                                              
         PACK  KEY+21(1),WORK(1)                                                
                                                                                
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
         A     RF,BUYBYTES                                                      
         ST    RF,BUYBYTES                                                      
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+7,C'Y'                                                   
         BNE   BUY30                                                            
         MVC   P+1(8),=C'BUY LNE:'                                              
         EDIT  RBUYKMLN,(3,P+10),ALIGN=LEFT                                     
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
BUY30    DS    0H                                                               
         B     BUY10                                                            
                                                                                
BUYX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CLSPROC: EXTRACT ALL BLAIR CLS RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
CLSPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCLSKEY,R6                                                       
         MVI   RCLSKTYP,X'0D'                                                   
         MVC   RCLSKREP,=C'BL'                                                  
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
         MVC   P+10(2),RCLSKCLS                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CLS10                                                            
                                                                                
CLSX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CATPROC: EXTRACT ALL BLAIR CAT RECS AND                       *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
CATPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCTGKEY,R6                                                       
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,=C'BL'                                                  
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
         MVC   P+10(2),RCTGKCTG                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CTG10                                                            
                                                                                
CTGX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  TYPEPROC: EXTRACT ALL BLAIR TYPE RECS AND                     *              
*           WRITE THEM OUT WITH NEW REP CODE KH                  *              
******************************************************************              
TYPEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCTYKEY,R6                                                       
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKREP,=C'BL'                                                  
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
         MVC   P+10(2),RCTYKCTY                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     CTY10                                                            
                                                                                
CTYX     DS    0H                                                               
         B     EXIT                                                             
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
         MVC   P+1(24),=C'SALS          PROCESSED:'                             
         EDIT  SALCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'PRDS          PROCESSED:'                             
         EDIT  PRDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MISCELLANEOUS PROCESSED:'                             
         EDIT  OTHERCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         L     RF,CONBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         D     RE,CONCTR           NUM BYTES / # CONTRACTS                      
         MVC   P+1(24),=C'AVERAGE CONTRACT RECORD:'                             
         EDIT  (RF),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         L     RF,BUYBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         D     RE,BUYCTR           NUM BYTES / # CONTRACTS                      
         MVC   P+1(19),=C'AVERAGE BUY RECORD:'                                  
         EDIT  (RF),(12,P+25),COMMAS=YES                                        
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
SALCTR   DS    F                                                                
PRDCTR   DS    F                                                                
OTHERCTR DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
NEWREP   DC    CL2'KH'             NEW REP CODE                                 
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
* BYTES 1-2 = GROUP/SUBGROUP                                                    
* BYTES 3-7 = BLAIR STATIONS CALL LETTERS                                       
* BYTES 8-9 = NEW REP KH GROUP/SUBGROUP                                         
* BYTES 10-14 = NEW REP KH STATION CALL LETTERS                                 
EXSTATAB DS    0CL14                                                            
         DC    C'TAKPNX ',C'T KKEH '                                            
         DC    C'TAWBAL ',C'T WJSS '                                            
         DC    C'TAWFSB ',C'T KNIK '                                            
         DC    C'TAWUSA ',C'T WUHR '                                            
         DC    C'TBKSHB ',C'T WAEN '                                            
         DC    C'TCKARE ',C'T KOKC '                                            
         DC    C'TCKCAL ',C'T KPEN '                                            
         DC    C'TCWDIV ',C'T WKUI '                                            
*                                                                               
         DC    C'TAKING ',C'T KGJG '                                            
         DC    C'TAKUSA ',C'T WTMK '                                            
         DC    C'TCKXAS ',C'T WDNV '                                            
         DC    C'TAWDSU ',C'T WSTD '                                            
         DC    C'TBWPTV ',C'T KMKE '                                            
         DC    X'FF'                                                            
         SPACE 3                                                                
* OFFICES                                                                       
EXOFFTAB DS    0CL2                                                             
*          DATA SET REREPKH02S AT LEVEL 012 AS OF 10/07/94                      
         DC    C'AT'                                                            
         DC    C'CH'                                                            
         DC    C'DA'                                                            
         DC    C'DE'                                                            
         DC    C'LA'                                                            
         DC    C'MN'                                                            
         DC    C'NY'                                                            
         DC    C'PH'                                                            
         DC    C'SF'                                                            
         DC    C'SL'                                                            
         DC    X'FF'                                                            
         SPACE 3                                                                
* AGENCY CODES                                                                  
EXAGYTAB DS    0CL6                                                             
         DC    C'BBDOAT'                                                        
         DC    C'JWT AT'                                                        
         DC    C'ZWIMAT'                                                        
         DC    C'LB  CH'                                                        
         DC    C'BBDOCH'                                                        
         DC    C'DMBBCH'                                                        
         DC    C'JWT CH'                                                        
         DC    C'SAATCH'                                                        
         DC    C'ZWIMCH'                                                        
         DC    C'HAL CH'                                                        
         DC    C'TL  DA'                                                        
         DC    C'BJKEDA'                                                        
         DC    C'JWT DA'                                                        
         DC    C'ZWIMDA'                                                        
         DC    C'BBDOLA'                                                        
         DC    C'ZWIMLA'                                                        
*        DC    C'RP&ALA'                                                        
         DC    C'RP',X'50',C'ALA'                                               
         DC    C'DMBBLA'                                                        
         DC    C'SAATLA'                                                        
         DC    C'BJKELA'                                                        
         DC    C'JWT LA'                                                        
         DC    C'DDBNLA'                                                        
         DC    C'SAATNY'                                                        
         DC    C'BSB NY'                                                        
         DC    C'DMBBNY'                                                        
         DC    C'JWT NY'                                                        
         DC    C'DDBNNY'                                                        
         DC    C'BJKENY'                                                        
         DC    C'ZWIMNY'                                                        
         DC    C'HAL NY'                                                        
         DC    C'FCB NY'                                                        
         DC    X'FF'                                                            
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
**PAN#1  DC    CL21'017REREPKH02 05/01/02'                                      
         END                                                                    
