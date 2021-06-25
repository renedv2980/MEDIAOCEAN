*          DATA SET REREPST0B  AT LEVEL 088 AS OF 05/01/02                      
*PHASE REST02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPST02 (REST02) - FULL FILE STRIP FOR TEST FILE'             
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPST02 -- EXTRACTS AND CREATE A NEW FILE WITH REP      *            
*                      CODE NEWREP FROM REP CODE OLDREP            *            
*                      TBLS  OF AGENCY AND ADVERTISER CODES        *            
*                      SERVE AS FILTERS                            *            
*                                                                  *            
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
         NMOD1 0,**ST02**,R8,R9,RR=R5                                           
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
         GOTO1 REPRPROC,DMCB,(RC)  PROCESS REP RECORDS                          
*                                                                               
         GOTO1 STATPROC,DMCB,(RC)  PROCESS STATION RECORDS                      
*                                                                               
         GOTO1 OFFCPROC,DMCB,(RC)  PROCESS OFFICE RECORDS                       
*                                                                               
         GOTO1 TEAMPROC,DMCB,(RC)  PROCESS TEAM   RECORDS                       
*                                                                               
         GOTO1 AGYPROC,DMCB,(RC)   PROCESS AGENCY RECORDS                       
*                                                                               
         GOTO1 ADVPROC,DMCB,(RC)   PROCESS ADVERTISER RECORDS                   
*                                                                               
         GOTO1 SALPROC,DMCB,(RC)   PROCESS SALESPERSON RECORDS                  
*                                                                               
         GOTO1 PRDPROC,DMCB,(RC)   PROCESS PRODUCT RECORDS                      
*                                                                               
         GOTO1 CONTPROC,DMCB,(RC)  PROCESS CONTRACT RECORDS                     
*                                                                               
***>>>   GOTO1 BUYRPROC,DMCB,(RC)  PROCESS BUY RECORDS ALONE                    
*                                                                               
         GOTO1 CLSPROC,DMCB,(RC)   PROCESS CLASS RECORDS                        
*                                                                               
         GOTO1 CATPROC,DMCB,(RC)   PROCESS CATEGORY RECORDS                     
*                                                                               
         GOTO1 TYPEPROC,DMCB,(RC)  PROCESS TYPE RECORDS                         
*                                                                               
         GOTO1 GRUPPROC,DMCB,(RC)  PROCESS TYPE RECORDS                         
*                                                                               
         GOTO1 OWNRPROC,DMCB,(RC)  PROCESS OWNER RECORDS                        
*                                                                               
         GOTO1 MRKTPROC,DMCB,(RC)  PROCESS TYPE RECORDS                         
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
                                                                                
         XC    CONCTR,CONCTR       CLEAR COUNTERS                               
         XC    CONCTR2,CONCTR2     CLEAR COUNTERS                               
         XC    BUYCTR,BUYCTR                                                    
         XC    BUYCTR2,BUYCTR2                                                  
         XC    OTHERCTR,OTHERCTR                                                
         XC    ADVCTR,ADVCTR                                                    
         XC    SALCTR,SALCTR                                                    
         XC    PRDCTR,PRDCTR                                                    
         XC    CONBYTES,CONBYTES                                                
         XC    BUYBYTES,BUYBYTES                                                
         MVC   OLDREP,QRECORD+36   'FROM' REP                                   
         MVC   NEWREP,QRECORD+38   'TO' REP                                     
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
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
         LA    R4,EXSTATAB         SET A(STATION TABLE)                         
STAT14   EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    STAT11              YES - NOT IN TABLE: SKIP                     
**       MVC   P+1(8),=C'STA TAB:'                                              
**       MVC   P+10(5),RSTAKSTA                                                 
**       MVC   P+18(5),2(R4)                                                    
**       GOTO1 REPORT                                                           
*                                                                               
         CLC   RSTAKSTA,2(R4)      NO  - STATION IN TABLE?                      
         BE    STAT16              YES - RETRIEVE IT                            
         LA    R4,L'EXSTATAB(R4)   NO  - BUMP TO NEXT ENTRY                     
         B     STAT14              GO BACK FOR NEXT                             
*                                                                               
         DROP  R6                                                               
*                                                                               
STAT16   EQU   *                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     NEW REP CODE                                 
         MVC   RSTAKSTA,9(R4)      INSERT NEW STATION CALLS                     
         MVC   RSTAGRUP,7(R4)      INSERT NEW STATION G/SUBG                    
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
         BNE   AGYP0120            NO  - AGENCY RECORDS FINISHED                
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
         LA    R4,EXAGYTAB         SET A(AGENCY TABLE)                          
AGYP0102 EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    AGYP0080            YES - NOT IN TABLE: SKIP                     
         CLC   KEY+19(4),0(R4)     NO  - AGENCY  IN TABLE?                      
         BE    AGYP0104            YES - RETRIEVE IT                            
*                                                                               
*   ONLY LOOK FOR 4 CHARS OF AGENCY CODE:  NO OFFICE                            
*                                                                               
         LA    R4,L'EXAGYTAB(R4)   NO  - BUMP TO NEXT ENTRY                     
         B     AGYP0102            GO BACK FOR NEXT                             
*                                                                               
AGYP0104 EQU   *                                                                
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
         BNE   AGYP0080                                                         
         MVC   P+1(8),=C'AGY REC:'                                              
         MVC   P+10(2),=C'0A'                                                   
         CLI   KEYTYPE,X'0A'                                                    
         BE    AGYP0110                                                         
         MVC   P+10(2),=C'1A'                                                   
AGYP0110 EQU   *                                                                
         MVC   P+20(4),RAGYKAGY                                                 
         MVI   P+24,C'-'                                                        
         MVC   P+25(2),RAGYKAOF                                                 
         GOTO1 REPORT                                                           
         B     AGYP0080            GO BACK FOR NEXT                             
         DROP  R6                                                               
AGYP0120 DS    0H                                                               
         CLI   KEYTYPE,X'1A'                                                    
         BE    AGYP0140                                                         
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEYTYPE,X'1A'       INSERT KEY TYPE                              
         MVI   KEY,X'1A'           INSERT INTO KEY ALSO                         
         B     AGYP0040                                                         
                                                                                
AGYP0140 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  ADVPROC: EXTRACT SELTL ADV RECS AS SPECIFIED BY TABLE AND     *              
*           WRITE THEM OUT WITH NEW REP CODE CT                  *              
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
         LA    R4,EXADVTAB         SET A(ADVERT TABLE)                          
ADVP0120 EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    ADVP0040            YES - NOT IN TABLE: SKIP                     
         CLC   KEY+21(4),0(R4)     NO  - ADVERT  IN TABLE?                      
         BE    ADVP0140            YES - RETRIEVE IT                            
         LA    R4,L'EXADVTAB(R4)   NO  - BUMP TO NEXT ENTRY                     
         B     ADVP0120            GO BACK FOR NEXT                             
*                                                                               
ADVP0140 EQU   *                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RADVREC,R6                                                       
         MVC   REC-4(2),RADVLEN    INSERT LENGTH FOR PUT                        
         MVC   RADVKREP,NEWREP     NEW REP CODE                                 
         DROP  R6                                                               
                                                                                
         L     RF,ADVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+3,C'Y'                                                   
         BNE   ADVP0160                                                         
         MVC   P+1(8),=C'ADV REC:'                                              
         MVC   P+10(2),=C'1A'                                                   
         MVC   P+14(4),21(R6)                                                   
         MVC   P+20(2),25(R6)                                                   
         GOTO1 REPORT                                                           
ADVP0160 EQU   *                                                                
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
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,OLDREP                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     SAL12                                                            
SAL11    EQU   *                                                                
         GOTO1 SEQDIR                                                           
SAL12    EQU   *                                                                
         CLC   KEY(24),KEYSAVE     COMPARE THRU REP CODE                        
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
                                                                                
         CLI   QUESTOR+4,C'Y'                                                   
         BNE   SAL11                                                            
         MVC   P+1(8),=C'SAL REC:'                                              
         MVC   P+10(3),RSALKSAL                                                 
         GOTO1 REPORT                                                           
         B     SAL11               GO BACK FOR NEXT SALESPERSON                 
                                                                                
SALX     DS    0H                                                               
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
         BE    PRD30               YES                                          
         CLC   OLDREP,=C'AM '       CORRECT REP?                                
         BE    PRD24               YES                                          
         CLC   OLDREP,=C'CQ'       CORRECT REP?                                 
         BE    PRD24               YES                                          
         CLC   OLDREP,=C'NK'       CORRECT REP?                                 
         BNE   PRD10                YES                                         
PRD24    EQU   *                                                                
         CLC   KEY+25(2),=C'MR'    CORRECT REP?                                 
         BNE   PRD10                NO  - GO BACK FOR NEXT                      
         SPACE                                                                  
*                                                                               
PRD30    EQU   *                                                                
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
         BNE   PRD10                                                            
         MVC   P+1(8),=C'PRD REC:'                                              
         MVC   P+10(9),RPRDKADV                                                 
         GOTO1 REPORT                                                           
         B     PRD10                                                            
                                                                                
PRDX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.                     *              
******************************************************************              
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
CONP0020 DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6          SET RECORD DEFINITION                        
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,OLDREP                                                  
         GOTO1 HIGHDIR                                                          
         B     CONP0060                                                         
                                                                                
CONP0040 DS    0H                                                               
         LA    R6,KEY              RESET A(USING)                               
         GOTO1 SEQDIR                                                           
                                                                                
CONP0060 DS    0H                                                               
         CLC   KEY(04),KEYSAVE                                                  
         BNE   CONP0400                                                         
*                                                                               
CONP0080 DS    0H                                                               
**STAT                                                                          
         LA    R4,EXSTATAB         SET A(STATION TABLE)                         
CONP0100 EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    CONP0040            YES - NOT IN TABLE: SKIP                     
         CLC   RCONKSTA,2(R4)      NO  - STATION IN TABLE?                      
         BE    CONP0120            YES - RETRIEVE IT                            
         LA    R4,L'EXSTATAB(R4)   NO  - BUMP TO NEXT ENTRY                     
         B     CONP0100            GO BACK FOR NEXT                             
*                                                                               
CONP0120 EQU   *                                                                
         MVC   SCONKSTA,9(R4)      SAVE   NEW STATION CALLS                     
         MVC   SCONKGRP,7(R4)      SAVE   NEW STATION G/SUBG                    
**STAT                                                                          
**AGY                                                                           
         LA    R4,EXAGYTAB         SET A(AGENCY TABLE)                          
CONP0140 EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    CONP0040            YES - NOT IN TABLE: SKIP                     
         CLC   RCONKAGY(6),0(R4)   NO  - AGENCY  IN TABLE?                      
         BE    CONP0160            YES - RETRIEVE IT                            
*                                                                               
*   LOOK FOR 6 CHARS OF AGENCY/OFFICE AGAINST TABLE                             
*                                                                               
         LA    R4,L'EXAGYTAB(R4)   NO  - BUMP TO NEXT ENTRY                     
         B     CONP0140            GO BACK FOR NEXT                             
*                                                                               
CONP0160 EQU   *                                                                
**AGY                                                                           
**ADV                                                                           
         LA    R4,EXADVTAB         SET A(ADVERT TABLE)                          
CONP0180 EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    CONP0040            YES - NOT IN TABLE: SKIP                     
         CLC   RCONKADV,0(R4)      NO  - ADVERT  IN TABLE?                      
         BE    CONP0200            YES - RETRIEVE IT                            
         LA    R4,L'EXADVTAB(R4)   NO  - BUMP TO NEXT ENTRY                     
         B     CONP0180            GO BACK FOR NEXT                             
*                                                                               
         DROP  R6                                                               
*                                                                               
CONP0200 EQU   *                                                                
**ADV                                                                           
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
         MVC   SAVEKEY,KEY                                                      
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUT                        
         MVC   RCONKREP,NEWREP     NEW REP CODE                                 
         MVC   RCONKSTA,SCONKSTA   INSERT NEW STATION CALLS                     
         MVC   RCONKGRP,SCONKGRP   INSERT NEW STATION G/SUBG                    
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         L     RF,CONCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         L     RF,CONCTR2          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR2                                                       
         CLC   CONCTR2,=F'0050'    DISPLAY EVERY N RECORDS                      
         BNE   CONP0220                                                         
         XC    CONCTR2,CONCTR2                                                  
         MVC   P+1(21),=C'PROCESSING CONTRACTS:'                                
         EDIT  CONCTR,(7,P+24)                                                  
         GOTO1 REPORT                                                           
CONP0220 EQU   *                                                                
*                                                                               
*  TEST                                                                         
*        CLC   CONCTR,=F'100'      PROCESS FIRST N RECORDS                      
*        BH    CONP0400                                                         
*  TEST END                                                                     
*                                                                               
                                                                                
         ZICM  RF,RCONLEN,2        ADD ALL CONTRACT LENGTHS                     
         A     RF,CONBYTES                                                      
         ST    RF,CONBYTES                                                      
                                                                                
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONP0240                                                         
         CLC   CONCTR,=F'100'      DISPLAY FIRST N RECORDS                      
         BH    CONP0240                                                         
         MVC   P+1(8),=C'CON REC:'                                              
         MVC   P+10(27),RCONREC                                                 
         EDIT  CONCTR,(7,P+40)                                                  
         EDIT  CONCTR2,(7,P+50)                                                 
         GOTO1 REPORT                                                           
                                                                                
CONP0240 DS    0H                                                               
*                                                                               
         BAS   RE,BUYPROC          PROCESS BUY RECORDS                          
*                                                                               
         MVC   KEY,SAVEKEY         CONTINUE WITH NEXT CONTRACT REC              
         OI    DMINBTS,X'08'       GET DELETED KEY ALSO                         
         GOTO1 HIGHDIR             RESTABLISH SEQ ORDER                         
                                                                                
         B     CONP0040                                                         
                                                                                
CONP0400 DS    0H                                                               
         B     EXIT                                                             
SCONKSTA DS    CL5                                                              
SCONKGRP DS    CL2                                                              
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  BUYRROC: EXTRACT ALL BUY RECORDS                              *              
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
*        CLC   BUYCTR,=F'100'      QUIT AFTER FIRST N RECORDS                   
*        BH    BUYRX                                                            
*   TEST END                                                                    
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
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP BUY RECD KEY                          
         MVC   KEY+16(2),OLDREP    INSERT REP CODE                              
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
         MVC   P+10(2),RCLSKCLS                                                 
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
         MVC   P+10(2),RCTGKCTG                                                 
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
         MVC   P+10(2),RCTYKCTY                                                 
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
         MVC   P+10(2),RGRPKGRP                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     GRP10                                                            
                                                                                
GRPX     DS    0H                                                               
         B     EXIT                                                             
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
         MVC   P+10(3),ROWNKOWN                                                 
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
         MVC   P+10(4),RMKTKMKT                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     MKT10                                                            
                                                                                
MKTX     DS    0H                                                               
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
         MVC   P+1(24),=C'AGY+AGY1 RECS PROCESSED:'                             
         EDIT  AGYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADVS          PROCESSED:'                             
         EDIT  ADVCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SALESPERSONS  PROCESSED:'                             
         EDIT  SALCTR,(12,P+30),COMMAS=YES                                      
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
         MVC   P+1(24),=C'MISCELLANEOUS PROCESSED:'                             
         EDIT  OTHERCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         L     RF,CONBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         LTR   RF,RF               ANY DIVIDEND?                                
         BZ    NODIV001            NO                                           
         D     RE,CONCTR           NUM BYTES / # CONTRACTS                      
         MVC   P+1(24),=C'AVERAGE CONTRACT RECORD:'                             
         EDIT  (RF),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
NODIV001 EQU   *                                                                
         L     RF,BUYBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         LTR   RF,RF               ANY DIVIDEND?                                
         BZ    NODIV002            NO                                           
         D     RE,BUYCTR           NUM BYTES / # CONTRACTS                      
         MVC   P+1(19),=C'AVERAGE BUY RECORD:'                                  
         EDIT  (RF),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
NODIV002 EQU   *                                                                
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
CONCTR2  DS    F                                                                
REPCTR   DS    F                                                                
STACTR   DS    F                                                                
OFFCTR   DS    F                                                                
TEMCTR   DS    F                                                                
CTYCTR   DS    F                                                                
GRPCTR   DS    F                                                                
OWNCTR   DS    F                                                                
MKTCTR   DS    F                                                                
CTGCTR   DS    F                                                                
CLSCTR   DS    F                                                                
BUYCTR   DS    F                                                                
BUYCTR2  DS    F                                                                
ADVCTR   DS    F                                                                
SALCTR   DS    F                                                                
PRDCTR   DS    F                                                                
OTHERCTR DS    F                                                                
AGYCTR   DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
NEWREP   DS    CL2                 NEW REP CODE                                 
OLDREP   DS    CL2                 OLD REP CODE                                 
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
* BYTES 3-7 = SELTL STATIONS CALL LETTERS                                       
* BYTES 8-9 = NEW REP CT GROUP/SUBGROUP                                         
* BYTES 10-14 = NEW REP CT STATION CALL LETTERS                                 
EXSTATAB DS    0CL14                                                            
         DC    C'FCKDVR ',C'FCKDDS '                                            
         DC    C'FCKSAZ ',C'FCKSDZ '                                            
         DC    C'FCKTVI ',C'FCKDVS '                                            
         DC    C'FCWHBQ ',C'FCWHDS '                                            
         DC    C'FCWTTG ',C'FCWDDG '                                            
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
* AGENCY CODES                                                                  
EXAGYTAB DS    0CL6                                                             
         DC    C'BBCC  '                                                        
         DC    C'CCP   '                                                        
         DC    C'DFS LA'                                                        
         DC    C'DFS CH'                                                        
         DC    C'DFS NY'                                                        
         DC    C'GBJBCH'                                                        
         DC    C'GREYNY'                                                        
         DC    C'TELVCH'                                                        
         DC    C'MIS LA'                                                        
         DC    C'MIS NY'                                                        
         DC    C'MIS SF'                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
* ADVERT CODES                                                                  
EXADVTAB DS    0CL4                                                             
         DC    C'BLOV'                                                          
         DC    C'INBC'                                                          
         DC    C'IQ  '                                                          
         DC    C'USW '                                                          
         DC    C'GMS0'                                                          
         DC    C'TOYB'                                                          
         DC    C'TMS '                                                          
         DC    C'JED '                                                          
         DC    C'QAAC'                                                          
         DC    C'QTAA'                                                          
         DC    C'L019'                                                          
         DC    C'KGML'                                                          
         DC    C'B004'                                                          
         DC    C'PG  '                                                          
         DC    C'CMD '                                                          
         DC    C'CN1 '                                                          
         DC    C'SYP '                                                          
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL2008              AREA FOR RECORD                              
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
RECORD   DS    CL2008                                                           
         PRINT OFF                                                              
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
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088REREPST0B 05/01/02'                                      
         END                                                                    
