*          DATA SET NEMEDYKT   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMEDYK    AT LEVEL 010 AS OF 10/16/85                      
*PHASE T31EAAA,*                                                                
         TITLE 'T31EAA-BOBS  INTERFACE TAPE'                                    
T31EAA   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEYK**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          R7-ANETWS2/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R6,ANETWS3                                                       
         USING NDDEMBLK,R6         R6-ANETWS3/NDDEMBLK,DEDBLOCK                 
         ST    R6,NBADEM                                                        
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
*                                  ANETWS4 FOR 3000  IS FREE                    
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMEDPZ (T31EPZ) NETWORK INTERFACE TAPE                    *          
*                                                                     *         
*  COMMENTS: WRITES A REPORT/TAPE                                     *         
*                                                                     *         
*  CALLS TO: NETIO                                                              
*                                                                     *         
*  GLOBAL: R7-MYWORKD (ANETWS2+500)                                   *         
*                                                                     *         
***********************                                               *         
*  LOGIC: READS UNIT RECS. SETS UP ONE REQUEST REC(TAPED1)            *         
*         AND SCHEDULE RECS(TAPED), PASSING THESE TO SORTER.          *         
*                                                                     *         
*         AT REQLAST, GETS RECS BACK FROM SORTER WRITING THEM         *         
*         TO TAPE AND PRINT LINE, CREATING A SUMMARY REC(TAPED3)      *         
*         AT BREAKS OF CLT/PRD/EST/NTWK/DAYPT/SPTLN OR WEEKS.         *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
* GETS UNIT RECS/WRITES TO TAPE/PRINT LINE                                      
*                                                                               
*****************************************                                       
LR       DS    0H                                                               
         ZAP   SEQNUM,=P'0'                                                     
         BAS   RE,CLEARTOT                                                      
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
         B     LR5                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,30,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
*                                                                               
LR5      DS    0H                                                               
         CLI   SPLTAPE,C'Y'                                                     
         BNE   LR7                                                              
         OPEN  (NTWKTP,OUTPUT)           OPEN TAPE                              
LR7      BAS   RE,GTWKLIST               GET WEEKLIST                           
         MVI   NBDATA,C'U'               SET UP FOR UNIT RECS                   
         MVI   NBSEQ,C'Q'                                                       
         MVI   NBESTOPT,C'Y'                                                    
         CLI   SPLDEM,C'E'                                                      
         BE    LR10                                                             
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,C'Y'                                                    
LR10     NETGO NSNETIO,DMCB,NETBLOCK     GET UNIT RECS                          
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BE    LR20                                                             
         CLI   NBMODE,NBREQLST     LAST RECORD                                  
         BNE   LR10                                                             
         MVI   FRST,0                                                           
         B     LR30                                                             
*                                                                               
LR20     CLI   FRST,0                    IS IT FIRST TIME                       
         BNE   LR25                                                             
         MVI   FRST,C'N'                                                        
         BAS   RE,WRITEREQ               PROCESS REQUEST REC                    
LR25     CLC   ESTSV,NBACTEST                                                   
         BE    *+8                                                              
         BAS   RE,GETEST                 GET ESTHEADER AND SET NDDEMOS          
         MVC   ESTSV,NBACTEST                                                   
         BAS   RE,DOTAPE                 DO SCHEDULE REC                        
         BAS   RE,PUTSORT                                                       
         B     LR10                      GET NXT UNIT REC                       
*                                                                               
LR30     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)                                                         
         MVC   SORTREC,0(R3)                                                    
         LTR   R3,R3                                                            
         BZ    LR40                EOF SORTER                                   
         CLI   FRST,0              IS IT FIRST TIME                             
         BNE   LR35                                                             
         MVI   FRST,1                                                           
         MVC   PREVREC(24),0(R3)       SAVE REC FOR BRKCHK                      
LR35     BAS   RE,BRKCHK                 CHECK BREAK                            
         BAS   RE,ADDTOTS                ADD TO TOTALS                          
*******  BAS   RE,WRITESCD               WRITE REPORT LINE                      
         MVI   BRK2FLG,0                                                        
         B     LR30                      GET NEXT REC FROM SORT                 
*                                                                               
LR40     DS    0H                                                               
         CLI   PREVREC,0           IS THERE ANY DATA                            
         BE    *+8                                                              
         BAS   RE,DOTAPE3                                                       
         CLI   SPLTAPE,C'Y'                                                     
         BNE   LRX                                                              
         CLOSE (NTWKTP)                                                         
LRX      B     EXIT                                                             
         EJECT                                                                  
*****************************************                                       
* ROUTINE SETS DATA FROM NETBLOCK TO                                            
*  TAPE NETWORK SCHEDULE RECORD FOR SORT                                        
******************************************                                      
DOTAPE   NTR1                                                                   
         LA    R2,RECWORK                                                       
         USING TAPED,R2                                                         
         MVI   TPMED,C'N'                      MEDIA                            
         GOTO1 NBCLUNPK,DMCB,NBACTCLI,TPCLT    CLIENT                           
         MVC   WORK(1),NBPRD                   PRODUCT                          
         BAS   RE,GETPRD                                                        
         MVC   TPPRD,WORK+10                       SET PRODUCT                  
         EDIT  (B1,NBACTEST),(3,TPEST),ALIGN=LEFT  ESTIMATE                     
         MVC   TPRECODE,=C'20'                     REC CODE                     
         MVC   TPNTWK(4),NBACTNET                  NETWORK                      
         MVC   TPDAYPT,NBACTDP                     DAYPART                      
         EDIT  (B1,NBLEN),(4,FULL)                 SPOTLENGTH                   
         PACK  TPSPTLN,FULL                        SET SOTLENGTH                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,TPACTDAT)  DATE                      
         EDIT  (B4,NBACTUAL),(10,WORK+20)                                       
         PACK  TPCOST,WORK+20(10)              ACTUAL DOLLARS                   
         CLI   NBACTUAL+3,0        IF ACT DOLS = 0                              
         BNE   DOT3                                                             
         ZAP   TPCOST,=P'0'        THEN ZAP COST FOR TOTS ADD                   
DOT3     MVC   TPUNITS,=X'00001F'              NUM OF UNITS                     
         MVC   TPPROGNM,NBPROGNM               PROGRAM NAME                     
         XC    WORK(4),WORK                                                     
         XC    TPSTART(15),TPSTART    SET STR,END,AFF FLDS= 0                   
         MVC   WORK(2),NBTIME                                                   
         GOTO1 UNTIME,DMCB,WORK,TPSTART                                         
         CLC   NBTIME+2(2),=2X'0'                                               
         BE    DOT5                                                             
         MVC   WORK(2),NBTIME+2                                                 
         GOTO1 UNTIME,DMCB,WORK,TPEND                                           
DOT5     CLC   NBAFFTIM,=2X'0'                                                  
         BE    DOT6                                                             
         GOTO1 UNTIME,DMCB,NBAFFTIM,TPAFFID                                     
DOT6     MVC   TPDAY,NBDAYNAM                                                   
*                                                                               
         MVC   DBCOMFCS,ACOMFACS         SET UP FOR DEMCON                      
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         LA    R3,NDESTDEM+2                    EST/ACT DEMOS                   
         MVI   TPDEMTYP,C'E'                                                    
         CLI   SPLDEM,C'E'                                                      
         BE    DOT10                                                            
         LA    R3,NDACTDEM+2                                                    
         MVI   TPDEMTYP,C'A'                                                    
DOT10    LA    R4,8                BCTLIMIT TO DEMOS                            
         LA    R2,TPDEMO        ** NOTE R2 NOW POINTS TO DEMO FLDS              
         DROP  R2                                                               
         SR    R5,R5                                                            
DOT15    NETGO NVDEMCON,DMCB,((R5),NDDEMBLK),DBLOCK,(7,WORK)                    
         CLI   WORK,X'5C'          IF NO  DEMOS                                 
         BNE   DOT17                                                            
DOT16    ZAP   7(4,R2),=P'0'    THEN ZAP WITH P'O' FOR PACKED TOTS              
         B     DOT20                                                            
DOT17    MVC   0(7,R2),WORK                                                     
         MVC   HALF,0(R3)                                                       
         CLC   HALF,=2X'0'         IF DEMO=0/ZAP IT                             
         BE    DOT16                                                            
         EDIT  (B2,HALF),(8,WORK+20)                                            
         PACK  7(4,R2),WORK+20(8)                                               
DOT20    LA    R2,11(R2)                                                        
         LA    R5,1(R5)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,DOT15                                                         
*                                                                               
DOTX     B     EXIT                                                             
         EJECT                                                                  
********************************************                                    
* ROUTINE SETS DATA TO REQUEST RECORD                                           
*  AND WRITES REQ REC TO REPORT AND TAPE                                        
*                                                                               
WRITEREQ NTR1                                                                   
         LA    R3,RECWORK                                                       
         USING TAPED1,R3                                                        
         MVI   0(R3),C' '                                                       
         MVC   1(199,R3),0(R3)            CLEAR TO SPACES                       
         MVI   TPMED1,C'N'                           MEDIA                      
         MVC   TPCLT1,SPLCLI                         CLIENT                     
         MVC   TPPRD1,SPLPRO                         PRODUCT                    
         EDIT  (B1,NBACTEST),(3,TPEST1),ALIGN=LEFT   ESTIMATE                   
         MVC   TPRECOD1,=C'00'                       REC CODE                   
         CLI   SPLNET,0                                                         
         BE    REQ5                                                             
         CLC   SPLNET(3),=C'ALL'                                                
         BE    REQ5                                                             
         MVC   TPNTWK1(4),NBACTNET                                              
         B     *+10                                                             
REQ5     MVC   TPNTWK1(3),=C'ALL'                                               
         GOTO1 DATCON,DMCB,(2,NBCMPSTR),(0,TPSTART1)    START DATE              
         GOTO1 DATCON,DMCB,(2,NBCMPEND),(0,TPEND1)      END DATE                
         MVC   TPREQNM(3),SPOOLID                                               
         CLI   SPLTAPE,C'Y'                                                     
         BNE   REQ7                                                             
         PUT   NTWKTP,RECWORK                   WRITE TAPE                      
*                                                                               
*        GOTO1 HEXOUT,DMCB,(R3),(R2),200                                        
*        BAS   RE,SPOOLIT                                                       
*                                                                               
REQ7     LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PCLT(3),TPCLT1                                                   
         MVC   PCLT+132(3),TPEST1                                               
         MVC   PCLT+264(5),TPNTWK1                                              
         MVC   PPRD,TPPRD1                                                      
         MVC   PACTDAT,TPSTART1                                                 
         MVI   PACTDAT+6,C'-'                                                   
         MVC   PCOST(6),TPEND1                                                  
         MVC   PDEMO(12),TPREQNM                                                
         MVI   SPACING,3                                                        
         BAS   RE,SPOOLIT                       WRITE REPORT                    
         MVI   RECWORK,C' '                                                     
         MVC   RECWORK+1(199),RECWORK           CLEAR RECWORK                   
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***************************************                                         
* ROUTINE SETS DATA TO SUMMARY RECORD                                           
* AND WRITES IT TO REPORT AND TAPE                                              
*                                                                               
DOTAPE3  NTR1                                                                   
         LA    R3,RECWORK                                                       
         USING TAPED3,R3                                                        
         MVC   RECWORK(24),PREVREC                                              
         MVC   TPRECOD3,=C'21'                                                  
         L     R5,APERLIST                                                      
         GOTO1 DATCON,DMCB,(2,0(R5)),(0,TPSTART3)                               
         GOTO1 DATCON,DMCB,(2,2(R5)),(0,TPEND3)                                 
         MVC   TPUNITS3,TOTUNITS                                                
         MVC   TPCOST3,TOTCOST                                                  
         MVI   TPDEMTP3,C'E'                                                    
         CLI   SPLDEM,C'E'                                                      
         BE    *+8                                                              
         MVI   TPDEMTP3,C'A'                                                    
         AP    SEQNUM,=P'1'                                                     
         MVC   TPSEQNM3,SEQNUM                                                  
*                                                                               
         LA    R2,TPDEMO3                                                       
         LA    R4,TOTDEMNM                                                      
         LA    R5,8                MAX DEMOS=8                                  
SUM5     MVC   0(11,R2),0(R4)                                                   
         LA    R4,11(R4)           BUMP TOTS                                    
         LA    R2,11(R2)           BUMP OUT FLD                                 
         BCT   R5,SUM5                                                          
*                                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PCLT(3),TPCLT3                                                   
         MVC   PCLT+132(3),TPEST3                                               
         MVC   PCLT+264(5),TPNTWK3                                              
         MVC   PPRD,TPPRD3                                                      
         MVC   PDAYPT,TPDAYPT3                                                  
         EDIT  (P2,TPSPTLN3),(3,PSPTLN)                                         
         MVC   PACTDAT(12),TPSTART3       START/END DATE                        
         MVC   PPROGNM(5),=C'COST='                                             
         LA    R5,PPROGNM+5                                                     
         EDIT  (P5,TPCOST3),(10,(R5)),2,ALIGN=LEFT                              
         MVC   PPROGNM+132(5),=C'UNIT='                                         
         LA    R5,PPROGNM+137                                                   
         EDIT  (P3,TPUNITS3),(4,(R5)),ALIGN=LEFT                                
         MVC   PDEMTYPE,TPDEMTP3                                                
* DEMOS                                                                         
         LA    R5,8                BCT LIMIT TO DEMOS                           
         LA    R4,TPDEMO3                                                       
         LA    R2,PDEMO            *** NOTE R2 NOW =DEMOS                       
         DROP  R2                                                               
SUM10    MVC   1(7,R2),0(R4)              STACK DEMONM/DEMVALUE                 
         CLC   7(4,R4),=X'0000000C'                                             
         BE    SUM12                                                            
         EDIT  (P4,7(R4)),(8,132(R2)),1                                         
SUM12    LA    R2,9(R2)                                                         
         LA    R4,11(R4)                                                        
         BCT   R5,SUM10                                                         
         MVI   SPACING,3                                                        
         BAS   RE,SPOOLIT                 WRITE TO PRINT LINE                   
*                                                                               
*        LA    R2,P                RESET R2 TO P LINE                           
*        GOTO1 HEXOUT,DMCB,(R3),(R2),200                                        
*        BAS   RE,SPOOLIT                                                       
         CLI   SPLTAPE,C'Y'                                                     
         BNE   SUM15                                                            
         PUT   NTWKTP,RECWORK             WRITE TAPE                            
SUM15    MVI   RECWORK,C' '               CLEAR RECWORK                         
         MVC   RECWORK+1(199),RECWORK                                           
SUMX     B     EXIT                                                             
         EJECT                                                                  
****************************************                                        
* ROUTINE SETS SCHEDULE REC TO PRINT LINE                                       
*         WRITES TO PRINT LINE/TAPE                                             
* INPUT R3=REC FROM SORTER                                                      
*                                                                               
WRITESCD NTR1                                                                   
         USING TAPED,R3                                                         
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*        AP    SEQNUM,=P'1'                                                     
*        MVC   TPSEQNUM,SEQNUM                                                  
*        CLI   BRK2FLG,C'Y'        IS IT AFTER A KEY BREAK                      
*        BNE   WRT10               IF NOT/SKIP KEY DATA FOR PRINT               
*        MVC   PCLT(3),TPCLT                                                    
*        MVC   PCLT+132(3),TPEST                                                
*        MVC   PCLT+264(5),TPNTWK                                               
*        MVC   PPRD,TPPRD                                                       
*        MVC   PDAYPT,TPDAYPT                                                   
*        EDIT  (P2,TPSPTLN),(3,PSPTLN)                                          
WRT10    MVC   PACTDAT,TPACTDAT                                                 
         EDIT  (P5,TPCOST),(10,PCOST),2                                         
*        MVC   PDEMTYPE(1),TPDEMTYP                                             
*        MVC   PPROGNM,TPPROGNM                                                 
*        MVC   PTIME,TPSTART                                                    
*        MVC   PTIME+132,TPEND                                                  
*        MVC   PTIME+264,TPAFFID                                                
*        MVC   PDAY,TPDAY                                                       
* DEMOS                                                                         
         LA    R5,8                BCT LIMIT TO DEMOS                           
         LA    R4,TPDEMO                                                        
         LA    R2,PDEMO            *** NOTE R2 NOW =DEMOS                       
         DROP  R2                                                               
DEMO5    MVC   1(7,R2),0(R4)              STACK DEMONM/DEMVALUE                 
         CLC   7(4,R4),=X'0000000C'     IS IT ZERO/SKIP                         
         BE    DEMO6                                                            
         EDIT  (P4,7(R4)),(8,132(R2)),1                                         
DEMO6    CLI   BRK2FLG,C'Y'             IF NOT NEW KEY                          
         BE    DEMO7                                                            
         MVC   0(8,R2),132(R2)          THEN MOVE DEMVAL OVER DEMNAME           
         XC    132(8,R2),132(R2)        AND CLEAR DEMVAL                        
DEMO7    LA    R2,9(R2)                                                         
         LA    R4,11(R4)                                                        
         BCT   R5,DEMO5                                                         
         BAS   RE,SPOOLIT                 WRITE TO PRINT LINE                   
*                                                                               
*        LA    R2,P                       RESET R2 TO PRINT LINE                
*        GOTO1 HEXOUT,DMCB,(R3),(R2),200                                        
*        BAS   RE,SPOOLIT                                                       
*        MVC   P(200),0(R3)                                                     
*        BAS   RE,SPOOLIT                                                       
         CLI   SPLTAPE,C'Y'                                                     
         BNE   WRTX                                                             
         PUT   NTWKTP,(R3)                 WRITE TAPE                           
WRTX     B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
********************************************                                    
* ROUTINE CHECKS FOR KEY BREAKS/PERIOD BREAK                                    
* IF A KEY BREAK/RESET APERLIST TO START OF LIST                                
*  IF A DATE BREAK BUMP TO NEXT PERIOD IN PERLIST                               
*                                                                               
BRKCHK   NTR1                                                                   
         CLC   PREVREC(24),0(R3)        CLT/PRD/EST/NTWK/DAYPT/SPTLN            
         BNE   BREAK2                                                           
         USING TAPED,R3                                                         
BRK1     GOTO1 DATCON,DMCB,(0,TPACTDAT),(2,HALF)                                
         L     R5,APERLIST                                                      
         CLC   HALF,2(R5)           ACTDAT GRTR THAN WEEK END DAT               
         BNH   BRKX                                                             
BREAK    BAS   RE,DOTAPE3        * DATE BREAK                                   
         BAS   RE,CLEARTOT                                                      
         L     R5,APERLIST         BUMP TO NEXT PERIOD IN PERLIST               
         LA    R5,4(R5)                                                         
         ST    R5,APERLIST                                                      
         B     BRK1                IS IT WITHIN NXT PERIOD                      
*                                  IN CASE MORE THAN ONE PERIOD IS              
*                                  EMPTY OF UNITS                               
*                                                                               
BREAK2   DS    0H          *  BREAK OF CLT/PRD/EST/NTWK/DAYPT/SPTLN             
         BAS   RE,DOTAPE3                                                       
         MVI   FORCEHED,C'Y'     FORCR NEW KEY TO PAGE TOP                      
         BAS   RE,CLEARTOT                                                      
         LA    R1,PERLIST      RESET APERLIST TO START OF LIST                  
         ST    R1,APERLIST                                                      
         MVC   PREVREC,0(R3)              SET NEW PREV REC                      
         MVI   BRK2FLG,C'Y'       SET FLG TO PRINT KEY DATA ON REPORT           
BRKX     B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
****************************************                                        
* ROUTINE ADDS TOTALS FOR SUMMARY REC                                           
*                                                                               
* CALLED AFTER RECS ARE SORTED                                                  
* INPUT R3 POINTS TO TAPED REC                                                  
*                                                                               
ADDTOTS  NTR1                                                                   
         USING TAPED,R3                                                         
         AP    TOTUNITS,=P'1'                                                   
         AP    TOTCOST,TPCOST                                                   
         LA    R4,8                                                             
         LA    R2,TOTDEMNM                                                      
         CLI   0(R2),0             IF NAMES ALREADY SET/SKIP                    
         BNE   ADD10                                                            
         LA    R5,TPDEMO                                                        
ADD5     MVC   0(7,R2),0(R5)                                                    
         LA    R5,11(R5)                                                        
         LA    R2,11(R2)                                                        
         BCT   R4,ADD5                                                          
ADD10    LA    R4,8                                                             
         LA    R2,TOTDEMO                                                       
         LA    R5,TPDEMVAL                                                      
ADD12    AP    0(4,R2),0(4,R5)                                                  
         LA    R2,11(R2)                                                        
         LA    R5,11(R5)                                                        
         BCT   R4,ADD12                                                         
         B     EXIT                                                             
         SPACE 2                                                                
***************************************                                         
* ROUTINE CLEARS AND ZAPS TOTAL FIELDS                                          
*                                                                               
CLEARTOT NTR1                                                                   
         ZAP   TOTCOST,=P'0'                                                    
         ZAP   TOTUNITS,=P'0'                                                   
         LA    R4,8                                                             
         LA    R3,TOTDEMNM                                                      
CLR5     XC    0(7,R3),0(R3)                                                    
         ZAP   7(4,R3),=P'0'                                                    
         LA    R3,11(R3)                                                        
         BCT   R4,CLR5                                                          
         B     EXIT                                                             
         EJECT                                                                  
***************************************                                         
* ROUTINE PUTS RECWORK TO SORTER                                                
*   AND CLEARS RECWORK                                                          
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',RECWORK                                      
         MVI   RECWORK,C' '                                                     
         MVC   RECWORK+1(199),RECWORK                                           
         B     EXIT                                                             
         SPACE 3                                                                
*******************************                                                 
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************                                              
*  TO GET PRD CODE FROM C LIST   *                                              
*  INPUT   WORK HAS PRDNO        *                                              
*  OUTPUT  PRDCODE IN WORK+10    *                                              
**********************************                                              
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   WORK+10(3),=C'UNA'   .SET TO UNDEFINED                           
         B     GPX                                                              
GP12     CLC   3(1,R2),WORK                                                     
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   WORK+10(3),0(R2)      SET 3 CHAR PRINTABLE PRD CODE              
*                                                                               
GPX      B     EXIT                                                             
         SPACE 2                                                                
********************************************                                    
* READ ESTIMATE HEADER                     *                                    
*      OUTPUT: NDDEMOS SET FROM EST HEADER *                                    
*                                          *                                    
********************************************                                    
         SPACE                                                                  
GETEST   NTR1                                                                   
         MVC   WORK(1),NBPRD                                                    
         BAS   RE,GETPRD               3CL PRD RETURNED IN WORK+10              
         LA    R3,KEY                                                           
         USING EKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,WORK+10                                                  
         CLC   EKEYPRD,=C'UNA'                                                  
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NBACTEST                                                 
         NETGO NVSETSPT,DMCB                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         L     RE,NBAIO               SAVE UNIT REC IN NBAIO                    
         L     RF,ANETWS4                                                       
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         GOTO1 GETREC                                                           
         L     R3,NBAIO                                                         
         USING ESTHDR,R3                                                        
         MVC   NDDEMOS(60),EDEMLST                                              
         MVC   NDDEMOS+60(3),EDEM21                                             
         SPACE                                                                  
GPNX     NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME   CLEAR FILENAME                               
         L     RF,NBAIO              RESET UNIT REC IN NBAIO                    
         L     RE,ANETWS4                                                       
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
****************************************                                        
* GET WEEKLIST INTO PERLIST                                                     
*                                                                               
GTWKLIST NTR1                                                                   
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
GTWK5    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   GTWK5                                                            
         MVI   PERTYPE,C'W'        WEEKS ONLY                                   
         LA    R3,15               FOR 15 WEEKS MAX                             
         ST    R3,NUMPER                                                        
         LA    R3,PERLIST                                                       
         ST    R3,APERLIST                                                      
         NETGO NVWKLST,DMCB,NUMPER,(R3),PERTYPE                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+15(20),SPLPRON                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(3),SPLEST                                                  
         MVC   H5+15(20),SPLESTN                                                
         SPACE                                                                  
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVI   BRK2FLG,C'Y'        SET FLG TO PRINT DUP DATA                    
         MVC   PCLT(3),=C'CLT'                                                  
         MVC   PCLT+132(3),=C'EST'                                              
         MVC   PCLT+264(4),=C'NTWK'                                             
         MVC   PPRD+264(3),=C'PRD'                                              
         MVC   PDAYPT+263(3),=C'DPT'                                            
         MVC   PSPTLN+264(3),=C'SLN'                                            
         MVC   PACTDAT+264(4),=C'DATE'                                          
         MVC   PCOST+264(4),=C'COST'                                            
         MVC   PDEMTYPE+264(3),=C'DEM'                                          
         MVC   PPROGNM+264(4),=C'PROG'                                          
         MVC   PTIME(5),=C'START'                                               
         MVC   PTIME+134(3),=C'END'                                             
         MVC   PTIME+264(5),=C'AFFID'                                           
         MVC   PDAY+264(3),=C'DAY'                                              
         LA    R5,8                                                             
         LA    R2,PDEMO+264                                                     
SETDEM   MVC   0(4,R2),=C'DEMO'                                                 
         LA    R2,9(R2)                                                         
         BCT   R5,SETDEM                                                        
*                                                                               
HDHK1A   DS    0H                                                               
         CLI   BOXSET,C'Y'          SET PARAMS FOR BOXES                        
         BE    HDX                                                              
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         USING PLINED,R5                                                        
         MVI   0(R5),C'L'                                                       
         MVI   PCLT+5,C'C'                                                      
         MVI   PPRD+3,C'C'                                                      
         MVI   PDAYPT+1,C'C'                                                    
         MVI   PSPTLN+3,C'C'                                                    
         MVI   PACTDAT+6,C'C'                                                   
         MVI   PCOST+10,C'C'                                                    
         LA    R2,8                                                             
         LA    R3,PDEMO+8                                                       
         MVI   0(R3),C'C'                                                       
         LA    R3,9(R3)                                                         
         BCT   R2,*-8                                                           
         MVI   PDEMTYPE+3,C'C'                                                  
         MVI   PPROGNM+8,C'C'                                                   
         MVI   PTIME+5,C'C'                                                     
         MVI   PDAY+3,C'R'                                                      
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,4(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,49(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' NETWORK INTERFACE TAPE'                                 
         SSPEC H2,52,C' ----------------------'                                 
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
NTWKTP   DCB   DDNAME=NTWKTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00200,                                            X        
               BLKSIZE=04000,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         LTORG                                                                  
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                                                                               
RELO     DS    F                                                                
ACLISTSV DS    F                                                                
NUMPER   DS    F                                                                
APERLIST DS    F                                                                
*                        TOTAL FIELDS                                           
TOTUNITS DS    PL3       UNITS                                                  
TOTCOST  DS    PL5       COST                                                   
BRK2FLG  DS    CL1                                                              
ESTSV    DS    CL1                                                              
*                                                                               
TOTDEMNM DS    CL7       8 X DEMO NAME AND VALUE                                
TOTDEMO  DS    PL4                                                              
         DS    CL7                                                              
         DS    PL4                                                              
         DS    CL7                                                              
         DS    PL4                                                              
         DS    CL7                                                              
         DS    PL4                                                              
         DS    CL7                                                              
         DS    PL4                                                              
         DS    CL7                                                              
         DS    PL4                                                              
         DS    CL7                                                              
         DS    PL4                                                              
         DS    CL7                                                              
         DS    PL4            END OF DEMO NAME/VALUE AREA                       
*                                                                               
PERLIST  DS    CL65                                                             
PERTYPE  DS    CL1                                                              
FRST     DS    CL1                                                              
BOXSET   DS    CL1                                                              
SEQNUM   DS    PL4                                                              
*                                                                               
PREVREC  DS    CL24                SAVE PREV REC FOR BREAK CHECKS               
RECWORK  DS    CL200               WORK AREA FOR TAPE RECORDS                   
SORTREC  DS    CL200               SO I CAN SEE SORTREC IN DUMPS                
*                                                                               
         EJECT                                                                  
*                                                                               
*        NETPAK REQUEST RECORD                                                  
*                                                                               
TAPED1   DSECT            DSECT FOR INTERFACE TAPE                              
TPMED1   DS    CL1                                                              
TPCLT1   DS    CL3                                                              
TPPRD1   DS    CL3                                                              
TPEST1   DS    CL3                                                              
TPRECOD1 DS    CL2                 C'00'                                        
         DS    CL4                                                              
TPNTWK1  DS    CL5                                                              
         DS    CL3                                                              
TPSTART1 DS    CL6                 NNNNA                                        
TPEND1   DS    CL6                 NNNNA                                        
         DS    CL152                                                            
TPREQNM  DS    CL12                REQUESTOR'S NAME                             
         EJECT                                                                  
*                                                                               
*        NETPAK SCHEDULE RECORD                                                 
*                                                                               
TAPED    DSECT            DSECT FOR INTERFACE TAPE                              
TPMED    DS    CL1                                                              
TPCLT    DS    CL3                                                              
TPPRD    DS    CL3                                                              
TPEST    DS    CL3                                                              
TPRECODE DS    CL2                 C'20'                                        
         DS    CL4                                                              
TPNTWK   DS    CL5                                                              
TPDAYPT  DS    CL1                                                              
TPSPTLN  DS    CL2                 PACKED                                       
TPACTDAT DS    CL6                 YYMMDD                                       
         DS    CL6                                                              
TPUNITS  DS    CL3                 NUMBER OF UNITS(ALWAYS=1)                    
TPCOST   DS    CL5                 PACKED                                       
TPDEMO   DS    CL7                 DEMO NAME                                    
TPDEMVAL DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
TPDEMTYP DS    CL1                 E=ESTIMATED/A=ACTUAL                         
TPPROGNM DS    CL16                PROGRAM NAME                                 
TPSTART  DS    CL5                 NNNNA                                        
TPEND    DS    CL5                 NNNNA                                        
TPAFFID  DS    CL5                 NNNNA                                        
TPDAY    DS    CL3                                                              
         DS    CL29                                                             
TPSEQNUM DS    CL4                 SEQUENCE NUMBER(PACKED)                      
         EJECT                                                                  
*                                                                               
*        NETPAK SUMMARY RECORD                                                  
*                                                                               
TAPED3   DSECT            DSECT FOR INTERFACE TAPE                              
TPMED3   DS    CL1                                                              
TPCLT3   DS    CL3                                                              
TPPRD3   DS    CL3                                                              
TPEST3   DS    CL3                                                              
TPRECOD3 DS    CL2                 C'21'                                        
         DS    CL4                                                              
TPNTWK3  DS    CL5                                                              
TPDAYPT3 DS    CL1                                                              
TPSPTLN3 DS    CL2                 PACKED                                       
TPSTART3 DS    CL6                 YYMMDD                                       
TPEND3   DS    CL6                 YYMMDD                                       
TPUNITS3 DS    CL3                 NUMBER OF UNITS(ALWAYS=1)                    
TPCOST3  DS    CL5                 PACKED                                       
TPDEMO3  DS    CL7                 DEMO NAME                                    
TPDEMVL3 DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
         DS    CL7                 DEMO NAME                                    
         DS    CL4                 DEMO VALUE(PACKED)                           
TPDEMTP3 DS    CL1                 E=ESTIMATED/A=ACTUAL                         
         DS    CL63                                                             
TPSEQNM3 DS    CL4                 SEQUENCE NUMBER(PACKED)                      
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
         DS    CL1                                                              
PCLT     DS    CL5                 CLIENT/ESTIMATE/NTWK(STACKED)                
         DS    CL1                                                              
PPRD     DS    CL3                 PROD CODE                                    
         DS    CL1                                                              
PDAYPT   DS    CL1                 DAYPART                                      
         DS    CL1                                                              
PSPTLN   DS    CL3                 SPOT LENGTH                                  
         DS    CL1                                                              
PACTDAT  DS    CL6                 BROADCAST DATE                               
         DS    CL1                                                              
PCOST    DS    CL10                COST(DECIMAL)                                
         DS    CL1                                                              
PDEMO    DS    CL8                 DEMO                                         
         DS    CL1                                                              
         DS    CL8                 DEMO                                         
         DS    CL1                                                              
         DS    CL8                 DEMO                                         
         DS    CL1                                                              
         DS    CL8                 DEMO                                         
         DS    CL1                                                              
         DS    CL8                 DEMO                                         
         DS    CL1                                                              
         DS    CL8                 DEMO                                         
         DS    CL1                                                              
         DS    CL8                 DEMO                                         
         DS    CL1                                                              
         DS    CL8                 DEMO                                         
         DS    CL1                                                              
PDEMTYPE DS    CL3                 E=EST/A=ACT                                  
         DS    CL1                                                              
PPROGNM  DS    CL8                 PROGRAM NAME                                 
         DS    CL1                                                              
PTIME    DS    CL5                 START/END/AFFID TIME(STACKED)                
         DS    CL1                                                              
PDAY     DS    CL3                 MON/TUE/ETC                                  
         DS    CL1                                                              
         EJECT                                                                  
         SPACE                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD9D                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMEDYKT  05/01/02'                                      
         END                                                                    
