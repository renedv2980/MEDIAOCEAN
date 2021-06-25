*          DATA SET NEMEDA9S   AT LEVEL 185 AS OF 05/01/02                      
*PHASE T31EA9A                                                                  
*INCLUDE DYNALLOC                                                               
         TITLE 'T31EA9-NETWORK INTERFACE TAPE'                                  
T31EA9   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEA9**,RR=R2                                                 
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
         LA    R5,1(RB)           R5=SECOND BASE REG                            
         LA    R5,4095(R5)                                                      
         USING T31EA9,RB,R5                                                     
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANTWKTP                                                       
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
         CLI   MODE,RUNLAST                                                     
         BE    LASTRUN                                                          
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
         ZAP   BBDOTOT,=P'0'                                                    
         ZAP   RUNACT,=P'0'                                                     
         ZAP   RUNASS,=P'0'                                                     
         XC    RUNUNITS,RUNUNITS                                                
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
         B     LR5                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,30,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
*                                                                               
DDNAME   DC    CL8'NTWKTP'                                                      
DSNAME   DC    CL20'NETTAPE.NE0OJAA1'                                           
DCBOPEN  DC    C'N'                                                             
*                                                                               
LR5      DS    0H                                                               
         CLI   SPLTAPE,C'Y'                                                     
         BNE   LR7                                                              
         L     RE,BOOKVAL                                                       
         CLC   =X'90EC',0(RE)                                                   
         BNE   LR7                                                              
         LA    RF,NTWKTP                                                        
         MVC   0(128,RE),0(RF)                                                  
         MVC   DSNAME+13(2),NBSELAGY                                            
         GOTO1 =V(DYNALLOC),DMCB,DDNAME,DSNAME                                  
         L     R2,ANTWKTP                                                       
         OPEN  ((R2),(OUTPUT))                                                  
*                                                                               
LR7      BAS   RE,GTWKLIST               GET WEEKLIST                           
         MVI   NBDATA,C'U'               SET UP FOR UNIT RECS                   
         MVI   NBSEQ,C'Q'                                                       
         MVI   NBSELUOP,C'A'       ACTUAL SCHEDULE                              
         MVI   NBSPLOPT,X'C0'       DO SPLIT 30'S                               
         NETGO NVDEMOPT,DMCB    RETURN EST DEMO FOR MAKEGOOD/NOT PFB            
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
*                                                                               
LR25     CLC   ESTSV,NBACTEST            .IF EST HAS CHANGED                    
         BE    LR27                                                             
         BAS   RE,GETEST                 .GET ESTHEADER AND SET NDDEMOS         
         MVC   ESTSV,NBACTEST                                                   
         NETGO NBNETVAL,DMCB,NETBLOCK                                           
*                                                                               
LR27     BAS   RE,DOTAPE                 DO SCHEDULE REC                        
         BAS   RE,PUTSORT                                                       
         XC    NBACTUAL,NBACTUAL   WILL THIS CLEAR BUG                          
         XC    NBASSIGN,NBASSIGN                                                
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
         BAS   RE,WRITESCD               WRITE REPORT LINE                      
         MVI   BRK2FLG,0                                                        
         B     LR30                      GET NEXT REC FROM SORT                 
*                                                                               
LR40     DS    0H                                                               
         CLI   PREVREC,0           IS THERE ANY DATA                            
         BE    LRX                                                              
         BAS   RE,DOTAPE3                                                       
         MVC   P+10(16),=C'RUN TOTAL UNITS='                                    
         MVC   P2+10(22),=C'RUN TOTAL ACTUAL COST='                             
         MVC   P3+10(22),=C'RUN TOTAL ASSIGN COST='                             
         EDIT  (B4,RUNUNITS),(9,P+27),ALIGN=LEFT                                
         EDIT  (P10,RUNACT),(17,P2+33),2,ALIGN=LEFT                             
         EDIT  (P10,RUNASS),(17,P3+33),2,ALIGN=LEFT                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLC   NBSELAGY,=C'BD'                                                  
         BNE   LRX                                                              
         MVC   P+5(27),=C'*** REPORT DOLLAR TOTALS = '                          
         EDIT  (P10,BBDOTOT),(17,P+33),2,ALIGN=LEFT                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
LRX      B     EXIT                                                             
         SPACE                                                                  
*                                                                               
LASTRUN  DS    0H                                                               
         L     R2,ANTWKTP                                                       
         CLC   =X'90EC',0(R2)                                                   
         BE    LASTRX                                                           
         CLOSE ((R2),)                                                          
LASTRX   B     EXIT                                                             
         EJECT                                                                  
*****************************************                                       
* ROUTINE SETS DATA FROM NETBLOCK TO                                            
*  TAPE NETWORK SCHEDULE RECORD FOR SORT                                        
******************************************                                      
DOTAPE   NTR1                                                                   
         MVC   FULL,NBACTUAL       FOR RUN TOTALS                               
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    RUNACT,DUB                                                       
         MVC   FULL,NBASSIGN                                                    
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    RUNASS,DUB                                                       
         L     R1,RUNUNITS                                                      
         LA    R1,1(R1)                                                         
         ST    R1,RUNUNITS                                                      
*                                                                               
         LA    R2,RECWORK                                                       
         USING TAPED,R2                                                         
         BAS   RE,SPECIALS         GETS SPECIAL CHARGES                         
         ZAP   TPCOST1,=P'0'                                                    
         ZAP   TPCOST,=P'0'                                                     
         MVI   TPMED,C'N'                      MEDIA                            
         GOTO1 NBCLUNPK,DMCB,NBACTCLI,TPCLT    CLIENT                           
         MVC   WORK(1),NBSPLPRN                PRODUCT                          
         BAS   RE,GETPRD                                                        
         MVC   TPPRD,WORK+10                       SET PRODUCT                  
         EDIT  (B1,NBACTEST),(3,TPEST),ALIGN=LEFT  ESTIMATE                     
         MVC   TPRECODE,=C'20'                     REC CODE                     
         MVC   TPNTWK(4),NBACTNET                  NETWORK                      
         MVC   TPDAYPT,NBACTDP                     DAYPART                      
         EDIT  (B1,NBLEN),(4,FULL)                 SPOTLENGTH                   
         PACK  TPSPTLN,FULL                        SET SOTLENGTH                
         MVC   TPPKGNM,NBPACK                      PACKAGE NUMBER               
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,TPACTDAT)  DATE                      
         CLI   INTGFLG,C'Y'                                                     
         BNE   DOT1                                                             
         EDIT  (B4,NBINTEG),(10,WORK+20)                                        
         PACK  TPINTEG,WORK+20(10)         INTEGRATION                          
         OC    NBINTEG,NBINTEG                                                  
         BNZ   DOT1                                                             
         ZAP   TPINTEG,=P'0'                                                    
DOT1     LA    R4,NBACTUAL                                                      
         CLI   DOLLARS,2           *** 1=ACT,2=ASS,3=BOTH ***                   
         BE    DOT2                                                             
         EDIT  (B4,NBACTUAL),(10,WORK+20)                                       
         PACK  TPCOST,WORK+20(10)         ACTUAL                                
         OC    0(4,R4),0(R4)       IF NO DOLLARS                                
         BNZ   DOT2                                                             
         ZAP   TPCOST,=P'0'        THEN ZAP COST FOR TOTS ADD                   
DOT2     DS    0H                                                               
         CLI   DOLLARS,1                                                        
         BE    DOT3                                                             
         EDIT  (B4,NBASSIGN),(10,WORK+20)                                       
         PACK  TPCOST1,WORK+20(10)          ASSIGNED                            
         OC    NBASSIGN,NBASSIGN                                                
         BNZ   DOT3                                                             
         ZAP   TPCOST1,=P'0'        ZAP COST FOR TOTS ADD                       
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
         MVC   TPFEED,NBFEED       FEED PERCENTAGE                              
         BAS   RE,PROMO            CHECK IF PROMO ELEMENT                       
         MVC   TPFEED+2(4),WORK    (FOR BACARDI FOR NOW)                        
*                                                                               
         CLC   NBSELAGY,=C'BD'     IF IT'S BBDO                                 
         BNE   DOT6B                                                            
         BAS   RE,BBDO             DO SPECIAL BBDO RTNS                         
         MVC   TPDAY+3(8),WORK     SET IN FILM NUMBER                           
*                                                                               
*                                   CONTINUE REGULAR PROCESSING                 
DOT6B    MVC   DBCOMFCS,ACOMFACS         SET UP FOR DEMCON                      
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
DOT10    MVI   MYBYTE,0                                                         
         LA    R2,TPDEMO        ** NOTE R2 NOW POINTS TO DEMO FLDS              
         DROP  R2                                                               
         SR    R4,R4                                                            
*DOT15    CLI   NDNDEMOS,0 *****                                                
*         BE    DOT16     *****DOT15 WASS ON NETGO                              
DOT15    NETGO NVDEMCON,DMCB,((R4),NDDEMBLK),DBLOCK,(7,WORK)                    
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
         LA    R4,1(R4)                                                         
         LA    R3,8(R3)                                                         
         ZIC   R1,MYBYTE                                                        
         LA    R1,1(R1)                                                         
         STC   R1,MYBYTE                                                        
*        CLC   MYBYTE,NDNDEMOS ***                                              
*        BE    DOTX          ***                                                
         CLI   MYBYTE,8            8=MAX NUM OF DEMOS                           
         BL    DOT15                                                            
*                                                                               
DOTX     B     EXIT                                                             
         EJECT                                                                  
*****************************************                                       
* SPECIAL DATA FOR BBDO TAPE                                                    
*     OUPUT: RETURNS FILM NO IN WORK(8)                                         
*            ADDS DOLLARS TO OVERALL COST                                       
*                                                                               
BBDO     NTR1                                                                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   BBDOX                                                            
         USING NUCMLEL,R2                                                       
         XC    WORK(8),WORK                                                     
         CLC   NBSPLPRN,NBPRD      IF 1ST PRD/1ST CML                           
         BNE   *+10                                                             
         MVC   WORK(8),NUCML1                                                   
         CLC   NBSPLPRN,NBPRD2    ELSE SECOND OR SKIP                           
         BNE   *+10                                                             
         MVC   WORK(8),NUCML2                                                   
         LA    R3,RECWORK                                                       
         USING TAPED,R3                                                         
         AP    BBDOTOT,TPCOST      ADD DOLLARS TO REPORT TOTS                   
BBDOX    XIT1                                                                   
         DROP  R2,R3                                                            
         SPACE                                                                  
* SPECIAL DATA FOR PROMOTION CODE (FOR MCANN FOR BACARDI)                       
*     OUPUT: RETURNS PROMO NO IN WORK(4)                                        
*                                                                               
PROMO    NTR1                                                                   
         XC    WORK(4),WORK                                                     
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROMX                                                            
         USING NUOTH,R2                                                         
         B     PROM4                                                            
PROM3    BAS   RE,NEXTEL                                                        
         BNE   PROMX                                                            
PROM4    CLI   NUOTTYP,C'P'                                                     
         BNE   PROM3                                                            
         MVC   WORK(4),NUOTHER                                                  
PROMX    XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*  GETS SPECIAL RATES                                                           
SPECIALS NTR1                                                                   
         LA    R4,RECWORK                                                       
         USING TAPED,R4                                                         
         ZAP   TPSPCHG,=P'0'                                                    
         OC    NBSPCHRG,NBSPCHRG                                                
         BZ    SPX                                                              
         MVC   FULL,NBSPCHRG                                                    
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    TPSPCHG,DUB                                                      
SPX      XIT1                                                                   
         DROP  R4                                                               
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
         L     R1,ANTWKTP                                                       
         PUT   (R1),RECWORK                   WRITE TAPE                        
*                                                                               
*        GOTO1 HEXOUT,DMCB,(R3),(R2),200                                        
*        BAS   RE,SPOOLIT                                                       
*        MVC   P(200),0(R3)                                                     
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
         L     R4,APERLIST                                                      
         GOTO1 DATCON,DMCB,(2,0(R4)),(0,TPSTART3)                               
         GOTO1 DATCON,DMCB,(2,2(R4)),(0,TPEND3)                                 
         MVC   TPUNITS3,TOTUNITS                                                
         MVC   TPCOST3,TOTCOST                                                  
         MVC   TPCOST31,TOTCOST1                                                
         MVC   TPINTEG3,TOTINTEG                                                
         MVC   TPSPCHG3,TOTSPCHG                                                
         MVI   TPDEMTP3,C'E'                                                    
         CLI   SPLDEM,C'E'                                                      
         BE    *+8                                                              
         MVI   TPDEMTP3,C'A'                                                    
         AP    SEQNUM,=P'1'                                                     
         MVC   TPSEQNM3,SEQNUM                                                  
*                                                                               
         LA    R2,TPDEMO3                                                       
         LA    R4,TOTDEMNM                                                      
         MVI   MYBYTE,0                                                         
SUM5     MVC   0(11,R2),0(R4)                                                   
         LA    R4,11(R4)           BUMP TOTS                                    
         LA    R2,11(R2)           BUMP OUT FLD                                 
         ZIC   R1,MYBYTE                                                        
         LA    R1,1(R1)                                                         
         STC   R1,MYBYTE                                                        
         CLI   MYBYTE,8            8=MAX NUM OF DEMOS                           
         BL    SUM5                                                             
*                                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PCLT(3),TPCLT3                                                   
         MVC   PCLT+132(3),TPEST3                                               
         MVC   PCLT+264(5),TPNTWK3                                              
         MVC   PPRD,TPPRD3                                                      
         MVC   PDAYPT,TPDAYPT3                                                  
         EDIT  (P2,TPSPTLN3),(3,PSPTLN)                                         
****     MVC   PACTDAT(12),TPSTART3       START/END DATE                        
         MVC   PACTDAT(6),TPSTART3                                              
         MVI   PACTDAT+6,C'-'                                                   
         MVC   PACTDAT+7(6),TPSTART3+6                                          
         MVC   PPROGNM(5),=C'COST='                                             
         LA    R4,PPROGNM+5                                                     
         CLI   DOLLARS,2                                                        
         BNE   SUM6                                                             
         EDIT  (P5,TPCOST31),(10,(R4)),2,ALIGN=LEFT                             
         B     SUM7                                                             
SUM6     DS    0H                                                               
         LA    R4,PPROGNM+5                                                     
         EDIT  (P5,TPCOST3),(10,(R4)),2,ALIGN=LEFT                              
         CLI   DOLLARS,3           BOTH                                         
         BNE   SUM7                                                             
         LA    R4,132(R4)                                                       
         EDIT  (P5,TPCOST31),(10,(R4)),2,ALIGN=LEFT                             
*SUM7     LA    R4,PPROGNM+132                                                  
*        CLI   DOLLARS,3                                                        
*        BNE   *+8                                                              
SUM7     LA    R4,127(R4)                                                       
         CLI   INTGFLG,C'Y'                                                     
         BNE   SUM9                                                             
         MVC   0(5,R4),=C'INTG='                                                
         EDIT  (P5,TPINTEG3),(10,5(R4)),2,ALIGN=LEFT                            
         LA    R4,132(R4)                                                       
SUM9     MVC   0(5,R4),=C'UNIT='                                                
         LA    R4,5(R4)                                                         
         EDIT  (P3,TPUNITS3),(4,(R4)),ALIGN=LEFT                                
         MVC   PDEMTYPE,TPDEMTP3                                                
* DEMOS                                                                         
         MVI   MYBYTE,0                                                         
         LA    R4,TPDEMO3                                                       
         LA    R2,PDEMO            *** NOTE R2 NOW =DEMOS                       
SUM10    MVC   1(7,R2),0(R4)              STACK DEMONM/DEMVALUE                 
         CLC   7(4,R4),=X'0000000C'                                             
         BE    SUM12                                                            
         EDIT  (P4,7(R4)),(8,132(R2)),1                                         
SUM12    LA    R2,9(R2)                                                         
         LA    R4,11(R4)                                                        
         ZIC   R1,MYBYTE                                                        
         LA    R1,1(R1)                                                         
         STC   R1,MYBYTE                                                        
         CLI   MYBYTE,8            8=MAX NUM OF DEMOS                           
         BL    SUM10                                                            
         BAS   RE,SPOOLIT                 WRITE TO PRINT LINE                   
         LA    R2,P                                                             
         CLC   TPSPCHG3,=X'000000000C'    SPECIAL CHARGES                       
         BE    SUM13                                                            
         LA    R4,PPROGNM+5                                                     
         MVC   PPROGNM+1(4),=C'SPC='                                            
         EDIT  (P5,TPSPCHG3),(10,0(R4)),2,ALIGN=LEFT                            
         BAS   RE,SPOOLIT                                                       
*                                                                               
*        LA    R2,P                RESET R2 TO P LINE                           
*        GOTO1 HEXOUT,DMCB,(R3),(R2),200                                        
*        BAS   RE,SPOOLIT                                                       
*        MVC   P(200),0(R3)                                                     
*        BAS   RE,SPOOLIT                                                       
*                                                                               
SUM13    MVI   SPACING,3                                                        
         BAS   RE,SPOOLIT                                                       
         CLI   SPLTAPE,C'Y'                                                     
         BNE   SUM15                                                            
         L     R1,ANTWKTP                                                       
         PUT   (R1),RECWORK             WRITE TAPE                              
SUM15    MVI   RECWORK,C' '               CLEAR RECWORK                         
         MVC   RECWORK+1(199),RECWORK                                           
SUMX     B     EXIT                                                             
         DROP  R2                                                               
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
         AP    SEQNUM,=P'1'                                                     
         MVC   TPSEQNUM,SEQNUM                                                  
         CLI   BRK2FLG,C'Y'        IS IT AFTER A KEY BREAK                      
         BNE   WRT10               IF NOT/SKIP KEY DATA FOR PRINT               
         MVC   PCLT(3),TPCLT                                                    
         MVC   PCLT+132(3),TPEST                                                
         MVC   PCLT+264(5),TPNTWK                                               
         MVC   PPRD,TPPRD                                                       
         MVC   PPRD+132(4),=C'PKG='                                             
         LA    R4,PPRD+136                                                      
         EDIT  (B1,TPPKGNM),(3,0(R4)),ALIGN=LEFT                                
         MVC   PDAYPT,TPDAYPT                                                   
         EDIT  (P2,TPSPTLN),(3,PSPTLN)                                          
WRT10    MVC   PACTDAT,TPACTDAT                                                 
         LA    R4,TPCOST1                                                       
         CLI   DOLLARS,2           IS IT ASSIGNED COST                          
         BE    *+8                                                              
         LA    R4,TPCOST                                                        
         EDIT  (P5,0(R4)),(10,PCOST),2                                          
         LA    R4,PCOST+132                                                     
WRT10AA  CLI   DOLLARS,3                     IS IT BOTH COSTS                   
         BNE   WRT10A                                                           
         EDIT  (P5,TPCOST1),(10,0(R4)),2      YES/ASSIGN TO NEXT LINE           
         LA    R4,132(R4)                                                       
WRT10A   CLI   INTGFLG,C'Y'        INTEGRATION OPTION                           
         BNE   WRT10C                                                           
         MVC   0(4,R4),=C'INT='                                                 
         EDIT  (P5,TPINTEG),(6,4(R4)),2                                         
         LA    R4,132(R4)                                                       
WRT10C   CLC   TPSPCHG,=X'000000000C'    SPECIAL CHARGES                        
         BE    WRT10D                                                           
         MVC   0(4,R4),=C'SPC='                                                 
         EDIT  (P5,TPSPCHG),(10,4(R4)),2,ALIGN=LEFT                             
WRT10D   MVC   PDEMTYPE(1),TPDEMTYP                                             
         MVC   PPROGNM,TPPROGNM                                                 
         MVC   PTIME,TPSTART                                                    
         MVC   PTIME+132,TPEND                                                  
         MVC   PTIME+264,TPAFFID                                                
         MVC   PDAY,TPDAY                                                       
         SR    R1,R1                                                            
         ICM   R1,3,TPFEED                                                      
         BZ    CHKPROMO                                                         
         LA    R4,PDEMO                                                         
         A     R4,=F'280'                                                       
         MVC   0(5,R4),=C'FEED='                                                
         EDIT  (R1),(6,5(R4)),2                                                 
*                                                                               
CHKPROMO OC    TPFEED+2(4),TPFEED+2       IS THERE PROMO NUMBER                 
         BZ    CHKBD                      FOR NOW IT PRINTS IN                  
         LA    R1,PDEMO                   SAME AREA AS BBDO FILM                
         A     R1,=F'264'                                                       
         MVC   0(6,R1),=C'PROMO='                                               
         MVC   7(4,R1),TPFEED+2                                                 
*                                                                               
CHKBD    CLC   NBSELAGY,=C'BD'     IS IT BBDO                                   
         BNE   DEMOS                                                            
         LA    R1,PDEMO                                                         
         A     R1,=F'264'                                                       
         MVC   0(5,R1),=C'FILM='                                                
         MVC   6(8,R1),TPDAY+3                                                  
*                                  CONTINUE REGULAR PROCESSING                  
DEMOS    MVI   MYBYTE,0                                                         
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
         ZIC   R1,MYBYTE                                                        
         LA    R1,1(R1)                                                         
         STC   R1,MYBYTE                                                        
         CLI   MYBYTE,8            8=MAX DEMO NUM                               
         BL    DEMO5                                                            
         BAS   RE,SPOOLIT                 WRITE TO PRINT LINE                   
*                                                                               
*        LA    R2,P                       RESET R2 TO PRINT LINE                
*        GOTO1 HEXOUT,DMCB,(R3),(R2),200                                        
*        BAS   RE,SPOOLIT                                                       
*        MVC   P(200),0(R3)                                                     
*        BAS   RE,SPOOLIT                                                       
*                                                                               
         CLI   SPLTAPE,C'Y'                                                     
         BNE   WRTX                                                             
         L     R1,ANTWKTP                                                       
         PUT   (R1),(R3)                 WRITE TAPE                             
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
         L     R4,APERLIST                                                      
         CLC   HALF,2(R4)           ACTDAT GRTR THAN WEEK END DAT               
         BNH   BRKX                                                             
BREAK    BAS   RE,DOTAPE3        * DATE BREAK                                   
         BAS   RE,CLEARTOT                                                      
         L     R4,APERLIST         BUMP TO NEXT PERIOD IN PERLIST               
         LA    R4,4(R4)                                                         
         ST    R4,APERLIST                                                      
         B     BRK1                IS IT WITHIN NXT PERIOD                      
*                                  IN CASE MORE THAN ONE PERIOD IS              
*                                  EMPTY OF UNITS                               
*                                                                               
BREAK2   DS    0H          *  BREAK OF CLT/PRD/EST/NTWK/DAYPT/SPTLN             
         BAS   RE,DOTAPE3                                                       
         MVI   FORCEHED,C'Y'     FORCR NEW KEY TO PAGE TOP                      
         BAS   RE,CLEARTOT                                                      
         MVC   PREVREC,0(R3)              SET NEW PREV REC                      
         MVI   BRK2FLG,C'Y'       SET FLG TO PRINT KEY DATA ON REPORT           
         LA    R4,PERLIST      RESET APERLIST TO START DATE OF NEW REC          
         L     R2,NUMPER                                                        
         GOTO1 DATCON,DMCB,(0,TPACTDAT),(2,HALF)                                
BREAK2A  CLC   HALF,2(R4)                                                       
         BNH   BREAK2B                                                          
         LA    R4,4(R4)                                                         
         BCT   R2,BREAK2A                                                       
BREAK2B  ST    R4,APERLIST                                                      
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
         AP    TOTCOST,TPCOST      ACT OR ASSIGN                                
         AP    TOTCOST1,TPCOST1    ASSIGN ( IF BOTH)                            
         AP    TOTSPCHG,TPSPCHG    SPECIAL CHARGES                              
         CLI   INTGFLG,C'Y'                                                     
         BNE   *+10                                                             
         AP    TOTINTEG,TPINTEG    INTEGRATION                                  
         MVI   MYBYTE,0                                                         
         LA    R2,TOTDEMNM                                                      
         CLI   0(R2),0             IF NAMES ALREADY SET/SKIP                    
         BNE   ADD10                                                            
         LA    R4,TPDEMO                                                        
ADD5     MVC   0(7,R2),0(R4)                                                    
         LA    R4,11(R4)                                                        
         LA    R2,11(R2)                                                        
         ZIC   R1,MYBYTE                                                        
         LA    R1,1(R1)                                                         
         STC   R1,MYBYTE                                                        
         CLI   MYBYTE,8            8=MAX NUM OF DEMOS                           
         BL    ADD5                                                             
ADD10    MVI   MYBYTE,0                                                         
         LA    R2,TOTDEMO                                                       
         LA    R4,TPDEMVAL                                                      
ADD12    AP    0(4,R2),0(4,R4)                                                  
         LA    R2,11(R2)                                                        
         LA    R4,11(R4)                                                        
         ZIC   R1,MYBYTE                                                        
         LA    R1,1(R1)                                                         
         STC   R1,MYBYTE                                                        
         CLI   MYBYTE,8            8=MAX NUM FO DEMOS                           
         BL    ADD12                                                            
         B     EXIT                                                             
         SPACE 2                                                                
***************************************                                         
* ROUTINE CLEARS AND ZAPS TOTAL FIELDS                                          
*                                                                               
CLEARTOT NTR1                                                                   
         ZAP   TOTCOST,=P'0'                                                    
         ZAP   TOTCOST1,=P'0'                                                   
         ZAP   TOTINTEG,=P'0'                                                   
         ZAP   TOTUNITS,=P'0'                                                   
         ZAP   TOTSPCHG,=P'0'                                                   
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
         MVC   NDDEMOS,EDEMLST                                                  
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
         MVI   NBRESUME,NBPROCPK                                                
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
         LA    R3,60               FOR 60 WEEKS MAX                             
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
****     MVC   PDAYPT+263(3),=C'DPT'                                            
         MVI   PDAYPT,C'D'                                                      
         MVI   PDAYPT+132,C'P'                                                  
         MVI   PDAYPT+264,C'T'                                                  
         MVC   PSPTLN+264(3),=C'SLN'                                            
         MVC   PACTDAT+264(4),=C'DATE'                                          
         CLI   DOLLARS,2                                                        
         BNE   HD3                                                              
         MVC   PCOST+264(10),=C'ASSGN COST'                                     
         B     HD5                                                              
HD3      MVC   PCOST+264(8),=C'ACT COST'                                        
         CLI   DOLLARS,3           IS IT BOTH                                   
         BNE   HD5                                                              
         MVC   PCOST+264(9),=C'ACT/ASSGN'                                       
HD5      MVC   PDEMTYPE+264(3),=C'DEM'                                          
         MVC   PPROGNM+264(4),=C'PROG'                                          
         MVC   PTIME(5),=C'START'                                               
         MVC   PTIME+134(3),=C'END'                                             
         MVC   PTIME+264(5),=C'AFFID'                                           
         MVC   PDAY+264(3),=C'DAY'                                              
         LA    R4,8                                                             
         LA    R2,PDEMO+264                                                     
SETDEM   MVC   0(4,R2),=C'DEMO'                                                 
         LA    R2,9(R2)                                                         
         BCT   R4,SETDEM                                                        
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
         LA    R4,BOXCOLS                                                       
         USING PLINED,R4                                                        
         MVI   0(R4),C'L'                                                       
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
         LA    R4,BOXROWS                                                       
         LA    R4,8(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,4(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' NETWORK INTERFACE TAPE'                                 
         SSPEC H2,52,C' ----------------------'                                 
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
         GETEL (R2),DATADISP,ELCODE                                             
         SPACE 2                                                                
         LTORG                                                                  
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                                                                               
*                     ** = FROM EDIT MODULE                                     
RELO     DS    F      **                                                        
ACLISTSV DS    F      **                                                        
NUMPER   DS    F      **                                                        
APERLIST DS    F      **                                                        
DOLLARS  DS    CL1    **                                                        
INTGFLG DS     CL1    **                                                        
*                                                                               
TOTUNITS DS    PL3       UNITS                                                  
TOTCOST  DS    PL5       COST                                                   
BRK2FLG  DS    CL1                                                              
ANTWKTP  DS    F                                                                
TOTCOST1 DS    PL5       COST(ASSIGNED IF BOTH COSTS)                           
TOTINTEG DS    PL5       INTEGRATION                                            
TOTSPCHG DS    PL5       SPECIAL CHARGES                                        
ESTSV    DS    CL1                                                              
DEMOPT   DS    CL1                                                              
BBDOTOT  DS    PL10                                                             
RUNUNITS DS    F                                                                
RUNACT   DS    PL10                                                             
RUNASS   DS    PL10                                                             
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
PERLIST  DS    CL300                                                            
PERTYPE  DS    CL1                                                              
FRST     DS    CL1                                                              
BOXSET   DS    CL1                                                              
MYBYTE   DS    CL1                                                              
SEQNUM   DS    PL4                                                              
*                                                                               
PREVREC  DS    CL24                SAVE PREV REC FOR BREAK CHECKS               
RECWORK  DS    CL200               WORK AREA FOR TAPE RECORDS                   
SORTREC  DS    CL200               SO I CAN SEE SORTREC IN DUMPS                
*                                                                               
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
       ++INCLUDE NETAPED                                                        
         PRINT OFF                                                              
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
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'185NEMEDA9S  05/01/02'                                      
         END                                                                    
