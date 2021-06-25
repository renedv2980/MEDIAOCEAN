*          DATA SET RESFM16S   AT LEVEL 220 AS OF 06/24/02                      
*PHASE T81816A,*                                                                
         TITLE 'T81816 - RESFM16 - A.U.R. REPORT:  COMBO/MARKET'                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM16 (T81816) --- A.U.R. REPORT: COMBO/MARKET         *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JUN17/93 (BU ) --- ORIGINAL ENTRY                               *             
*                                                                 *             
* JUN28/93 (BU ) --- REPORT GENERATOR SECTION                     *             
*                                                                 *             
* JUL01/93 (BU ) --- ADDITIONAL COMBO TOTAL SECTION               *             
*                                                                 *             
* JUL14/93 (BU ) --- SUPPRESS 'STATION AVERAGES' PAGE, SUPPRESS   *             
*                    '-C' STATIONS.                               *             
*                                                                 *             
* JUL16/93 (BU ) --- EXPAND AGENCY/AGENCY-OFFICE FILTER           *             
*                                                                 *             
* AUG06/93 (BU ) --- FIX NO-SHOW OF DAYPARTS WITH NO SUB-DYPT     *             
*                                                                 *             
* SEP03/98 (SKU) --- FIX MARKET NAME DISPLAY                      *             
*                                                                 *             
* JUL13/99 (AST) --- LOOK FOR 7/13/99                             *             
*                                                                 *             
*                    **  END TOMBSTONE  **                        *             
*******************************************************************             
*                                                                               
T81816   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1816**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R2,RELO                                                          
         SPACE                                                                  
         LA    RF,SAVEREGS                                                      
         STM   R2,RC,0(RF)         SAVE REGS 2 -> C                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PRNTREPT                                                         
         B     XIT                                                              
         EJECT                                                                  
XIT      XIT1                                                                   
         SPACE 2                                                                
FOOT     ST    RE,FULL             MOVE SCREEN INPUT TO FOOT LINE               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCFUT                                                        
         LA    R5,1(RE,R5)                                                      
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         ST    R5,SVFOOTF                                                       
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
MVCFUT   MVC   0(0,R5),8(R2)       MOVE SCREEN INPUT TO FOOT LINE               
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PRNTREPT EQU   *                                                                
         SPACE                                                                  
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,FHOOK                                                         
         ST    R1,FOOTHOOK                                                      
         MVI   FOOTLNS,1                                                        
         MVC   FOOTSW,FOOTLNS                                                   
*                                                                               
         L     R3,VADUMMY                                                       
         ST    R3,ABFTABLE                                                      
         A     R3,=F'40000'        LEAVE SPACE FOR LINES                        
         ST    R3,AAURTABL         A(AUR TABLE)                                 
         ST    R3,AAURNXT          A(NEXT SPACE IN TABLE)                       
         MVC   0(3,R3),=C'EOT'     SET TABLE TO 'EMPTY'                         
         L     R2,ABFTABLE         CLEAR OUT PRINT BUFFER                       
         LA    R3,150                                                           
PRRE0020 MVC   0(132,R2),SPACES                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,PRRE0020                                                      
         SPACE 1                                                                
         GOTO1 BUFFALO,DMCB,=C'SET',(0,ADBUFC)                                  
*                                                                               
         CLC   =C'MKT=',AUCSTA                                                  
         BNE   PRRE0030                                                         
         GOTO1 =A(GETMKT),RR=RELO                                               
*                                                                               
PRRE0030 DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'2C'                                                        
         MVC   KEY+AREPE(2),AGENCY     INSERT REP CODE                          
         MVC   KEY+AGRPE(2),GROUP  INSERT GROUP                                 
         MVC   KEY+ASTAE(15),STATN                                              
*                                  STA,REC TYPE,CONTYPE/OFF/AGY                 
         LA    RF,STATLIST         SET A(1ST STATION, IN PROGRESS)              
         ST    RF,ASTATN           SAVE IT                                      
         LA    RF,1                SET STATION IN PROGRESS #                    
         ST    RF,STATPROG                                                      
         L     RF,ASTATN           RELOAD A(STATION IN PROGRESS)                
         CLI   6(RF),C'C'          COMBO STATION?                               
         BNE   PRRE0140            NO  - PROCESS IT                             
*                                  YES - SKIP OVER IT                           
PRRE0040 L     RF,ASTATN           A(STATION IN PROGRESS)                       
         LA    RF,7(RF)            BUMP TO NEXT TABLE ENTRY                     
         ST    RF,ASTATN           STORE IT BACK                                
         CLI   0(RF),0             ANY ENTRY?                                   
         BE    PRRE0820            NO  - FINISHED                               
         XC    KEY+ATPEE(17),KEY+ATPEE                                          
*                                  CLEAR REMAINDER OF KEY                       
         MVC   KEY+AGRPE(2),0(RF)  LOAD GROUP TO KEY                            
*                                     IN CASE GROUP HAS CHANGED                 
         MVC   STATN,2(RF)         LOAD STATION TO 'SCREEN STATION'             
         MVC   KEY+ASTAE(15),STATN                                              
*                                  RELOAD KEY SELECTION CRITERION               
         L     RF,STATPROG         LOAD STATION IN PROGRESS #                   
         LA    RF,1(RF)            INCREMENT                                    
         ST    RF,STATPROG         STORE IT BACK                                
         L     RF,ASTATN           RELOAD A(STATION IN PROGRESS)                
         CLI   6(RF),C'C'          COMBO STATION?                               
         BE    PRRE0040            YES - SKIP OVER IT                           
*                                                                               
PRRE0060 EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(8),KEYSAVE     ID/REP/GROUP                                  
         BNE   PRRE0040            CHECK TABLE FOR ANOTHER STATION              
*                                                                               
         CLC   KEY+ASTAE(5),KEYSAVE+ASTAE                                       
         BE    PRRE0080            SAME STATION                                 
*                                                                               
*  READ STATION TO GET MARKET NAME AND IF NECESSARY,                            
*                    OWNER, TVB, AND RANK FILTER CHECK                          
*                                                                               
         BAS   RE,READSTA                                                       
         LTR   R3,R3               DOES STATION MEET FILTER CHECK               
         BZ    PRRE0040            NO                                           
*                                                                               
PRRE0080 EQU   *                                                                
         CLI   AGOFFFLG,X'0'       ANY AGENCY OFFICE CODE FLAG?                 
         BE    PRRE0090            NO  - TEST 7 CHARS....                       
         CLI   AGOFFFLG,C'O'       YES - ANY AGENCY OFFICE CODE?                
         BNE   PRRE0095            NO  - TEST 5 CHARS....                       
PRRE0090 EQU   *                                                                
         CLC   KEY+ATPEE(07),FILTER    CORRECT FILTER AND CODE                  
         BE    PRRE0100                YES                                      
         BH    PRRE0040                MOVE TO NEXT STATION                     
         B     PRRE0099                                                         
PRRE0095 EQU   *                                                                
         CLC   KEY+ATPEE(05),FILTER    CORRECT FILTER AND CODE                  
         BE    PRRE0100                YES                                      
         BH    PRRE0040                MOVE TO NEXT STATION                     
PRRE0099 EQU   *                                                                
         MVC   KEY+ATPEE(07),FILTER    MAYBE WE CAN FIND IT                     
         XC    KEY+ADPTE(7),KEY+ADPTE     A BIT FURTHER                         
         B     PRRE0060                                                         
*                                                                               
PRRE0100 CLI   DPTCNT,0                                                         
         BE    PRRE0120                                                         
         XC    KEY+ADPTE(7),KEY+ADPTE                                           
         LA    R3,DPT              YES, RESET TO START OF FILTERS               
         ST    R3,SVADPTF                                                       
*                                                                               
PRRE0120 CLI   LENCNT,0                                                         
         BE    PRRE0140                                                         
         XC    KEY+ASLNE(4),KEY+ASLNE                                           
         LA    R3,LEN                                                           
         ST    R3,SVALENF                                                       
*                                                                               
         SPACE 1                                                                
PRRE0140 CLI   DPTCNT,0            DAYPART FILTER                               
         BE    PRRE0220                                                         
         LA    R3,DPT                                                           
         ST    R3,SVADPTF                                                       
PRRE0160 CLI   0(R3),X'FE'         PROGRAM TYPE?                                
         BE    *+14                                                             
         MVC   KEY+ADPTE(2),0(R3)   1ST DAYPART - R3 ALSO SET BELOW             
         B     PRRE0180                                                         
*                                                                               
         MVC   KEY+ADPTE(2),=X'FEFE'                                            
         MVC   KEY+APRGE(1),1(R3)                                               
*                                                                               
PRRE0180 CLI   LENCNT,0            LENGTH FILTER                                
         BE    PRRE0220                                                         
         LA    R3,LEN                                                           
         ST    R3,SVALENF                                                       
PRRE0200 MVC   KEY+ASLNE(2),0(R3)      1ST LENGTH                               
         MVC   KEY+AYME(2),PSMONTH                                              
*                                                                               
PRRE0220 EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     PRRE0260                                                         
*                                                                               
PRRE0240 EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
PRRE0260 CLC   KEY(8),KEYSAVE      ID/REP/GROUP                                 
         BNE   PRRE0040            CHECK TABLE FOR ANOTHER STATION              
****     BAS   RE,TESTDUMP         **TEST**                                     
*                                                                               
         CLI   AGOFFFLG,X'0'       ANY AGENCY OFFICE CODE FLAG?                 
         BE    PRRE0270            NO  - TEST 12 CHARS....                      
         CLI   AGOFFFLG,C'O'       YES - ANY AGENCY OFFICE CODE?                
         BNE   PRRE0275            NO  - TEST 10 CHARS....                      
PRRE0270 EQU   *                                                                
         CLC   KEY+ASTAE(12),STATN                                              
*                                  SAME STATION/TYPE/TYPE CODE                  
         BNE   PRRE0040            NO  - CHECK TABLE FOR NEXT STATION           
         B     PRRE0280                                                         
PRRE0275 EQU   *                                                                
         CLC   KEY+ASTAE(10),STATN                                              
*                                  SAME STATION/TYPE/TYPE CODE                  
         BNE   PRRE0040            NO  - CHECK TABLE FOR NEXT STATION           
         SPACE 1                                                                
* FILTER RECORDS                                                                
PRRE0280 CLI   DPTCNT,0            DAYPART FILTER                               
         BE    PRRE0400                                                         
         SPACE 1                                                                
         L     R3,SVADPTF                                                       
         CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    PRRE0360                                                         
         CLC   KEY+ADPTE(2),0(R3)                                               
         BE    PRRE0400                                                         
         CLI   1(R3),0             IF BLANK PASS ALL 2NDARY DPT                 
         BNE   PRRE0300                                                         
         CLC   KEY+ADPTE(1),0(R3)                                               
         BE    PRRE0400                                                         
         SPACE 1                                                                
PRRE0300 LA    R3,2(R3)            NEXT ENTRY - CHECK IF IT = TO KEY            
         CLI   0(R3),0                                                          
         BNE   PRRE0320                                                         
         B     PRRE0040            CHECK TABLE FOR NEXT STATION                 
*                                                                               
PRRE0320 CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    PRRE0380                                                         
         ST    R3,SVADPTF                                                       
         CLI   1(R3),0             IF BLANK PASS ALL 2NDARY DPT                 
         BNE   PRRE0340                                                         
         CLC   KEY+ADPTE(1),0(R3)                                               
         BH    PRRE0300            KEY GT NEXT FILTER                           
         BE    PRRE0400                                                         
PRRE0340 CLC   KEY+ADPTE(2),0(R3)                                               
         BH    PRRE0300            KEY GT NEXT FILTER                           
         BE    PRRE0400                                                         
         XC    KEY+APRGE(5),KEY+APRGE     CLEAR TO END OF KEY                   
         B     PRRE0160                                                         
*                                                                               
PRRE0360 CLC   KEY+APRGE(1),1(R3)  PROGRAM TYPE                                 
         BE    PRRE0400                                                         
         SPACE 1                                                                
PRRE0370 LA    R3,2(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   PRRE0380            FINISH DAYPART FILTERS                       
         B     PRRE0040            CHECK TABLE FOR NEXT STATION                 
*                                                                               
PRRE0380 ST    R3,SVADPTF                                                       
         CLC   KEY+APRGE(1),1(R3)                                               
         BH    PRRE0370            KEY GT NEXT FILTER                           
         BE    PRRE0400                                                         
         XC    KEY+ASLNE(4),KEY+ASLNE       CLEAR TO END OF KEY                 
         B     PRRE0160                                                         
*                                                                               
PRRE0400 CLI   LENCNT,0            LENGTH FILTER                                
         BE    PRRE0460                                                         
         CLC   KEY+ADPTE(3),SVKEY+ADPTE                                         
         BE    *+12                                                             
         LA    R3,LEN              NEW DAYPART-RESTART LENGTH TABLE             
         ST    R3,SVALENF                                                       
         L     R3,SVALENF                                                       
         CLC   KEY+ASLNE(2),0(R3)                                               
         BE    PRRE0460                                                         
         BL    PRRE0200                                                         
PRRE0420 LA    R3,2(R3)                                                         
         OC    0(2,R3),0(R3)                                                    
         BNZ   PRRE0440            FINISH LENGTH FILTERS                        
         ZIC   RF,KEY+APRGE                                                     
         LA    RF,1(RF)                                                         
         STC   RF,KEY+APRGE        BUMP DAYPART                                 
         XC    KEY+ASLNE(4),KEY+ASLNE       CLEAR TO END OF KEY                 
         B     PRRE0220                                                         
*                                                                               
PRRE0440 ST    R3,SVALENF                                                       
         CLC   KEY+ASLNE(2),0(R3)                                               
         BH    PRRE0420            KEY GT FILTER NEXT FILTER                    
         BL    PRRE0200                                                         
*                                                                               
PRRE0460 CLC   KEY+AYME(2),PSMONTH                                              
         BNL   *+14                                                             
         MVC   KEY+AYME(2),PSMONTH                                              
         B     PRRE0220                                                         
*                                                                               
         MVI   PRIORSW,C'Y'                                                     
         CLC   KEY+AYME(2),PEMONTH                                              
         BNH   PRRE0480                                                         
*                                                                               
         MVI   PRIORSW,C'N'                                                     
         CLC   KEY+AYME(2),SMONTH                                               
         BNL   *+14                                                             
         MVC   KEY+AYME(2),SMONTH                                               
         B     PRRE0220                                                         
*                                                                               
         CLC   KEY+AYME(2),EMONTH                                               
         BNH   PRRE0480                                                         
         MVI   KEY+AYME,X'FF'      BUMP YEAR - CAN'T BUMP LENGTH                
         B     PRRE0220            IF PLAN, IT WOULD ALREADY BE X'FF'           
         EJECT                                                                  
*                                                                               
* GET RECORD - BUILD AND PUT TO BUFFALO.  AT LEVEL BREAKS, READ,PRINT,          
*               AND CLEAR.                                                      
         SPACE 2                                                                
PRRE0480 EQU   *                                                                
         CLI   REPTYP,C'L'         LENGTH FORMAT                                
         BE    PRRE0500                                                         
         CLI   REPTYP,C'P'         PERIOD FORMAT                                
         BE    PRRE0500                                                         
         MVC   CONHEAD(L'INVFMT),INVFMT                                         
         B     MYEND                                                            
         SPACE 1                                                                
PRRE0500 OC    SVKEY,SVKEY                                                      
         BNZ   PRRE0520            FIRST TIME                                   
         MVC   SVMKT,MARKET                                                     
         B     PRRE0600                                                         
         SPACE 1                                                                
*RRE0520 CLC   SVKEY(10),KEY        STATION CHANGED                             
PRRE0520 EQU   *                                                                
* TEST                                                                          
*        CLC   =X'2C000000C4F4D9D3D2D5E9D9C101000000000000C6C1',KEY             
*        BNE   *+6                                                              
*        DC    H'0'                                                             
* END TEST                                                                      
         CLC   SVKEY(13),KEY        STATION CHANGED, DU!!!                      
         BE    PRRE0540                                                         
*** NEW                                                                         
         BAS   RE,SUM1                                                          
         BAS   RE,SUM2                                                          
*** END NEW                                                                     
         MVI   CURDPTC,C' '                                                     
         MVC   CURDPT,SPACES                                                    
         MVC   CURPRG,SPACES                                                    
         CLI   TOTPAGE,C'Y'        TOTAL PAGE ONLY?                             
         BE    PRRE0600            YES - DON'T CHANGE MARKET NAME               
         MVC   SVMKT,MARKET                                                     
         B     PRRE0600                                                         
*                                                                               
PRRE0540 CLC   SVKEY(21),KEY       DPT CHANGED                                  
         BE    PRRE0560                                                         
** TEST                                                                         
*        MVC   P+1(07),=C'DPT CHA'                                              
*        MVC   P+20(56),BUFREC                                                  
*        GOTO1 SPOOL,DMCB,(R8)                                                  
** END TEST                                                                     
         BAS   RE,SUM1                                                          
         BAS   RE,SUM2                                                          
         BAS   RE,SUM3                                                          
         B     PRRE0600                                                         
*                                                                               
PRRE0560 CLC   SVKEY(22),KEY       SUB DPT CHANGED                              
         BNE   PRRE0600                                                         
*                                                                               
PRRE0580 CLC   SVKEY(23),KEY       PROGRAM TYPE CHANGED                         
         BE    PRRE0620                                                         
*                                                                               
PRRE0600 BAS   RE,READDPT          GET NEW DPT/SUB DPT/PROG TYPE NAME           
         B     PRRE0620                                                         
*                                                                               
*                                                                               
*         LEVEL 3 IS FOR STATION/LENGTH TOTALS                                  
*           *****  NOT BEING OUTPUT  *****                                      
*                                                                               
PRRE0620 EQU   *                                                                
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,C'3'         NOW BUILD NEW BUF RECORDS                    
         MVC   BUFSTA2,KEY+ASTAE   CARRY STATION AS MEMO ITEM                   
         L     RF,ASTATN           A(STATION IN PROGRESS)                       
         MVC   BUFSTA,2(RF)        INSERT STATION IN PROGRESS                   
         MVC   BUFLEN,KEY+ASLNE    SPOT LENGTH                                  
         CLC   KEY+ADPTE(3),=X'FEFEFC' PLAN                                     
         BNE   *+10                                                             
         MVC   BUFLEN,=X'FEFE'     INDICATE PLAN LENGTH                         
*                                                                               
         CLI   REPTYP,C'P'         PERIOD FORMAT                                
         BNE   PRRE0640                                                         
         TM    PRNTOPT,X'20'       LENGTH=TOT                                   
         BZ    *+10                                                             
         MVC   BUFLEN(2),=X'FFFF'  COMPRESS TO 1 LENGTH                         
         CLI   MONFMT,C'A'         ALL MONTH                                    
         BE    *+10                                                             
         MVC   BUFYR(2),KEY+AYME   YEAR/MONTH                                   
*                                                                               
         CLI   MONFMT,C'Q'         QUARTER FORMAT                               
         BNE   PRRE0640                                                         
         BAS   RE,GETQTR                                                        
         OI    BUFYR,X'F0'         INDICATE QUARTERS                            
         STC   R1,BUFMON                                                        
*                                                                               
PRRE0640 BAS   RE,GETUNDL                                                       
         CLI   PRIORSW,C'Y'                                                     
         BE    PRRE0660                                                         
         MVC   BUFUN,UNACCM                                                     
         MVC   BUFDL,DLACCM                                                     
         B     PRRE0680                                                         
*                                                                               
PRRE0660 MVC   BUFPUN,UNACCM                                                    
         MVC   BUFPDL,DLACCM                                                    
         CLI   MONFMT,C'A'         ALLMONTH                                     
         BE    PRRE0680                                                         
         ZIC   RE,BUFYR            ADJUST PRIOR YEAR TO MATCH CURRENT           
         AH    RE,=H'1'                                                         
         STC   RE,BUFYR                                                         
PRRE0680 EQU   *                                                                
*                                                                               
*         LEVEL 2 IS FOR STATION/PRIMARY DAYPART/LENGTH TOTALS                  
*         *** NOT BEING OUTPUT ***                                              
*                                                                               
         MVI   BUFTYP,C'2'                                                      
*                                                                               
         CLI   KEY+ADPTE,X'FF'      PSEUDO DAYPART                              
         BNE   PRRE0700                                                         
         MVC   BUFDPT(2),=X'FFFF'                                               
         XC    BUFDPTN,BUFDPTN     CLEAR DAYPART NAME AREA                      
         MVC   BUFDPTN(6),=C'PSEUDO'                                            
         B     PRRE0760                                                         
*                                                                               
PRRE0700 CLI   KEY+ADPTE,X'FE'      PROGRAM TYPE OR PLAN                        
         BNE   PRRE0740                                                         
         MVC   BUFDPT(2),=X'FEFE'                                               
         MVC   BUFDPT+2(1),KEY+APRGE                                            
         CLI   KEY+APRGE,X'FC'     PLAN                                         
         BNE   PRRE0720                                                         
         MVC   BUFDPTN(4),=C'PLAN'                                              
         B     PRRE0760                                                         
PRRE0720 MVC   BUFDPTN,CURPRG      PROGRAM TYPE NAME                            
         B     PRRE0760                                                         
*                                                                               
PRRE0740 MVC   BUFDPT(1),KEY+ADPTE   PRIMARY DAYPART                            
         MVC   BUFDPTN(5),CURDPT                                                
*                                                                               
PRRE0760 EQU   *                                                                
*                                                                               
*  LEVEL 1 IS FOR STATION/SUB DAYPART/LENGTH TOTALS                             
*                                                                               
         MVC   BUFTYP,STATPROG+3   LOAD STATION IN PROGRESS #                   
*                                                                               
PRRE0770 EQU   *                                                                
         CLC   CURSDPT,=C'XXXXX'   IF NO SUB DAYPART                            
         BE    PRRE0790               DON'T PLAY WITH NAME                      
*                                                                               
         CLC   =C'PSEUDO',BUFDPTN  PSEUDO?                                      
         BE    PRRE0790            YES - DON'T PLAY WITH NAME                   
*                                                                               
         LA    R0,5                SEPARATE NAMES WITH A /                      
         LA    RE,BUFDPTN+4                                                     
PRRE0780 CLI   0(RE),X'40'                                                      
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,PRRE0780                                                      
         MVI   1(RE),C'/'                                                       
         MVC   2(5,RE),CURSDPT                                                  
*                                                                               
         MVC   BUFSDPT(1),KEY+ASDTE    SUB DAYPART                              
PRRE0790 EQU   *                                                                
         GOTO1 =A(PUTBUFF),DMCB,(RC),0,RR=RELO                                  
         GOTO1 =A(GENTOTS),DMCB,(RC),RR=RELO   GEN TTL RECS TO BUFFALO          
*                                                                               
PRRE0800 EQU   *                                                                
** TEST                                                                         
*        OC    SVKEY,SVKEY                                                      
*        BZ    PRRE0801            FIRST TIME?                                  
*        XC    P,P                                                              
*        MVC   P+1(07),=C'PRRE800'                                              
*        MVC   P+20(L'KEY),KEY                                                  
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*        CLC   SVKEY(21),KEY       DPT CHANGED                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
** END TEST                                                                     
PRRE0801 MVC   SVKEY,KEY                                                        
         B     PRRE0240                                                         
*                                                                               
PRRE0820 EQU   *                                                                
         MVI   SAVEAUR,C'Y'        SET SAVE FLAG TO 'Y'                         
         MVI   AURTITLE,C'N'       SET HEADING DISPLAY FLAG                     
         L     R2,STATCTR          LOAD STATION COUNTER                         
         LA    RF,STATLIST         A(STATION LIST)                              
         ST    RF,ASTATN           A(STATION IN PROGRESS)                       
         LA    R1,1                LOOP CONTROL                                 
PRRE0840 EQU   *                                                                
         CR    R1,R2               ANY STATIONS LEFT?                           
         BH    PRRE0860            NO                                           
         BL    PRRE0850                                                         
         MVI   SAVEAUR,C'N'        LAST STATION:  DON'T SAVE                    
         CLI   COMBOFLG,C'Y'       COMBO REQUEST?                               
         BE    PRRE0860            YES - DON'T PRINT STATION AVGS               
*                                                                               
*   STATION AVERAGES ARE SKIPPED.  TO REINSTITUTE THIS PAGE, REMOVE             
*     THE 'B   PRRE0860' INSTRUCTION ABOVE.                                     
*                                                                               
PRRE0850 EQU   *                                                                
         STC   R1,BUFTYP           LOAD STATION IN PROGRESS                     
         BAS   RE,PRNTSUM                                                       
         LA    R1,1(R1)            BUMP TO NEXT STATION                         
         L     RF,ASTATN           BUMP STATION IN PROGRESS                     
         LA    RF,7(RF)                                                         
         ST    RF,ASTATN                                                        
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE TO PRINT                      
         B     PRRE0840            GO BACK FOR NEXT                             
PRRE0860 EQU   *                                                                
         CLI   COMBOFLG,C'Y'       COMBO REQUEST?                               
         BNE   PRRE0880            NO                                           
         GOTO1 =A(BUFCOMBO),DMCB,(RC),RR=RELO                                   
         MVI   AURTITLE,C'Y'                                                    
         MVI   BUFTYP,1            RESET BUFTYP TO 1                            
         BAS   RE,PRNTSUM                                                       
PRRE0880 EQU   *                                                                
         MVI   FORCEFUT,C'Y'       FORCE FOOT LINES TO PRINT                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  TESTDUMP:  TEST ROUTINE TO DUMP AFTER REACHING A CERTAIN POINT               
*     FOR A SPECIFIED STATION.                                                  
*                                                                               
TESTDUMP NTR1                                                                   
         CLC   =C'METLA',KEY+ASTAE                                              
*                                  ***   DUMP STATION   ***                     
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
*  ROUTINES TO INDICATE CORRECT LEVEL TO PRINT                                  
         SPACE 2                                                                
SUM1     ST    RE,SAVEE                                                         
         MVI   BUFTYP,C'1'                                                      
         BAS   RE,PRNTSUM                                                       
         L     RE,SAVEE                                                         
         BR    RE                                                               
*                                                                               
SUM2     ST    RE,SAVEE                                                         
         MVI   BUFTYP,C'2'                                                      
         BAS   RE,PRNTSUM                                                       
         L     RE,SAVEE                                                         
         BR    RE                                                               
*                                                                               
SUM3     ST    RE,SAVEE                                                         
         MVI   BUFTYP,C'3'                                                      
         BAS   RE,PRNTSUM                                                       
         L     RE,SAVEE                                                         
         BR    RE                                                               
         EJECT                                                                  
*  ROUTINE TO READ FROM BUFFALO AND PRINT OUT DATA                              
         SPACE 2                                                                
PRNTSUM  NTR1                                                                   
** TEST                                                                         
*        MVC   P+1(07),=C'PRNTSUM'                                              
*        MVC   P+20(56),BUFREC                                                  
*        GOTO1 SPOOL,DMCB,(R8)                                                  
** END TEST                                                                     
         XC    TOTACCM,TOTACCM     CLEAR OUT ACCUMULATORS                       
         XC    TOTACCM3,TOTACCM3     CLEAR OUT ACCUMULATORS                     
         XC    BUFREC+1(L'BUFREC-1),BUFREC+1                                    
         XC    SAVLEN,SAVLEN                                                    
*                                                                               
         L     R2,ABFTABLE                                                      
         USING PRNTD,R2                                                         
         SR    R3,R3               LINE COUNTER                                 
*                                                                               
         MVI   SAVDPT,X'FF'        FOR 1ST TIME PRINT                           
*                                                                               
PS50     EQU   *                                                                
         XC    BUFREC2,BUFREC2                                                  
         XC    BUFREC3,BUFREC3                                                  
*        MVC   P+1(11),=C'PS50='  **TEST                                        
*        L     R3,ADBUFC                                                        
*        MVC   P+15(117),0(R3)      **TEST                                      
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',(BUFTYP,ADBUFC),BUFREC,1                   
*                                                                               
         MVC   BYTE,DMCB+8                                                      
** TEST                                                                         
*        MVC   P+1(7),=C'PS50  ='        **TEST                                 
*        MVC   P+10(L'BUFREC),BUFREC     **TEST                                 
*        GOTO1 SPOOL,PARAS,(R8)          **TEST                                 
* TEST                                                                          
*        CLC   =X'2C000000C4F4D9D3D2D5E9D9C101000000000000C6C1',KEY             
*        BNE   *+6                                                              
*        DC    H'0'                                                             
* END TEST                                                                      
         TM    BYTE,X'80'                                                       
         BO    RXEXT               NO RECORDS FOUND                             
*        MVC   P+1(11),=C'PS50RETURN='  **TEST                                  
*        MVC   P+15(48),BUFREC     **TEST                                       
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
*                                                                               
*                                                                               
         CLI   COMBOFLG,C'Y'       COMBO STATION REQUEST?                       
         BNE   PS150               NO                                           
         GOTO1 PUTCOMBO,DMCB,(RC),RR=RELO                                       
         B     PS150                                                            
*                                                                               
PS100    EQU   *                                                                
*** NEW                                                                         
         OC    TOTACCM3,TOTACCM3     FIRST ONE?                                 
         BNZ   *+10                                                             
         MVC   TOTACCM3,BUFACCM    YES, START NEW TOTALS                        
*** END NEW                                                                     
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFTYP,ADBUFC),BUFREC,1                    
         TM    DMCB+8,X'80'                                                     
         BO    PS450               NO MORE RECORDS FOUND                        
*        MVC   P+1(12),=C'PS100RETURN='  ** TEST                                
*        MVC   P+40(L'BUFREC),BUFREC     ** TEST                                
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
*                                                                               
******** NEW                                                                    
         TM    PRNTOPT,X'80'       SUPPRESS SUB-DAYPART?                        
         BO    PS101               YES, ALWAYS ACCUMULATE                       
         CLI   BUFDPT,X'FF'        PSEUDO OR STA TOTAL?                         
         BE    PS101                                                            
         CLC   BUFTOTAL,=C'TOTS'   IS THIS SUMMARY DPT?                         
         BE    PS105               YES, DON'T ACCUMULATE                        
                                                                                
PS101    EQU   *                                                                
         CLC   =C'PSEUDO',BUFREC3+16 WAS LAST PSEUDO?                           
         BNE   *+14                NO,CONTINUE                                  
         CLC   =C'PSEUDO',BUFDPTN  IS CURRENT PSEUDO?                           
         BNE   PS104               LAST PSEUDO, CURRENT STA TOT,                
*                                  -- NOW MUST PRINT TOTAL                      
*                                                                               
         OC    BUFREC3,BUFREC3     FIRST ONE?                                   
         BZ    PS102               YES, JUST ACCUMULATE                         
         CLC   BUFDPT,BUFREC3+6    NEW DAYPART?                                 
         BNE   PS104                                                            
         CLC   =C'PSEUDO',BUFREC3+16 WAS LAST PSEUDO?                           
         BNE   PS102                                                            
*                                                                               
PS102    EQU   *                                                                
         MVC   BUFREC3,BUFREC      SET 'PREVIOUS RECORD'                        
         SR    RE,RE               KEEP ACCUMS FOR 1 LINE TOTAL                 
         ICM   RE,15,TOTUN3        SAVE GRAND TOTALS                            
         A     RE,BUFUN                                                         
         STCM  RE,15,TOTUN3                                                     
*                                                                               
         ICM   RE,15,TOTPUN3                                                    
         A     RE,BUFPUN                                                        
         STCM  RE,15,TOTPUN3                                                    
*                                                                               
         ICM   RE,15,TOTDL3                                                     
         A     RE,BUFDL                                                         
         STCM  RE,15,TOTDL3                                                     
*                                                                               
         ICM   RE,15,TOTPDL3                                                    
         A     RE,BUFPDL                                                        
         STCM  RE,15,TOTPDL3                                                    
*                                                                               
         B     PS105                                                            
*                                                                               
PS104    EQU   *                                                                
         CLI   REPTYP,C'L'                                                      
         BNE   *+10                                                             
         MVC   PLEN(4),=C'TOT*'                                                 
         CLI   REPTYP,C'P'                                                      
         BNE   *+10                                                             
         MVC   PYR(4),=C'TOT*'                                                  
         MVC   PURB(76),STARS                                                   
         LA    R4,PURB             POINT TO OUTPUT ADDRESS                      
         LA    R6,TOTACCM3         POINT TO A(UNITS/DOLLARS)                    
         BAS   RE,PRNTUNDL                                                      
         LA    R3,1(R3)            LINE COUNTER                                 
         LA    R2,132(R2)          NEXT LINE                                    
         MVC   0(132,R2),SPACES    SPACING LINE                                 
         LA    R3,1(R3)                                                         
*                                                                               
         XC    BUFREC3,BUFREC3     START TOTALLING AGAIN                        
         XC    TOTACCM3,TOTACCM3     CLEAR OUT ACCUMULATORS                     
*                                                                               
******** END NEW                                                                
*                                                                               
PS105    EQU   *                                                                
         CLI   COMBOFLG,C'Y'       COMBO STATION REQUEST?                       
         BNE   PS110               NO                                           
         GOTO1 PUTCOMBO,DMCB,(RC),RR=RELO                                       
PS110    EQU   *                                                                
         MVI   TOTPRNT,C'Y'        NEED TO PRINT TOTALS FOR THIS LEVEL          
         CLC   =C'STATION TOT',BUFDPTN                                          
         BE    *+12                TOTAL, DON'T ACCUMULATE                      
         TM    PRNTOPT,X'80'       SUPPRESS SUB-DAYPART?                        
         BO    PS150               YES, ALWAYS ACCUMULATE                       
         CLC   =C'TOTS',BUFTOTAL   IS IT A TOTAL RECORD?                        
         BNE   PS150               NO  - ACCUMULATE IT                          
*** TEST                                                                        
*        MVC   P+1(06),=C'PS110='  **TEST                                       
*        MVC   P+10(L'BUFACCM),BUFACCM                                          
*        MVC   P+30(L'BUFACCM),BUFREC2+40                                       
*        MVC   P+50(L'TOTACCM),TOTACCM                                          
*        MVC   P+70(L'BUFREC),BUFREC                                            
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
*** END TEST                                                                    
         LA    RF,BUFREC2          A(PREVIOUS RECORD)                           
         CLC   BUFACCM,BUFACCM-BUFREC(RF)                                       
*                                  BUCKETS OF PREVIOUS =                        
*                                     BUCKETS OF TOTAL RECORD?                  
         BE    PS100               YES - SKIP IT - ONLY A SINGLE                
*                                     ITEM MADE UP THE TOTAL                    
         MVC   BUFREC2,BUFREC      SET 'PREVIOUS RECORD' TO KEEP                
*                                     FROM DUPLICATING TOTAL FIGURES            
         B     PS160                                                            
PS150    EQU   *                                                                
*** TEST                                                                        
*        MVC   P+1(06),=C'PS150='  **TEST                                       
*        MVC   P+10(L'TOTACCM),TOTACCM                                          
*        MVC   P+30(L'BUFREC),BUFREC                                            
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
*** END TEST                                                                    
         MVC   BUFREC2,BUFREC      SET 'PREVIOUS RECORD'                        
         SR    RE,RE               KEEP ACCUMS FOR 1 LINE TOTAL                 
         ICM   RE,15,TOTUN         SAVE GRAND TOTALS                            
         A     RE,BUFUN                                                         
         STCM  RE,15,TOTUN                                                      
*                                                                               
         ICM   RE,15,TOTPUN                                                     
         A     RE,BUFPUN                                                        
         STCM  RE,15,TOTPUN                                                     
*                                                                               
         ICM   RE,15,TOTDL                                                      
         A     RE,BUFDL                                                         
         STCM  RE,15,TOTDL                                                      
*                                                                               
         ICM   RE,15,TOTPDL                                                     
         A     RE,BUFPDL                                                        
         STCM  RE,15,TOTPDL                                                     
*** TEST                                                                        
*        MVC   P+1(07),=C'PS150X='  **TEST                                      
*        MVC   P+10(L'TOTACCM),TOTACCM                                          
*        MVC   P+30(L'BUFREC),BUFREC                                            
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
*** END TEST                                                                    
*                                                                               
PS160    EQU   *                                                                
         CLC   SAVDPT(3),BUFDPT       ONLY PRINT DPT NAME ONCE                  
         BE    PS300                                                            
         MVC   SAVDPT(3),BUFDPT                                                 
         MVC   PDPTN,BUFDPTN                                                    
         XC    SAVLEN,SAVLEN                                                    
         CLI   BUFDPT,X'FF'        IF PSEUDO OR                                 
         BE    PS300                                                            
         CLC   BUFDPT(3),=X'FEFEFC'    PLAN, DON'T PRINT DAYPART CODE           
         BE    PS300                                                            
         CLI   BUFDPT,X'FE'        IF PROG TYPE, DON'T PRINT *                  
         BNE   PS200                                                            
         MVC   PDPT(1),BUFSDPT+1                                                
         B     PS300                                                            
PS200    MVC   PDPT(2),BUFDPT      DAYPART CODE                                 
         B     PS300                                                            
         SPACE 1                                                                
PS250    CLI   BUFTYP,C'3'         LEVEL 3                                      
         BNE   PS100                                                            
         CLI   SAVDPT,0                                                         
         BE    *+10                                                             
         MVC   PDPT(13),=C'STATION TOTAL'                                       
         XC    SAVDPT(3),SAVDPT                                                 
         SPACE 1                                                                
PS300    CLC   SAVLEN(2),BUFLEN    ONLY PRINT LENGTH ONCE (PERIOD FMT)          
         BE    PS340                                                            
         MVC   SAVLEN(2),BUFLEN                                                 
         BAS   RE,LENEDIT          EDIT LENGTH                                  
         SPACE 1                                                                
PS340    CLI   MONFMT,C'A'         ALLMONTH                                     
         BE    PS400                                                            
         CLI   MONFMT,C'Q'         QUARTERS                                     
         BNE   PS370                                                            
         MVC   PYR(3),=C'QTR'                                                   
         MVC   PYR+3(1),BUFMON                                                  
         OI    PYR+3,X'F0'                                                      
         B     PS400                                                            
PS370    MVC   WORK(2),BUFYR       MONTHS                                       
         MVI   WORK+2,1                                                         
*        MVC   P+1(7),=C'PS3701='  **TEST                                       
*        MVC   P+10(03),WORK       **TEST                                       
*        MVC   P+16(12),DMCB       **TEST**                                     
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
         GOTO1 DATCON,DMCB,(3,WORK),(6,PYR)                                     
*        MVC   P+1(7),=C'PS3702='  **TEST                                       
*        MVC   P+10(03),WORK       **TEST                                       
*        MVC   P+16(12),DMCB       **TEST**                                     
*        GOTO1 SPOOL,PARAS,(R8)    **TEST                                       
*                                                                               
PS400    LA    R4,PURB             POINT TO OUTPUT ADDRESS                      
         LA    R6,BUFACCM          POINT TO A(UNITS/DOLLARS)                    
         BAS   RE,PRNTUNDL                                                      
         LA    R3,1(R3)            LINE COUNTER                                 
         LA    R2,132(R2)          NEXT LINE                                    
         B     PS100                                                            
*                                                                               
PS450    EQU   *                                                                
** TEST                                                                         
*        MVC   P+1(7),=C'PS450 ='        **TEST                                 
*        MVC   P+15(L'TOTACCM),TOTACCM                                          
*        MVC   P+40(L'BUFREC),BUFREC     **TEST                                 
*        GOTO1 SPOOL,DMCB,(R8)                                                  
** END TEST                                                                     
         CLI   TOTPRNT,C'Y'        ANYTHING TO PRINT FOR THIS LEVEL             
         BNE   PS460                                                            
         CLI   REPTYP,C'L'                                                      
         BNE   *+10                                                             
         MVC   PLEN(4),=C'TOT*'                                                 
         CLI   REPTYP,C'P'                                                      
         BNE   *+10                                                             
         MVC   PYR(4),=C'TOT*'                                                  
         MVC   PURB(76),STARS                                                   
         LA    R4,PURB             POINT TO OUTPUT ADDRESS                      
         LA    R6,TOTACCM          POINT TO A(UNITS/DOLLARS)                    
         BAS   RE,PRNTUNDL                                                      
         LA    R3,1(R3)            LINE COUNTER                                 
         LA    R2,132(R2)          NEXT LINE                                    
PS460    MVC   0(132,R2),SPACES    SPACING LINE                                 
         LA    R3,1(R3)                                                         
*                                                                               
         L     R2,ABFTABLE         NOW PRINT THE CLUMP                          
         STC   R3,ALLOWLIN                                                      
PS465    MVC   P,0(R2)                                                          
         MVC   FOOTSW,FOOTLNS                                                   
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   ALLOWLIN,0                                                       
         MVC   0(132,R2),SPACES    CLEAR LINE FOR NEXT TIME                     
         LA    R2,132(R2)                                                       
         BCT   R3,PS465                                                         
*                                                                               
PS470    EQU   *                                                                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFTYP,ADBUFC),(X'80',1)                 
         MVI   TOTPRNT,C'N'                                                     
*                                                                               
RXEXT    B     XIT                                                              
*                                                                               
         SPACE 3                                                                
LENEDIT  ST    RE,SAVERE                                                        
         CLC   BUFLEN,=X'FFFF'     DON'T PRINT ANYTHING FOR ALL LENGTHS         
         BE    LE60                                                             
         CLC   BUFLEN,=X'FEFE'     PRINT 'PLAN' IF PLAN                         
         BNE   *+14                                                             
         MVC   PLEN,=C'PLAN'                                                    
         B     LE60                                                             
         SPACE 1                                                                
         TM    BUFLEN,X'80'                                                     
         BZ    LE40                                                             
         MVC   HALF,BUFLEN                                                      
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(2,PLEN)                                                
         MVI   PLEN+2,C'M'                                                      
         B     LE60                                                             
*                                                                               
LE40     EDIT  (2,BUFLEN),(3,PLEN)                                              
LE60     L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                   UPRI    UCUR    UIND    RPRI    RCUR    RIND   BPRI         
STARS    DC    CL76'       *       *     *          *       *    *     X        
                      *          *     *'                                       
*                      BCUR       BIND                                          
         SPACE 2                                                                
         EJECT                                                                  
* ROUTINE TO OUTPUT ONE PRINT LINE OF UNITS AND DOLLARS                         
* PARMS R4 = OUTPUT ADDRESS        R6 = A(/UNITS/DOLLARS)                       
         SPACE 2                                                                
PRNTUNDL NTR1                                                                   
         USING PUNDLD,R4                                                        
*                                                                               
*        CLI   BUFAUR,C'Y'         AUR VALUE RECORD?                            
*        BE    PU120               YES                                          
         SR    R5,R5                                                            
         ICM   R5,15,0(R6)          ** UNITS **                                 
         BZ    PU20                                                             
         EDIT  (R5),(7,PUPRI),MINUS=YES                                         
*                                                                               
PU20     ICM   R5,15,4(R6)                                                      
         BZ    PU40                                                             
         EDIT  (R5),(7,PUCUR),MINUS=YES                                         
*                                                                               
         SR    R3,R3               GET INDEX %                                  
         SR    RE,RE                                                            
         ICM   RE,15,0(R6)                                                      
         BZ    PU40                                                             
         ICM   R3,15,4(R6)                                                      
         M     R2,=F'100'                                                       
         SR    R2,R2                                                            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL             CURRENT DIVIDED BY PRIOR                     
         SLL   R2,1                ROUND                                        
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         CH    R3,=H'9999'                                                      
         BNH   *+14                                                             
         MVC   PUIND+1(3),=C'MAX'                                               
         B     PU40                                                             
         EDIT  (R3),(4,PUIND)                                                   
         CLC   PUIND,SPACES                                                     
         BNE   PU40                                                             
         MVC   PUIND+1(3),=C'MIN'                                               
*                                                                               
PU40     SR    R2,R2               ** RATE **                                   
         SR    R5,R5                                                            
         SR    RE,RE                                                            
         ICM   RE,15,0(R6)          PRIOR UNITS                                 
         BZ    PU60                                                             
         ICM   R3,15,8(R6)         PRIOR DOLLARS                                
         BZ    PU60                                                             
         BNM   *+6                 RATE COULD BE MINUS                          
         LCR   R3,R3               BUT WE NEED TO WORK WITH POSITIVE            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         LR    R5,R3               SAVE PRIOR RATE (AS POSITIVE)                
         ICM   RF,15,8(R6)                                                      
         BNM   *+6                                                              
         LCR   R3,R3               RESTORE NEGATIVE                             
         EDIT  (R3),(7,PRPRI),MINUS=YES                                         
*                                                                               
PU60     SR    R2,R2               CURRENT                                      
         SR    RE,RE                                                            
         ICM   RE,15,4(R6)                                                      
         BZ    PU80                                                             
         ICM   R3,15,12(R6)                                                     
         BZ    PU80                                                             
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         ST    R3,FULL             SAVE CURRENT RATE                            
         ICM   RF,15,12(R6)                                                     
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         EDIT  (R3),(7,PRCUR),MINUS=YES                                         
         L     R3,FULL                                                          
*                                                                               
         LTR   R5,R5               R5=PRIOR RATE    (R3=CURRENT RATE)           
         BZ    PU80                                                             
         ST    R5,FULL                                                          
         M     R2,=F'100'                                                       
         SR    R2,R2                                                            
         D     R2,FULL             CURRENT DIVIDED BY PRIOR                     
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         CH    R3,=H'9999'                                                      
         BNH   *+14                                                             
         MVC   PRIND+1(3),=C'MAX'                                               
         B     PU80                                                             
         EDIT  (R3),(4,PRIND)                                                   
         CLC   PRIND,SPACES                                                     
         BNE   PU80                                                             
         MVC   PRIND+1(3),=C'MIN'                                               
*                                                                               
PU80     ICM   R3,15,8(R6)         ** BILLING **                                
         BZ    PU100                                                            
         EDIT  (R3),(10,PBPRI),MINUS=YES                                        
*                                                                               
PU100    ICM   R3,15,12(R6)                                                     
         BZ    XIT                                                              
         EDIT  (R3),(10,PBCUR),MINUS=YES                                        
*                                                                               
         ICM   RE,15,8(R6)                                                      
         BZ    XIT                                                              
         ST    RE,FULL                                                          
         M     R2,=F'100'                                                       
         SR    R2,R2                                                            
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         CH    R3,=H'9999'                                                      
         BNH   *+14                                                             
         MVC   PBIND+1(3),=C'MAX'                                               
         B     XIT                                                              
         EDIT  (R3),(4,PBIND)                                                   
         CLC   PBIND,SPACES                                                     
         BNE   XIT                                                              
         MVC   PBIND+1(3),=C'MIN'                                               
         B     XIT                                                              
PU120    EQU   *                                                                
         MVC   FULL,8(R6)          GET PRIOR AVERAGE UNIT RATE                  
         L     R3,FULL                                                          
         EDIT  (R3),(7,PRPRI),MINUS=YES                                         
         MVC   FULL,12(R6)         GET CURNT AVERAGE UNIT RATE                  
         L     R3,FULL                                                          
         EDIT  (R3),(7,PRCUR),MINUS=YES                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* READ STATION TO SEE IF IT MEETS FILTER (OWNERSHIP, TVB, RANK)                 
* REQUIREMENTS.  ON EXIT, TAKE RECORD IF R3 IS POSITIVE,                        
*                         REJECT RECORD IF R3 IS ZERO                           
         SPACE 2                                                                
READSTA  NTR1                                                                   
         SR    R3,R3                                                            
         MVC   SVATNKY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SVATNKY+ASTAE                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
***      BE    *+6                                                              
***      DC    H'0'                                                             
         BNE   NO                  NO  - NOT ON FILE?  TREAT AS NO              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         USING RSTAKEY,R6                                                       
         MVC   MARKET(20),RSTAMKT  MARKET NAME                                  
         OC    OWNER,OWNER                                                      
         BZ    RS20                                                             
         CLC   OWNER,RSTAOWN                                                    
         BNE   NO                                                               
         SPACE 1                                                                
RS20     OC    TVB,TVB                                                          
         BZ    RS40                                                             
         CLC   TVB,RSTATVB                                                      
         BNE   NO                                                               
         SPACE 1                                                                
RS40     OC    RANK,RANK                                                        
         BZ    YES                                                              
         CLC   RANK,RSTARANK                                                    
         BNE   NO                                                               
         DROP  R6                                                               
         SPACE 1                                                                
YES      LA    R3,1                                                             
         SPACE 1                                                                
NO       MVC   KEY(34),SVATNKY     REREAD ATHENA KEY                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    STEXT                                                            
         DC    H'0'                                                             
STEXT    XIT1  REGS=(R3)                                                        
         EJECT                                                                  
* READ & CHECK IF SCREEN POSTION HAS ENOUGH ROOM TO OUTPUT ALL 2NDARY           
* -- NOTE -- IT EXPECTS RECORD TO REMAIN IN AIO2                                
READDPT  NTR1                                                                   
         LR    R5,RE               RE NZ NOT 1ST TIME FOR DPT                   
         MVC   SVATNKY,KEY         SAVE ATHENA KEY                              
         CLC   KEY+ADPTE(2),=X'FFFF'                                            
         BE    DNEXT               PSUEDO                                       
         CLC   KEY+ADPTE(3),=X'FEFEFC'                                          
         BE    DNEXT               PLAN                                         
         CLC   KEY+ADPTE(2),=X'FEFE'                                            
         BE    RD60                PROGRAM TYPE                                 
         XC    CURSDPT,CURSDPT     CLEAR CURRENT DAYPART                        
         CLC   KEY+ADPTE(1),CURDPTC                                             
         BE    RD20                GET NEXT 2NDARY DPT                          
         SR    R5,R5                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'24'                                                        
         MVC   KEY+24(2),AGENCY     REP                                         
         MVC   KEY+26(1),SVATNKY+ADPTE DAYPART                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
RD20     L     R6,AIO2                                                          
         MVC   CURDPT,RDPTNAME-RDPTKEY(R6)                                      
         MVC   CURDPTC,RDPTKDPT-RDPTKEY(R6)                                     
         XC    CURSDPT,CURSDPT                                                  
         TM    PRNTOPT,X'80'      SUPPRESS SUB DAYPART                          
         BO    RD140                                                            
         MVI   ELCODE,X'02'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
RD40     BAS   RE,NEXTEL                                                        
         BNE   RD120                                                            
*                                                                               
         CLC   2(1,R6),SVATNKY+ASDTE                                            
         BNE   RD40                                                             
         MVC   CURSDPT,3(R6)                                                    
         B     RD120                                                            
*                                                                               
RD60     XC    KEY,KEY             READ PROGRAM RECORD                          
         MVI   KEY,X'25'                                                        
         MVC   KEY+24(2),AGENCY                                                 
         MVC   KEY+26(1),SVATNKY+APRGE                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    RD80                                                             
         MVC   CURPRG,=CL11'UNKNOWN'                                            
         B     RD100                                                            
RD80     MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R6,AIO2                                                          
         MVC   CURPRG,RPGTNAME-RPGTKEY(R6)                                      
RD100    SR    R5,R5                                                            
*                                                                               
RD120    OC    CURSDPT,CURSDPT                                                  
         BNZ   *+10                                                             
         MVC   CURSDPT,=C'XXXXX'                                                
RD140    MVC   KEY(34),SVATNKY     REREAD ATHENA KEY                            
         LTR   R5,R5               DON'T RE-READ IF DPT REC NOT READ            
         BNZ   DNEXT                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    DNEXT                                                            
         DC    H'0'                                                             
DNEXT    B     XIT                                                              
         EJECT                                                                  
*  ROUTINE TO CHECK THAT PERIOD IS VALID QUARTER DATES                          
         SPACE                                                                  
CHKQTR   CLI   SMONTH+1,1          MUST START WITH JANUARY                      
         BE    CQ20                                                             
         CLI   SMONTH+1,4          OR APRIL                                     
         BE    CQ20                                                             
         CLI   SMONTH+1,7          OR JULY                                      
         BE    CQ20                                                             
         CLI   SMONTH+1,X'0A'      OR OCTOBER                                   
         BNE   CQERR                                                            
         SPACE                                                                  
CQ20     CLI   EMONTH+1,3          AND END WITH MARCH                           
         BE    CQ40                                                             
         CLI   EMONTH+1,6          OR JULY                                      
         BE    CQ40                                                             
         CLI   EMONTH+1,9          OR SEPTEMBER                                 
         BE    CQ40                                                             
         CLI   EMONTH+1,X'0C'      OR DECEMBER                                  
         BE    CQ40                                                             
CQERR    LA    R2,AUCPERH          POINT CURSOR TO PERIOD                       
         MVC   CONHEAD+10(L'NOTQTR),NOTQTR                                      
         B     MYEND                                                            
         SPACE 1                                                                
CQ40     BR    RE                                                               
         SPACE 2                                                                
*  ROUTINE TO SLOT MONTHS INTO THE CORRECT QUARTERS                             
*     R1 WILL HAVE QUARTER NUMBER (1-4) ON EXIT                                 
         SPACE 2                                                                
GETQTR   LA    R1,4                                                             
*                                                                               
         CLI   KEY+AYME+1,9                                                     
         BH    GQ20                                                             
         BCTR  R1,0                                                             
         CLI   KEY+AYME+1,6                                                     
         BH    GQ20                                                             
         BCTR  R1,0                                                             
         CLI   KEY+AYME+1,3                                                     
         BH    GQ20                                                             
         BCTR  R1,0                                                             
*                                                                               
GQ20     BR    RE                                                               
         EJECT                                                                  
*  ROUTINE TO GET UNITS AND DOLLARS FROM ATHENA RECORD                          
         SPACE 2                                                                
GETUNDL  ST    RE,FULL                                                          
         MVI   RDUPDATE,C'N'       REPORTING FUNCTION CAN READ RECORDS          
         GOTO1 GETREC              UNLOCKED                                     
         XC    UNDL,UNDL           UNIT/$ ACCUM                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETU0020 BAS   RE,NEXTEL                                                        
         BNE   GETU0140                                                         
*                                                                               
         OC    ASAT,ASAT                                                        
         BZ    *+14                                                             
         CLC   ASAT,2(R6)                                                       
         BL    GETU0020                                                         
*                                                                               
         CLI   OPT$$$,C'R'         REGULAR DOLLARS REQUESTED?                   
         BNE   GETU0040            NO  - REGULAR NOT NEEDED                     
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   GETU0020            NO  - GO BACK FOR NEXT                       
         SR    RE,RE               YES                                          
         ICM   RE,15,UNACCM        ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         B     GETU0120                                                         
GETU0040 EQU   *                                                                
         CLI   OPT$$$,C'B'         BOTH $$$ TYPES REQUESTED?                    
         BNE   GETU0080            NO                                           
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   GETU0060            NO  - CHECK COMBO $$$                        
         SR    RE,RE               YES                                          
         ICM   RE,15,UNACCM        ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   GETU0120            NO  - GO BACK FOR NEXT                       
         SR    RE,RE               YES                                          
         ICM   RE,15,UNACCM        ADD TO UNIT ACCUMULATOR                      
         AH    RE,11(R6)           USE SECOND BUCKETS                           
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,13(R6)           USE SECOND BUCKETS                           
         STCM  RE,15,DLACCM                                                     
         B     GETU0120                                                         
GETU0060 EQU   *                                                                
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   GETU0020            NO  - GO BACK FOR NEXT                       
*                                                                               
*   ABOVE SHOULD NEVER HAPPEN.  THIS WOULD INDICATE THAT THERE WAS              
*     A DOLLAR BUCKET WITH NEITHER REGULAR OR COMBO FIELDS                      
*                                                                               
         SR    RE,RE               YES                                          
         ICM   RE,15,UNACCM        ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         B     GETU0120                                                         
GETU0080 EQU   *                                                                
         CLI   OPT$$$,C'C'         COMBO $$ REQUESTED?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO TYPE??????                                
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   GETU0020            NO  - GO BACK FOR NEXT                       
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   GETU0100            NO  - USE FIRST SET OF BUCKETS               
         SR    RE,RE               YES - USE SECOND SET OF BUCKETS              
         ICM   RE,15,UNACCM                                                     
         AH    RE,11(R6)                                                        
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,13(R6)                                                        
         STCM  RE,15,DLACCM                                                     
         B     GETU0120                                                         
GETU0100 EQU   *                                                                
         SR    RE,RE               USE FIRST  SET OF BUCKETS                    
         ICM   RE,15,UNACCM                                                     
         AH    RE,5(R6)                                                         
         STCM  RE,15,UNACCM                                                     
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)                                                         
         STCM  RE,15,DLACCM                                                     
GETU0120 EQU   *                                                                
*                                                                               
         B     GETU0020                                                         
*                                                                               
GETU0140 L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*  FOOT HOOK ROUTINE FOR REQUEST DETAILS                                        
FHOOK    NTR1                                                                   
         USING FHOOK,RF                                                         
         LA    R1,SAVEREGS-FHOOK    RESTORE REGISTERS                           
         AR    R1,RF                                                            
         DROP  RF                                                               
         LM    R2,RC,0(R1)         LOAD R2 -> RC                                
         MVC   P,SPACES                                                         
         CLI   FOOTSW,0            HAVE I ALREADY PRINTED EVERYTHING            
         BE    XIT                 YES                                          
         L     R5,SVFOOTF                                                       
         LA    R1,SVFOOT                                                        
         CR    R5,R1               IS THERE ANYTHING TO PRINT                   
         BE    FH40                NO                                           
         BCTR  R5,0                YES, BACK UP AND BLANK OUT COMMA             
         CLI   0(R5),C','                                                       
         BE    FH20                                                             
         CLI   0(R5),C' '          OR MAY ALREADY BE BLANKED OUT                
         BE    FH20                                                             
         DC    H'0'                SOMETHING IS WRONG                           
         SPACE 1                                                                
FH20     MVI   0(R5),C' '                                                       
         MVC   P(8),=C'DETAILS-'                                                
         MVC   P+8(101),SVFOOT                                                  
FH40     MVI   FOOTSW,0            INDICATE NO MORE TO PRINT                    
         B     XIT                                                              
         EJECT                                                                  
*  HOOK ROUTINE FOR HEADLINE DETAIL                                             
         SPACE 1                                                                
HOOK     NTR1                                                                   
         USING HOOK,RF                                                          
         LA    R1,SAVEREGS-HOOK    RESTORE REGISTERS                            
         AR    R1,RF                                                            
         DROP  RF                                                               
         LM    R2,RC,0(R1)         LOAD R2 -> RC                                
         L     RF,ASTATN           A(STATION IN PROGRESS)                       
         OC    0(7,RF),0(RF)       ANY STATION?                                 
         BNZ   HK1                 YES                                          
         MVC   H6+2(20),=C'ALL-STATION AVERAGES'                                
         CLI   AURTITLE,C'N'       AUR AGGREGATE PASS?                          
         BE    HK2                                                              
         MVC   H6+2(20),=C'ALL-STATION CUMES   '                                
         B     HK2                                                              
HK1      EQU   *                                                                
         MVC   H6+1(7),=C'DETAILS'                                              
         MVC   H6+9(4),2(RF)       LOAD STATION CALL LETTERS                    
         MVI   H6+13,C'-'                                                       
         MVC   H6+14(1),6(RF)      LOAD MEDIA                                   
HK2      EQU   *                                                                
         MVC   H8+15(6),=C'LENGTH'                                              
         MVC   H9+15(6),DASH                                                    
         CLI   REPTYP,C'L'                                                      
         BNE   HK3                                                              
         MVC   H1+17(6),=C'LENGTH'                                              
HK3      CLI   REPTYP,C'P'                                                      
         BNE   HK5                                                              
         MVC   H1+17(6),=C'PERIOD'                                              
         CLI   MONFMT,C'A'                                                      
         BE    *+16                                                             
         MVC   H8+24(4),=C'DATE'                                                
         MVC   H9+23(6),DASH                                                    
         TM    PRNTOPT,X'20'       NO LENGTH                                    
         BZ    HK5                                                              
         MVC   H8+15(6),SPACES                                                  
         MVC   H9+15(6),SPACES                                                  
*                                                                               
HK5      MVC   H5+17(20),SVMKT                                                  
HK10     MVC   H5+9(7),AUCSTA                                                   
*                                                                               
         CLC   =C'MKT=',AUCSTA                                                  
         BNE   HK20                                                             
         MVC   H5+9(7),SPACES                                                   
         MVC   H5+17(20),SPACES                                                 
         MVC   H5+9(10),AUCSTA                                                  
         MVC   H5+20(20),SVMKT2                                                 
*                                                                               
HK20     LA    R2,H5+84                                                         
         GOTO1 DATCON,DMCB,(3,SMONTH),(6,0(R2))   START MONTH                   
         CLC   SMONTH,EMONTH                                                    
         BE    HKXIT                                                            
         LA    R2,6(R2)                                                         
         MVI   0(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,EMONTH),(6,1(R2))     END MONTH                   
HKXIT    B     XIT                                                              
SAVEREGS DS    11F                                                              
         SPACE 2                                                                
DASH     DC    10C'-'                                                           
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
ADBUFC   DC    A(BUFFALOC)                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*  MY OWN ERROR MESSAGES                                                        
         SPACE 2                                                                
GRPORSTA DC    C'EITHER GROUP OR STATION REQUIRED'                              
STAORMKT DC    C'STATION OR MKT=CODE REQUIRED'                                  
MKTONLY  DC    C'NOT MKT= - CAN NOT FILTER ON OWNER,TVB,RANK'                   
RANKE2   DC    C'RANK MUST BE FROM 1-7'                                         
TYPER    DC    C'REPORT TYPE L MUST BE WHOLE MONTH'                             
INVFMT   DC    C'INVALID FORMAT AT THIS TIME'                                   
MANYDPT  DC    C'MAXIMUM 4 DAYPARTS ALLOWED'                                    
MNYLEN   DC    C'MAXIMUM 4 LENGTHS ALLOWED'                                     
MANYLENP DC    C'FOR REPORT TYPE P, ONLY 1 LENGTH ALLOWED'                      
DATEFMT  DC    C'FORMAT IS MMM/YY'                                              
NOTQTR   DC    C'DATES MUST INCLUDE COMPLETE QUARTERS'                          
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,C'RATE ANALYSIS BY'                                         
         PSPEC H2,1,23C'-'                                                      
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REPORT                                                     
         PSPEC H2,92,REQUESTOR                                                  
         PSPEC H5,1,C'STATION'                                                  
         PSPEC H5,76,C'PERIOD'                                                  
         PSPEC H8,1,C'DAYPART'                                                  
         PSPEC H9,1,C'--------------'                                           
         PSPEC H7,33,C'- - - - UNITS - - - -'                                   
         PSPEC H8,33,C'PRIOR   CURRENT INDEX'                                   
         PSPEC H9,33,C'---------------------'                                   
         PSPEC H7,57,C'- - - - RATE - - - - '                                   
         PSPEC H8,57,C'PRIOR   CURRENT INDEX'                                   
         PSPEC H9,57,C'---------------------'                                   
         PSPEC H7,81,C'- - - - -  BOOKED   - - - -'                             
         PSPEC H8,81,C'PRIOR      CURRENT    INDEX'                             
         PSPEC H9,81,C'---------------------------'                             
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*   WORK AREA                                                                   
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'**BUFF**'                                                    
         BUFF  LINES=200,ROWS=1,COLUMNS=4,FLAVOR=BINARY,KEYLIST=(16,A),X        
               COMMENT=24                                                       
         EJECT                                                                  
PRNTD    DSECT                                                                  
PDPT     DS    CL2                 DAYPART                                      
         DS    CL2                                                              
PDPTN    DS    CL11                DAYPART NAME                                 
         DS    CL2                                                              
PLEN     DS    CL4                 LENGTH                                       
         DS    CL2                                                              
PYR      DS    CL6                 MONTH/YEAR                                   
         DS    CL3                                                              
PURB     DS    0CL1                UNITS/RATE/BILLING                           
         SPACE 2                                                                
PUNDLD   DSECT                                                                  
PUPRI    DS    CL7                 PRIOR UNITS                                  
         DS    CL1                                                              
PUCUR    DS    CL7                 CURRENT UNITS                                
         DS    CL1                                                              
PUIND    DS    CL4                 INDEX UNITS                                  
         DS    CL4                                                              
PRPRI    DS    CL7                 PRIOR RATE                                   
         DS    CL1                                                              
PRCUR    DS    CL7                 CURRENT RATE                                 
         DS    CL1                                                              
PRIND    DS    CL4                 INDEX RATE                                   
         DS    CL4                                                              
PBPRI    DS    CL10                PRIOR BILLING                                
         DS    CL1                                                              
PBCUR    DS    CL10                CURRENT BILLING                              
         DS    CL1                                                              
PBIND    DS    CL4                 INDEX BILLING                                
         EJECT                                                                  
AURWORK  DSECT                                                                  
AURDESC1 DS    CL(BUFAUR+1-BUFDPT)                                              
AURDESC2 DS    CL(BUFTOTAL+4-BUFDPTN)                                           
AURAURPU DS    CL4                                                              
AURAURU  DS    CL4                                                              
AURAURPD DS    CL4                                                              
AURAURD  DS    CL4                                                              
*URAURPR DS    CL4                                                              
*URAURCU DS    CL4                                                              
LAURWORK EQU   *-AURDESC1                                                       
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         SPACE 2                                                                
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
* RESFMFCD                                                                      
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMECD                                                       
         EJECT                                                                  
* REGENSTA                                                                      
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
* REGENMKT                                                                      
       ++INCLUDE REGENMKT                                                       
         EJECT                                                                  
* REGENATNA                                                                     
       ++INCLUDE REGENAUR                                                       
         EJECT                                                                  
* REGENSDD                                                                      
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
* REGENDPT                                                                      
       ++INCLUDE REGENDPT                                                       
         EJECT                                                                  
* REGENPGT                                                                      
       ++INCLUDE REGENPGT                                                       
         EJECT                                                                  
* RESFMWORKD                                                                    
       ++INCLUDE RESFMWORKD                                                     
         SPACE 2                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*                  ---  WORK AREA ---                                           
         DS    0F                                                               
MYWORK   DS    0CL768                                                           
UNDL     DS    0CL8                                                             
DLACCM   DS    F                   DOLLAR ACCUM                                 
UNACCM   DS    F                   UNIT ACCUM                                   
PRIORSW  DS    CL1                 Y = RECORD IS PRIOR MONTH                    
GROUP    DS    CL2                 GROUP                                        
STATN    DS    CL5                 STATION                                      
FILTER   DS    CL1                 ATHENA RECORD TYPE  (FILTER)                 
*                                  1=ALL (BLANK)                                
*                                  2=SERVICE (S)                                
*                                  3=CATEGORY (C)                               
*                                  4=ADVERTISER (A)                             
*                                                                               
TYPCODE  DS    CL9                 ADV/SERVICE/CATEGORY CODE                    
PSMONTH  DS    CL2                 PRIOR START YEAR/MONTH (BINARY)              
PEMONTH  DS    CL2                 PRIOR END YEAR/MONTH (BINARY)                
SMONTH   DS    CL2                 START MONTH                                  
EMONTH   DS    CL2                 END MONTH                                    
REPTYP   DS    CL1                 REPORT TYPE                                  
*                                   L=LENGTH                                    
*                                   P=PERIOD                                    
*                                   D=DAYPART                                   
*                                                                               
OPT$$$   DS    CL1                 DOLLAR/UNIT TYPE                             
*                                  R  =  REGULAR DOLLARS/UNITS                  
*                                  C  =  COMBO   DOLLARS/UNITS                  
*                                  B  =  BOTH COMBO + REGULAR                   
AGOFFFLG DS    CL1                 AGENCY/AGENCY OFFICE FLAG                    
*                                  X'0'  -  NO ENTRY                            
*                                  C'C'  -  AGENCY CODE ONLY                    
*                                  C'O'  -  AGENCY OFFICE CODE                  
TOTPAGE  DS    CL1                 Y  =  TOTAL PAGE ONLY                        
DPT      DS    CL8                 DAYPARTS                                     
         DS    CL1                 ENDING 0                                     
DPTCNT   DS    CL1                 DAYPART COUNTER                              
LEN      DS    CL8                 LENGTHS                                      
         DS    CL2                 ENDING 0                                     
LENCNT   DS    CL1                 LENGTH COUNTER                               
PRNTOPT  DS    CL1                 X'80'  SUPPRESS 2NDARY DAYPART               
*                                  X'40'  STATION TOTALS ONLY                   
*                                  X'20'  LENGTH = TOT                          
*                                  X'10'  LIMITED ACCESS                        
*                                                                               
OWNER    DS    CL3                 OWNER                                        
OWNERN   DS    CL20                OWNER NAME                                   
TVB      DS    CL2                 TVB REGION                                   
TVBN     DS    CL18                TVB REGION NAME                              
RANK     DS    CL1                 RANK                                         
MARKET   DS    CL20                MARKET NAME                                  
SVMKT    DS    CL20                MARKET NAME (SAVED FOR PRINTING)             
GROUPN   DS    CL10                GROUP NAME                                   
         DS    CL1               FOR SPACE BTWN GROUP & SUBGROUP NAME           
SGROUPN  DS    CL10                SUB GROUP NAME                               
MONFMT   DS    CL1                 FORMAT                                       
*                                     M=MONTH                                   
*                                     Q=QUARTER                                 
*                                     A=WHOLE MONTH                             
         SPACE 1                                                                
ASAT     DS    CL2                 COMPRESSED AS AT DATE                        
CURPRG   DS    CL11                CURRENT PROGRAM NAME                         
CURDPTC  DS    CL1                 CURRENT DAYPART CODE                         
CURDPT   DS    CL5                 CURRENT DAYPART NAME                         
CURSDPT  DS    CL5                 CURRENT SECONDARY DAYPART NAME               
FOOTSW   DS    CL1                                                              
MULTSDPT DS    XL1                 X'80'  1 OR MORE SUB DAYPARTS                
*                                  X'40'  2 OR MORE SUB DAYPARTS                
         DS    0F                                                               
TOTACCM  DS    0CL16               TOTAL ACCUMULATOR                            
TOTPUN   DS    F                   PRIOR UNITS                                  
TOTUN    DS    F                   UNITS                                        
TOTPDL   DS    F                   PRIOR DOLLARS                                
TOTDL    DS    F                   DOLLARS                                      
         DS    0F                                                               
SVATNKY  DS    CL34                SAVE KEY                                     
SVKEY    DS    CL34                SAVE KEY                                     
SAVDPT   DS    CL1                 LAST DAYPART FILTER PRINTED                  
SAVSDPT  DS    CL2                 LAST SUB DAYPART/PROG TYPE PRINTED           
SAVLEN   DS    CL2                 LAST LENGTH PRINTED                          
SVADPTF  DS    A                   A(LAST DAYPART FILTER READ)                  
SVALENF  DS    A                   A(LAST LENGTH FILTER READ)                   
SVFOOTF  DS    A                   A(NEXT POSITION IN FOOTLINE)                 
SAVERE   DS    F                                                                
SAVEE    DS    F                                                                
TOTPRNT  DS    CL1                 Y=SOMETHING TO PRINT FOR LEVEL               
SVFOOT   DS    CL132                                                            
         SPACE 2                                                                
         DS    0F                                                               
BUFREC   DS    0CL56                                                            
BUFKEY   DS    0CL16                                                            
BUFTYP   DS    CL1                                                              
BUFSTA   DS    CL5                                                              
BUFDPT   DS    CL1                                                              
BUFSDPT  DS    CL2                                                              
BUFLEN   DS    CL2                                                              
BUFYR    DS    CL1                                                              
BUFMON   DS    CL1                                                              
BUFAUR   DS    CL1                 Y = DOLLARS CONTAIN AUR                      
         DS    CL2                 NOT DEFINED                                  
BUFDPTN  DS    CL15                                                             
BUFSTA2  DS    CL5                                                              
BUFTOTAL DS    CL4                                                              
         DS    0F                                                               
BUFACCM  DS    0CL16                                                            
BUFPUN   DS    F                                                                
BUFUN    DS    F                                                                
BUFPDL   DS    F                                                                
BUFDL    DS    F                                                                
*                                                                               
BUFREC2  DS    CL(*-BUFREC)                                                     
*                                                                               
BUFREC3  DS    CL(*-BUFREC)                                                     
*                                                                               
STATLIST DS    24CL7               24 GROUP/STATION ENTRIES MAX                 
STADELIM DS    XL1                 DELIMITER                                    
COMBOFLG DS    CL1                                                              
SECVIOL  DS    CL1                 SECURITY VIOLATION FLAG                      
SAVMKTCD DS    CL4                 MARKET CODE FROM X'2B' RECORD                
ASTATN   DS    A                   A(STATION IN PROGRESS)                       
STATCTR  DS    F                   STATION COUNTER                              
STATPROG DS    F                   STATION IN PROGRESS                          
ABFTABLE DS    A                   A(BUFFER PRINT AREA)                         
AAURTABL DS    A                   A(AUR VALUE TABLE)                           
AAURNXT  DS    A                   A(NEXT SPACE IN TABLE)                       
SAVEAUR  DS    CL1                 FLIP-FLOP SWITCH                             
AURTITLE DS    CL1                 HEADING DISPLAY INDICATOR                    
SVMKT2   DS    CL20                MARKET NAME FROM X'2B' RECORD                
SVAIO    DS    A                                                                
*                                                                               
** NEW ACCUMS 7/13/99                                                           
*                                                                               
         DS    0F                                                               
TOTACCM3 DS    0CL16               TOTAL ACCUMULATOR                            
TOTPUN3  DS    F                   PRIOR UNITS                                  
TOTUN3   DS    F                   UNITS                                        
TOTPDL3  DS    F                   PRIOR DOLLARS                                
TOTDL3   DS    F                   DOLLARS                                      
*                                                                               
AREPE    EQU   RAURKREP-RAURKEY    REP                                          
AGRPE    EQU   RAURKGRP-RAURKEY    GROUP                                        
ASTAE    EQU   RAURKSTA-RAURKEY    STATION                                      
ATPEE    EQU   RAURKTPE-RAURKEY    RECORD TYPE (FILTER)                         
ATCPE    EQU   RAURKTCD-RAURKEY    TYPE CODE                                    
AAGYE    EQU   RAURKAGY-RAURKEY    AGENCY                                       
ACTYPE   EQU   RAURKCTP-RAURKEY    CONTRACT TYPE                                
AOFFE    EQU   RAURKOFF-RAURKEY    OFFICE                                       
ADPTE    EQU   RAURKDPT-RAURKEY    DAYPART                                      
ASDTE    EQU   RAURKSDT-RAURKEY    SUB DAYPART                                  
APRGE    EQU   RAURKPRG-RAURKEY    PROGRAM CODE                                 
ASLNE    EQU   RAURKSLN-RAURKEY    SPOT LENGTH                                  
AYME     EQU   RAURKYM-RAURKEY     YEAR/MONTH                                   
*                                                                               
         EJECT                                                                  
T81816   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*TVB TABLE                                                                      
       ++INCLUDE RETVBTAB                                                       
         EJECT                                                                  
*                                                                               
*   GENTOTS:  PUTS OUT ADDITIONAL BUFFALO RECORDS FOR TOTAL BREAKS              
*                                                                               
GENTOTS  NMOD1 0,*GTOT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   BUFREC2,BUFREC      SAVE CURRENT BUFFALO RECORD                  
*                                                                               
*   BUFF RECORD IS MODIFIED TO IDENTIFY TOTALS.  'TOTS' IS INSERTED,            
*      TO PROVIDE AN INDICATOR.                                                 
*                                                                               
         BAS   RE,GENTOT2          SPACE-FILL NAME                              
         MVC   BUFTOTAL,=C'TOTS'   SET TOTAL INDICATOR                          
         CLI   REPTYP,C'L'         LENGTH REPORT?                               
         BNE   GTOT0030            NO  - PERIOD REPORT                          
         MVC   BUFLEN,=X'FFFF'     LENGTH                                       
         GOTO1 =A(PUTBUFF),DMCB,(RC),1,RR=RELO                                  
         MVC   BUFSDPT,=X'FFFF'    SUBDYPART                                    
         CLC   =C'PSEUDO',BUFDPTN  'PSEUDO' DAYPART?                            
         BE    GTOT0010            YES - DON'T PUT OUT SUBDYPT TOTL             
         GOTO1 =A(PUTBUFF),DMCB,(RC),1,RR=RELO                                  
GTOT0010 EQU   *                                                                
         MVI   BUFDPT,X'FF'        DAYPART                                      
         MVC   BUFDPTN,=C'STATION TOTAL'                                        
         GOTO1 =A(PUTBUFF),DMCB,(RC),1,RR=RELO                                  
         MVC   BUFREC,BUFREC2      RELOAD BUFF RECORD                           
         BAS   RE,GENTOT2          SPACE-FILL NAME                              
         MVC   BUFTOTAL,=C'TOTS'   SET TOTAL INDICATOR                          
         MVC   BUFSDPT,=X'FFFF'    SUBDYPART W/LENGTH                           
         CLC   =C'PSEUDO',BUFDPTN  'PSEUDO' DAYPART?                            
         BE    GTOT0020            YES - DON'T PUT OUT SUBDYPT TOTL             
         GOTO1 =A(PUTBUFF),DMCB,(RC),1,RR=RELO                                  
GTOT0020 EQU   *                                                                
         MVI   BUFDPT,X'FF'        DAYPART W/LENGTH                             
         MVC   BUFDPTN,=C'STATION TOTAL'                                        
         GOTO1 =A(PUTBUFF),DMCB,(RC),1,RR=RELO                                  
         B     GTOT0050                                                         
GTOT0030 EQU   *                                                                
         MVC   BUFSDPT,=X'FFFF'    SUBDYPART                                    
         CLC   =C'PSEUDO',BUFDPTN  'PSEUDO' DAYPART?                            
         BE    GTOT0040            YES - DON'T PUT OUT SUBDYPT TOTL             
         GOTO1 =A(PUTBUFF),DMCB,(RC),1,RR=RELO                                  
GTOT0040 EQU   *                                                                
         MVI   BUFDPT,X'FF'        DAYPART                                      
         MVC   BUFDPTN,=C'STATION TOTAL'                                        
         GOTO1 =A(PUTBUFF),DMCB,(RC),1,RR=RELO                                  
GTOT0050 EQU   *                                                                
         MVC   BUFREC,BUFREC2      RESET ORIGINAL BUFF RECORD                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  GENTOT2:  BLANK-FILL DAYPART NAME                                            
GENTOT2  NTR1                                                                   
         LA    RF,BUFDPTN          A(DAYPART NAME)                              
         LA    R0,11               LOOP CONTROL                                 
GENT0010 EQU   *                                                                
         CLI   0(RF),C'/'          SEPARATOR?                                   
         BE    GENT0016            YES                                          
         LA    RF,1(RF)            NO  - BUMP TO NEXT POSITION                  
         BCT   R0,GENT0010         GO BACK FOR NEXT                             
         B     GENT0020            NO SEPARATOR - USE AS IS                     
GENT0016 EQU   *                                                                
         MVI   0(RF),C' '          SPACE-FILL THE BYTE                          
         LA    RF,1(RF)                                                         
         BCT   R0,GENT0016         DO REMAINING BYTES                           
GENT0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   PUTBUFF:  PUTS OUT NECESSARY RECORDS TO BUFFALO                             
*                                                                               
PUTBUFF  NMOD1 0,*PBUF*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            TEST DISPLAY FLAG                            
*                                                                               
**       LTR   R2,R2               ANY VALUE IN R2?                             
**       BNZ   PUTB0050            YES - SKIP DISPLAY                           
*****>   BZ    PUTB0050            NO  - SKIP DISPLAY                           
**       MVC   P+1(7),=C'OUTPUT='        **TEST                                 
**       MVC   P+10(L'BUFREC),BUFREC     **TEST                                 
**       GOTO1 SPOOL,PARAS,(R8)          **TEST                                 
*                                  YES - APPLY ALL OTHER TESTS ALSO             
PUTB0050 EQU   *                                                                
         TM    PRNTOPT,X'40'       STATION TOTALS ONLY?                         
         BNO   PUTB0100            NO                                           
         CLC   =C'PSEUDO',BUFDPTN  YES - PSEUDO DAYPART?                        
         BE    PUTB0300            YES - DON'T OUTPUT IT                        
         CLC   BUFDPT(2),=X'FFFF'  YES - DAYPART/SUBDAYPART?                    
         BNE   PUTB0300            YES - NO OUTPUT FOR THIS                     
PUTB0100 EQU   *                                                                
         CLC   =C'PSEUDO',BUFDPTN  PSEUDO DAYPART?                              
         BE    PUTB0200            YES - IGNORE FOLLOWING TESTS                 
         TM    PRNTOPT,X'80'       SUPPRESS SUBDAYPART?                         
         BNO   PUTB0200            NO                                           
         CLC   BUFSDPT(1),=X'FF'   YES - SUBDAYPART?                            
         BNE   PUTB0300            YES - NO OUTPUT FOR THIS                     
PUTB0200 EQU   *                                                                
         CLI   TOTPAGE,C'Y'        TOTAL PAGE ONLY?                             
         BNE   PUTB0220            NO                                           
         CLC   BUFTYP,STATCTR+3    YES - IS RECORD TYPE 'TOTAL',                
*                                     WHICH IS LAST STATION?                    
         BNE   PUTB0240            NO  - SKIP IT                                
PUTB0220 EQU   *                                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',(0,ADBUFC),BUFREC                           
*        MVC   P+1(7),=C'PUTBUF='        **TEST                                 
*        MVC   P+10(L'BUFREC),BUFREC     **TEST                                 
*        GOTO1 SPOOL,PARAS,(R8)          **TEST                                 
PUTB0240 EQU   *                                                                
         MVC   BUFREC3,BUFREC      SAVE IT TEMPORARILY                          
         MVC   BUFTYP,STATCTR+3    INSERT TOTAL STATION NUMBER                  
         MVC   BUFSTA,STATLIST+2   INSERT TOTAL STATION CALLS                   
         CLI   TOTPAGE,C'Y'        TOTAL PAGE ONLY?                             
         BNE   PUTB0260            NO                                           
         CLC   BUFTYP,STATCTR+3    YES - IS RECORD TYPE 'TOTAL',                
*                                     WHICH IS LAST STATION?                    
         BNE   PUTB0280            NO  - SKIP IT                                
PUTB0260 EQU   *                                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',(0,ADBUFC),BUFREC                           
*        MVC   P+1(7),=C'OUTPUT='        **TEST                                 
*        MVC   P+10(L'BUFREC),BUFREC     **TEST                                 
*        GOTO1 SPOOL,PARAS,(R8)          **TEST                                 
PUTB0280 EQU   *                                                                
         MVC   BUFREC,BUFREC3      RESET ORIGINAL RECORD                        
PUTB0300 EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   MKTSTATS:  UTILIZES THE MARKET CODE ENTERED BY USER TO SELECT               
*        STATIONS FROM THE FILE FOR INCLUSION ON REPORT                         
*                                                                               
MKTSTATS NMOD1 0,*MKTS*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   OWNER,AUCOWN        LOAD UNVALIDATED OWNER CODE                  
         MVC   TVB,AUCTVB          LOAD UNVALIDATED TVB CODE                    
         MVC   RANK,AUCRNK         LOAD UNVALIDATED RANK                        
*                                                                               
*   STATIONS MUST BE FILTERED AS THEY ARE TABLED.  FILTER IS AGAINST            
*     UNVALIDATED VALUE.  IF INVALID LATER, RUN WILL NOT GO THROUGH             
*     ANYWAY, AND TABLE WILL BE REBUILT.                                        
*                                                                               
         XC    STATLIST,STATLIST                                                
         MVI   STADELIM,0          SET DELIMITER                                
         XC    STATCTR,STATCTR                                                  
         MVI   SECVIOL,C'N'                                                     
         LA    R4,STATLIST                                                      
         XC    KEY,KEY             VALIDATE MARKET CODE                         
         MVI   KEY,X'2B'           INSERT REC TYPE                              
         MVC   KEY+21(2),AGENCY    INSERT REP CODE                              
         LA    RF,AUCSTAH          GET LENGTH OF INPUT                          
         ZIC   RF,5(RF)                                                         
         LA    RE,4                SUBTRACT L(MKT=)                             
         SR    RF,RE                                                            
         EX    RF,MKTMOVE                                                       
         B     MKTS0020                                                         
MKTMOVE  MVC   KEY+23(0),AUCSTA+4  INSERT CODE BY LENGTH                        
MKTS0020 EQU   *                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    MKTS0040            YES                                          
         MVC   CONHEAD+10(L'MKTBAD),MKTBAD                                      
         B     MKTS0220            EXIT WITH ERROR                              
MKTS0040 EQU   *                                                                
         MVC   SAVMKTCD,KEY+23     SAVE THE MARKET CODE                         
         XC    KEY,KEY             CYCLE THROUGH STATIONS                       
         MVI   KEY,X'02'           INSERT REC TYPE                              
         MVC   KEY+20(2),AGENCY    INSERT REP CODE                              
         GOTO1 HIGH                GET FIRST RECORD                             
         B     MKTS0080                                                         
MKTS0060 EQU   *                                                                
         GOTO1 SEQ                                                              
MKTS0080 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME TYPE/REP?                               
         BNE   MKTS0200            NO  - FINISHED                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'        GET EXTENDED DESCRIP ELT                     
         BAS   RE,GETEL                                                         
         BNE   MKTS0060            NOT FOUND - SKIP STATION                     
         CLC   SAVMKTCD,RSTAMKTC-RSTAXXEL(R6)                                   
         BNE   MKTS0060            NOT RIGHT MARKET - SKIP IT                   
         L     R6,AIO              MARKET FOUND -                               
         USING RSTAREC,R6                                                       
MKTS0100 EQU   *                                                                
         OC    OWNER,OWNER         ANY OWNER?                                   
         BZ    MKTS0120            NO                                           
         CLC   OWNER,RSTAOWN       SAME OWNER?                                  
         BNE   MKTS0060            NO  - SKIP IT                                
MKTS0120 EQU   *                                                                
         OC    TVB,TVB             ANY TVB?                                     
         BZ    MKTS0140            NO                                           
         CLC   TVB,RSTATVB         SAME TVB?                                    
         BNE   MKTS0060            NO  - SKIP IT                                
MKTS0140 EQU   *                                                                
         OC    RANK,RANK           ANY RANK?                                    
         BZ    MKTS0160            NO                                           
         CLC   RANK,RSTARANK       SAME RANK?                                   
         BNE   MKTS0060            NO  - SKIP IT                                
MKTS0160 EQU   *                                                                
         OC    STATLIST(7),STATLIST  ANY STATIONS ENTERED?                      
         BNZ   MKTS0180            YES - NOT FIRST TIME                         
         MVC   STATN,KEY+22        NO  - SAVE FIRST STATION FOUND               
         L     R6,AIO              GET DESCRIPTIVE INFORMATION                  
         MVC   GROUP,RSTAGRUP-RSTAREC(R6)                                       
         MVC   MARKET(20),RSTAMKT-RSTAREC(R6)                                   
         TM    PRNTOPT,X'10'       LIMITED ACCESS                               
         BZ    MKTS0180                                                         
         MVI   ERROR,SECLOCK                                                    
         MVI   ELCODE,X'06'        GET ELEMENT FOR VALID SIGN-ON IDS            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
MKTS0170 BAS   RE,NEXTEL                                                        
         BNE   MKTS0240            RETURN TO ERREND....                         
         CLC   TWAORIG,10(R6)      SIGN-ON ID                                   
         BNE   MKTS0170                                                         
MKTS0180 EQU   *                                                                
         L     R6,AIO              RESET A(IO AREA)                             
         MVC   0(2,R4),RSTAGRUP-RSTAREC(R6)                                     
*                                  INSERT GROUP INTO TABLE                      
         MVC   2(5,R4),RSTAKSTA-RSTAREC(R6)                                     
*                                  INSERT STATION INTO TABLE                    
         L     RF,STATCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,STATCTR          INCREMENT STATION COUNTER                    
         LA    R4,7(R4)            BUMP TO NEXT TABLE POSITION                  
         B     MKTS0060                                                         
MKTS0200 EQU   *                                                                
         L     RF,STATCTR          ADD 1 STATION FOR TOTALS                     
         LA    RF,1(RF)                                                         
         ST    RF,STATCTR                                                       
         SR    R0,R0               SET CC = ZERO GOOD RETURN                    
         B     MKTS0260            GO BACK                                      
MKTS0220 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO BAD RETURN                   
         B     MKTS0260            GO BACK                                      
MKTS0240 EQU   *                                                                
         MVI   SECVIOL,C'Y'        SECURITY VIOLATION RETURN                    
         SR    R0,R0               CC OVERRIDDEN ON BAD RETURN                  
MKTS0260 EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
MKTBAD   DC    C'MARKET CODE ENTERED NOT RECOGNIZED'                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ MARKET CODE                                                              
***********************************************************************         
GETMKT   NTR1  BASE=*,LABEL=*                                                   
         XC    SVMKT2,SVMKT2                                                    
         XC    KEY,KEY             VALIDATE MARKET CODE                         
         MVI   KEY,X'2B'           INSERT REC TYPE                              
         MVC   KEY+21(2),AGENCY    INSERT REP CODE                              
         LA    RF,AUCSTAH          GET LENGTH OF INPUT                          
         ZIC   RF,5(RF)                                                         
         LA    RE,4                SUBTRACT L(MKT=)                             
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+23(0),AUCSTA+4  INSERT CODE BY LENGTH                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BNE   GETMKX                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         USING RMKTREC,R6                                                       
         MVC   SVMKT2,RMKTNAME     SAVE THE MARKET CODE                         
         DROP  R6                                                               
*                                                                               
         MVC   AIO,SVAIO                                                        
*                                                                               
GETMKX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   PUTCOMBO:  FORMATS RECORDS TO BE SAVED IN DUMMY FOR LATER XFER              
*        TO BUFFALO FOR ACCUMULATION.  ALSO CALCULATES RATE AND                 
*        INSERTS INTO CARRY-THROUGH AREA OF BUFFREC.                            
*                                                                               
PUTCOMBO NMOD1 0,*PCOM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLI   SAVEAUR,C'Y'        SAVE THE AVERAGES?                           
         BNE   PUTC0120            NO  -                                        
         L     R1,AAURNXT          A(NEXT TABLE ENTRY)                          
         USING AURWORK,R1                                                       
*        MVI   BUFAUR,C'Y'         SET AUR ENTRY FLAG ON                        
         MVC   AURDESC1,BUFDPT     SAVE BUFREC DESCRIPTIVE INFO                 
         MVC   AURDESC2,BUFDPTN    SAVE DPT NAME, ETC                           
* NEW                                                                           
         MVC   AURAURPU,BUFPUN                                                  
         MVC   AURAURU,BUFUN                                                    
         MVC   AURAURPD,BUFPDL     *** THIS IS WRONG!!!                         
         MVC   AURAURD,BUFDL      *** THIS IS WRONG!!!                          
* END NEW                                                                       
         XC    BUFAUR,BUFAUR       CLEAR THE AUR ENTRY FLAG                     
*&&DO                                                                           
         LA    R6,BUFACCM          A(UNITS/DOLLARS)                             
         SR    R2,R2               ** RATE **                                   
         SR    R5,R5                                                            
         SR    RE,RE                                                            
         ICM   RE,15,0(R6)          PRIOR UNITS                                 
         BZ    PUTC0060                                                         
         ICM   R3,15,8(R6)         PRIOR DOLLARS                                
         BZ    PUTC0060                                                         
         BNM   *+6                 RATE COULD BE MINUS                          
         LCR   R3,R3               NEED TO WORK WITH POSITIVE                   
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         LR    R5,R3               SAVE PRIOR RATE (AS POSITIVE)                
         ICM   RF,15,8(R6)                                                      
         BNM   *+6                                                              
         LCR   R3,R3               RESTORE NEGATIVE                             
         ST    R3,FULL                                                          
         MVC   AURAURPR,FULL       SAVE PRIOR RATE                              
*                                                                               
PUTC0060 SR    R2,R2               CURRENT                                      
         SR    RE,RE                                                            
         ICM   RE,15,4(R6)                                                      
         BZ    PUTC0100                                                         
         ICM   R3,15,12(R6)                                                     
         BZ    PUTC0100                                                         
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         ST    R3,FULL             SAVE CURRENT RATE                            
         ICM   RF,15,12(R6)                                                     
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         ST    R3,FULL                                                          
         MVC   AURAURCU,FULL                                                    
*&&                                                                             
*                                                                               
PUTC0100 EQU   *                                                                
*        LR    R3,R1               **TEST                                       
*        MVC   P+1(7),=C'AURSAV='        **TEST                                 
*        MVC   P+10(LAURWORK),AURDESC1   **TEST                                 
*        GOTO1 SPOOL,PARAS,(R8)          **TEST                                 
*        LR    R1,R3               **TEST                                       
         LA    R1,LAURWORK(R1)                                                  
         ST    R1,AAURNXT                                                       
         MVC   AURDESC1(3),=C'EOT'       SET END OF TABLE                       
PUTC0120 EQU   *                                                                
         XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   BUFCOMBO:  RETRIEVES STORED RECORDS FROM AURTABLE, REFORMATS                
*        BUFFALO RECORDS, AND PUTS THEM INTO TYPE 1, WHICH ARE                  
*        THEN RETURNED BY THE REGULAR PRINT PROCESS.                            
*                                                                               
         DS    0H                                                               
BUFCOMBO NMOD1 0,*PCOM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,AAURTABL         START OF AUR TABLE                           
         USING AURWORK,R2                                                       
         XC    BUFREC,BUFREC       CLEAR THE BUFFALO RECORD                     
BCMB0020 EQU   *                                                                
         CLC   0(3,R2),=C'EOT'     END OF DATA?                                 
         BE    BCMB0040            YES                                          
*                                                                               
*   REBUILD A BUFFALO RECORD FROM THE TABLE ENTRY                               
*                                                                               
         MVI   BUFTYP,1            INSERT BUFF TYPE                             
         MVC   BUFDPT(BUFAUR+1-BUFDPT),AURDESC1                                 
         MVC   BUFDPTN(BUFTOTAL+4-BUFDPTN),AURDESC2                             
* NEW                                                                           
         MVC   BUFPUN,AURAURPU                                                  
         MVC   BUFUN,AURAURU                                                    
         MVC   BUFPDL,AURAURPD                                                  
         MVC   BUFDL,AURAURD                                                    
* END NEW                                                                       
         GOTO1 BUFFALO,DMCB,=C'PUT',(0,ADBUFC),BUFREC                           
***      MVC   P+1(7),=C'AURTOT='        **TEST                                 
***      MVC   P+10(L'BUFREC),BUFREC     **TEST                                 
***      GOTO1 SPOOL,PARAS,(R8)          **TEST                                 
         LA    R2,LAURWORK(R2)     BUMP TO NEXT ENTRY                           
         B     BCMB0020            GO BACK FOR NEXT                             
BCMB0040 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'220RESFM16S  06/24/02'                                      
         END                                                                    
