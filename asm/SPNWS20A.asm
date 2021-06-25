*          DATA SET SPNWS20A   AT LEVEL 128 AS OF 07/17/02                      
*PHASE T20720A,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'NWS20 - BUYERS WORK SHEET - TIME SCHEDULE REPORT'               
*------------------------------------------------------------------*            
* ERRNUM - SET TO THE CORRESPONDING NUMBER IF IT DECIDED                        
* TO TAKE A DELIBERATE HIT                                                      
*                                                                               
*   IN -------  T20720 (APPLICATION)                                            
*                                                                               
*   ERRNUM                LOCATION                                              
*  ----------        -----------------                                          
*                                                                               
*    10       INIT - BAD CC FROM CALL OVERLAY (30 OR 50)                        
*    12       READREC - READING DETAIL RECORDS - COST PROBLEM                   
*    18       READREC - DAYPART TABLE PROBLEM                                   
*    19       READREC - SPOT LENGTH TABLE PROBLEM                               
*    20       GOALS   - BAD CC FROM GOTO1 GETGOAL                               
*    23       GOALS   - BAD CC FROM GETDPT                                      
*    30       READREC - BAD CC FROM READING NEW CAMPAIGN                        
*                                                                               
* IN --------  T20730 (SYSDRIVER)                                               
*                                                                               
*    15       RERDTL ROUTINE - PROBLEM RE-READING RECORD                        
*    35       SETIND ROUTINE - COST NOT IN RECORD                               
*    36-39    GETSPTS ROUTINE -COSTIND NOT SET OR CAN'T FIND                    
*                              DATE IN TABLE                                    
*-----------------------------------------------------------*                   
         EJECT                                                                  
T20720   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20720**,RA,R9,RR=RE                                           
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R2,APRECKEY        A(CAMPAIGN MARKET HEADER KEY)                 
         USING BWHRECD,R2                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     VALQ                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     INIT                                                             
         B     INPUT                                                            
         B     OUTPUT                                                           
         B     DRHOOK                                                           
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*====================*                                                          
* VALIDATE REQUEST   *                                                          
*====================*                                                          
*                                                                               
VALQ     GOTO1 AFVAL,REPREQH                                                    
         BH    VALQX                                                            
         BL    *+10                                                             
         MVC   INUSER,FVIFLD      SET REQUESTOR                                 
         CLI   ASONOFF,ASON       TEST ONLINE                                   
         BNE   VALQSTRT                                                         
         GOTO1 AVALWHEN,REPWENH   VALIDATE WHEN                                 
         BNE   VALQX              ERROR TO SCREEN                               
         GOTO1 AVALDEST,REPDIDH   VALIDATE DESTINATION ID                       
         BNE   VALQX              ERROR TO SCREEN                               
         GOTO1 AVALOTYP,REPOUTH   VALIDATE OUTPUT TYPE                          
         BNE   VALQX              ERROR TO SCREEN                               
*                                                                               
VALQSTRT XC    BWHKEY,BWHKEY      SET UP HEADER KEY                             
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
VALMED   XC    REPMDN,REPMDN      VALIDATE MEDIA FIELD                          
         OI    REPMDNH+6,FVOXMT   SET TRANSMIT BYTE                             
         GOTO1 AVALMED,REPMEDH                                                  
         BNE   VALQX              ERROR TO SCREEN                               
         MVC   REPMDN,MEDNM       RE-DISPLAY NAME                               
         MVC   BWHKAGMD,BAGYMD    MOVE INTO RECORD                              
*                                                                               
VALBYR   XC    REPBYN,REPBYN      VALIDATE BUYER FIELD                          
         OI    REPBYNH+6,FVOXMT   SET XMIT                                      
         GOTO1 AVALBYR,REPBYRH                                                  
         BNE   VALQX              ERROR TO SCREEN                               
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   REPBYN,BYRNM       RE-DISPLAY                                    
         MVC   BWHKBYR,BBYR       MOVE INTO REC                                 
         CLI   REPREQH+5,0        ANY REQUEST INPUT?                            
         BNE   *+10                                                             
         MVC   INUSER,QBYR        NO - SET BUYER                                
         OC    BYRPW,BYRPW        ANY SPECIAL PASSWORD?                         
         BZ    VALCAM                                                           
         GOTO1 AVALPWD                                                          
         BNE   VALQX              INCORRECT PASSWORD                            
*                                                                               
VALCAM   MVI   FRNGCAM,C'N'       VALIDATE CAMPAIGN FIELD                       
         CLI   REPCMPH+5,0        IF CAMPAIGN REQUESTED                         
         BNE   VALCAM1            GO VALIDATE IT                                
*****                                                                           
         TM    NWSFLAG,NWSFCLST    X'80' - USES CLIENT LIST?                    
         BNO   VALCAM0                                                          
         MVC   FVMSGNO,=AL2(FVFNONE)   NEED CAMPAIGN IF USES CLT LIST           
         B     ERRORCAM                                                         
*****                                                                           
VALCAM0  CLI   INWHEN,MIXIOKN     ALL CAMPAIGN REQUEST                          
         BE    ECAM               NOT VALID FOR NOW  REQUEST                    
         B     VALMOS                                                           
VALCAM1  XC    APELEM,APELEM      CLEAR OUT AREA                                
         GOTO1 VSCANNER,APPARM,REPCMPH,APELEM                                   
         ZIC   R0,APPARM+4                                                      
         CH    R0,=H'2'           # OF CAMPS REQUESTED                          
         BH    CAMERR                                                           
         CH    R0,=H'1'                                                         
         BNH   VALCAM4                                                          
         CLI   INWHEN,MIXIOKN                                                   
         BE    ECAM               CAMP RANGE NOT VALID FOR NOW REQUESTS         
         BAS   RE,CMPRNG                                                        
         BNE   VALQX                                                            
         B     VALMOS                                                           
*                                                                               
VALCAM4  GOTO1 AVALCAM,REPCMPH    SINGLE CAMPAIGN                               
         BNE   VALQX              ERROR TO SCREEN                               
         MVC   BWHKCAM,BCAM       MOVE IN RECORD                                
         OI    SELKIND,SELKCAM                                                  
         BAS   RE,GETCPE                                                        
         BNE   VALQX              CLI/PRD/OR EST INVALID -                      
*                                                                               
VALMOS   MVI   MKTSW,0            VALIDATE MARKET OR STATION FIELD              
         XC    SVSTA,SVSTA                                                      
         MVI   STAFILT,0                                                        
         XC    REPMSN,REPMSN      CLEAR MKT/STA FIELD                           
         OI    REPMSNH+6,FVOXMT   SET XMIT                                      
         CLI   REPMSTH+5,0        IF INPUT IN FIELD                             
         BE    VALDPT                                                           
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL,REPMSTH                                                    
         TM    FVIIND,FVINUM       AND DATA IS NUMERIC                          
         BNO   VALSTA                                                           
*                                                                               
VALMKT   LA    R1,REPMSTH        THEN VALIDATE AS MARKET                        
*                                                                               
VALMKT2  GOTO1 AVALMKT                                                          
         BNE   VALQX                                                            
         MVC   REPMSN,MKTNM       RE-DISPLAY MARKET                             
         MVC   BWHKMKT,BMKT                                                     
         MVC   SELMKT,BMKT                                                      
         OI    SELKIND,SELKMKT                                                  
         B     VALDPT                                                           
*                                                                               
VALSTA   CLC   REPMST(8),=C'ALL,ALL/' TEST ALL MARKETS AND CABLE                
         BE    *+14                                                             
         CLC   REPMST(8),=C'ALL,ALL-' OR ALL MARKETS AND NON-CABLE              
         BNE   *+14                                                             
         MVC   STAFILT,REPMST+7    YES-SAVE THE STATION FILTER                  
         B     VALDPT                                                           
         LA    RF,6                SERACH FOR ALL/ OR ALL-                      
         LA    R1,REPMST                                                        
         CLC   0(3,R1),=C'ALL'                                                  
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,*-14                                                          
         B     VALSTA2                                                          
         CLI   3(R1),C'/'                                                       
         BE    *+12                                                             
         CLI   3(R1),C'-'                                                       
         BNE   VALSTA2                                                          
         MVC   STAFILT,3(R1)       FOUND - SAVE / OR -                          
         CLC   REPMST(3),=C'ALL'   TEST AT BEGINNING OF FIELD                   
         BNE   *+16                                                             
         CLI   REPMSTH+FVILEN-FVIHDR,4  AND LENGTH=4                            
         BE    VALDPT              YES- THEN IT'S ALL/ OR ALL- ONLY             
         B     VALSTA2                                                          
         BCTR  R1,0                ELSE THE FORMAT IS 'MKT,ALL/'                
         CLI   0(R1),C','                                                       
         BNE   VALSTA2                                                          
         XC    APWORK,APWORK       MOVE MARKET TO DUMMY TWA FIELD               
         LA    RE,REPMST           AND VALIDATE                                 
         SR    R1,RE                                                            
         BNP   VALSTA2                                                          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   APWORK+L'FVIHDR(0),REPMST                                        
         LA    R1,1(R1)                                                         
         STC   R1,APWORK+(FVILEN-FVIHDR)                                        
         LA    R1,L'FVIHDR(R1)                                                  
         STC   R1,APWORK                                                        
         LA    R1,APWORK                                                        
         B     VALMKT2                                                          
*                                                                               
VALSTA2  MVI   STAFILT,0                                                        
         GOTO1 AVALSTA,REPMSTH     VALIDATE STATION                             
         BNE   VALQX                                                            
         MVC   BWHKMKT,BMKT        MOVE INTO RECORD                             
         MVC   SELMKT,BMKT                                                      
         OI    SELKIND,SELKMKT                                                  
         MVC   SVSTA,QCABLE                                                     
         MVC   CBLSYS,ACWORK       SAVE CABLE SYSTEM NAME (IF ANY)              
         OC    QCABLE,QCABLE       TEST CABLE FILTER                            
         BNZ   VALDPT              YES                                          
         MVC   SVSTA,QSTA                                                       
         OI    SELKIND,SELKSTA                                                  
         MVC   SVQSTA,QSTA         SAVE REQUESTED STATION                       
*                                                                               
VALDPT   XC    SVBDPT,SVBDPT       VALIDATE DAYPART                             
         XC    SVBSLN,SVBSLN                                                    
         CLI   REPDPLH+5,0         ANY INPUT?                                   
         BE    VALRNK                                                           
         TM    SELKIND,SELKCAM    IF MULTIPLE CAMPAIGN                          
         BO    VALDPT5                                                          
         MVI   FVMINL,1            THEN WE NEED SPOT LENGTH                     
         MVI   FVMAXL,4                                                         
         GOTO1 AFVAL,REPDPLH                                                    
         BNE   SLNERR                                                           
         CLI   FVILEN,2                                                         
         BE    SLNERR             CAN'T JUST HAVE SPOT LENGTH                   
         CLI   FVILEN,1                                                         
         BNE   VALDPT2                                                          
         TM    FVIIND,FVINUM      ALLOWED TO JUST INPUT                         
         BZ    SLNERR             DAYPART BUT NOT JUST SPOT LENGTH              
         OI    SELKIND,SELKDPT                                                  
         B     VALRNK                                                           
*                                                                               
VALDPT2  OI    SELKIND,SELKDPT    MUST BE DAYPART & SPOTLENGTH                  
         ZIC   RE,FVILEN                                                        
         BCTR  RE,0                                                             
         LA    RF,FVIFLD+1                                                      
VALDPT2A CLI   0(RF),C'0'                                                       
         BL    SLNERR                                                           
         CLI   0(RF),C'9'                                                       
         BH    SLNERR                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,VALDPT2A                                                      
*                                                                               
         LA    RE,1                                                             
         CLI   FVILEN,3                                                         
         BE    *+8                                                              
         LA    RE,2                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,FVIFLD+1(0)                                                
         CVB   RE,APDUB                                                         
         STC   RE,APBYTE                                                        
*                                                                               
         LA    RE,SLNTAB          CHECK IF IN SPOT LENGTH TABLE                 
VALDPT4  CLI   0(RE),0                                                          
         BE    SLNERR             NOT IN TABLE                                  
         CLC   APBYTE,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     VALDPT4                                                          
         OI    SELKIND,SELKSLN    TURN ON SLN INDICATOR                         
         MVC   SVBSLN(1),0(RE)    SAVE REQUESTED SPOT LENGTH                    
         B     VALRNK                                                           
*                                                                               
VALDPT5  GOTO1 AVALDPL,REPDPLH                                                  
         BNE   VALQX                                                            
         CLI   BDPT,0             ALL DPT(ALSO IMPLIES ALL LENGTHS)             
         BE    VALDPT8                                                          
         MVC   SVBDPT,BDPT        SAVE SPECIFIED DAYPART                        
         OI    SELKIND,SELKDPT    NO -SPECIFIED ONE DAYPART                     
VALDPT8  CLI   BSLN,0             ALL LENGTHS?                                  
         BE    VALRNK                                                           
         OI    SELKIND,SELKSLN    NO -SPECIFIED ONE LENGTH                      
         MVC   SVBSLN,BSLN        SAVE IT                                       
*                                                                               
VALRNK   DS    0H                   VALIDATE RANK SEQUENCE                      
         MVI   RANK,C'Y'            DEFAULT - RANK ON CPP OR DEMO               
         CLI   REPRNKH+5,0                                                      
         BE    VALDEMO                                                          
         CLC   REPRNK(3),=C'CPP'                                                
         BE    VALDEMO                                                          
         CLC   REPRNK(4),=C'DEMO'                                               
         BNE   *+12                                                             
         OI    SELKIND,SELKRDM                                                  
         B     VALDEMO                                                          
*                                                                               
         CLC   REPRNK(3),=C'STA'                                                
         BNE   *+12                                                             
         OI    SELKIND2,SELKRST                                                 
         B     VALDEMO                                                          
         CLC   REPRNK(2),=C'DT'                                                 
         BNE   VALRNK5                                                          
         OI    SELKIND2,SELKRDT                                                 
         MVI   RANK,C'N'          DON'T RANK ON CPP/DEMO                        
         B     VALDEMO                                                          
*                                                                               
VALRNK5  OI    SELKIND2,SELKRST                                                 
         CLC   REPRNK(4),=C'SCPP'                                               
         BE    VALDEMO                                                          
         CLC   REPRNK(4),=C'SDEM'                                               
         BNE   *+12                                                             
         OI    SELKIND,SELKRDM                                                  
         B     VALDEMO                                                          
         CLC   REPRNK(3),=C'SDT'                                                
         BNE   RNKERR                                                           
         OI    SELKIND2,SELKRDT                                                 
         MVI   RANK,C'N'          DON'T RANK ON CPP/DEMO                        
*                                                                               
VALDEMO  MVI   OVDEMO,C'N'         NO OVERRIDING DEMOS                          
         XC    SESTDEMS,SESTDEMS                                                
         CLI   REPRATH+5,0         ANY DEMOS?                                   
         BE    VALOPT                                                           
         MVI   OVDEMO,C'Y'         YES OVERRIDING DEMOS                         
         L     R8,AIOAREA3                                                      
         USING DBLOCKD,R8                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBCOMFCS,ACOM                                                    
         XC    APWORK,APWORK                                                    
         CLC   REPRATH+8(4),=CL4'NULL'    NO DEMO DISPLAY                       
         BE    VALDEMO6                                                         
         GOTO1 VDEMOVAL,APPARM,(1,REPRATH),(6,APWORK),(C'S',DBLOCK),0           
         CLI   4(R1),0                                                          
         BE    DEMERR                                                           
*                                                                               
VALDEMO6 XC    LDEMHLD,LDEMHLD                                                  
         MVC   LDEMHLD(2),APWORK+1                                              
         MVC   LDEMHLD+6(2),APWORK+4                                            
         MVC   LDEMHLD+12(2),APWORK+7                                           
         MVC   LDEMHLD+18(2),APWORK+10                                          
         MVC   LDEMHLD+24(2),APWORK+13                                          
         MVC   LDEMHLD+30(2),APWORK+16                                          
         MVI   LDEMEND,X'FF'                                                    
*                                                                               
VALOPT   CLI   REPOPTH+5,0         VALIDATE OPTIONS FIELD                       
         BE    VALSOPT                                                          
         GOTO1 VSCANNER,ACPARM,REPOPTH,(4,AIOAREA2),C',=,='                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LA    R1,REPOPTH                                                       
         ST    R1,FVADDR                                                        
         SR    R0,R0                                                            
         ICM   R0,1,ACPARM+4       R0 = NUMBER OF OPTIONS                       
         BZ    VALQX                                                            
         L     RF,AIOAREA2         VALIDATE OPTIONS                             
VALOPT10 LA    RE,OPTTAB                                                        
VALOPT20 CLI   0(RE),X'FF'                                                      
         BE    VALQX               INVALID                                      
         CLC   0(3,RE),12(RF)                                                   
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     VALOPT20                                                         
         OC    OPTSW,3(RE)         VALID OPTION                                 
         LA    RF,32(RF)           NEXT OPTION                                  
         BCT   R0,VALOPT10                                                      
*                                                                               
VALSOPT  MVI   SUMIND,0            VALIDATE SUMMARY OPTION                      
         GOTO1 AFVAL,REPSUMH                                                    
         BH    VALQX                                                            
         BL    VALSOPT5                                                         
         CLC   FVIFLD(4),=C'DPT '                                               
         BNE   VALSOPT1                                                         
         OI    SUMIND,SUMIDPT                                                   
         B     VALSOPT5                                                         
*                                                                               
VALSOPT1 CLC   FVIFLD(4),=C'ALL '                                               
         BNE   VALSOPT4                                                         
         OI    SUMIND,SUMISUB                                                   
         TM    SELKIND,SELKDPT    ALL DAYPARTS - GO AHEAD                       
         BNO   VALSOPT5                                                         
         MVC   TMPDPT,BDPT        ONE DAYPART                                   
         BAS   RE,GETSUBS         GET ALL OTHER CORRESPONDING DPTS              
         BNE   SOPTERR            PROBLEM                                       
         CLI   SUBTYPE,C'R'       REGULAR - ONE DPT                             
         BNE   VALSOPT2                                                         
         MVC   SUBS(1),BDPT       PUT IT INTO SUBS TABLE                        
         MVC   COMDPLST(1),SUBS   AND DPT TABLE                                 
         B     VALSOPT5           THAT'S IT                                     
*                                                                               
VALSOPT2 LA    R1,SUBS                                                          
         LA    R0,L'SUBS                                                        
VALSOPT3 CLI   0(R1),0                                                          
         BE    VALSOPT5                                                         
         MVC   TMPDPT,0(R1)                                                     
         BAS   RE,ADDPT                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,VALSOPT3                                                      
         B     VALSOPT5                                                         
*                                                                               
VALSOPT4 CLC   FVIFLD(4),=C'STA '                                               
         BNE   SOPTERR                                                          
         OI    SUMIND,SUMISTA                                                   
*                                                                               
VALSOPT5 MVI   SUPDEM,C'N'         OPTION TO SUPPRESS DEMOS                     
         GOTO1 AFVAL,REPDEMH                                                    
         BH    VALQX                                                            
         BL    VALCPPM                                                          
         MVC   SUPDEM,FVIFLD                                                    
         CLI   SUPDEM,C'N'                                                      
         BE    VALCPPM                                                          
         CLI   SUPDEM,C'Y'                                                      
         BE    VALCPPM                                                          
         LA    R1,REPDEMH                                                       
         ST    R1,FVADDR                                                        
         B     EINV                                                             
*                                                                               
VALCPPM  MVI   SUPCPPM,C'N'       OPTION TO SUPPRESS CPP/CPM                    
         GOTO1 AFVAL,REPCPMH                                                    
         BH    VALQX                                                            
         BL    VALCST                                                           
         MVC   SUPCPPM,FVIFLD                                                   
         CLI   SUPCPPM,C'N'                                                     
         BE    VALCST                                                           
         CLI   SUPCPPM,C'Y'                                                     
         BE    VALCST                                                           
         LA    R1,REPCPMH                                                       
         ST    R1,FVADDR                                                        
         B     EINV                                                             
*                                                                               
VALCST   MVI   SUPCST,C'N'         OPTION TO SUPPRESS COST                      
         GOTO1 AFVAL,REPCSTH                                                    
         BH    VALQX                                                            
         BL    VALSPC                                                           
         MVC   SUPCST,FVIFLD                                                    
         CLI   SUPCST,C'N'                                                      
         BE    VALSPC                                                           
         CLI   SUPCST,C'Y'                                                      
         BE    VALSPC                                                           
         LA    R1,REPCSTH                                                       
         ST    R1,FVADDR                                                        
         B     EINV                                                             
*                                                                               
VALSPC   MVI   SPAC,1              OPTION TO CHANGE SPACING                     
         CLI   REPSPCH+5,0                                                      
         BE    VALPSTA                                                          
         MVI   SPAC,2                                                           
         CLI   REPSPC,C'2'                                                      
         BE    VALPSTA                                                          
         MVI   SPAC,3                                                           
         CLI   REPSPC,C'3'                                                      
         BE    VALPSTA                                                          
         LA    R1,REPSPCH                                                       
         ST    R1,FVADDR                                                        
         B     EINV                                                             
*                                                                               
VALPSTA  DS    0H                 PAGE BY STATION? (OVERRIDE PROFILE)           
         MVI   PSTA,0             NO INPUT - USE VALUE FROM PROFILE             
         CLI   REPPSTH+5,0                                                      
         BE    VALRD                                                            
         MVI   PSTA,C'Y'                                                        
         CLI   REPPST,C'Y'        OVERRIDE PROFILE?                             
         BE    VALRD              YES                                           
         MVI   PSTA,C'N'                                                        
         CLI   REPPST,C'N'                                                      
         BE    VALRD              NO -DON'T OVERRIDE PROFILE                    
         LA    R1,REPPSTH                                                       
         ST    R1,FVADDR                                                        
         B     EINV                                                             
*                                                                               
VALRD    MVC   SELKEY,APRECKEY     SET SELECT KEY                               
         L     R8,AREP                                                          
         USING REPD,R8                                                          
         MVI   REPCLASS,C'B'       CLASS B REPORT                               
         MVC   REPDESC,REPDESCL                                                 
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0          SET NO FOOTLINES REQUIRED                    
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS          SET A(SPEC POOL)                             
         MVI   REPHEADH,1         MUST SET THESE VALUES INORDER                 
         MVI   REPMIDSH,1         TO GET  A HOOK                                
         DROP  R8                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   IOKEY(13),BWHKTYP                                                
         BAS   RE,GETNAMES        GET EXPANDED DEMO NAMES                       
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI   READ DETAIL HEADER                           
*                                                                               
         MVC   HOLDKEY(13),APRECKEY                                             
         CLC   IOKEY(4),IOKEYSAV  COMPARE MEDIA/BUYER                           
         BNE   VALQERR            EXIT IF NOT EQUAL                             
*                                                                               
         TM    SELKIND,SELKCAM    TEST CAMPAIGN                                 
         BZ    VALRD30            ALL OR RANGE OF CAMPAIGN                      
         CLC   IOKEY(6),IOKEYSAV                                                
         BNE   VALQERR            TEST ON SINGLE CAMPAIGN                       
*                                                                               
         TM    SELKIND,SELKMKT                                                  
         BZ    VALRD40                                                          
         CLC   IOKEY(8),IOKEYSAV                                                
         BNE   VALQERR                                                          
         B     VALRD50                                                          
*                                                                               
VALRD30  XC    HOLDKEY+4(6),HOLDKEY+4                                           
         B     VALRD50                                                          
*                                                                               
VALRD40  XC    HOLDKEY+6(4),HOLDKEY+6                                           
         B     VALRD50                                                          
*                                                                               
VALQERR  OI    TWAFLAG,TWANODET    INVALID REQUEST                              
         LA    R1,REPREQH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
*                                                                               
VALRD50  DS    0H                                                               
VALQX    B     EXIT                                                             
         EJECT                                                                  
*===========================*                                                   
* ERRORS CALLED FROM VALQ   *                                                   
*===========================*                                                   
*                                                                               
RNKERR   LA    R1,REPRNKH                                                       
         ST    R1,FVADDR                                                        
         B     ERRSET                                                           
*                                                                               
DEMERR   LA    R1,REPRATH                                                       
         ST    R1,FVADDR                                                        
         B     ERRSET                                                           
*                                                                               
CAMERR   LA    R1,REPCMPH                                                       
         ST    R1,FVADDR                                                        
         B     ERRSET                                                           
*                                                                               
SLNERR   LA    R1,REPDPLH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVISLN)                                             
         B     VALQX                                                            
*                                                                               
SOPTERR  DS    0H                                                               
EINV     DS    0H                                                               
ERRSET   MVC   FVMSGNO,=AL2(FVFNOTV)      GO TO ERRROR                          
         B     VALQX                                                            
*                                                                               
OPTERR   MVC   FVMSGNO,=AL2(FVDPTDEM)       YES - INVALID                       
         B     VALQX                                                            
*                                                                               
ECAM     MVC   FVMSGNO,=AL2(FVONECAM)                                           
ERRORCAM LA    R1,REPCMPH                                                       
         ST    R1,FVADDR                                                        
         B     VALQX                                                            
*                                                                               
OPTTAB   DC    CL3'SCH',AL1(SCHEDLIN)                                           
         DC    CL3'COM',AL1(COMMLIN)                                            
         DC    CL3'DPT',AL1(OPTDPT)                                             
         DC    CL3'SIX',AL1(SIXDEMS)                                            
         DC    X'FF'                                                            
*                                                                               
REPDESCL DC    CL11'BTIME SCHED'                                                
*                                                                               
XFF      DC    XL4'FFFFFFFF'                                                    
*                                                                               
SLNTAB   DS    0XL1                                                             
         DC    AL1(10,15,20,30,40,45,50,60,75,90,120,105,150,75,5)              
         DC    AL1(0)                                                           
*                                                                               
FF       EQU   X'FF'                                                            
*                                                                               
         EJECT                                                                  
*=============================*                                                 
* ROUTINES CALLED FROM VALQ   *                                                 
*=============================*                                                 
*                                                                               
* -- ROUTINE TO GET CLIENT, PRODUCT, AND ESTIMATE DETAILS                       
*                                                                               
GETCPE   NTR1                                                                   
         GOTO1 AGETCLT,CMPCLTC    (GET CLIENT)                                  
         BNE   CPEBAD                                                           
         XC    LPRDNM1,LPRDNM1                                                  
         XC    LPRDNM2,LPRDNM2                                                  
         CLI   CMPPRD1,0          TEST FOR PIGGBACKS                            
         BE    GETCPE2                                                          
         GOTO1 AGETPRD,CMPPRD1    GET PIGGYBACK PRD 1                           
         MVC   LPRDNM1,PRDNM                                                    
         CLI   CMPPRD2,0                                                        
         BE    GETCPE2                                                          
         GOTO1 AGETPRD,CMPPRD2    GET PIGGYBACK PRD 2                           
         MVC   LPRDNM2,PRDNM                                                    
*                                                                               
GETCPE2  GOTO1 AGETPRD,CMPPRDN    (GET PRODUCT)                                 
         BNE   CPEBAD                                                           
         GOTO1 AGETEST,CMPESTN    GET CAMPAIGN ESTIMATE DETAILS                 
         BNE   CPEBAD                                                           
* THIS WILL ONLY HAPPEN IF REQUESTING MULTIPLE CAMPAIGNS (ON 2,3 ETC)           
* WE DON'T WANT TO CREAM THE OVERRIDES                                          
         CLI   OVDEMO,C'Y'         OVERRIDING DEMOS                             
         BE    GETCPEY                                                          
         XC    LDEMHLD,LDEMHLD                                                  
         LA    RE,LDEMHLD                                                       
         LA    RF,ESTDEMS+1                                                     
         LA    R0,6               ALLOW UP TO SIX DEMOS                         
GETCPE4  MVC   0(2,RE),0(RF)                                                    
         LA    RE,6(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,GETCPE4                                                       
GETCPEY  CR    RB,RB                                                            
         B     GETCPEX                                                          
*                                                                               
CPEBAD   LTR   RB,RB                                                            
GETCPEX  B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
*--GETNAMES RETRIEVES THE DEMO EXPANSION NAMES FROM LDEMHLD TABLE               
*                                                                               
GETNAMES NTR1                                                                   
         L     R8,AIOAREA3                                                      
         USING DBLOCKD,R8                                                       
         XC    DBLOCK,DBLOCK                                                    
         XC    APDUB,APDUB                                                      
         MVC   SESTDEMS+1(2),LDEMHLD                                            
         MVC   SESTDEMS+4(2),LDEMHLD+6                                          
         MVC   SESTDEMS+7(2),LDEMHLD+12                                         
         MVC   SESTDEMS+10(2),LDEMHLD+18                                        
         MVC   SESTDEMS+13(2),LDEMHLD+24                                        
         MVC   SESTDEMS+16(2),LDEMHLD+30                                        
         XC    DBLOCK,DBLOCK       GET DEMO NAMES                               
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    RE,SESTDEMS                                                      
         MVI   APBYTE,6                                                         
         ICM   RE,8,APBYTE                                                      
         ST    RE,APPARM                                                        
         LA    RE,ESTUSRNM                                                      
         ST    RE,APPARM+12                                                     
         XC    COMDNAMS,COMDNAMS                                                
         GOTO1 VDEMOCON,APPARM,,(2,COMDNAMS),(C'S',DBLOCK)                      
         B     EXIT                                                             
         EJECT                                                                  
* CHECKS RANGE OF CAMPAIGNS AND FIRST DEMO IF NECESSARY                         
*                                                                               
CMPRNG   NTR1                                                                   
         ZIC   R0,APPARM+4                                                      
         LA    R4,APELEM          CAMP RANGE REQUESTED                          
         LA    R8,RNGCAM          CAMPAIGN RANGE                                
         LA    R1,RNGCAMB         BINARY VALUE OF CAMPAIGN RANGE                
CMPRNG10 CLI   1(R4),0                                                          
         BNE   CMPERR             NO SECOND HALF WANTED                         
         TM    2(R4),X'80'        TEST NUMERIC                                  
         BZ    CMPERR                                                           
         MVC   0(2,R1),6(R4)                                                    
         MVC   0(2,R8),6(R4)                                                    
         XC    0(2,R8),XFF                                                      
         LA    R4,32(R4)                                                        
         LA    R8,2(R8)                                                         
         LA    R1,2(R1)                                                         
         BCT   R0,CMPRNG10                                                      
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,RNGCAMB+2                                                   
         SR    R1,R1                                                            
         ICM   R1,3,RNGCAMB                                                     
         STCM  R1,3,CURCMP                                                      
         CR    RE,R1                                                            
         BNH   CMPERR             END MUST BE GREATER THA START                 
         SR    RE,R1                                                            
         LA    RE,1(RE)                                                         
         CH    RE,=H'25'                                                        
         BH    CMPERR             RANGE TOO LARGE                               
*                                                                               
CMPRNG20 SR    R2,R2                                                            
         ICM   R2,3,CURCMP                                                      
         CVD   R2,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  CMP,APDUB                                                        
         XC    APWORK,APWORK                                                    
         MVC   APWORK+8(L'CMP),CMP                                              
         LA    R1,L'CMP                                                         
         STC   R1,APWORK+5                                                      
         LA    R1,8(R1)                                                         
         STC   R1,APWORK                                                        
         GOTO1 AVALCAM,APWORK                                                   
         BNE   CMPERR                                                           
         CLI   REPSUMH+5,0        IF NO SUMMARY OPTION                          
         BE    CMPRNG30           THAT'S ALL WE NEED TO CHECK                   
         CLI   REPRATH+5,0        IF OVERRIDE DEMO SPECIFIED                    
         BNE   CMPRNG30           THAT'S ALL WE NEED TO CHECK                   
         BAS   RE,CHKDEM          CHECK DEMO                                    
         BNE   CMPRNGN            MSG ALREADY SET                               
CMPRNG30 SR    R1,R1                                                            
         ICM   R1,3,CURCMP                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,CURCMP                                                      
         CLM   R1,3,RNGCAMB+2                                                   
         BNH   CMPRNG20                                                         
         MVI   FRNGCAM,C'Y'       RANGE OF CAMPAIGNS                            
         CR    RB,RB                                                            
CMPRNGX  B     EXIT                                                             
         SPACE                                                                  
CMPERR   MVC   FVMSGNO,=AL2(FVFNOTV)      GO TO ERRROR                          
CMPRNGN  LA    R1,REPCMPH                                                       
         ST    R1,FVADDR                                                        
         LTR   RB,RB                                                            
         B     CMPRNGX                                                          
         SPACE                                                                  
CHKDEM   NTR1                                                                   
         GOTO1 AGETCLT,CMPCLTC    (GET CLIENT)                                  
         BNE   CHKDEMX                                                          
         XC    LPRDNM1,LPRDNM1                                                  
         XC    LPRDNM2,LPRDNM2                                                  
         CLI   CMPPRD1,0          TEST FOR PIGGBACKS                            
         BE    CHKDEM2                                                          
         GOTO1 AGETPRD,CMPPRD1    GET PIGGYBACK PRD 1                           
         MVC   LPRDNM1,PRDNM                                                    
         CLI   CMPPRD2,0                                                        
         BE    CHKDEM2                                                          
         GOTO1 AGETPRD,CMPPRD2    GET PIGGYBACK PRD 2                           
         MVC   LPRDNM2,PRDNM                                                    
*                                                                               
CHKDEM2  GOTO1 AGETPRD,CMPPRDN    (GET PRODUCT)                                 
         BNE   CHKDEMX                                                          
         GOTO1 AGETEST,CMPESTN    GET CAMPAIGN ESTIMATE DETAILS                 
         BNE   CHKDEMX                                                          
*                                                                               
         CLC   CURCMP,RNGCAMB     IF FIRST ONE                                  
         BNE   CHKDEM5                                                          
         XC    LDEMHLD,LDEMHLD    SAVE THE DEMO TO COMPARE WITH                 
         LA    RE,LDEMHLD         THE NEXT CAMPAIGNS                            
         LA    RF,ESTDEMS+1                                                     
         LA    R0,6               ALLOW UP TO SIX DEMOS                         
CHKDEM3  MVC   0(2,RE),0(RF)                                                    
         LA    RE,6(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,CHKDEM3                                                       
         B     CHKDEMY                                                          
*                                                                               
CHKDEM5  LA    RE,LDEMHLD                                                       
         LA    RF,ESTDEMS+1                                                     
         CLC   0(2,RE),0(RF)                                                    
         BE    CHKDEMY                                                          
         MVC   FVMSGNO,=AL2(FVNEQDEM) DEMOS MUST BE CONSISTENT                  
         LTR   RB,RB                                                            
         B     CHKDEMX                                                          
*                                                                               
CHKDEMY  CR    RB,RB                                                            
CHKDEMX  B     EXIT                                                             
         EJECT                                                                  
*==============================*                                                
* INIT - INITIALIZATION MODE   *                                                
*==============================*                                                
*                                                                               
INIT     MVI   FSTINPUT,C'Y'      FIRST TIME FOR INPUT SWITCH                   
         MVI   ENDSW,0            NOT FINISHED READING RECORDS                  
         MVI   READRC,C'D'        READING DETAIL RECORDS                        
         XC    DISPSLN,DISPSLN                                                  
         XC    DISPDPT,DISPDPT                                                  
         XC    MDPTLST,MDPTLST                                                  
         XC    COMSLLST,COMSLLST                                                
         XC    COMDPLST,COMDPLST                                                
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BE    INIT50                OFFLINE- DO MAINTENANCE CALLS              
* ------------ONLINE ONLY ---------------------*                                
         SPACE                                                                  
         GOTO1 VCOLY,APPARM,(X'30',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GLASYSDR,0(R1)     SAVE ADDRESS OF SYSTEM DRIVER                 
*                                                                               
         GOTO1 VCOLY,APPARM,(X'50',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GLAPROG,0(R1)      SAVE ADDRESS OF DPG PROGRAM                   
*                                                                               
         LA    RF,SPARE                                                         
         ST    RF,GLADTAB                                                       
         LH    RF,=Y(SPAREX-SPARE)                                              
         ST    RF,GLSIZE                                                        
         LA    R0,REPSPEC                                                       
         ST    R0,GLASPECS        MUST DO THIS FOR DROOL                        
         B     INIT55                                                           
* -----------  OFFLINE ONLY ---------------------*                              
         SPACE                                                                  
INIT50   XC    APPARM(12),APPARM      LOAD SYSTEM DRIVER                        
         L     R1,=V(DUMMY)                                                     
         A     R1,APRELO                                                        
         LA    R1,7(R1)           WANT TO DOUBLE ALIGN ADDRESS TO               
         SRL   R1,3               THE NEXT AVAILABLE SPOT FOWARD                
         SLL   R1,3                                                             
         ST    R1,APPARM                                                        
         MVC   APPARM+4(4),=X'D9020730'                                         
         GOTO1 VCOLY,APPARM                                                     
         CLI   APPARM+4,X'FF'                                                   
         BE    INITBAD                                                          
         BAS   RE,GETLEN          GET LENGTH OF SYSDRIVER                       
         MVC   SVLEN,APFULL       AND SAVE IT                                   
         MVC   GLASYSDR,APPARM    AND SAVE ADDRESS TOO                          
*                                                                               
         XC    APPARM(12),APPARM  LOAD DPG PROGRAM                              
         L     R1,GLASYSDR        IN BEHIND SYSDRIVER                           
         A     R1,APRELO                                                        
         A     R1,SVLEN                                                         
         ST    R1,APPARM                                                        
         MVC   APPARM+4(4),=X'D9020750'                                         
         GOTO1 VCOLY,APPARM                                                     
         CLI   4(R1),X'FF'                                                      
         BE    INITBAD                                                          
         BAS   RE,GETLEN          GET LENGTH OF PHASE JUST LOADED               
         MVC   SVLEN2,APFULL      SAVE IT                                       
         MVC   GLAPROG,APPARM     AND SAVE ADDRESS OF DPG                       
         SPACE                                                                  
* ---------BOTH OFFLINE AND ONLINE ---------*                                   
*                                 SET GLOBAL OPTIONS                            
INIT55   MVI   GLOPTS,C'Y'        STATION SEQUENCE                              
         TM    SELKIND2,SELKRST                                                 
         BO    INIT60                                                           
         MVI   GLOPTS,C'N'                                                      
*                                                                               
INIT60   MVI   GLOPTS+1,C'Y'      RANK BY DEMO/CPP                              
         CLI   RANK,C'Y'                                                        
         BE    *+8                                                              
         MVI   GLOPTS+1,C'N'                                                    
*                                                                               
         MVI   GLOPTS+2,C'N'      N=(DON'T PUT DAYPART IN HEADLINE)             
         TM    OPTSW,OPTDPT                                                     
         BO    *+8                                                              
         MVI   GLOPTS+2,C'Y'      PUT IN HEADLINE                               
*                                                                               
         MVI   GLOPTS+4,C'N'      NO SUMMARY                                    
         CLI   SUMIND,0                                                         
         BE    INIT61                                                           
         MVI   GLOPTS+4,C'Y'      SUMMARY YES                                   
         MVI   GLOPTS+5,C'D'      DAYPART SUMMARY                               
         TM    SUMIND,SUMIDPT                                                   
         BO    INIT61                                                           
         TM    SUMIND,SUMISUB                                                   
         BO    INIT61                                                           
         MVI   GLOPTS+5,C'S'      STATION SUMMARY                               
*                                                                               
INIT61   MVI   GLOPTS+6,C'Y'                                                    
         CLI   PSTA,0             ANY INPUT FOR PAGE BY STATION?                
         BE    INIT62             NOPE - USE PROFILE                            
*                                                                               
         CLI   PSTA,C'Y'          OVERRIDE PROFILE WITH PAGE BREAK?             
         BE    INIT65             YES                                           
         MVI   GLOPTS+6,C'N'      NO - DON'T PAGE BREAK                         
         B     INIT65                                                           
*                                                                               
INIT62   CLI   CLTBWPRO+1,C'Y'    PROFILE A YES?                                
         BE    *+8                                                              
         MVI   GLOPTS+6,C'N'                                                    
*                                                                               
INIT65   MVI   GLOPTS+7,C'N'                                                    
         CLI   SUPDEM,C'N'        IF SUPPRESSING DEMOS                          
         BE    INIT68                                                           
         MVI   GLOPTS+7,C'Y'      SUPPRESS GOAL RECAP REPORT                    
*                                                                               
INIT68   MVI   GLOPTS+8,C'N'      DON'T ORDER SUB DAYPARTS                      
         TM    SUMIND,SUMISUB                                                   
         BNO   *+8                                                              
         MVI   GLOPTS+8,C'S'      ORDER SUB DAYPARTS                            
         MVI   GLOPTS+9,C'N'                                                    
         CLI   FRNGCAM,C'Y'                                                     
         BNE   *+8                                                              
         MVI   GLOPTS+9,C'Y'      RECAP OF CAMPAIGNS                            
*                                                                               
         MVI   GLFHEADL,10                                                      
         MVI   GLLHEADL,13                                                      
         MVI   GLBOXOPT,C'N'                                                    
         MVI   GLDETHED,C'N'                                                    
*                                        ALWAYS GIVE A TOTAL LINE               
         OI    GLINDS,GLPALTOT+GLPALDET     AND ALL DETAILS                     
         OI    GLINDS2,GLPWHOLE   ALWAYS PRINT WHOLE DETAIL LINE                
*                                                                               
         MVI   GLTWORKD,GLTSPBWS  WORKD PROVIDING                               
         ST    R7,GLAWORKD        GIVE ADDRESS OF WORKING STORAGE               
*                                 DOUBLE OR TRIPLE SPACING REQUESTED?           
         CLI   SPAC,2                                                           
         BNE   *+8                                                              
         MVI   GLSPACE,2                                                        
         CLI   SPAC,3                                                           
         BNE   *+8                                                              
         MVI   GLSPACE,3                                                        
         MVI   GLTRACE,C'N'       PATCH TO YES FOR TRACE                        
         LA    R1,CLTBWPRO                                                      
         ST    R1,GLAPPROF        ADDRESS OF PROGRAM PROFILE                    
INTDRIVX B     EXIT                                                             
*                                                                               
INITBAD  MVI   ERRNUM,10          BAD NEWS                                      
         B     CLDUMP                                                           
         SPACE 2                                                                
*                                                                               
* -- GET LENGTH OF PHASE JUST LOADED                                            
* -- RETURN IT IN APFULL                                                        
*                                                                               
GETLEN   DS    0H                                                               
         LR    R0,RE                                                            
         SR    RE,RE                                                            
         ICM   RE,7,9(R1)                                                       
         LA    RE,7(RE)                                                         
         SRL   RE,3                                                             
         SLL   RE,3                                                             
         ST    RE,APFULL          STORE IT IN FULL                              
         LR    RE,R0              AND BRANCH BACK                               
         BR    RE                                                               
         EJECT                                                                  
*================================*                                              
* INPUT - INPUT FOR DRIVER/DROOL *                                              
*================================*                                              
*                                                                               
INPUT    TM    INWHEN,MIXIOKN     IF NOW REQUEST                                
         BNO   INPUT5                                                           
         GOTO1 VGETFACT,APPARM,0  GET COUNT FROM GETFACT                        
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    R3,=H'8'                                                         
         D     R2,=F'10'           80 PERCENT OF MAX IOS IN R3                  
         CLM   R3,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    INPUT5              NO - STILL WITHIN 90 PERCENT                 
         MVC   FVMSGNO,=X'FFF2'    TOO LARGE FOR NOW REQUEST                    
         B     INPUTX                                                           
*                                                                               
INPUT5   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   MKTSW,0                                                          
         BNE   INPUT10            READING GOALS INBETWEEN MARKETS               
         CLI   READRC,C'D'        READING DETAIL RECORDS                        
         BNE   INPUT20                                                          
*                                                                               
INPUT10  BAS   RE,READREC         READ NEXT RECORD FOR INPUT                    
         CLI   ENDSW,X'FF'                                                      
         BNE   INPUTX                                                           
*                                                                               
         TM    SELKIND,SELKMKT    IF ALL MARKETS -                              
         BZ    INPUT30            ALREADY READ GOAL RECORDS                     
         MVI   READRC,C'G'        READING GOALS                                 
         MVI   ENDSW,0            RESET END SWITCH                              
         MVI   FSTGIN,C'Y'        FIRST TIME READING GOALS                      
*                                                                               
INPUT20  BAS   RE,GOALS                                                         
         CLI   ENDSW,X'FF'                                                      
         BNE   INPUTX                                                           
INPUT30  MVI   APMODE,APMDREND                                                  
*                                                                               
INPUTX   B     EXIT                                                             
         EJECT                                                                  
*=========================================*                                     
* READREC - READS THE DETAIL RECORDS      *                                     
* SWITCHES OFF MEAN ALL REQUEST FOR FIELD *                                     
* SWITCHES ON MEAN FIELD SPECIFIED        *                                     
* SETS ENDSW TO X'FF'  WHEN FINISHED      *                                     
* READING RECORDS                         *                                     
*=========================================*                                     
*                                                                               
READREC  NTR1                                                                   
         L     R3,AIOAREA1        CURRENT RECORD                                
         CLI   MKTSW,0                                                          
         BNE   RDREC705           READING GOALS BETWEEN MKT                     
*                                                                               
         CLI   FSTINPUT,C'Y'      FIRST INPUT CALL?                             
         BNE   RDREC550           NOPE - GET NEXT RECORD                        
         MVI   FSTINPUT,C'N'      RESET SWITCH                                  
         CLI   FSTSW,X'FF'                                                      
         BE    RDREC20                                                          
*                                                                               
RDREC10  MVC   IOKEY(13),HOLDKEY                                                
RDREC15  GOTO1 AIO,IOSPTDIR+IOHI+IO1                                            
         B     RDREC40                                                          
*                                                                               
RDREC20  MVC   IOKEY(13),HOLDKEY                                                
         GOTO1 AIO,IOSPTDIR+IORD+IO1                                            
         GOTO1 AIO,IOSPTDIR+IOSQ+IO1                                            
         MVI   FSTREC,C'N'                                                      
RDREC40  CLC   IOKEY(4),HOLDKEY   IF NOT SAME MEDIA/BUYER                       
         BNE   NOMORE             EXIT                                          
*                                 ELSE - CHECK CAMPAIGN                         
RDREC60  CLC   IOKEY+4(2),HOLDKEY+4   IF SAME CAMPAIGN                          
         BE    RDREC76                MAKE SURE HAVE DETAILS, IF NOT            
         TM    SELKIND,SELKCAM        CHECK IF SINGLE CAMPAIGN REQUEST          
         BO    NOMORE                 YES - END                                 
*                                     FORCE RE-READ OF CAMPAIGN DETAILS         
         XC    BCAM,BCAM              IF RANGE OR ALL CAMP REQUEST              
*                                                                               
         CLI   FRNGCAM,C'Y'           CAMPAIGN RANGE                            
         BNE   RDREC76                NO---ALL CAMPAIGNS                        
         CLC   IOKEY+4(2),RNGCAM+2    TEST WITHIN CAMPAIGN RANGE                
         BNL   RDREC65                                                          
         GOTO1 AIO,IOSPTDIR+IOSQ+IO1  STILL NOT WITHIN RANGE                    
         B     RDREC40                TEST NEXT ONE                             
RDREC65  CLC   IOKEY+4(2),RNGCAM                                                
         BH    NOMORE                 PAST RANGE                                
*                                                                               
RDREC76  CLC   IOKEY+4(2),BCAM     TEST ALREADY HAVE CAMP DETAILS               
         BE    RDREC80                                                          
         MVC   KEY1(13),IOKEY                                                   
         GOTO1 AGETCAM,IOKEY+4     NO-NEW CAMP                                  
         BE    RDREC78                                                          
         MVI   ERRNUM,30                                                        
         B     CLDUMP                                                           
RDREC78  MVC   IOKEY(13),KEY1                                                   
         BAS   RE,GETCPE          GET CLIENT/PRODUCT/ESTIMATE DETAILS           
         BNE   RDREC79            ONE OF THESE INVALID-SKIP TO NEXT CMP         
         BAS   RE,GETNAMES                                                      
         MVC   IOKEY(13),KEY1                                                   
         TM    SELKIND,SELKDPT                                                  
         BZ    RDREC80                                                          
         GOTO1 AVALDPL,REPDPLH                                                  
         BNE   RDREC79                                                          
         MVC   SVBDPT,BDPT                                                      
         B     RDREC80                                                          
RDREC79  XC    BCAM,BCAM             CLEAR CAMPAIGN DETAILS                     
         GOTO1 AIO,IOSPTDIR+IOHI+IO1 READ FOR NEXT RECORD                       
         GOTO1 AIO,IOSPTDIR+IOSEQ+IO1                                           
         B     RDREC40                                                          
*                                                                               
RDREC80  GOTO1 AGETMKT,IOKEY+6    GET DETAILS FOR THIS MKT                      
         BNE   RDREC100           BAD MARKET -- SKIP IT                         
         TM    SELKIND,SELKMKT    TEST ALL MARKET REQUEST                       
         BNO   RDREC400                                                         
         CLC   IOKEY+6(2),SELMKT  TEST IF SAME MARKET                           
         BE    RDREC400           YES - READ RECORD                             
         TM    SELKIND,SELKCAM    ONE MARKET - BUT ALL CAMPAIGNS                
         BO    NOMORE                                                           
RDREC100 GOTO1 AIO,IOSPTDIR+IOHI+IO1                                            
         GOTO1 AIO,IOSPTDIR+IOSEQ+IO1                                           
         B     RDREC40                                                          
*                                                                               
RDREC400 MVC   HOLDKEY(13),IOKEY   SAVE THE HEADER KEY                          
         MVI   APBYTE,0                                                         
         MVC   APHALF,HOLDKEY+8   SAVE SEQUENCE FOR DETAIL RECORD               
         TM    SELKIND,SELKSTA    IF SPECIFIC STATION REQUESTED                 
         BZ    RDREC470           MUST GET STATION CODE FROM                    
*                                 X'01' ELEMENT                                 
         BAS   RE,GETHDR          GET HEADER RECORD                             
         BNE   RDREC20            NO '01' ELEMENT FOUND - SKIP REC              
*                                                                               
RDREC470 LA    R3,IOKEY                                                         
         USING BWDRECD,R3         BUILD DETAIL RECORD KEY                       
         XC    BWDKEY(13),BWDKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    X'0D'                                        
         MVI   BWDKSUB,BWDKSUBQ    X'68'                                        
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,APHALF                                                   
         MVI   BWDKELCD,1                                                       
         MVC   BWDKELST,APBYTE                                                  
         MVC   SAVEKEY(13),IOKEY    SAVE KEY FOR LATER USE                      
         LA    R1,MINHI1                                                        
RDREC480 GOTO1 AMIN                                                             
         BE    RDREC500                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     RDREC700                                                         
*                                                                               
RDREC500 CLC   IOKEY(BWDKELCD-BWDKEY),SAVEKEY                                   
         BNE   RDREC700           NO FURTHER MATCH ON DTL                       
         TM    SELKIND,SELKSTA    IF SPECIFIC STATION                           
         BZ    RDREC510           REQUESTED NEED TO CHECK FURTHER               
         CLC   IOKEY(BWDKELPO-BWDKEY),SAVEKEY                                   
         BNE   RDREC560           NOT A STATION MATCH                           
*                                                                               
RDREC510 L     R3,AIOAREA1                                                      
         OC    QCABLE,QCABLE       TEST CABLE FILTER                            
         BZ    *+14                                                             
         CLC   QCABLE,BWDSTA       YES-CABLE SYSTEM MUST MATCH                  
         BNE   RDREC560                                                         
         CLI   STAFILT,0           TEST STATION FILTER                          
         BE    RDREC512                                                         
         CLI   STAFILT,C'/'        YES-TEST CABLE ONLY                          
         BNE   *+16                                                             
         CLI   BWDSTA,C'0'                                                      
         BL    RDREC560                                                         
         B     RDREC512                                                         
         CLI   BWDSTA,C'0'         OR NON-CABLE ONLY                            
         BNL   RDREC560                                                         
*                                                                               
RDREC512 CLC   SVSTA,BWDSTA       TEST STATION OR CABLE SYSTEM CHANGE           
         BE    RDREC520                                                         
         MVC   SVSTA,BWDSTA       YES-                                          
         GOTO1 AGETSTA,BWDSTA     GET STATION DETAILS                           
         MVC   CBLSYS,ACWORK      AND SAVE CABLE SYSTEM NAME (IF ANY)           
*                                                                               
RDREC520 TM    SELKIND,SELKDPT    ALL DPTS                                      
         BZ    RDREC525                                                         
         TM    SUMIND,SUMISUB     F=ALL?                                        
         BNO   RDREC524           NO - GO CHECK DPT                             
         LA    RE,SUBS                                                          
         LA    RF,L'SUBS                                                        
RDREC522 CLC   BWDDPT,0(RE)                                                     
         BE    RDREC525                                                         
         CLI   BWDSUBDP,0                                                       
         BE    RDREC523                                                         
         CLC   BWDSUBDP,0(RE)                                                   
         BE    RDREC525                                                         
RDREC523 LA    RE,1(RE)                                                         
         BCT   RF,RDREC522                                                      
         B     RDREC560                                                         
*                                                                               
RDREC524 CLC   BWDDPT,SVBDPT                                                    
         BNE   RDREC560           NOT A MATCH - DO SEQUENTIAL                   
RDREC525 TM    SELKIND,SELKSLN    ALL SPOT LENGTHS                              
         BZ    RDREC530                                                         
         CLC   BWDSLN,SVBSLN                                                    
         BNE   RDREC560           SPOT LENGTHS MATCH- CONTINUE                  
*                                                                               
RDREC530 BAS   RE,GETRTG                                                        
         MVI   COSTIND,1          PROCESS LINE W/1ST EFFECTIVE DATE             
RDREC535 TM    OPTSW,SCHEDLIN                                                   
         BZ    RDREC541                                                         
         BAS   RE,CHKSKED         IF NO SCHEDULED SPOTS                         
         BNE   RDREC550           GET NEXT RECORD                               
*                                                                               
RDREC541 TM    SUMIND,SUMISUB     F=ALL?                                        
         BNO   RDREC542           NO                                            
         TM    SELKIND,SELKDPT    IF SINGLE DPT - RELEVANT                      
         BO    RDREC543           DAYPARTS ALREADY ADDED TO TABLE               
         MVC   TMPDPT,BWDDPT                                                    
         BAS   RE,GETSUBS         GET SUB-DAYPARTS FOR THIS DAYPART             
         BNE   RDREC550                                                         
         CLI   SUBTYPE,C'R'       REGULAR DAYPART ?                             
         BNE   RDREC54A                                                         
         MVC   TMPDPT,BWDDPT                                                    
         BAS   RE,ADDPT                                                         
         B     RDREC543                                                         
*                                                                               
RDREC54A LA    R1,SUBS                                                          
         LA    R0,L'SUBS                                                        
RDREC54B CLI   0(R1),0                                                          
         BE    RDREC543                                                         
         MVC   TMPDPT,0(R1)                                                     
         BAS   RE,ADDPT                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,RDREC54B                                                      
         B     RDREC543                                                         
*                                                                               
RDREC542 TM    SELKIND,SELKDPT    ALL DAYPART REQUEST?                          
         BO    RDREC543           NO - SINGLE DAYPART DON'T BUILD LIST          
         MVC   TMPDPT,BWDDPT                                                    
         BAS   RE,ADDPT                                                         
*                                                                               
RDREC543 TM    SELKIND,SELKSLN    ALL SPOT LENGTH REQUEST?                      
         BO    RDREC545           ONE SPOT LENGTH - DON'T BUILD LIST            
         LA    R1,COMSLLST                                                      
         LA    R0,L'COMSLLST                                                    
RDREC544 CLI   0(R1),0            IF SPOT LENGTH NOT                            
         BNE   *+14               IN LIST                                       
         MVC   0(1,R1),BWDSLN     ADD IT                                        
         B     RDREC545                                                         
         CLC   BWDSLN,0(R1)       IF SLN ALREADY IN LIST                        
         BE    RDREC545           DON'T RE-ADD IT                               
         LA    R1,1(R1)                                                         
         BCT   R0,RDREC544                                                      
         MVI   ERRNUM,19                                                        
         B     CLDUMP                                                           
*                                                                               
RDREC545 DS    0H                                                               
******** L     R2,AREP                                                          
******** USING REPD,R2                                                          
******** GOTO1 VHEXOUT,APPARM,(R3),REPP1,100,=C'TOG'                            
******** MVI   REPACTN,REPAPUT    PUT A LINE                                    
*********GOTO1 VREPORT,(R2)                                                     
**********ROP  R2                                                               
         B     RDEXIT             THIS WILL CALL DRIVER FOR INPUT               
*                                                                               
*                                                                               
RDREC550 CLI   COSTIND,1          SEE IF THERES ANY MORE DATES                  
         BNE   RDREC555                                                         
         OC    BWDEFDT2,BWDEFDT2                                                
         BZ    RDREC560                                                         
         MVI   COSTIND,2          2ND DATE - CALL DRIVER W/ SAME REC            
         B     RDREC535                                                         
RDREC555 CLI   COSTIND,2                                                        
         BNE   RDREC560                                                         
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    RDREC560                                                         
         MVI   COSTIND,3          2ND DATE - CALL DRIVER W/ SAME REC            
         B     RDREC535                                                         
RDREC560 LA    R1,MINSEQ1         SEQ READ FOR NEXT DTL                         
         B     RDREC480                                                         
*                                                                               
RDREC700 TM    SELKIND,SELKCAM                                                  
         BZ    RDREC702           ALL CAMPAIGN REQUEST -READ HEADER             
         TM    SELKIND,SELKMKT                                                  
         BO    NOMORE             ONE CAMPAIGN,ONE MARKET -EXIT                 
*                                                                               
* IF MULTIPLE MARKETS - READ GOALS RECORDS ON MARKET BREAK                      
*                                                                               
RDREC702 MVI   MKTSW,X'FF'        INBETWEEN MARKETS - READING GOALS             
         MVI   FSTGIN,C'Y'        FIRST TIME READING GOAL                       
         MVI   READRC,C'G'        FOR I/O ROUTINES                              
*                                                                               
RDREC705 BAS   RE,GOALS                                                         
         CLI   ENDSW,X'FF'                                                      
         BNE   EXIT                                                             
         XC    SUBS,SUBS                                                        
         XC    MDPTLST,MDPTLST                                                  
         XC    COMSLLST,COMSLLST                                                
         XC    COMDPLST,COMDPLST                                                
         XC    DISPSLN,DISPSLN                                                  
         XC    DISPDPT,DISPDPT                                                  
         MVI   ENDSW,0            RESET END SWITCH                              
         MVI   FSTGIN,C'Y'        RESET GOAL FLAG FOR NEXT MARKET               
         MVI   MKTSW,0            RESET MARKET SWITCH                           
         MVI   READRC,C'D'        GO BACK TO READING DETAIL RECS                
*                                                                               
RDREC710 B     RDREC20            READ NEXT HEADER                              
*                                                                               
NOMORE   MVI   ENDSW,X'FF'        MARK NO MORE RECORDS                          
RDEXIT   B     EXIT                                                             
         EJECT                                                                  
ADDPT    NTR1                                                                   
         LA    R1,COMDPLST        YES - POINT TO DPT LIST                       
         LA    R0,L'COMDPLST                                                    
ADDPT5   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),TMPDPT     IF DAYPART NOT IN LIST - ADD IT               
         B     ADDPTX                                                           
         CLC   TMPDPT,0(R1)         IS THIS DAYPART ALREADY IN LIST?            
         BE    ADDPTX             YES - DON'T ADD AGAIN                         
         LA    R1,1(R1)                                                         
         BCT   R0,ADDPT5                                                        
         MVI   ERRNUM,18                                                        
         B     CLDUMP                                                           
ADDPTX   XIT1                                                                   
         EJECT                                                                  
GETSUBS  NTR1                                                                   
         XC    APPARM,APPARM                                                    
         MVC   APPARM(2),CUAALF                                                 
         MVC   APPARM+2(1),QMED                                                 
         MVC   APPARM+3(1),ESTDMENU                                             
         GOTO1 VDPTRD,APPARM,,APWORK,VDMGR                                      
         CLI   APPARM+8,X'FF'                                                   
         BE    GETSBNO                                                          
         LA    RE,APWORK                                                        
*                                                                               
GETSB2   CLI   0(RE),0                                                          
         BE    GETSBNO                                                          
         CLC   0(1,RE),TMPDPT                                                   
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     GETSB2                                                           
         ZIC   R1,1(RE)           ISOLATE WHETHER IT BELONGS                    
         SRL   R1,4               TO MASTER/SUB-GROUP                           
         MVI   SUBTYPE,C'R'                                                     
         LTR   R1,R1              ZERO=REGULAR DPT                              
         BZ    GETSBY                                                           
         MVI   SUBTYPE,C'0'       EITHER MASTER OR SUB                          
         LA    R0,L'SUBS                                                        
         LA    R2,SUBS                                                          
         LA    RE,APWORK                                                        
         SR    RF,RF                                                            
*                                                                               
GETSB4   CLI   0(RE),0                                                          
         BE    GETSBY                                                           
         IC    RF,1(RE)                                                         
         SRL   RF,4                                                             
         CR    RF,R1              IS THIS SAME GROUP                            
         BNE   GETSB6                                                           
         MVC   0(1,R2),0(RE)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,5(RE)                                                         
         BCT   R0,GETSB4                                                        
         B     GETSBY                                                           
*                                                                               
GETSB6   LA    RE,5(RE)                                                         
         B     GETSB4                                                           
*                                                                               
GETSBNO  LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
GETSBY   CR    RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
*===============================================*                               
* CHKSKED- CHECKS IF THIS RECORD HAS ANY SPOTS  *                               
*     XIT - CC CODE SET                         *                               
*===============================================*                               
*                                                                               
CHKSKED  NTR1                                                                   
         TM    BWDINDS,BWDIPKG    PACKAGES?                                     
         BNO   SKED02                                                           
         CLI   BWDKELSQ,0                                                       
         BNE   SKED03             MUST CHECK EACH SLAVE FOR PRINTING            
         BAS   RE,RDPKGSV         CHECK IF PACKAGE SLAVES SCHEDULED             
         BE    SKEDYES            PRINT MASTER - A SLAVE IS SCHEDULED           
         B     SKEDNO             DON'T PRINT MASTER - NO SLAVES SCHED          
*                                                                               
SKED02   TM    BWDINDS,BWDIORB    ORBIT?                                        
         BNO   SKED03                                                           
         CLI   BWDKELSQ,0                                                       
         BE    SKED03             ORBIT MASTER - CHECK ELEMENT                  
         CLI   PRTFLG,C'Y'                                                      
         BE    SKEDYES            IF MASTER PRINTED - PRINT SLAVES TOO          
         B     SKEDNO             IF MASTER NOT PRINTED - SKIP SLAVES           
*                                                                               
SKED03   OC    BWDEFDT2,BWDEFDT2                                                
         BZ    SKED05                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(2,APDUB)                            
SKED05   OC    BWDEFDT3,BWDEFDT3                                                
         BZ    SKED07                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(2,APDUB+2)                          
*                                                                               
SKED07   LA    R1,BWDEL                                                         
SKED09   CLI   0(R1),0            END OF RECORD?                                
         BE    SKEDNO                                                           
         CLI   0(R1),SPWELCDQ                                                   
         BE    SKED14                                                           
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     SKED09                                                           
*                                                                               
         USING SPWEL,R1                                                         
SKED14   LA    R2,SPWPERWK        SPOTS IN ELEMENT                              
         LA    RE,CMPDATSP        DATES TO MATCH SPOTS IN ELEMENT               
         ZIC   R0,CMPNWKS         N'WEEKS = N'WEEKS IN CAMPAIGN, OR             
         ZIC   RF,SPWELLN         N'WEEKS IN ELEMENT, WHICHEVER IS LESS         
         SH    RF,=Y(SPWPERWK-SPWEL)                                            
         CR    R0,RF                                                            
         BNH   SKED15                                                           
         LR    R0,RF                                                            
*                                                                               
SKED15   SR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    SKED25                                                           
         MVI   BYTE,1                                                           
         OC    BWDEFDT2,BWDEFDT2                                                
         BZ    SKED20             SCHEDULED IN 1ST PERIOD                       
         CLC   2(2,RE),APDUB                                                    
         BL    SKED20             SCHEDULED IN 1ST PERIOD                       
         MVI   BYTE,2                                                           
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    SKED20             SCHEDULED IN 2ND PERIOD                       
         CLC   2(2,RE),APDUB+2                                                  
         BL    SKED20             SCHEDULED IN 2ND PERIOD                       
         MVI   BYTE,3             SCHEDULED IN 3RD PERIOD                       
SKED20   CLC   BYTE,COSTIND       SCHEDULED SPOT IN PERIOD LOOKING FOR?         
         BE    SKED30                                                           
SKED25   LA    R2,1(R2)           NO                                            
         LA    RE,4(RE)                                                         
         BCT   R0,SKED15          TRY NEXT WEEK                                 
         B     SKEDNO             NO SCHEDULED SPOTS                            
*                                                                               
SKED30   TM    BWDINDS,BWDIORB    ORBIT?                                        
         BNO   SKEDYES                                                          
         MVI   PRTFLG,C'Y'        INDICATE MASTER HAS PRINTED                   
*                                                                               
SKEDYES  CR    RB,RB              CC  SET EQUAL                                 
         B     SKEDX                                                            
*                                                                               
SKEDNO   TM    BWDINDS,BWDIORB                                                  
         BNO   *+8                                                              
         MVI   PRTFLG,C'N'         ORBIT MASTER NOT SKED                        
         LTR   RB,RB              CC SET NOT EQUAL                              
SKEDX    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*===============================================================*               
* RDPKGSV - READ AHEAD FOR PACKAGE SLAVE TO CHECK FOR SCHEDULED *               
*           LINES                                               *               
*===============================================================*               
*                                                                               
RDPKGSV  NTR1                                                                   
         MVC   SVPKGKEY,IOKEY                                                   
RDPKG2   LA    R1,MINSEQ1         SEQ READ FOR NEXT DTL                         
         GOTO1 AMIN                                                             
         BNE   RDPKGNO            NO MORE RECORDS                               
         TM    BWDINDS,BWDIPKG    PACKAGES?                                     
         BNO   RDPKGNO            NO - DIDN'T FIND ANYTHING SCHEDULED           
         CLI   BWDKELSQ,0                                                       
         BE    RDPKGNO            NO MORE SLAVES - NOTHING SCHEDULED            
RDPKG5   LA    R1,BWDEL                                                         
RDPKG8   CLI   0(R1),0            END OF RECORD?                                
         BE    RDPKG2                                                           
         CLI   0(R1),SPWELCDQ                                                   
         BE    RDPKG10                                                          
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RDPKG8                                                           
*                                                                               
RDPKG10  SR    RE,RE                                                            
         ICM   RE,1,1(R1)                                                       
         SH    RE,=H'5'                                                         
         LA    R1,SPWPERWK-SPWEL(R1)                                            
         EX    RE,RDPKG15                                                       
         B     *+10                                                             
RDPKG15  OC    0(0,R1),0(R1)      SEE IF ANY SPOTS                              
         BZ    RDPKG2             TRY NEXT SLAVE IN PACKAGE                     
*                                                                               
RDPKGYES MVC   IOKEY,SVPKGKEY                                                   
         LA    R1,MINHI1                                                        
         GOTO1 AMIN               RESET PACKAGE MASTER KEY                      
         CLC   IOKEY,SVPKGKEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CR    RB,RB              SET CC - SPOTS SCHEDULED                      
         B     EXIT                                                             
*                                                                               
RDPKGNO  MVC   IOKEY,SVPKGKEY                                                   
         LA    R1,MINHI1                                                        
         GOTO1 AMIN               RESET PACKAGE MASTER KEY                      
         CLC   IOKEY,SVPKGKEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         LTR   RB,RB              SET CC - NO SPOTS SCHEDULED                   
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=================================================================*             
* GOALS - READ GOAL RECORDS FOR EACH DPT/SLN FOR POINTS & DOLLARS *             
*         FOR EACH MARKET                                         *             
*=================================================================*             
GOALS    NTR1                                                                   
         LA    R8,COMSLLST        R8 ->SPOT LENGTH LIST                         
         A     R8,DISPSLN                                                       
         CLI   FSTGIN,C'Y'        FIRST TIME READING GOALS?                     
         BE    GOALF              YES                                           
*                                                                               
         LA    R4,SVWORK          DAYPART LIST                                  
         A     R4,DISPDPT                                                       
         CLI   SLNCNT,0           NO - CONTINUE WHERE WE LEFT OFF               
         BE    GOAL16             GET NEXT DAYPART                              
         ZIC   R2,SLNCNT                                                        
         B     GOAL18             GET NEXT SPOTLEN                              
*                                                                               
GOALF    LA    R4,COMDPLST         R4 ->DAYPART LIST                            
         A     R4,DISPDPT                                                       
         MVI   FSTGIN,C'N'         NOT FIRST TIME EVER AGAIN                    
         XC    SVWORK,SVWORK                                                    
         TM    SELKIND,SELKDPT                                                  
         BZ    *+10                                                             
         MVC   BDPT,SVBDPT                                                      
         TM    SELKIND,SELKSLN                                                  
         BZ    *+10                                                             
         MVC   BSLN,SVBSLN                                                      
*                                                                               
         MVC   SVBYTE,BSLN         SAVE THE SPOT LENGTH                         
         MVC   SVFLAG,BDPT         SAVE DAYPART                                 
         MVC   SVDOPT(1),CMPDPOPT  SAVE CAMPAIGN DAYPART OPTION                 
*                                  FOR F=ALL ALWAYS LOOK AT TABLE               
         TM    SELKIND,SELKDPT     TEST SINGLE DAYPART REQUEST                  
         BO    GOAL1               YES                                          
         CLI   1(R4),0             NO-TEST ONLY ONE DAYPART ANYWAY              
         BNE   GOAL2                                                            
         GOTO1 AGETDPT,(R4)        YES-GET IT                                   
         BE    GOAL1                                                            
         CLI   ASONOFF,ASON                                                     
         BE    *+12                                                             
         MVI   ERRNUM,23          INVALID DAYPART                               
         B     CLDUMP                                                           
         MVC   FVMSGNO,=AL2(FVIDPT)                                             
         XC    FVXTRA,FVXTRA                                                    
         MVC   FVXTRA(1),0(R4)                                                  
         B     GOALX                                                            
*                                                                               
GOAL1    CLI   DPTTYPE,C'S'                                                     
         BNE   GOAL1A                                                           
         MVC   SVWORK(1),BDPT     SINGLE DAYPART                                
         MVC   MDPTLST(1),BDPT                                                  
         B     GOAL14                                                           
*                                                                               
GOAL1A   MVC   0(1,R4),BDPT                                                     
         MVI   1(R4),0                                                          
GOAL2    LA    R2,L'COMDPLST       YES-BUILD LIST OF ALL DAYPARTS               
         LA    R8,SVWORK                                                        
GOAL3    CLI   0(R4),0                                                          
         BE    GOAL13                                                           
         MVC   APDUB(1),0(R4)                                                   
         GOTO1 AGETDPT,APDUB                                                    
         BE    GOAL3A                                                           
         CLI   ASONOFF,ASON                                                     
         BE    *+12                                                             
         MVI   ERRNUM,23          INVALID DAYPART                               
         B     CLDUMP                                                           
         MVC   FVMSGNO,=AL2(FVIDPT)                                             
         XC    FVXTRA,FVXTRA                                                    
         MVC   FVXTRA(1),0(R4)                                                  
         B     GOALX                                                            
*                                                                               
GOAL3A   LA    R1,MDPTLST                                                       
         LA    R0,L'MDPTLST                                                     
GOAL3B   CLI   0(R1),0                                                          
         BE    GOAL3C                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,GOAL3B                                                        
*                                                                               
GOAL3C   MVC   0(1,R1),BDPT                                                     
         CLI   DPTTYPE,C'S'                                                     
         BNE   *+18                                                             
         MVC   DPTSUBS(1),BDPT                                                  
         MVI   DPTSUBS+1,0                                                      
         B     GOAL4                                                            
         MVC   0(1,R8),0(R4)                                                    
         LA    R8,1(R8)                                                         
         CLI   DPTTYPE,C'M'                                                     
         BNE   GOAL12                                                           
*                                                                               
GOAL4    LA    RF,L'DPTSUBS                                                     
         LA    R1,DPTSUBS                                                       
         XC    APFULL,APFULL                                                    
*                                                                               
GOAL5    CLI   0(R1),0                                                          
         BE    GOAL10                                                           
         LA    RE,SVWORK                                                        
GOAL6    CLI   0(RE),0                                                          
         BNE   *+18                                                             
         MVC   0(1,RE),0(R1)                                                    
         ST    RE,APFULL                                                        
         B     GOAL8                                                            
         CLC   0(1,RE),0(R1)                                                    
         BE    GOAL8                                                            
         LA    RE,1(RE)                                                         
         B     GOAL6                                                            
*                                                                               
GOAL8    LA    R1,1(R1)                                                         
         BCT   RF,GOAL5                                                         
*                                                                               
GOAL10   ICM   R1,15,APFULL                                                     
         BZ    GOAL12                                                           
         LR    R8,R1                                                            
         LA    R8,1(R8)                                                         
*                                                                               
GOAL12   LA    R4,1(R4)                                                         
         BCT   R2,GOAL3                                                         
GOAL13   MVI   CMPDPOPT,C'S'      FAKE CAMP DPT OPT TO SUBDPT=SEP               
*                                                                               
GOAL14   LA    R4,SVWORK          GET GOALS FOR ALL DAYPARTS                    
         CLI   0(R4),0            ANY DAYPARTS?                                 
         BE    GOAL15             NONE                                          
*                                                                               
         AH    R4,=Y(L'SVWORK-1)  TRY TO STICK IN '$' DAYPART                   
         CLI   0(R4),0            DO WE HAVE AN OPENING FOR '$' DPT?            
         BNE   GOAL15             NO                                            
*                                                                               
         LA    R1,L'SVWORK                                                      
         BCTR  R4,0                                                             
GOAL14A  BCT   R1,*+6                                                           
         DC    H'0'                                                             
         CLI   0(R4),0             HIT LAST DAYPART OF THE LIST?                
         BE    *+12                                                             
         MVI   1(R4),C'$'          YES, PUT '$' DAYPART AFTER LAST ONE          
         B     GOAL15                                                           
         BCT   R4,GOAL14A                                                       
*                                                                               
GOAL15   LA    R4,SVWORK          GET GOALS FOR ALL DAYPARTS                    
         XC    DISPDPT,DISPDPT                                                  
         TM    SELKIND,SELKSLN                                                  
         BZ    GOAL16             NO - ALL SPOT LENGTHS                         
         LA    R1,COMSLLST        YES -SINGLE REQUEST                           
         A     R1,DISPSLN                                                       
         MVC   0(1,R1),SVBYTE                                                   
         MVI   1(R1),0                                                          
*                                                                               
GOAL16   CLI   0(R4),0            IF WE REACH END OF DAYPART LIST               
         BE    GOAL24             WE ARE DONE                                   
         MVC   BDPT,0(R4)         IF NOT - TRY THIS DAYPART WITH                
         LA    R8,COMSLLST        ALL SPOT LENGTHS                              
         A     R8,DISPSLN                                                       
         LA    R2,L'COMSLLST                                                    
*                                                                               
GOAL18   CLI   0(R8),0            ANY MORE SPOT LENGTHS                         
         BNE   GOAL19             YES                                           
         XC    DISPSLN,DISPSLN                                                  
         LA    R4,1(R4)           AND-- TRY NEXT DAYPART                        
         B     GOAL16                                                           
*                                                                               
GOAL19   MVC   BSLN,0(R8)                                                       
*                                                                               
         CLI   BDPT,C'$'           $ GOALS HAVE A SPOT LENGTH OF 1              
         BNE   GOAL20                                                           
         MVI   BSLN,1                                                           
         LA    R2,1                                                             
         LA    R8,1(R8)            SO NO MORE LENGTHS                           
         CLI   0(R8),0                                                          
         BNE   *-8                                                              
*                                                                               
GOAL20   LA    R0,L'MDPTLST                                                     
         LA    R1,MDPTLST                                                       
GOAL20A  CLC   0(1,R1),BDPT                                                     
         BE    GOAL20B                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,GOAL20A                                                       
         B     GOAL20C            MUST BE SUBDAYPART TO PREV DAYPART            
*                                                                               
GOAL20B  MVC   MASTDPT,BDPT       NEW DAYPART                                   
GOAL20C  GOTO1 AGETGOAL                                                         
         BE    GOAL21                                                           
         MVI   ERRNUM,20                                                        
         B     CLDUMP             MAJORLY WRONG                                 
*                                                                               
GOAL21   L     R1,AIOAREA3                                                      
         OC    0(120,R1),0(R1)                                                  
         BNZ   GOAL21A                                                          
*                                                                               
         CLI   0(R4),C'$'         $ GOALS?                                      
         BE    *+12               YES, HAVE ONLY ONE LENGTH (LEN=1)             
         LA    R8,1(R8)           NO GOAL -GET NEXT SPOT LENGTH                 
         BCT   R2,GOAL18          IF NO MORE SPOT LENGTHS                       
         XC    DISPSLN,DISPSLN                                                  
         LA    R4,1(R4)           BUMP TO NEXT DAYPART                          
         B     GOAL16             AND SEE IF THIS GOAL RECORDS                  
*                                                                               
GOAL21A  TM    ESTIND,ESTICS2      USING COST2?                                 
         BZ    GOAL21X             NO, NOTHING TO ADJUST                        
         LA    RE,0(R1)            A(GOAL TO BE ADJUSTED)                       
         ST    RE,APPARM                                                        
         GOTO1 C2ADJ,APPARM                                                     
*                                                                               
         L     R1,AIOAREA3                                                      
         LA    R3,8(R1)            A(1ST WEEKLY GOAL)                           
GOAL21A4 ST    R3,APPARM                                                        
         OC    0(4,R3),0(R3)       ZERO GOAL FOR THE WEEK?                      
         BZ    GOAL21A6            YES, THEN SKIP THIS WEEK                     
         GOTO1 C2ADJ,APPARM                                                     
GOAL21A6 LA    R3,4(R3)            CHECK THE NEXT WEEK                          
         L     R1,AIOAREA3                                                      
         LA    RF,64(R1)                                                        
         CR    R3,RF                                                            
         BL    GOAL21A4                                                         
*                                                                               
GOAL21X  LA    R8,1(R8)            NEXT SPOT LENGTH                             
         LA    R0,COMSLLST                                                      
         SR    R8,R0                                                            
         ST    R8,DISPSLN                                                       
         LTR   R2,R2                                                            
         BNZ   *+12                                                             
         MVI   SLNCNT,0                                                         
         B     GOAL22                                                           
*                                                                               
         S     R2,=F'1'                                                         
         STC   R2,SLNCNT                                                        
         LA    R0,SVWORK                                                        
         SR    R4,R0                                                            
         ST    R4,DISPDPT                                                       
         B     EXIT                CALL DRIVER FOR INPUT                        
*                                                                               
GOAL22   LA    R4,1(R4)            NEXT DAYPART                                 
         LA    R0,SVWORK                                                        
         SR    R4,R0                                                            
         ST    R4,DISPDPT                                                       
         XC    DISPSLN,DISPSLN                                                  
         MVI   SLNCNT,0                                                         
         B     EXIT                CALL DRIVER FOR INPUT                        
*                                                                               
GOAL24   MVC   BSLN,SVBYTE         RESTORE VALUES                               
         MVC   BDPT,SVFLAG                                                      
         MVC   CMPDPOPT,SVDOPT                                                  
         MVI   ENDSW,X'FF'                                                      
GOALX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADJUST THE GOAL POINTED BY THE ADDRESS STORED IN APPARM BY THE C2             
*                                                                               
* ON ENTRY:    APPARM              A(GOAL)                                      
*                                                                               
* ON EXIT:     APPARM              A(ADJUSTED GOAL)                             
***********************************************************************         
C2ADJ    NTR1                                                                   
         L     R1,APPARM                                                        
         ICM   RE,15,0(R1)         YES, ADJUST GOAL WITH C2 FACTOR              
         CVD   RE,APDUB                                                         
         ZAP   APWORK(16),APDUB                                                 
         SRP   APWORK(16),6,0      MULTIPLY BY  1,000,000                       
         XR    R1,R1                                                            
         ICM   R1,7,ESTPW                                                       
         CVD   R1,APDUB                                                         
         LA    RE,8                NUMBER OF BYTES OF DIVISOR                   
         LA    RF,APDUB            1ST BYTE OF DIVISOR                          
C2ADJ10  CLI   0(RF),0             FIRST BYTE OF DIVISOR                        
         BNE   C2ADJ20                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,C2ADJ10          LAST BYTE SHOULD NEVER BE 0                  
*                                                                               
C2ADJ20  BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         DP    APWORK(16),0(0,RF)                                               
*                                                                               
         AHI   RE,2                HIGH ORDER PORTION IS THE QUOTIENT           
         LA    RF,16                                                            
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   APDUB,APWORK(0)                                                  
         CVB   RE,APDUB                                                         
*                                                                               
         L     R1,APPARM           SAVE THE ADJUSTED GOAL                       
         STCM  RE,15,0(R1)                                                      
C2ADJX   B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* GETHDR - READS HEADER REC INTO AIO1 & CHECKS FOR STATION MATCH  *             
*     XIT  APBYTE SET TO STATION CODE IF FOUND                    *             
*=================================================================*             
*                                                                               
GETHDR   NTR1                                                                   
         GOTO1 AIO,IOSPTFIL+IOGET+IO1                                           
         L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         LA    R1,BWHFSTEL                                                      
         SR    R0,R0                                                            
*                                                                               
GETHDR10 CLI   0(R1),0            END OF RECORD?                                
         BE    GETHDR30           NO MATCH -GET OUT                             
         CLI   0(R1),BWHELCDQ                                                   
         BNE   *+14                                                             
         USING BWHEL,R1                                                         
         CLC   SVQSTA,BWHSTA                                                    
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETHDR10                                                         
         MVC   APBYTE,BWHSEQ      SAVE ONE CHAR STA CODE FOR DETAIL             
         XR    R0,R0                                                            
         LTR   R0,R0              SET GOOD CONDITION CODE                       
         B     EXIT               RECORD                                        
*                                                                               
GETHDR30 LTR   RB,RB              SET BAD CONDITION CODE                        
         B     EXIT               INDICATE READ NEXT RECORD                     
         DROP  R1,R2                                                            
         EJECT                                                                  
*=============================================*                                 
* GETRTG - GET'S RATINGS FOR REQUESTED DEMOS  *                                 
*=============================================*                                 
*                                                                               
GETRTG   NTR1                                                                   
         L     R3,AIOAREA1                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         LA    R1,BWDEL                                                         
GTRTG05  CLI   0(R1),0            END OF RECORD                                 
         BNE   GTRTG10                                                          
         MVI   ERRNUM,5                                                         
         B     CLDUMP                                                           
*                                                                               
GTRTG10  CLI   0(R1),X'02'        DEMO ELEMENT                                  
         BE    GTRTG15                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GTRTG05                                                          
*                                                                               
GTRTG15  LA    R0,DEMNUM          CLEAR OUT DEMO RATINGS                        
         LA    R8,LDEMHLD+2                                                     
GTRTG18  XC    0(4,R8),0(R8)                                                    
         LA    R8,6(R8)                                                         
         BCT   R0,GTRTG18                                                       
*                                 GET RATINGS FOR THIS RECORD                   
         LA    R0,DEMNUM          # OF DEMOS TO DISPLAY                         
         LA    R8,LDEMHLD         SAVE DEMO AREA                                
         LR    R2,R1              SAVE ADDRESS OF BEGINNING                     
*                                                                               
GTRTG30  DS    0H                 MUST SEARCH FOR OVERIDE DEMOS                 
         LA    RE,L'DMODEMO       LENGTH TO BUMP                                
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0               END POINT                                     
         LA    R1,DMODEMO-DMOEL(R1) PT TO DEMO                                  
         CLC   1(2,R1),0(R8)                                                    
         BE    GTRTG40                                                          
         BXLE  R1,RE,*-10                                                       
         B     GTRTG50         IF THIS DEMO NOT IN ELEM-TRT NEXT DEMO           
*                                                                               
GTRTG40  MVC   2(4,R8),4(R1)      MOVE IN RATING                                
*                                                                               
GTRTG50  LA    R8,6(R8)                                                         
         LR    R1,R2                                                            
         BCT   R0,GTRTG30                                                       
GTRTGX   B     EXIT                                                             
         EJECT                                                                  
*======================*                                                        
* CLDUMP - DUMP        *                                                        
*======================*                                                        
*                                                                               
CLDUMP   L     R8,AREP                                                          
         USING REPD,R8                                                          
         MVI   REPACTN,REPACLO    CLOSE THE REPORT                              
         GOTO1 VREPORT,(R8)                                                     
         DROP  R8                                                               
         DC    H'0'                                                             
         SPACE                                                                  
*===========================================================*                   
* OUTPUT - CALLED BEFORE DRIVER/DROOL IS CALLED FOR OUTPUT  *                   
*===========================================================*                   
OUTPUT   DS    0H                                                               
         B     EXIT                                                             
         SPACE                                                                  
*=====================*                                                         
* DRIVER HOOK         *                                                         
*=====================*                                                         
*                                                                               
DRHOOK   L     R8,AREP                                                          
         USING REPD,R8                                                          
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   GLHOOK,GLPRINT     PRINT                                         
         BE    PRINT                                                            
         CLI   GLHOOK,GLHEAD      HEADHOOK                                      
         BE    HEADHK                                                           
         B     EXIT                                                             
         SPACE 2                                                                
PRINT    CLI   DONTPRT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT                                                    
         MVI   DONTPRT,C'N'                                                     
*                                                                               
         MVI   REPACTN,REPAPUT    PUT A LINE                                    
         CLI   ASONOFF,ASON                                                     
         BE    EXIT               ONLINE -ALREADY DONE IN CONTROLLER            
         MVI   REPPRNTN,1         ONLY ONE                                      
         B     EXIT                                                             
         EJECT                                                                  
*===============================*                                               
*HEADHK - HEADLINE HOOK ROUTINE *                                               
*===============================*                                               
*                                                                               
HEADHK   MVC   REPH4(5),=C'MEDIA'                                               
         MVC   REPH5(5),=C'BUYER'                                               
         MVC   REPH6(8),=C'CAMPAIGN'                                            
         MVC   REPH7(6),=C'MARKET'                                              
*                                                                               
         MVC   REPH4+9(L'QMED),QMED                                             
         MVC   REPH4+17(L'MEDNM),MEDNM                                          
         MVC   REPH5+9(L'QBYR),QBYR                                             
         MVC   REPH5+17(L'BYRNM),BYRNM                                          
         MVC   REPH6+9(L'QCAM),QCAM                                             
         MVC   REPH6+17(L'CMPNM),CMPNM                                          
         MVC   REPH7+9(L'QMKT),QMKT                                             
         MVC   REPH7+17(L'MKTNM),MKTNM                                          
*                                                                               
         MVC   REPH4+47(6),=C'CLIENT'                                           
         MVC   REPH5+47(7),=C'PRODUCT'                                          
         MVC   REPH6+47(8),=C'ESTIMATE'                                         
         MVC   REPH4+56(3),QCLT                                                 
         MVC   REPH4+61(L'CLTNM),CLTNM                                          
         MVC   REPH5+56(3),QPRD                                                 
         MVC   REPH5+61(L'PRDNM),PRDNM                                          
         OC    LPRDNM1,LPRDNM1                                                  
         BZ    HEADHK10                                                         
         MVC   REPH5+61(10),LPRDNM1                                             
         MVI   REPH5+71,C'-'                                                    
         MVC   REPH5+72(9),LPRDNM2                                              
*                                                                               
HEADHK10 MVC   REPH6+56(3),QEST                                                 
         MVC   REPH6+61(L'ESTNM),ESTNM                                          
*                                                                               
         XC    REPH7+55(20),REPH7+55                                            
         MVC   REPH4+99(16),=C'RATING SOURCE - '                                
         CLI   CLTSRC,C'A'                                                      
         BNE   HEADHK12                                                         
         MVC   REPH4+115(3),=C'ARB'                                             
         CLI   CUDMED,C'C'        IF CANADIAN?                                  
         BNE   HEADHK14                                                         
         MVC   REPH4+115(3),=C'BBM'                                             
         B     HEADHK14                                                         
*                                                                               
HEADHK12 MVC   REPH4+115(3),=C'NSI'                                             
         CLI   CUDMED,C'C'        IF CANADIAN?                                  
         BNE   HEADHK14                                                         
         MVC   REPH4+115(3),=C'CSI'                                             
*                                                                               
HEADHK14 MVC   REPH5+99(L'CMPUPIN),CMPUPIN                                      
         MVC   REPH6+99(L'CMPUPIN2),CMPUPIN2                                    
*                                                                               
* ---- MOVE OUT TYPE OF REPORT SEQUENCE                                         
*                                                                               
HEADHK15 LA    R3,REPH7+104                                                     
         TM    SELKIND2,SELKRST    TEST FOR STA IN DEMO VALUE SEQ               
         BZ    HEADHK16                                                         
         MVC   0(4,R3),=CL4'STA/'                                               
         LA    R3,4(R3)                                                         
HEADHK16 TM    SELKIND,SELKRDM     TEST FOR RANK IN DEMO VALUE SEQ              
         BZ    HEADHK17                                                         
         MVC   0(4,R3),=CL4'DEMO'                                               
         B     HEADHK20                                                         
HEADHK17 TM    SELKIND2,SELKRDT     TEST FOR RANK IN DAY TIME                   
         BZ    HEADHK18                                                         
         MVC   0(2,R3),=CL2'DT'                                                 
         B     HEADHK20                                                         
HEADHK18 MVC   0(3,R3),=CL3'CPP'                                                
*                                                                               
HEADHK20 TM    SELKIND2,SELKRST                                                 
         BO    HEADHK60                                                         
*--- NOT STATION SEQUENCE                                                       
         CLI   SUPDEM,C'Y'                                                      
         BE    HEADHK30                                                         
*----NOT STATION SEQUENCE, AND NOT SUPPRESSING DEMOS                            
         CLI   GLRECNO,5                                                        
         BE    HSUMM2             MARKET SUMMARY ACROSS CAMPAIGNS               
         CLI   GLRECNO,4                                                        
         BE    HSUMM              MARKET SUMMARY                                
         LA    R3,REPM2+45                                                      
         CLI   GLRECNO,2                                                        
         BNE   *+8                                                              
         LA    R3,REPM2+59                                                      
         BAS   RE,HDATES          MOVE DATES OUT                                
         CLI   GLRECNO,3                                                        
         BE    HGOALS             GOAL REPORT                                   
         BAS   RE,HDEMOS          MOVE DEMOS OUT                                
         CLI   GLRECNO,2                                                        
         BNE   HWORK              DETAIL REPORT                                 
         B     HSTA               STATION TOTAL REPORT                          
         SPACE 2                                                                
*--- NOT STATION SEQUENCE, AND SUPPRESSING DEMOS                                
HEADHK30 CLI   GLRECNO,4                                                        
         BE    HSUMM2             MARKET SUMMARY ACROSS CAMPAIGNS               
         CLI   GLRECNO,3                                                        
         BE    HSUMM              MARKET SUMMARY                                
         LA    R3,REPM2+45                                                      
         CLI   GLRECNO,2                                                        
         BNE   *+8                                                              
         LA    R3,REPM2+59                                                      
         BAS   RE,HDATES          MOVE DATES OUT                                
         CLI   GLRECNO,2                                                        
         BE    HSTA               STATION TOTAL REPORT                          
         BAS   RE,HDEMOS          MOVE DEMOS OUT                                
         B     HWORK              DETAIL REPORT                                 
         SPACE 2                                                                
*--- STATION SEQUENCE                                                           
         SPACE 2                                                                
HEADHK60 CLI   SUPDEM,C'Y'                                                      
         BE    HEADHK90                                                         
*--- STATION SEQUENCE, AND NOT SUPPRESSING DEMOS                                
         CLI   GLRECNO,4                                                        
         BE    HSUMM2             MARKET SUMMARY ACROSS CAMPAIGNS               
         CLI   GLRECNO,3                                                        
         BE    HSUMM              MARKET SUMMARY                                
         LA    R3,REPM2+45                                                      
         BAS   RE,HDATES          MOVE DATES OUT                                
         CLI   GLRECNO,2                                                        
         BE    HGOALS             GOAL REPORT                                   
         BAS   RE,HDEMOS          MOVE DEMOS OUT                                
         B     HWORK              DETAIL REPORT                                 
         SPACE 2                                                                
*--- STATION SEQUENCE, AND SUPPRESSING DEMOS                                    
HEADHK90 CLI   GLRECNO,3                                                        
         BE    HSUMM2             MARKET SUMMARY ACROSS CAMPAIGNS               
         CLI   GLRECNO,2                                                        
         BE    HSUMM              MARKET SUMMARY                                
         LA    R3,REPM2+45                                                      
         BAS   RE,HDATES          MOVE DATES OUT                                
         BAS   RE,HDEMOS          MOVE DEMOS OUT                                
         B     HWORK              DETAIL REPORT                                 
         SPACE 3                                                                
*-- MOVE TELECAST DATES OUT TO LINE                                             
*                                                                               
HDATES   NTR1                                                                   
         LA    R2,CMPDATSP        PACKED DATES                                  
         SR    R8,R8                                                            
         ICM   R8,1,CMPNWKS       # OF PACKED DATES                             
*                                                                               
HDATES5  GOTO1 VDATCON,APPARM,(2,(R2)),(4,TEMPDT)                               
*                                                                               
         MVC   0(3,R3),TEMPDT     MOVE MONTH OUT OF MIDLINE 2                   
         LA    R1,133(R3)                                                       
         MVC   0(2,R1),TEMPDT+3   MOVE DAY OUT MIDLINE 3                        
*                                                                               
         LA    R3,4(R3)           BUMP MIDLINE 2                                
         LA    R2,4(R2)           NEXT PACKED DATES GROUP                       
         BCT   R8,HDATES5                                                       
HDATEX   B     EXIT                                                             
         SPACE 2                                                                
* -- STATION TOTAL                                                              
HSTA     DS    0H                                                               
         MVC   REPM1(57),SPACES                                                 
         MVC   REPM2(8),=CL8' STATION'                                          
         MVC   REPM2+8(26),SPACES                                               
         MVC   REPM2+34(20),=C'TLCSTS          COST'                            
         MVI   REPM3,C' '                                                       
         MVC   REPM3+1(57),DASHES                                               
         MVC   REPM1+57(19),DASHES                                              
         MVC   REPM1+76(19),=C'NUMBER OF TELECASTS'                             
         MVC   REPM1+95(19),DASHES                                              
         MVC   REPM1+114(17),SPACES                                             
         MVC   REPM2+114(17),SPACES                                             
         B     EXIT                                                             
         SPACE 3                                                                
*-- DETAIL REPORT                                                               
*                                                                               
HWORK    DS    0H                                                               
         MVC   REPM1+1(24),=CL25'STA      BUY PERIOD  DAY'                      
         MVC   REPM1+30(14),=CL14'TIME    DPTLEN'                               
         MVC   REPM2+1(43),DASHES                                               
         MVC   REPM3+1(20),=CL20'SEQ      PROGRAMMING'                          
         MVC   REPM3+33(11),=CL11'COST    N/W'                                  
         MVC   REPM1+45(18),DASHES                                              
         MVC   REPM1+63(19),=C'NUMBER OF TELECASTS'                             
         MVC   REPM1+82(18),DASHES                                              
         B     EXIT                                                             
         SPACE 2                                                                
*-- MOVE DEMO NAMES OUT TO LINE                                                 
*                                                                               
HDEMOS   NTR1                                                                   
         CLI   SUPDEM,C'Y'                                                      
         BE    HDEMOX                                                           
         MVC   REPM1+102(7),COMDNAMS                                            
         MVC   REPM1+110(7),COMDNAMS+7                                          
         MVC   REPM1+117(7),COMDNAMS+14                                         
         MVC   REPM1+125(7),COMDNAMS+21                                         
         TM    OPTSW,SIXDEMS                                                    
         BZ    HDEMOX              ONLY FOUR REQUESTED                          
         MVC   REPM2+102(7),COMDNAMS+28                                         
         MVC   REPM2+110(7),COMDNAMS+35                                         
HDEMOX   B     EXIT                                                             
         SPACE 2                                                                
*--- GOALS REPORT                                                               
*                                                                               
HGOALS   DS    0H                                                               
         XC    REPM1(43),REPM1                                                  
         XC    REPM2(43),REPM2                                                  
         XC    REPM3(43),REPM3                                                  
         MVC   REPM2+20(24),=CL24'%DOL %PTS      ACHIEVED'                      
         MVC   REPM1+35(8),=C'GOALS/  '                                         
         MVC   REPM3+35(9),=C'DOLLARS  '                                        
         MVC   REPM1+43(24),DASHES                                              
         MVC   REPM1+67(12),=C'WEEKLY DEMOS'                                    
         TM    CMPOPTS,CAMODLY                                                  
         BZ    *+10                                                             
         MVC   REPM1+67(6),=C'-DAILY'                                           
         MVC   REPM1+79(21),DASHES                                              
         XC    REPM1+101(30),REPM1+101                                          
         XC    REPM2+101(30),REPM2+101                                          
         MVC   REPM1+101(6),=C'GOALS/'                                          
         MVC   REPM2+101(8),=C'ACHIEVED'                                        
         MVC   REPM3+101(6),=C'POINTS'                                          
HGOALX   B     EXIT                                                             
         SPACE                                                                  
* -- MARKET SUMMARY REPORT                                                      
*                                                                               
HSUMM    MVC   REPH7+55(20),=C'** MARKET SUMMARY **'                            
         B     HSUMM5                                                           
*                                                                               
HSUMM2   MVC   REPH7+45(37),=C'** MARKET SUMMARY ACROSS CAMPAIGNS **'           
         XC    REPH6(40),REPH6     NO CAMPAIGN INFO                             
HSUMM5   TM    SUMIND,SUMIDPT                                                   
         BO    *+12                                                             
         TM    SUMIND,SUMISUB                                                   
         BNO   HSUMM8                                                           
         MVC   REPM2+29(7),=C'DPT-LEN'                                          
         LA    RE,REPM1+38                                                      
         LA    RF,REPM2+38                                                      
         B     HSUMM10                                                          
HSUMM8   MVC   REPM2+17(7),=C'STATION'                                          
         LA    RE,REPM1+51                                                      
         LA    RF,REPM2+51                                                      
HSUMM10  MVC   0(9,RE),=C'----GOAL('                                            
         MVC   9(7,RE),COMDNAMS                                                 
         MVC   16(6,RE),=C')-----'                                              
         MVC   23(12,RE),=C'--PURCHASED('                                       
         MVC   35(7,RE),COMDNAMS                                                
         MVC   42(3,RE),=C')--'                                                 
         MVC   0(22,RF),=C'PNTS  DOLLARS      CPP'                              
         MVC   23(22,RF),=C'PNTS  DOLLARS      CPP'                             
         MVC   46(5,RF),=C'SPOTS'                                               
         MVC   52(3,RE),=C'AVG'                                                 
         MVC   52(3,RF),=C'PTS'                                                 
         MVC   56(9,RE),=C'PCT-ACHMT'                                           
         MVC   56(9,RF),=C'PNTS-DOLS'                                           
HSUMMX   B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
REPSPEC  DS    0X                  ** REPORT SPEC POOL **                       
         SPEC  H1,1,RUN                                                         
         SPEC  H1,55,C'WORKSHEET SCHEDULE'                                      
         SPEC  H2,55,C'------------------'                                      
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
*                                                                               
         SPEC  H7,100,C'RANK='                                                  
         SPEC  END                                                              
         SPACE                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*==================*                                                            
* LITERAL POOL     *                                                            
*==================*                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
SPACES   DC    CL60' '                                                          
DASHES   DC    CL50'--------------------------------------------------'         
         DC    CL10'----------'                                                 
         EJECT                                                                  
SPARE    DS    9000X                                                            
SPAREX   EQU   *                                                                
         EJECT                                                                  
* DEDBLOCK                                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* SPNWSWRK                                                                      
       ++INCLUDE SPNWSWRK                                                       
         EJECT                                                                  
* LOCAL STORAGE AREA                                                            
LOCALD   DSECT                                                                  
       ++INCLUDE SPNWSDRVD                                                      
*                                                                               
         DS    0F                                                               
SVLEN    DS    F                                                                
SVLEN2   DS    F                                                                
DISPSLN  DS    F                                                                
DISPDPT  DS    F                                                                
*                                                                               
FSTINPUT DS    CL1                 FIRST TIME THRU DETAIL INPUT                 
FSTGIN   DS    CL1                 FIRST TIME THRU GOAL INPUT                   
SLNCNT   DS    CL1                                                              
RANK     DS    CL1                 Y=RANK ON CPP/DEMO                           
FSTSW    DS    CL1                 FIRST TIME THRU SWITCH                       
MKTSW    DS    CL1                                                              
ENDSW    DS    CL1                 END OF READS SWITCH                          
SPAC     DS    CL1                 SPACING 2 OR 3                               
FSTREC   DS    CL1                                                              
SVBYTE   DS    CL1                                                              
SVFLAG   DS    CL1                                                              
SVBSLN   DS    CL1                                                              
SVBDPT   DS    CL1                                                              
PSTA     DS    CL1                 PAGE BY STATION                              
FRNGCAM  DS    CL1                 FLAG FOR RANGE OF CAMPAIGNS                  
PRTFLG   DS    XL1                 Y=PRINT ORBIT                                
STAFILT  DS    CL1                 STATION FILTER / OR -                        
SVPKGKEY DS    CL13                                                             
*                                                                               
SVDOPT   DS    CL2                                                              
SELMKT   DS    XL2                 MARKET FILTER                                
RNGCAM   DS    CL4                 COMPLEMENTED BINARY VALUE                    
RNGCAMB  DS    CL4                 BINARY VALUE                                 
CURCMP   DS    XL2                                                              
CMP      DS    CL(L'QCAM)                                                       
SVQSTA   DS    CL(L'QSTA)                                                       
SVSTA    DS    CL5                 SAVED STATION OR CABLE SYSTEM                
TEMPDT   DS    CL5                 TEMPORARY DATE                               
FAKEHDR  DS    CL8                                                              
FAKEFLD  DS    CL8                                                              
HOLDKEY  DS    XL13                STORED KEY (HEADER)                          
SAVEKEY  DS    XL13                SAVE DETAIL KEY                              
KEY1     DS    XL13                SAVE KEY                                     
MMKTKEY  DS    XL13                SAVE KEY BETWEEN MARKETS                     
SELKEY   DS    XL32                SELECT KEY                                   
COMSLLST DS    CL8                SPOT LENGTHS FOR GETTING GOALS                
COMDPLST DS    CL36               DAYPART LIST FOR GETTING GOALS                
MDPTLST  DS    CL(L'COMDPLST)     MASTER DPT LIST FOR GETTING GOALS             
SVWORK   DS    CL(L'COMDPLST)     SAVED DAYPART LIST                            
SESTDEMS DS    CL(L'ESTDEMS)                                                    
*                                                                               
TMPDPT   DS    CL1                TEMP DAYPART SENT TO GETSUBS                  
SUBS     DS    CL17               SAVED DAYPART LIST                            
SUBTYPE  DS    CL1                DAYPART TYPE                                  
         ORG   LOCALD+LDRVRLOC                                                  
*                                                                               
         ORG   LOCALD+4096                                                      
*                                                                               
LOCALX   EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSF8D                                                       
*                                                                               
*============================*                                                  
* THESE DSECTS ALL HIDDEN    *                                                  
* SPNWSHDR                   *                                                  
* SPNWSCAM                   *                                                  
* DRGLOBAL AND DROOLLOCAL    *                                                  
*============================*                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         SPACE 1                                                                
       ++INCLUDE SPNWSCAM                                                       
         SPACE 1                                                                
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE DROOLLOCAL                                                     
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'128SPNWS20A  07/17/02'                                      
         END                                                                    
