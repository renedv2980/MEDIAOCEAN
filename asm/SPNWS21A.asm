*          DATA SET SPNWS21A   AT LEVEL 029 AS OF 07/17/02                      
*PHASE T20721A,*                                                                
         TITLE 'T20721 - BUYER''S WORKSHEET UPDATE REPORT'                      
T20721   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1024,T20721**,RA,RR=RE,CLEAR=YES                                 
         USING TWAD,R5             R5=A(TWA)                                    
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         ST    RE,APRELO                                                        
         LR    R1,RC                                                            
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    R1,ARECTAB                                                       
         LA    R0,8                                                             
         LA    R1,1024(R1)                                                      
         BCT   R0,*-4                                                           
         ST    R1,ARECTABX                                                      
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   UPREP2                                                           
         SR    R1,RC               YES-DOUBLE WORKING STORAGE                   
         L     RF,4(RD)                                                         
         AR    RD,R1                                                            
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         L     R0,ARECTABX                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         AR    R1,R0                                                            
         ST    R1,ARECTABX                                                      
*                                                                               
UPREP2   ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         MVC   REPRLH,=XL2'000C'                                                
         MVC   REPRDH,=XL2'0003'                                                
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
         B     VALREQ              VALIDATE THE REQUEST                         
         B     PRINTREP            PRINT THE REPORT                             
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
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE REQUEST                                                *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   LA    R2,APRECKEY         R2=A(CAMPAIGN MARKET HEADER KEY)             
         USING BWHRECD,R2                                                       
         XC    APRECKEY,APRECKEY                                                
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         MVI   REPCLASS,C'B'       CLASS B REPORT                               
*                                                                               
         XC    INUSER,INUSER       REQUESTOR                                    
         GOTO1 AFVAL,UPRREQH                                                    
         BH    VALRX                                                            
         BL    *+10                                                             
         MVC   INUSER,FVIFLD                                                    
         CLI   ASONOFF,ASON        TEST ONLINE                                  
         BNE   VALR2                                                            
*                                                                               
         GOTO1 AVALWHEN,UPRWENH    VALIDATE WHEN                                
         BNE   VALRX                                                            
*                                                                               
******** OC    ASIOASTR,ASIOASTR   TEST EXTENDED WORKING STORAGE                
******** BZ    VALR1               IS AVAILABLE                                 
******** CLI   INWHEN,MIXIOKN      NO-TEST NOW                                  
******** BNE   VALR1                                                            
******** MVC   FVMSGNO,=AL2(FVFNOTV)  YES-INVALID                               
******** B     VALRX                                                            
*                                                                               
VALR1    GOTO1 AVALDEST,UPRDIDH    VALIDATE DESTINATION ID                      
         BNE   VALRX                                                            
*                                                                               
         GOTO1 AVALOTYP,UPROUTH    VALIDATE OUTPUT TYPE                         
         BNE   VALRX                                                            
*                                                                               
VALR2    XC    UPRMDN,UPRMDN       MEDIA                                        
         OI    UPRMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,UPRMEDH                                                  
         BNE   VALRX                                                            
         MVC   UPRMDN,MEDNM                                                     
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         XC    UPRBYN,UPRBYN       BUYER                                        
         OI    UPRBYNH+6,FVOXMT                                                 
         GOTO1 AVALBYR,UPRBYRH                                                  
         BNE   VALRX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   UPRBYN,BYRNM                                                     
         MVC   BWHKBYR,BBYR                                                     
         OC    INUSER,INUSER       TEST REQUESTOR ID SET YET                    
         BNZ   *+10                                                             
         MVC   INUSER,QBYR         NO - SET IT TO THE BUYER                     
         OC    BYRPW,BYRPW         CHECK BUYER'S PASSWORD                       
         BZ    VALR3                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALRX                                                            
*                                                                               
VALR3    MVI   SELKIND,0                                                        
         XC    CAMPST,CAMPST                                                    
         MVC   CAMPEND,XFF                                                      
         XC    UPRCMN,UPRCMN       CAMPAIGN                                     
         OI    UPRCMNH+6,FVOXMT                                                 
         GOTO1 AVALCAM,UPRCMPH                                                  
         BE    VALR8                                                            
         CLC   FVMSGNO,=AL2(FVFNONE)   MANDATORY FIELD                          
         BE    VALRX                                                            
*****                                                                           
         CLC   FVIFLD(4),=C'ALL '  ALL CAMPAIGNS                                
         BNE   VALR3A                                                           
         TM    NWSFLAG,NWSFCLST    X'80' - USES CLIENT LIST?                    
         BNO   VALR6                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)   CAN'T USE ALL IF USES CLT LIST           
         B     VALRX                                                            
*                                  VALIDATE RANGE OF CAMPAIGNS                  
VALR3A   XC    APELEM,APELEM                                                    
         GOTO1 VSCANNER,APPARM,UPRCMPH,(2,APELEM),C',=,='                       
         CLI   4(R1),2                                                          
         BNE   VALRX                                                            
         LA    R4,APELEM                                                        
         LA    R3,CAMPEND                                                       
         LA    R0,2                                                             
*                                                                               
VALR4    CLI   1(R4),0                                                          
         BNE   VALRX                                                            
         TM    2(R4),X'80'         TEST NUMERIC                                 
         BZ    VALRX                                                            
         OC    4(2,R4),4(R4)                                                    
         BNZ   VALRX                                                            
         MVC   0(2,R3),6(R4)                                                    
         XC    0(2,R3),XFF                                                      
         LA    R4,32(R4)                                                        
         LA    R3,CAMPST                                                        
         BCT   R0,VALR4                                                         
         CLC   CAMPST,CAMPEND      TEST START NOT AFTER END                     
         BH    VALRX                                                            
*                                                                               
VALR6    CLI   INWHEN,MIXIOKN      TEST NOW REQUEST                             
         BE    VALR97              YES - ONLY SINGLE CAMPAIGN                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALR10                                                           
*                                                                               
VALR8    MVC   CAMPST,BCAM         SINGLE CAMPAIGN                              
         MVC   CAMPEND,BCAM                                                     
         MVC   UPRCMN,CMPNM                                                     
         OI    SELKIND,SELKCAM                                                  
         BAS   RE,GETCAM           GET CAMPAIGN INFORMATION                     
*                                                                               
VALR10   MVC   BWHKCAM,CAMPST                                                   
         MVI   KEYCOMP,BWDKELST-BWDKEY-1                                        
         MVI   STAFILT,0                                                        
         XC    UPRMSN,UPRMSN       MARKET/STATION                               
         OI    UPRMSNH+6,FVOXMT                                                 
         GOTO1 AFVAL,UPRMSTH                                                    
         BH    VALRX                                                            
         BL    VALR14                                                           
         TM    FVIIND,FVINUM       TEST FOR NUMERIC                             
         BZ    VALR12                                                           
         LA    R1,UPRMSTH                                                       
*                                                                               
VALR11   DS    0H                                                               
         GOTO1 AVALMKT             YES-VALIDATE MARKET                          
         BNE   VALRX                                                            
         MVC   UPRMSN,MKTNM                                                     
         MVC   SVBMKT,BMKT                                                      
         OI    SELKIND,SELKMKT                                                  
         B     VALR14                                                           
*                                                                               
VALR12   CLC   UPRMST(8),=C'ALL,ALL/' TEST ALL MARKETS AND CABLE                
         BE    *+14                                                             
         CLC   UPRMST(8),=C'ALL,ALL-' OR ALL MARKETS AND NON-CABLE              
         BNE   *+14                                                             
         MVC   STAFILT,UPRMST+7    YES-SAVE THE STATION FILTER                  
         B     VALR14                                                           
         LA    RF,6                SERACH FOR ALL/ OR ALL-                      
         LA    R1,UPRMST                                                        
         CLC   0(3,R1),=C'ALL'                                                  
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,*-14                                                          
         B     VALR13                                                           
         CLI   3(R1),C'/'                                                       
         BE    *+12                                                             
         CLI   3(R1),C'-'                                                       
         BNE   VALR13                                                           
         MVC   STAFILT,3(R1)       FOUND - SAVE / OR -                          
         CLC   UPRMST(3),=C'ALL'   TEST AT BEGINNING OF FIELD                   
         BNE   *+16                                                             
         CLI   UPRMSTH+FVILEN-FVIHDR,4  AND LENGTH=4                            
         BE    VALR14              YES- THEN IT'S ALL/ OR ALL- ONLY             
         B     VALR13                                                           
         BCTR  R1,0                ELSE THE FORMAT IS 'MKT,ALL/'                
         CLI   0(R1),C','                                                       
         BNE   VALR13                                                           
         XC    APWORK,APWORK       MOVE MARKET TO DUMMY TWA FIELD               
         LA    RE,UPRMST           AND VALIDATE                                 
         SR    R1,RE                                                            
         BNP   VALR13                                                           
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   APWORK+L'FVIHDR(0),UPRMST                                        
         LA    R1,1(R1)                                                         
         STC   R1,APWORK+(FVILEN-FVIHDR)                                        
         LA    R1,L'FVIHDR(R1)                                                  
         STC   R1,APWORK                                                        
         LA    R1,APWORK                                                        
         B     VALR11                                                           
*                                                                               
VALR13   MVI   STAFILT,0                                                        
         GOTO1 AVALSTA,UPRMSTH     VALIDATE STATION                             
         BNE   VALRX                                                            
         MVC   SVBMKT,BMKT                                                      
         OI    SELKIND,SELKMKT                                                  
         OC    QCABLE,QCABLE       TEST CABLE SYSTEM FILTER                     
         BNZ   VALR14              YES                                          
         MVC   SVQSTA,QSTA         NO-REAL STATION                              
         MVI   KEYCOMP,BWDKELPO-BWDKEY-1                                        
         OI    SELKIND,SELKSTA                                                  
*                                                                               
VALR14   GOTO1 AFVAL,UPRDPLH       VALIDATE DAYPART/LENGTH                      
         BL    VALR16                                                           
         BH    VALRX                                                            
         GOTO1 AVALDPL,UPRDPLH                                                  
         BNE   VALRX                                                            
         MVC   SVBDPT,BDPT                                                      
         MVC   SVBSLN,BSLN                                                      
         CLI   BDPT,0                                                           
         BE    *+8                                                              
         OI    SELKIND,SELKDPT                                                  
         CLI   BSLN,0                                                           
         BE    *+8                                                              
         OI    SELKIND,SELKSLN                                                  
*                                                                               
VALR16   MVI   RNKOPT,RNKCPP       RANK OPTION                                  
         GOTO1 AFVAL,UPRRNKH                                                    
         BH    VALRX                                                            
         BL    VALR18                                                           
         CLC   FVIFLD(4),=C'CPP '                                               
         BE    VALR18                                                           
         MVI   RNKOPT,RNKDEM                                                    
         CLC   FVIFLD(5),=C'DEMO '                                              
         BE    VALR18                                                           
         MVI   RNKOPT,RNKSTA                                                    
         CLC   FVIFLD(4),=C'STA '                                               
         BNE   VALR99                                                           
*                                                                               
VALR18   XC    DEMOVR,DEMOVR       DEMOS OPTION                                 
         GOTO1 AFVAL,UPRDEMH                                                    
         BH    VALRX                                                            
         BL    VALR21                                                           
         LA    R8,APELEM                                                        
         USING DBLOCKD,R8                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP'                                                    
         MVC   DBSELMED,CUDMED                                                  
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBCOMFCS,ACOM                                                    
         XC    APWORK,APWORK                                                    
         GOTO1 VDEMOVAL,APPARM,(1,FVIHDR),(2,DEMOVR),(C'S',DBLOCK),0            
         CLI   4(R1),0                                                          
         BE    VALR99                                                           
         CLI   4(R1),1                                                          
         BNE   VALR20                                                           
         MVC   DEMOVR+3(3),DEMOVR                                               
         CLI   DEMOVR+1,C'R'                                                    
         BNE   *+12                                                             
         MVI   DEMOVR+4,C'I'                                                    
         B     VALR20                                                           
         CLI   DEMOVR+1,C'I'                                                    
         BNE   *+12                                                             
         MVI   DEMOVR+4,C'R'                                                    
         B     VALR20                                                           
         XC    DEMOVR+3(3),DEMOVR+3                                             
*                                                                               
VALR20   GOTO1 GETNAMES,DEMOVR     GET OVERRIDE DEMO NAMES                      
*                                                                               
VALR21   MVI   DPTBRK,C'N'         DAYPART BREAK OPTION                         
         GOTO1 AFVAL,UPRBRKH                                                    
         BH    VALRX                                                            
         BL    VALR22                                                           
         MVC   DPTBRK,FVIFLD                                                    
         CLI   DPTBRK,C'N'                                                      
         BE    VALR22                                                           
         CLI   DPTBRK,C'Y'                                                      
         BNE   VALR99                                                           
*                                                                               
VALR22   MVI   SECDEM,C'N'         SECONDARY DEMO OPTION                        
         GOTO1 AFVAL,UPRSECH                                                    
         BH    VALRX                                                            
         BL    VALR23                                                           
         MVC   SECDEM,FVIFLD                                                    
         CLI   SECDEM,C'N'                                                      
         BE    VALR23                                                           
         CLI   SECDEM,C'Y'                                                      
         BNE   VALR99                                                           
*                                                                               
VALR23   TM    SELKIND,SELKCAM     TEST CAMPAIGN FILTER                         
         BZ    VALR24                                                           
         BAS   RE,GETDEM           YES - GET DEMO DETAILS                       
         GOTO1 GETNAMES,DEMOS                                                   
*                                                                               
VALR24   TM    SELKIND,SELKCAM     TEST SINGLE CAMPAIGN                         
         BZ    *+10                                                             
         MVC   BWHKMKT,SVBMKT      YES-PUT MARKET IN KEY (IF ANY)               
         MVC   IOKEY,APRECKEY                                                   
         GOTO1 AIO,DIRHI+IO1       READ FIRST CAMPAIGN MARKET HEADER            
         BNE   VALR98                                                           
         MVC   APRECKEY,IOKEY      SAVE THE FIRST KEY                           
*                                                                               
         CLC   IOKEY(BWHKCAM-BWHKEY),IOKEYSAV  TEST RECORD EXISTS               
         BNE   VALR98                                                           
         TM    SELKIND,SELKCAM     TEST MULTIPLE CAMPAIGNS                      
         BO    VALR25                                                           
         XC    BCAM,BCAM                                                        
         CLC   BWHKCAM,CAMPEND     YES-TEST ANYTHING IN RANGE                   
         BH    VALR98                                                           
         B     VALR28                                                           
*                                                                               
VALR25   CLC   IOKEY(BWHKMKT-BWHKEY),IOKEYSAV  SINGLE CAMPAIGN                  
         BNE   VALR98                                                           
         TM    SELKIND,SELKMKT     TEST SINGLE MARKET                           
         BZ    VALR28                                                           
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BNE   VALR98                                                           
         TM    SELKIND,SELKSTA     TEST SINGLE CAMPAIGN/STATION                 
         BZ    VALR28                                                           
         GOTO1 AIO,FILGET1         YES-TEST STATION EXISTS                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         LA    R1,BWHFSTEL                                                      
         SR    R0,R0                                                            
*                                                                               
VALR26   CLI   0(R1),0                                                          
         BE    VALR98                                                           
         CLI   0(R1),BWHELCDQ                                                   
         BNE   VALR27                                                           
         USING BWHEL,R1                                                         
         CLC   QSTA,BWHSTA                                                      
         BNE   VALR27                                                           
         MVC   BSTACD,BWHSEQ       YES-SAVE STATION CODE                        
         B     VALR28                                                           
*                                                                               
VALR27   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALR26                                                           
*                                                                               
VALR28   MVC   REPDESC,RPTDESC                                                  
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,RPTSPEC                                                       
         ST    R0,REPAPHS          A(SPEC POOL)                                 
         LA    R0,RPTHOOK                                                       
         ST    R0,REPAUSR          A(REPORT USER HOOK)                          
         MVC   FVMSGNO,=AL2(FVFOK) OK EXIT                                      
         B     VALRX                                                            
*                                                                               
VALR97   MVC   FVMSGNO,=AL2(FVONECAM)  ONE CAMPAIGN FOR NOW REQUEST             
         B     VALRX                                                            
*                                                                               
VALR98   MVC   FVMSGNO,=AL2(FVFERNF)   RECORD NOT FOUND                         
         LA    R1,UPRMEDH                                                       
         ST    R1,FVADDR                                                        
         B     VALRX                                                            
*                                                                               
VALR99   MVC   FVMSGNO,=AL2(FVFNOTV)   INVALID FIELD                            
*                                                                               
VALRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT THE REPORT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PRINTREP MVI   FIRST,C'Y'                                                       
         MVI   RPTSW,C'N'                                                       
*                                                                               
PRIN2    BAS   RE,NEXTREC          READ NEXT CAMPAIGN MARKET HEADER             
         BNE   PRIN4                                                            
         BAS   RE,READRECS         READ DETAIL RECORDS                          
         BNE   PRIN2                                                            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   PRINX                                                            
         MVI   RPTSW,C'Y'                                                       
         BAS   RE,REPORT           GENERATE THE REPORT                          
         B     PRIN2                                                            
*                                                                               
PRIN4    CLI   RPTSW,C'Y'          END-TEST ANY REPORT                          
         BE    PRINX                                                            
         MVC   FVMSGNO,=AL2(FVFERNF)  NO-RECORD NOT FOUND                       
         LA    R1,UPRMEDH                                                       
         ST    R1,FVADDR                                                        
*                                                                               
PRINX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ NEXT CAMPAIGN MARKET HEADER RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
NEXTREC  NTR1  ,                                                                
         MVC   IOKEY,APRECKEY                                                   
         LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         CLI   FIRST,C'Y'          TEST FIRST TIME                              
         BNE   NEXT4               NO                                           
         MVI   FIRST,C'N'          YES                                          
*                                                                               
NEXT2    LA    R1,DIRHI+IO1                                                     
         B     NEXT6                                                            
*                                                                               
NEXT4    GOTO1 AIO,DIRRD                                                        
         LA    R1,DIRSQ+IO1                                                     
*                                                                               
NEXT6    GOTO1 AIO                                                              
         CLC   IOKEY(BWHKCAM-BWHKEY),IOKEYSAV   TEST END                        
         BNE   NEXT99                                                           
         CLC   IOKEY(BWHKMKT-BWHKEY),IOKEYSAV   TEST CAMPAIGN BREAK             
         BE    NEXT10                                                           
         TM    SELKIND,SELKCAM     YES-TEST SINGLE CAMPAIGN                     
         BO    NEXT99              YES-END                                      
         CLC   BWHKCAM,CAMPST      NO-TEST WITHIN RANGE                         
         BE    NEXT10                                                           
         BH    NEXT8                                                            
         MVC   BWHKCAM,CAMPST                                                   
         XC    BWHKMKT(7),BWHKMKT                                               
         B     NEXT2                                                            
NEXT8    CLC   BWHKCAM,CAMPEND                                                  
         BH    NEXT99                                                           
*                                                                               
NEXT10   TM    SELKIND,SELKMKT     TEST SINGLE MARKET                           
         BZ    NEXT12                                                           
         CLC   BWHKMKT,SVBMKT      YES-CHECK THE MARKET                         
         BE    NEXT12              EQ                                           
         BL    *+14                                                             
         MVC   BWHKMKT(7),XFF      HI-READ NEXT CAMPAIGN                        
         B     NEXT2                                                            
         MVC   BWHKMKT,SVBMKT      LO-SKIP TO REQ MKT                           
         XC    BWHKSEQ(5),BWHKSEQ                                               
         B     NEXT2                                                            
*                                                                               
NEXT12   GOTO1 AIO,FILGET1         GET THE HEADER RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         TM    SELKIND,SELKSTA     TEST SINGLE STATION                          
         BZ    NEXT18                                                           
         TM    SELKIND,SELKCAM     YES-TEST SINGLE CAMPAIGN                     
         BO    NEXT18              YES-ALREADY HAVE STATION CODE                
         LA    R1,BWHFSTEL         NO-TEST STATION EXISTS                       
         SR    R0,R0                                                            
*                                                                               
NEXT14   CLI   0(R1),0                                                          
         BE    NEXT16                                                           
         CLI   0(R1),BWHELCDQ                                                   
         BNE   NEXT15                                                           
         USING BWHEL,R1                                                         
         CLC   SVQSTA,BWHSTA                                                    
         BNE   NEXT15                                                           
         MVC   BSTACD,BWHSEQ       YES-SAVE STATION CODE                        
         B     NEXT18                                                           
*                                                                               
NEXT15   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     NEXT14                                                           
*                                                                               
NEXT16   LA    R2,IOKEY            NO-NEXT CAMPAIGN                             
         MVC   BWHKMKT(7),XFF                                                   
         B     NEXT2                                                            
*                                                                               
NEXT18   MVC   APRECKEY(13),IOKEY  SAVE HEADER KEY                              
         CLC   BWHKCAM,BCAM        TEST CAMPAIGN BREAK                          
         BNE   NEXT20                                                           
         CLC   BWHKMKT,BMKT        TEST MARKET BREAK                            
         BNE   NEXT22                                                           
         B     NEXT24                                                           
*                                                                               
NEXT20   GOTO1 AGETCAM,BWHKCAM     GET CAMPAIGN DETAILS                         
         BAS   RE,GETCAM                                                        
         BAS   RE,GETDEM                                                        
         OC    DEMOVR,DEMOVR       TEST DEMO OVERRIDES                          
         BNZ   NEXT22                                                           
         GOTO1 GETNAMES,DEMOS      NO - GET ESTIMATE DEMO NAMES                 
*                                                                               
NEXT22   GOTO1 AGETMKT,BWHKMKT     GET MARKET                                   
*                                                                               
NEXT24   B     NEXTX                                                            
*                                                                               
NEXT99   LTR   RB,RB               CC NE - END                                  
         B     EXIT                                                             
*                                                                               
NEXTX    CR    RB,RB               CC EQ - HEADER FOUND                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ THE DETAIL RECORDS                                             *         
* OUTPUT : CC EQ - RECORD(S) ADDED TO SORT                            *         
*          CC NE - NO RECORDS                                         *         
***********************************************************************         
         SPACE 1                                                                
READRECS NTR1                                                                   
         L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         LA    R3,IOKEY                                                         
         USING BWDRECD,R3                                                       
         XC    IOKEY,IOKEY         BUILD DETAIL RECORD KEY                      
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BWHKAGMD                                                
         MVC   BWDKBYR,BWHKBYR                                                  
         MVC   BWDKSEQ,BWHKSEQ                                                  
         MVI   BWDKELCD,BWDELCDQ                                                
         TM    SELKIND,SELKSTA     TEST SINGLE STATION                          
         BZ    *+10                                                             
         MVC   BWDKELST,BSTACD     YES-MOVE IN STATION CODE                     
         MVC   SVDTLKEY,BWDKEY     SAVE FIRST PART OF DETAIL KEY                
*                                                                               
         SR    R8,R8               R8=RECORD COUNT                              
         SR    RE,RE               CLEAR RECORD TABLE                           
         SR    RF,RF                                                            
         L     R0,ARECTAB                                                       
         ST    R0,ANEXTREC                                                      
         L     R1,ARECTABX                                                      
         SR    R1,R0                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,MINHI2                                                        
         B     READ2+4                                                          
*                                                                               
READ2    LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     READ24                                                           
         ZIC   RE,KEYCOMP                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   READ24                                                           
         L     R3,AIOAREA2                                                      
         OC    QCABLE,QCABLE       TEST FOR CABLE FILTER                        
         BZ    *+14                                                             
         CLC   QCABLE,BWDSTA       YES-CABLE SYSTEM MUST MATCH                  
         BNE   READ2                                                            
         CLI   STAFILT,0           TEST STATION FILTER                          
         BE    READ3                                                            
         CLI   STAFILT,C'/'        YES-TEST CABLE ONLY                          
         BNE   *+16                                                             
         CLI   BWDSTA,C'0'                                                      
         BL    READ2                                                            
         B     READ3                                                            
         CLI   BWDSTA,C'0'         OR NON-CABLE ONLY                            
         BNL   READ2                                                            
*                                                                               
READ3    TM    SELKIND,SELKSTA     TEST FOR STATION FILTER                      
         BZ    *+16                                                             
         CLC   BWDSTA,SVQSTA       YES-STATIONS MUST MATCH                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         TM    SELKIND,SELKDPT     CHECK DAYPART                                
         BZ    *+14                                                             
         CLC   SVBDPT,BWDDPT                                                    
         BNE   READ2                                                            
         TM    SELKIND,SELKSLN     CHECK LENGTH                                 
         BZ    *+14                                                             
         CLC   SVBSLN,BWDSLN                                                    
         BNE   READ2                                                            
*                                                                               
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT                           
         BE    READ4               NO                                           
         CLI   BWDKELSQ,0          YES-TEST SLAVE                               
         BNE   READ2               YES-IGNORE                                   
         TM    BWDINDS,BWDIPKG     TEST PACKAGE MASTER                          
         BZ    *+12                                                             
         OI    SINDS,SIPKG         YES-INDICATE PACKAGE MASTER                  
         B     READ4                                                            
         OI    SINDS,SIORB         ELSE INDICATE ORBIT MASTER                   
*                                                                               
READ4    XC    SREC(SRECL),SREC    INITIALIZE SORT RECORD                       
         MVC   SELKEY,BWDKELST     ELEM KEY STA/PKG-ORB/DAYS/TIMES/SEQ          
*                                  SET THE SORT KEY                             
         MVC   SDPT,BWDDPT         DAYPART/LENGTH ALWAYS HIGH                   
         MVC   SSLN,BWDSLN                                                      
         MVC   SSTA,BWDSTA                                                      
         MVC   STIME,BWDTIMES                                                   
         MVC   SSEQ,BWDKELSQ                                                    
         MVI   SDAY,X'FF'                                                       
         CLI   BWDDAYS,0           TEST PACKAGE/ORBIT MASTER                    
         BE    READ6               YES-SORT LAST                                
         MVI   SDAY,0              SET PROPER SORTING SEQUENCE FOR DAYS         
         CLI   BWDDAYS,X'7C'       M-F                                          
         BE    READ6                                                            
         MVI   SDAY,1                                                           
         CLI   BWDDAYS,X'7F'       M-SU                                         
         BE    READ6                                                            
         SR    RF,RF                                                            
         ZIC   R0,BWDDAYS                                                       
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         STC   RF,SDAY                                                          
*                                                                               
READ6    LA    R1,BWDEL            GET THE RATING                               
         SR    R0,R0                                                            
*                                                                               
READ8    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     READ8                                                            
         MVC   APWORK(2),INORTG+1                                               
         OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
         BNZ   READ10                                                           
         CLI   BWDKELPO,0              NO-TEST PKG/ORBIT MASTER                 
         BE    *+14                                                             
READ8A   MVC   APWORK(2),ESTDEMS+1     YES-LOOK FOR PRIMARY DEMO                
         B     READ10                                                           
         LA    RF,DMODEMO-DMOEL(R1)  NO-EXTRACT RATING                          
         CLC   1(2,RF),ESTDEMS+1     1ST CATEGORY MATCHES ESTIMATE'S?           
         BNE   READ8A                                                           
         ST    RF,APFULL                                                        
         B     READ14                                                           
*                                                                               
READ10   LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO-DMOEL(R1)                                             
         XC    APFULL,APFULL                                                    
*                                                                               
READ12   CLC   1(2,R1),APWORK                                                   
         BNE   *+12                                                             
         ST    R1,APFULL                                                        
         B     READ14                                                           
         BXLE  R1,RE,READ12                                                     
*                                                                               
READ14   LA    RF,BWDCOST1         RF=A(COST)                                   
*                                                                               
READ16   CLI   RNKOPT,RNKSTA       TEST SORT IN STA/DAY/TIME SEQ                
         BE    READ20              YES-SORT RECORD ALREADY DONE                 
         SR    RE,RE                                                            
         ICM   R1,15,APFULL                                                     
         BZ    *+8                                                              
         ICM   RE,7,5(R1)          RE=RATING                                    
         CLI   RNKOPT,RNKDEM       TEST RANK IN DEMO VALUE SEQ                  
         BE    READ18              YES-USE INVERSE RATING                       
         LTR   RE,RE               NO-RANK IN CPP SEQ                           
         BZ    READ20              ZERO RATING-CPP=ZERO                         
         ICM   R1,15,0(RF)         R1=COST                                      
         BNZ   *+16                TEST ZERO COST                               
         CLI   CLTBWPRO+10,C'Y'    YES-TEST RANK BONUSES FIRST                  
         BE    READ20                  YES-                                     
         B     READ18                  NO-RANK IN DEMO SEQUENCE                 
         SR    R0,R0               CALCULATE CPP                                
         M     R0,=F'20'                                                        
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         STCM  R1,7,SCPP                                                        
         B     READ20                                                           
*                                                                               
READ18   LNR   RE,RE               INVERSE RATING                               
         BCTR  RE,0                MINUS ONE FOR ZERO                           
         STCM  RE,7,SDEMO                                                       
*                                                                               
READ20   L     R1,ANEXTREC         ADD SORT RECORD TO TABLE                     
         LA    RE,SRECL(R1)                                                     
         C     RE,ARECTABX                                                      
         BH    READ98                                                           
         MVC   0(SRECL,R1),SREC                                                 
         ST    RE,ANEXTREC                                                      
         LA    R8,1(R8)            INCREMENT RECORD COUNT                       
*                                                                               
         OC    BWDEFDT2,BWDEFDT2   TEST EFFECTIVE DATES                         
         BZ    READ2               NO                                           
         TM    SINDS,SIEFFDT3      YES-TEST PROCESSED EFFECTIVE COST 3          
         BO    READ2               YES-DONE WITH THIS RECORD                    
         TM    SINDS,SIEFFDT2      TEST JUST PROCESSED EFFECTIVE COST 2         
         BO    *+16                                                             
         OI    SINDS,SIEFFDT2      NO-PROCESS EFFECTIVE COST 2                  
         LA    RF,BWDCOST2                                                      
         B     READ16                                                           
         OC    BWDEFDT3,BWDEFDT3   YES-TEST EFFECTIVE DATE 3                    
         BZ    READ2                                                            
         NI    SINDS,255-SIEFFDT2  YES-PROCESS EFFETCIVE COST 3                 
         OI    SINDS,SIEFFDT3                                                   
         LA    RF,BWDCOST3                                                      
         B     READ16                                                           
*                                                                               
READ24   LTR   R8,R8               TEST ANY RECORDS                             
         BZ    READ99                                                           
         LA    R1,APPARM           YES-SORT THEM                                
         LA    RE,SRECL                                                         
         ST    RE,8(R1)                                                         
         LA    RE,SKEYL                                                         
         ST    RE,12(R1)                                                        
         GOTO1 VXSORT,(R1),ARECTAB,(R8),,,0                                     
         CR    RB,RB                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     READX                                                            
*                                                                               
READ98   MVC   FVMSGNO,=AL2(FVTMR)   TOO MANY RECORDS TO LIST                   
         CR    RB,RB                                                            
         B     READX                                                            
*                                                                               
READ99   LTR   RB,RB               CC NE - NO RECORDS                           
*                                                                               
READX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GENERATE THE REPORT                                                 *         
***********************************************************************         
         SPACE 1                                                                
REPORT   NTR1                                                                   
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         L     R9,AREP                                                          
         USING REPD,R9                                                          
         OI    REPHEADI,REPHFRCE                                                
         MVI   PAGE,C'A'                                                        
         MVI   LINE,C'1'                                                        
         MVI   SVDPT,0                                                          
         L     R1,ARECTAB                                                       
         ST    R1,ANEXTREC                                                      
*                                                                               
REP2     L     R1,ANEXTREC         GET NEXT RECORD                              
         LA    RE,SRECL(R1)                                                     
         C     RE,ARECTABX                                                      
         BH    REPXIT                                                           
         OC    0(SRECL,R1),0(R1)                                                
         BZ    REPXIT                                                           
         MVC   SREC(SRECL),0(R1)                                                
         ST    RE,ANEXTREC                                                      
*                                                                               
REP4     LA    R3,IOKEY            GET THE BWS RECORD                           
         MVC   BWDKEY,SVDTLKEY                                                  
         MVC   BWDKEL+1(6),SELKEY                                               
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2                                                      
         MVI   REPP1,C' '          CLEAR THE PRINT LINE                         
         MVC   REPP1+1(L'REPP1-1),REPP1                                         
         LA    R1,PMISC                                                         
         ST    R1,AMISC                                                         
         MVC   PLINE(1),PAGE       PAGE/LINE                                    
         MVC   PLINE+1(1),LINE                                                  
         MVC   PSTA,BWDSTA         STATION                                      
         CLI   PSTA+4,C'T'                                                      
         BNE   *+8                                                              
         MVI   PSTA+4,C' '                                                      
         CLI   PSTA,C'0'           TEST CABLE                                   
         BL    *+8                                                              
         MVI   PSTA+4,C'/'                                                      
*                                                                               
         CLI   BWDDAYS,0           TEST DAYS PRESENT                            
         BNE   REP8                                                             
         SR    RE,RE               NO-CHECK FOR PACKAGE/ORBIT                   
         ICM   RE,1,BWDKELPO                                                    
         BZ    REP10                                                            
         L     R1,AMISC                                                         
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  3(3,R1),APDUB                                                    
         MVI   6(R1),C','                                                       
         LA    RE,7(R1)                                                         
         ST    RE,AMISC                                                         
         TM    BWDINDS,BWDIORB                                                  
         BO    REP6                                                             
         MVC   PDAYS(19),=C'-- P A C K A G E --'                                
         MVC   0(3,R1),=C'PKG'                                                  
         B     REP12                                                            
*                                                                               
REP6     MVC   PDAYS(19),=C'---- O R B I T ----'                                
         MVC   0(3,R1),=C'ORB'                                                  
         B     REP12                                                            
*                                                                               
REP8     GOTO1 AGETDAY,BWDDAYS     DAYS                                         
         MVC   PDAYS,QDAYS                                                      
*                                                                               
REP10    GOTO1 AGETTIM,BWDTIMES    TIMES                                        
         MVC   PTIME,QTIMES                                                     
*                                                                               
REP12    XC    EBLOCK,EBLOCK       COST                                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBFLOAT,C'$'                                                     
         MVI   EBDECS,2                                                         
         LA    R1,PCOST                                                         
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'PCOST                                                   
         LA    RF,BWDCOST2                                                      
         TM    SINDS,SIEFFDT2                                                   
         BO    REP14                                                            
         LA    RF,BWDCOST3                                                      
         TM    SINDS,SIEFFDT3                                                   
         BO    REP14                                                            
         LA    RF,BWDCOST1                                                      
*                                                                               
REP14    ST    RF,EBAIN                                                         
         MVC   COST,0(RF)          SAVE DETAIL COST                             
         SR    RE,RE                                                            
         ICM   RF,15,0(RF)                                                      
         BNZ   *+14                                                             
         MVC   PCOST+4(2),=C'$0'                                                
         B     REP20                                                            
         D     RE,=F'100'                                                       
         LTR   RE,RE                                                            
         BZ    REP16                                                            
         LA    RE,L'PCOST-4                                                     
         LTR   RE,RE                                                            
         BNP   REP16                                                            
         LA    R1,1                                                             
         MH    R1,=H'10'                                                        
         BCT   RE,*-4                                                           
         CR    RF,R1                                                            
         BL    REP18                                                            
*                                                                               
REP16    MVI   EBSCIN,X'82'        SCALE PENNIES TO DOLLARS                     
         MVI   EBDECS,0                                                         
*                                                                               
REP18    GOTO1 VEDITOR,APPARM,EBLOCK   FORMAT THE COST                          
*                                                                               
REP20    MVC   PPROG,BWDPROG       PROGRAMMING                                  
*                                                                               
         MVC   APFULL(3),BWDEFDT2  EFFECTIVE DATE                               
         TM    SINDS,SIEFFDT2                                                   
         BO    *+18                                                             
         TM    SINDS,SIEFFDT3                                                   
         BZ    REP22                                                            
         MVC   APFULL(3),BWDEFDT3                                               
         L     R2,AMISC                                                         
         GOTO1 VDATCON,APPARM,(3,APFULL),(4,(R2))                               
         MVI   5(R2),C','                                                       
         LA    R2,6(R2)                                                         
         ST    R2,AMISC                                                         
*                                                                               
REP22    LA    R2,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
REP24    CLI   0(R2),0             RATING                                       
         BE    REP28                                                            
         CLI   0(R2),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     REP24                                                            
         USING DMOEL,R2                                                         
         LA    R1,DMODEMO          FIND TARGET RATING AND IMPRESSION            
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R2)                                                         
         AR    RF,R2                                                            
         BCTR  RF,0                                                             
         XC    APDUB,APDUB                                                      
         MVI   EBFLOAT,0                                                        
*                                                                               
REP26    CLC   1(2,R1),DEMOS+1                                                  
         BNE   *+10                                                             
         MVC   APDUB(4),4(R1)      APDUB = 1ST/2ND RATINGS                      
         CLC   1(2,R1),DEMOS+4                                                  
         BNE   *+10                                                             
         MVC   APDUB+4(4),4(R1)                                                 
         BXLE  R1,RE,REP26                                                      
*                                                                               
         LA    R2,DEMOS            EDIT THE RATINGS/IMPS                        
         LA    R4,APDUB                                                         
         LA    R0,2                                                             
         LA    R1,APFULL                                                        
         ST    R1,EBAIN                                                         
         MVI   EBLIN,4                                                          
         LA    R1,PDEMO                                                         
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'PDEMO                                                   
*                                                                               
REP27    MVI   EBFLOAT,0                                                        
         TM    0(R4),DMODEMOV                                                   
         BZ    *+12                                                             
         MVI   EBFLOAT,C'*'                                                     
         NI    0(R4),255-DMODEMOV                                               
         MVC   APFULL,0(R4)                                                     
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLI   1(R2),C'I'          TEST FOR IMPRESSION                          
         BNE   *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R2,3(R2)                                                         
         LA    R4,4(R4)                                                         
         LA    R1,PDEMO2                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'PDEMO2                                                  
         BCT   R0,REP27                                                         
*                                                                               
         ICM   R1,15,COST          TEST FOR NON-ZERO COST                       
         BZ    REP28                                                            
         OC    APDUB(4),APDUB      TEST FOR NON-ZERO RATING                     
         BZ    REP28                                                            
         SR    R0,R0               YES - CALCULATE CPP                          
         M     R0,=F'20'                                                        
         D     R0,APDUB                                                         
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,APFULL                                                        
         MVI   EBDECS,2                                                         
         MVI   EBSCIN,0                                                         
         LA    R1,PCPP                                                          
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'PCPP                                                    
         MVI   EBFLOAT,C'$'                                                     
         CLC   APFULL,=F'1000000'                                               
         BL    *+16                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         B     *+18                                                             
         CLC   APFULL,=F'100000'                                                
         BL    *+8                                                              
         MVI   EBFLOAT,0                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK     PRINT CPP                              
*                                                                               
         OC    APDUB+4(4),APDUB+4  CPM                                          
         BZ    REP28                                                            
         SR    R0,R0                                                            
         L     R1,COST                                                          
         M     R0,=F'20'                                                        
         D     R0,APDUB+4                                                       
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,APFULL                                                        
         LA    R1,PCPM                                                          
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'PCPM                                                    
         MVI   EBFLOAT,C'$'                                                     
         MVI   EBDECS,2                                                         
         MVI   EBSCIN,0                                                         
         CLC   APFULL,=F'1000000'                                               
         BL    *+16                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         B     *+18                                                             
         CLC   APFULL,=F'100000'                                                
         BL    *+8                                                              
         MVI   EBFLOAT,0                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
REP28    MVC   PDPTLEN(1),BWDDPT   DAYPART/LENGTH                               
         ZIC   RE,BWDSLN                                                        
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PDPTLEN+1(3),APDUB                                               
         CLI   PDPTLEN+1,C'0'                                                   
         BNE   *+14                                                             
         MVC   PDPTLEN+1(2),PDPTLEN+2                                           
         MVI   PDPTLEN+3,C' '                                                   
*                                                                               
         OC    BWDUCODE,BWDUCODE   USER CODE                                    
         BZ    *+10                                                             
         MVC   PCODE,BWDUCODE                                                   
*                                                                               
         OC    BWDDATES,BWDDATES   DATES                                        
         BZ    REP30                                                            
         GOTO1 VDATCON,APPARM,(2,BWDDATES),(4,PDATES)                           
         MVI   PDATES+5,C'-'                                                    
         GOTO1 (RF),(R1),(2,BWDDATES+2),(4,PDATES+6)                            
*                                                                               
REP30    CLI   BWDSUBDP,0          SUB-DAYPART                                  
         BE    REP32                                                            
         L     R1,AMISC                                                         
         MVC   0(4,R1),=C'DPT='                                                 
         MVC   4(1,R1),BWDSUBDP                                                 
         MVI   5(R1),C','                                                       
         LA    R1,6(R1)                                                         
         ST    R1,AMISC                                                         
*                                                                               
REP32    CLI   BWDADJ,0            PROGRAM ADJACENCY CODE                       
         BE    REP36                                                            
         L     R1,AMISC                                                         
         MVC   0(3,R1),=C'AJ='                                                  
         MVC   3(1,R1),BWDADJ                                                   
         LA    RE,4(R1)                                                         
         CLI   BWDADJ,C'A'         TEST ALPHA                                   
         BNL   REP34                                                            
         UNPK  APFULL(3),BWDADJ(2) NO-THEN NUMERIC                              
         MVC   3(2,R1),APFULL                                                   
         LA    RE,5(R1)                                                         
*                                                                               
REP34    MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         ST    RE,AMISC                                                         
*                                                                               
REP36    LA    R1,PMISC            CLEAN UP MISCELLANEOUS FIELD                 
         C     R1,AMISC                                                         
         BNL   REP38                                                            
         L     R1,AMISC                                                         
         BCTR  R1,0                                                             
         MVI   0(R1),C' '                                                       
*                                                                               
REP38    CLC   SVDPT,SDPT          TEST DAYPART BREAK                           
         BE    REP39                                                            
         MVC   SVDPT,SDPT                                                       
         CLI   DPTBRK,C'Y'         YES-TEST PAGE BREAK                          
         BNE   REP39                                                            
         OI    REPHEADI,REPHFRCE   YES                                          
*                                                                               
REP39    GOTO1 VREPORT,REPD        *** PRINT A LINE ***                         
*                                                                               
         CLI   LINE,C'*'           TEST BEYOND END                              
         BE    REP42                                                            
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)            INCREMENT LINE NUMBER                        
         STC   R1,LINE                                                          
         CLI   LINE,MAXLINES       TEST = MAX                                   
         BNH   REP42                                                            
         MVI   LINE,C'1'           YES - RESET LINE NUMBER                      
         LA    R1,ALPHATAB               AND INCREMENT THE PAGE NUM             
*                                                                               
REP40    CLI   1(R1),X'FF'                                                      
         BNE   *+16                                                             
         MVI   PAGE,C'*'                                                        
         MVI   LINE,C'*'                                                        
         B     REP42                                                            
         CLC   PAGE,0(R1)                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     REP40                                                            
         MVC   PAGE,1(R1)                                                       
*                                                                               
REP42    B     REP2                NEXT RECORD                                  
*                                                                               
REPXIT   MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* HEADLINE HOOK                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REPD,R9                                                          
RPTHOOK  LR    R0,RE                                                            
         STCM  RF,8,HOOKN                                                       
         CLI   HOOKN,1             HEAD HOOK                                    
         BNE   HOOK4                                                            
         MVC   REPH4+9(L'QMED),QMED                                             
         MVC   REPH5+9(L'QBYR),QBYR                                             
         MVC   REPH6+9(L'QCAM),QCAM                                             
         MVC   REPH7+9(L'QMKT),QMKT                                             
         MVC   REPH4+16(L'MEDNM),MEDNM                                          
         MVC   REPH5+16(L'BYRNM),BYRNM                                          
         MVC   REPH6+16(L'CMPNM),CMPNM                                          
         MVC   REPH7+16(L'MKTNM),MKTNM                                          
         MVC   REPH5+60(L'QCLT),QCLT                                            
         MVC   REPH6+60(L'QPRD),QPRD                                            
         MVC   REPH7+60(L'QEST),QEST                                            
         MVC   REPH5+68(L'CLTNM),CLTNM                                          
         MVC   REPH6+68(L'CLTNM),PRDNM                                          
         MVC   REPH7+68(L'ESTNM),ESTNM                                          
         CLI   CMPPRD1,0           TEST PIGGYBACK PRODUCTS                      
         BE    HOOKX                                                            
         MVC   APWORK(16),BEST                                                  
         MVC   APWORK+16(48),QEST                                               
         GOTO1 AGETPRD,CMPPRD1                                                  
         MVC   REPH6+60(L'QPRD),QPRD                                            
         MVC   REPH6+68(L'PRDNM),PRDNM                                          
         CLI   CMPPRD2,0                                                        
         BE    HOOK2                                                            
         GOTO1 AGETPRD,CMPPRD2                                                  
         MVI   REPH6+63,C'-'                                                    
         MVC   REPH6+64(L'QPRD),QPRD                                            
         MVI   REPH6+78,C'-'                                                    
         MVC   REPH6+79(9),PRDNM                                                
*                                                                               
HOOK2    MVC   BEST(BSLN-BEST+L'BSLN),APWORK                                    
         MVC   QEST(QSLN-QEST+L'QSLN),APWORK+16                                 
         B     HOOKX                                                            
*                                                                               
HOOK4    CLI   HOOKN,2             MIDS HOOK                                    
         BNE   HOOKX                                                            
         MVC   REPM2+12(6),DEMNAMES   DEMO NAMES TO MIDLINE                     
         CLI   DEMNAMES+5,C' '                                                  
         BH    *+14                                                             
         MVC   REPM2+13(5),DEMNAMES                                             
         MVI   REPM2+12,C' '                                                    
*                                                                               
         MVC   REPM2+72(6),DEMNAMES+6                                           
         CLI   DEMNAMES+11,C' '                                                 
         BH    *+14                                                             
         MVC   REPM2+73(5),DEMNAMES+6                                           
         MVI   REPM2+72,C' '                                                    
*                                                                               
         MVI   REPM2+23,C'P'       CPP/CPM                                      
         CLI   DEMOS+1,C'R'                                                     
         BE    *+8                                                              
         MVI   REPM2+23,C'M'                                                    
         MVI   REPM2+83,C'M'                                                    
         CLI   DEMOS+4,C'R'                                                     
         BNE   *+8                                                              
         MVI   REPM2+83,C'P'                                                    
         B     HOOKX                                                            
*                                                                               
HOOKX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET CAMPAIGN DETAILS                                                *         
***********************************************************************         
         SPACE 1                                                                
GETCAM   LR    R0,RE                                                            
         GOTO1 AGETCLT,CMPCLTC     GET CLIENT                                   
         BE    GETCAM10                                                         
         LA    R1,UPRCMPH                                                       
         ST    R1,FVADDR                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GETCAM10 GOTO1 AGETPRD,CMPPRDN     GET PRODUCT                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET DEMO INFORMATION                                                *         
***********************************************************************         
         SPACE 1                                                                
GETDEM   MVC   DEMOS,DEMOVR                                                     
         OC    DEMOVR,DEMOVR       TEST DEMOS OVERRIDE                          
         BNZ   GETDX                                                            
         MVC   DEMOS,ESTDEMS       SET THE DEMOS                                
         CLI   SECDEM,C'Y'         TEST 2ND DEMO SHOULD BE SECONDARY            
         BE    GETDX               YES                                          
         MVC   DEMOS+3(3),DEMOS    NO-                                          
         CLI   DEMOS+1,C'R'                                                     
         BE    *+12                                                             
         CLI   DEMOS+1,C'E'                                                     
         BNE   *+12                                                             
         MVI   DEMOS+4,C'I'                                                     
         B     GETDX                                                            
         CLI   DEMOS+1,C'I'                                                     
         BNE   *+12                                                             
         MVI   DEMOS+4,C'R'                                                     
         B     GETDX                                                            
         XC    DEMOS+3(3),DEMOS+3                                               
*                                                                               
GETDX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET DEMO NAMES                                                      *         
* NTRY - R1=A(DEMOS)                                                  *         
* EXIT - DEMNAMES SET                                                 *         
***********************************************************************         
         SPACE 1                                                                
GETNAMES NTR1                                                                   
         XC    DEMNAMES,DEMNAMES                                                
         OC    0(6,R1),0(R1)                                                    
         BZ    GETNAMEX                                                         
         LA    R8,APELEM                                                        
         USING DBLOCKD,R8                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         ST    R1,APPARM                                                        
         MVI   APPARM,2                                                         
         OC    3(3,R1),3(R1)                                                    
         BNZ   *+8                                                              
         MVI   APPARM,1                                                         
         GOTO1 VDEMOCON,APPARM,,(2,DEMNAMES),(C'S',DBLOCK),ESTUSRNM             
*                                                                               
GETNAMEX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS, ETC                                                      *         
***********************************************************************         
         SPACE 1                                                                
RPTDESC  DC    CL11'UPDATE REPT'                                                
XFF      DC    16X'FF'                                                          
SPACES   DC    16C' '                                                           
ALPHATAB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'FF'                    
*                                                                               
MAXLINES EQU   C'7'                                                             
*                                                                               
RPTSPEC  DS    0X                  ** REPORT SPEC POOL **                       
         SPEC  H1,1,RUN                                                         
         SPEC  H1,60,C'UPDATE REPORT'                                           
         SPEC  H2,60,C'-------------'                                           
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,1,C'MEDIA'                                                    
         SPEC  H5,1,C'BUYER'                                                    
         SPEC  H6,1,C'CAMPAIGN'                                                 
         SPEC  H7,1,C'MARKET'                                                   
         SPEC  H5,52,C'CLIENT'                                                  
         SPEC  H6,52,C'PRODUCT'                                                 
         SPEC  H7,52,C'ESTIMATE'                                                
         SPEC  HOOK,1              HEAD HOOK                                    
         SPEC  M2,1,CL45'LN STATION-        --CPP-- --DAYS- ---TIMES--'         
         SPEC  M2,46,CL42'- -COST- -----PROGRAM-----        --CPM-- '           
         SPEC  M2,88,CL40'DPTLEN CODE ---DATES--- ------MISC------'             
         SPEC  HOOK,2              MIDS HOOK                                    
         SPEC  END                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                     ** LOCAL WORKING STORAGE **                  
*                                                                               
ARECTAB  DS    A                                                                
ARECTABX DS    A                                                                
ANEXTREC DS    A                                                                
AMISC    DS    A                                                                
COST     DS    F                                                                
*                                                                               
DEMOVR   DS    XL6                 2 DEMOS                                      
DEMOS    DS    CL6                                                              
DEMNAMES DS    XL12                                                             
CAMPST   DS    XL2                                                              
CAMPEND  DS    XL2                                                              
SVBMKT   DS    CL(L'BMKT)                                                       
SVQSTA   DS    CL(L'QSTA)                                                       
SVBDPT   DS    CL(L'BDPT)                                                       
SVBSLN   DS    CL(L'BSLN)                                                       
SVDTLKEY DS    XL13                                                             
SVDPT    DS    CL1                                                              
FIRST    DS    CL1                                                              
RPTSW    DS    CL1                                                              
PAGE     DS    CL1                                                              
LINE     DS    CL1                                                              
STAFILT  DS    CL1                                                              
HOOKN    DS    XL1                                                              
KEYCOMP  DS    XL1                                                              
*                                                                               
SELKIND  DS    XL1                 SELECT INDICATORS                            
SELKCAM  EQU   X'80'                                                            
SELKMKT  EQU   X'40'                                                            
SELKSTA  EQU   X'20'                                                            
SELKDPT  EQU   X'10'                                                            
SELKSLN  EQU   X'08'                                                            
*                                                                               
RNKOPT   DS    CL1                 RANK INDICATOR                               
RNKCPP   EQU   C'C'                                                             
RNKDEM   EQU   C'D'                                                             
RNKSTA   EQU   C'S'                                                             
*                                                                               
DPTBRK   DS    CL1                                                              
SECDEM   DS    CL1                                                              
*                                                                               
SREC     DS    0X                  SORT RECORD                                  
SKEY     DS    0X                                                               
SDPT     DS    CL1                 DAYPART                                      
SSLN     DS    XL1                 LENGTH                                       
SDEMO    DS    XL3                 DEMO VALUE                                   
SCPP     DS    XL3                 CPP                                          
SSTA     DS    CL8                 STATION                                      
SDAY     DS    XL1                 DAYS                                         
STIME    DS    XL4                 TIMES                                        
SSEQ     DS    XL1                 SEQUENCE NUM                                 
SKEYL    EQU   *-SKEY                                                           
*                                                                               
SDATA    DS    0CL7                                                             
SINDS    DS    XL1                 INDICATOR                                    
SIPKG    EQU   X'80'               PACKAGE                                      
SIORB    EQU   X'40'               ORBIT                                        
SIEFFDT2 EQU   X'20'               EFFECTIVE DATE 2                             
SIEFFDT3 EQU   X'10'               EFFECTIVE DATE 3                             
SELKEY   DS    CL6                 ELEMENT KEY                                  
*                                                                               
SRECL    EQU   *-SREC                                                           
*                                                                               
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT ON                                                               
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSEFD                                                       
         EJECT                                                                  
REPD     DSECT                                                                  
         ORG   REPP1                                                            
PLINE    DS    CL2                                                              
         DS    CL1                                                              
PSTA     DS    CL8                                                              
         DS    CL1                                                              
PDEMO    DS    CL6                                                              
         DS    CL1                                                              
PCPP     DS    CL7                                                              
         DS    CL1                                                              
PDAYS    DS    CL7                                                              
         DS    CL1                                                              
PTIME    DS    CL11                                                             
         DS    CL1                                                              
PCOST    DS    CL6                                                              
         DS    CL1                                                              
PPROG    DS    CL17                                                             
         DS    CL1                                                              
PDEMO2   DS    CL6                                                              
         DS    CL1                                                              
PCPM     DS    CL7                                                              
         DS    CL1                                                              
PDPTLEN  DS    CL6                                                              
         DS    CL1                                                              
PCODE    DS    CL4                                                              
         DS    CL1                                                              
PDATES   DS    CL11                                                             
         DS    CL1                                                              
PMISC    DS    CL17                                                             
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPNWS21A  07/17/02'                                      
         END                                                                    
