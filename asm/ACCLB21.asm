*          DATA SET ACCLB21    AT LEVEL 013 AS OF 08/16/00                      
*PHASE T62121A                                                                  
CLB21    TITLE '- BILL PROGRAM UPDATE TRANSFERS'                                
CLB21    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CB21**,RR=RE                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING MIXLST,LSMIXLST                                                  
         USING PBLKD,R7                                                         
         USING POSTVALS,PBPOSTV                                                 
         L     RC,AOVERWRK                                                      
         USING XWORKD,RC                                                        
*                                                                               
         L     R4,PBADATAB         DA TABLE                                     
UPDX100  STCM  R4,15,PBLSTDA       SAVE A(CURRENT TABLE ENTRY)                  
         CLI   0(R4),FF            FIN                                          
         BE    UPDX900             CLOSE REPORT ETC                             
         TM    L'TRNKDA(R4),TRNSXFRP                                            
         BZ    UPDX800             NO TRANSFER PENDING HERE                     
         MVC   IODAOVER,0(R4)                                                   
         LA    R1,IOGET+IOACCMST+IO1                                            
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
*                                                                               
*        GOTO1 AGETOPT,BODMCB,AIO1                                              
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         SR    R0,R0                                                            
UPDX120  IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   UPDX120                                                          
         CLI   PTATYPE,PTATTRFT    TEST TRANSFER TO                             
         BNE   UPDX120                                                          
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BNO   UPDX120                                                          
*                                  UPDATE PTAEL/TRNREC PENDING STATUS           
         NI    PTASTAT1,FF-PTASPEND                                             
         MVC   PTAMOA,PBXFRMP                                                   
*&&US*&& MVC   PTADATE,BCTODAYC    SET TODAY FOR DISPLAY                        
         NI    TRNRSTA2,FF-TRNSXFRP                                             
         CLI   PBMODE,PBDRAFTQ     TEST ACTION IS DRAFT                         
         BE    UPDX130                                                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRSELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   *+12                                                             
         L     RE,BODMCB+12                                                     
         OI    TRSSTAT3-TRSELD(RE),TRSSNBIL                                     
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRXELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   UPDX122                                                          
         L     RE,BODMCB+12                                                     
         NI    TRXSTA2-TRXELD(RE),FF-TRXSXFRP                                   
         CP    PTANET,TRNRFST+(TRNAMNT-TRNELD)(L'TRNAMNT)                       
         BNE   UPDX122                                                          
         OI    TRXSTA1-TRXELD(RE),TRXSXALC                                      
*                                                                               
UPDX122  GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         L     R1,AIO1                                                          
         MVC   TRNKEY,0(R1)                                                     
         GOTO1 AIO,IORDUPD+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TRNKSTA2,FF-TRNSXFRP                                             
         GOTO1 AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  UPDATE WORK-CODE SUB-TOTALS                  
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),LSPRATAS,0                
         GOTO1 ASETTRN,BODMCB,(C'S',IOKEY),AIO1,LSPRATA                         
         GOTO1 ASUBTOT,BOPARM,(C'U',LSPRATA),LSPRATAS                           
*                                                                               
UPDX130  L     R2,AIO1             RESET R2=A(TRNREC)                           
*                                                                               
         ZAP   XPLUS,PTANET        SET +/- PTANET                               
         ZAP   XMINUS,PTANET                                                    
         MP    XMINUS,=P'-1'                                                    
*                                                                               
         MVC   XTOCST,BCSPACES     CLEAR ACCOUNT CODES                          
         MVC   XSIACC,BCSPACES                                                  
         MVC   XSICST,BCSPACES                                                  
         MVC   XSKACC,BCSPACES                                                  
*                                                                               
         MVC   XFRCST,BCCMPPRF+(PPRCOSTU-PPRELD)                                
         MVC   XFROFC,CSOFFICE     SET FORM 1C ACCOUNT/OFFICE                   
*                                                                               
         LA    RE,TRNRFST                                                       
         XR    RF,RF                                                            
UPDX132  CLI   0(RE),0                                                          
         BE    UPDX140                                                          
*                                                                               
         USING SPAELD,RE                                                        
         CLI   SPAEL,SPAELQ        TEST 1C ACCOUNT SAVED ON TRANSACTION         
         BNE   UPDX134                                                          
         CLI   SPATYPE,SPATCCST                                                 
         BNE   UPDX134                                                          
         MVC   XFRCST,SPAAULA                                                   
         B     UPDX138                                                          
*                                                                               
         USING ANOELD,RE                                                        
UPDX134  CLI   ANOEL,ANOELQ        TEST FOR CLIENT OFFICE                       
         BNE   UPDX138                                                          
         CLI   ANOTYPE,ANOTCLI                                                  
         BNE   UPDX138                                                          
         MVC   XFROFC,ANOOFFC                                                   
         DROP  RE                                                               
*                                                                               
UPDX138  IC    RF,1(RE)                                                         
         BXH   RE,RF,UPDX132                                                    
*                                                                               
UPDX140  CLC   =C'SK',TRNKULC      SET SK OR SI ACCOUNT FROM CONTRA             
         BNE   *+10                                                             
         MVC   XSKACC,TRNKULC                                                   
         CLC   =C'SI',TRNKULC                                                   
         BNE   *+10                                                             
         MVC   XSIACC,TRNKULC                                                   
         CLC   XSKACC,BCSPACES                                                  
         BNE   UPDX150                                                          
         CLC   XSIACC,BCSPACES                                                  
         BNE   UPDX150                                                          
         BAS   RE,GETSKSI          FIND SK OR SI ACCOUNT IN MEMO                
*                                                                               
UPDX150  CLC   XSIACC,BCSPACES     IF SI ACCOUNT READ FOR COSTING CODE          
         BE    UPDX160                                                          
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+(L'CUABIN)(L'TRNKULC),XSIACC                             
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         L     RF,0(R1)                                                         
         MVC   XSICST,PBACANC-PBACTABD(RF)                                      
*                                                                               
         USING PBACTABD,R4                                                      
UPDX160  LA    R0,PBMAXAC          SEE IF THIS JOB IN ACCOUNT TABLE             
         L     R4,PBAACTAB                                                      
         CLI   PBACKEY,0                                                        
         BE    UPDX170                                                          
         CLC   PTATJOB,PBACKEY+(TRNKACT-TRNKUNT)                                
         BE    UPDX200                                                          
         LA    R4,PBACTABL(R4)                                                  
         BCT   R0,UPDX160+L'UPDX160                                             
         L     R4,PBAACTAB                                                      
*                                                                               
UPDX170  ST    R4,XSAVAC           SAVE ACC TABLE ENTRY                         
         GOTO1 GETJBCST            READ CLI/PRO/JOB AND SAVE COSTING            
                                                                                
UPDX200  MVC   XTOCST,PBACANC      SET TO JOB COSTING ACCOUNT                   
         MVC   XTOOFC,PBACOFF      SET TO JOB OFFICE CODE                       
         DROP  R4                                                               
*                                                                               
UPDX240  MVC   POSTBTMC,PBXFRMC                                                 
         MVC   POSTBTRF,PBXFRREF                                                
         MVI   POSTTYPE,POSTTRNF                                                
         MVC   POSTACT,TRNKULA                                                  
         MVC   POSTACTN,BCJOBNAM                                                
         MVC   POSTCAC,TRNKCULC                                                 
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   POSTREF,TRNKREF                                                  
         MVC   POSTDATE,TRNKDATE                                                
         MVI   POSTSTAT,TRNSDR                                                  
         TM    TRNRFST+(TRNSTAT-TRNELD),TRNSNOCM                                
         BNO   *+8                                                              
         OI    POSTSTAT,TRNSNOCM   DR TO ORIG JOB GETS SAME STATUS              
         TM    TRNRFST+(TRNSTAT-TRNELD),TRNSAUTH                                
         BNO   *+8                                                              
         OI    POSTSTAT,TRNSAUTH   POSTINGS GET ORIGINAL AUTH STATUS            
         ZAP   POSTAMNT,XMINUS                                                  
         MVC   POSTOFFC,TRNKWORK                                                
         MVI   PBPTRS,FF                                                        
         XC    POSTNARR,POSTNARR                                                
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNNARR-TRNELD+1)                                          
         BM    *+14                                                             
         EX    RF,*+4                                                           
         MVC   POSTNARR(0),TRNNARR                                              
         GOTO1 AXTRAELS,BCPARM,TRNELD,PTAELD                                    
*                                  COPY OVER THE 1C ACCOUNT CODE                
         LR    R4,R2                                                            
         USING SPAELD,R4                                                        
UPDX242  SR    RE,RE                                                            
         IC    RE,SPALN                                                         
         AR    R4,RE                                                            
         CLI   SPAEL,0               TEST E-O-R                                 
         BE    UPDX246                                                          
         CLI   SPAEL,SPAELQ                                                     
         BNE   UPDX244                                                          
         CLI   SPATYPE,SPATCCST                                                 
         BNE   UPDX242                                                          
         IC    RE,SPALN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM(0),SPAELD                                                 
         GOTO1 AADDXTRA                                                         
         B     UPDX242                                                          
*                                                                               
         USING ANOELD,R4                                                        
UPDX244  CLI   ANOEL,ANOELQ        COPY ANALYSED OFFICE ELEMENT                 
         BNE   UPDX245                                                          
         CLI   ANOTYPE,ANOTCLI                                                  
         BNE   UPDX242                                                          
         SR    RE,RE                                                            
         IC    RE,ANOLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM(0),ANOELD                                                 
         GOTO1 AADDXTRA                                                         
         B     UPDX242                                                          
*                                                                               
         USING TPRELD,R4                                                        
UPDX245  CLI   TPREL,TPRELQ        COPY PRICELIST INFO (GERMAN)                 
         BNE   UPDX242                                                          
         IC    RE,TPRLN                                                         
         EX    RE,*+4                                                           
         MVC   BOELEM(0),TPRELD                                                 
         GOTO1 AADDXTRA                                                         
         B     UPDX242                                                          
         DROP  R4                                                               
*                                                                               
         PUSH  USING                                                            
         USING PXDELD,BOELEM                                                    
UPDX246  MVI   PXDEL,PXDELQ                                                     
         MVI   PXDLN,PXDLNQ                                                     
         MVI   PXDTYPE,PXDTTO                                                   
         MVC   PXDDATE,BCTODAYP                                                 
         MVC   PXDFRTOC,CUABIN                                                  
         MVC   PXDFRTOU(2),POSTACT                                              
         MVC   PXDFRTOA,PTATJOB                                                 
         GOTO1 AADDXTRA                                                         
         POP   USING                                                            
*                                                                               
         PUSH  USING                                                            
         USING TRSELD,BOELEM                                                    
         XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSNBIL   NOT BILLABLE ON A21                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRSELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   UPDX250                                                          
         L     RE,BODMCB+12                                                     
         MVC   BOBYTE1,TRSSTAT2-TRSELD(RE)                                      
         NI    BOBYTE1,TRSSTADJ+TRSSTMSS+TRSSTIME                               
         MVC   TRSSTAT2,BOBYTE1                                                 
UPDX250  GOTO1 AADDXTRA                                                         
         POP   USING                                                            
*                                                                               
UPDX300  LA    R0,XPBXTRA          COPY EXTRA ELEMENT AREA INTO SAVE            
         LA    RE,PBXTRA                                                        
         LH    R1,=Y(L'XPBXTRA)                                                 
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         NI    POSTSTAT,FF-TRNSNOCM                                             
         TM    PTASTAT2,PTASXCOM   SET NEW COMMISSIONABLE STATUS                
         BO    *+8                                                              
         OI    POSTSTAT,TRNSNOCM                                                
*                                                                               
         LA    R0,PBXTRA           RESTORE EXTRA ELEMENT SAVE AREA              
         LA    RE,XPBXTRA                                                       
         LH    R1,=Y(L'XPBXTRA)                                                 
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     RF,POSTXTRA         REVERSE SCIEL AMOUNTS                        
         SR    R0,R0                                                            
         USING SCIELD,RF                                                        
UPDX310  CLI   SCIEL,0                                                          
         BE    UPDX350                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   UPDX315                                                          
         ZAP   BODUB1,SCIAMNT                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   SCIAMNT,BODUB1                                                   
         CLI   SCILN,SCILN2Q                                                    
         BNE   UPDX348                                                          
         ZAP   BODUB1,SCIADMN                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   SCIADMN,BODUB1                                                   
         B     UPDX348                                                          
*                                                                               
         USING PRTELD,RF                                                        
UPDX315  CLI   PRTEL,PRTELQ        REVERSE PRTHOUR                              
         BNE   UPDX320                                                          
         ZAP   BODUB1,PRTHOUR                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   PRTHOUR,BODUB1                                                   
         B     UPDX348                                                          
*                                                                               
         USING PXDEL,RF                                                         
UPDX320  CLI   PXDEL,PXDELQ        CHANGE POSTING TRANSFER ELEMENT              
         BNE   UPDX330                                                          
         MVI   PXDTYPE,PXDTFROM                                                 
         MVC   PXDFRTOA,BCJOBCOD                                                
         B     UPDX348                                                          
*                                                                               
         USING TRXELD,RF                                                        
UPDX330  CLI   TRXEL,TRXELQ        REMOVE EXCLUDE FROM ALLOC STATUS             
         BNE   UPDX340                                                          
         NI    TRXSTA1,FF-TRXSXALC                                              
         B     UPDX348                                                          
*                                                                               
         USING TRSELD,RF                                                        
UPDX340  CLI   TRSEL,TRSELQ        REMOVE NOT 21 BILLABLE STATUS                
         BNE   UPDX342                                                          
         NI    TRSSTAT3,FF-TRSSNBIL                                             
         B     UPDX348                                                          
*                                                                               
         USING SPAELD,RF                                                        
UPDX342  CLI   SPAEL,SPAELQ                                                     
         BNE   UPDX344                                                          
         CLI   SPATYPE,SPATCCST                                                 
         BNE   UPDX344                                                          
         MVC   SPAAULA,XTOCST                                                   
         B     UPDX348                                                          
*                                                                               
         USING ANOELD,RF                                                        
UPDX344  CLI   ANOEL,ANOELQ                                                     
         BNE   UPDX346                                                          
         MVC   ANOOFFC,XTOOFC                                                   
         B     UPDX348                                                          
*                                                                               
         USING UNPELD,RF                                                        
UPDX346  CLI   UNPEL,UNPELQ                                                     
         BNE   UPDX348                                                          
         MP    UNPUNIT,=P'-1'                                                   
*                                                                               
UPDX348  IC    R0,UNPLN                                                         
         AR    RF,R0                                                            
         B     UPDX310                                                          
         DROP  RF                                                               
*                                                                               
UPDX350  MVC   POSTAACT,PTATJOB                                                 
         GOTO1 ASETACTN,BOPARM,POSTACT,POSTACTN                                 
         MVC   POSTOFFC,PTATWRK                                                 
         CLI   PTALN,PTATLN1Q      TEST SK ACCOUNT OVERRIDDEN                   
         BNH   UPDX360                                                          
         MVC   POSTCAC+(ACTKACT-ACTKCPY)(L'PTATSKAC),PTATSKAC                   
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
UPDX360  L     RF,AIO1                                                          
         ZAP   POSTAMNT,XPLUS                                                   
*                                                                               
         TM    PTASTAT2,PTASXISA   TEST IF TRANSFER IS ALLOCATED                
         BNO   UPDX380                                                          
         MVI   POSTSTA2,POSTALCQ   SET ITEM IS ALLOCATED                        
         GOTO1 ALLXFR                                                           
         CLI   BOELEM,PTAELQ       CHECK IF ELEMENT BUILT                       
         BNE   UPDX370                                                          
         GOTO1 AADDXTRA            ADD EXTRA ELEMENT                            
         B     UPDX380                                                          
UPDX370  MVI   POSTSTA2,POSTNALQ   SET 'SHOULD HAVE BEEN ALLOC' FLAG            
         MVC   FVMSGNO,=Y(AI$LOKXF)                                             
         MVI   FVPARMS,4                                                        
         MVC   FVPARMS+1(3),PTATJOB                                             
*                                                                               
UPDX380  L     RE,AIO1                                                          
         LA    RE,TRNRFST-TRNRECD(RE)                                           
         SR    R0,R0                                                            
         USING FFTELD,RE                                                        
UPDX382  CLI   FFTEL,0             TEST E-O-R                                   
         BE    UPDX386                                                          
         CLI   FFTEL,FFTELQ                                                     
         BE    *+14                                                             
UPDX384  IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
         B     UPDX382                                                          
         CLI   FFTTYPE,FFTTPEDT    TEST PERIOD END DATE                         
         BNE   UPDX384                                                          
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         EX    RF,*+4                                                           
         MVC   BOELEM(0),FFTEL     COPY ONTO NEW POSTING                        
         DROP  RE                                                               
         GOTO1 AADDXTRA                                                         
*                                                                               
UPDX386  GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVI   POSTSTA2,0                                                       
         L     RE,AIO3             IF ALLOCATED UPDATE JOB                      
         TM    TRNRSTA2-TRNRECD(RE),TRNSBILP                                    
         BNO   UPDX390                                                          
         CLI   POSTMODE,POSTLVQ                                                 
         BNE   UPDX390                                                          
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
         L     RE,AIO4             CHECK JOB RECORD IS THERE                    
         CLC   POSTACT,ACTKUNT-ACTKEY(RE)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO4,0,LSPRATA                        
         EJECT                                                                  
***********************************************************************         
*              MAKE EXTRA TRANSFER POSTINGS SK SI AND COSTING         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
UPDX390  L     R2,AIO1                                                          
                                                                                
         CLC   XSKACC,BCSPACES     TEST ANY SK ACCOUNT                          
         BE    UPDX400                                                          
*&&UK                                                                           
         CLC   PTATJOB,TRNKACT                                                  
         BE    UPDX400             SAME JOB - DO NOT BOTHER                     
*&&                                                                             
         MVC   POSTACT,XSKACC      POST TO ORIGINAL SK ACCOUNT                  
         MVC   POSTCAC,TRNKCULA                                                 
         MVC   POSTCACN,BCJOBNAM                                                
         MVC   POSTOFFC,XFROFC                                                  
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,0                                                       
         ZAP   POSTAMNT,XMINUS                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
         CLI   PTALN,PTATLN1Q      TEST SK ACCOUNT OVERRIDDEN                   
         BNH   *+10                                                             
         MVC   POSTACT+(ACTKACT-ACTKUNT),PTATSKAC                               
         MVC   POSTCAC+(ACTKACT-ACTKEY),PTATJOB                                 
         MVC   POSTOFFC,XTOOFC                                                  
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         ZAP   POSTAMNT,XPLUS                                                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
UPDX400  CLC   XSIACC,BCSPACES     TEST ANY SI ACCOUNT                          
         BE    UPDX500                                                          
         MVC   POSTACT,XSIACC                                                   
         MVC   POSTCAC,TRNKCULA    CONTRA IS FROM-JOB                           
         MVC   POSTCACN,BCJOBNAM                                                
         MVC   POSTOFFC,XFROFC                                                  
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,0          CREDIT SI                                    
         ZAP   POSTAMNT,XMINUS                                                  
*&&US                                                                           
         GOTO1 AADDWMDT,BOPARM,PTAELD,BCJOBCOD ADD MEDIA XFER ELEMENT           
         LA    R1,TRNKWORK                                                      
         GOTO1 AADDRFL             ADD WORKCODE FILTER ELEMENT                  
*                                                                               
         LA    RE,PBPTRS           ADD ANALYSIS POINTERS                        
         LA    R1,TRNKULC                                                       
         CLC   =C'1R',0(R1)                                                     
         BNE   *+16                                                             
         ST    R1,0(RE)                                                         
         MVI   0(RE),APENSDR       DR - 1R                                      
         LA    RE,4(RE)                                                         
         LA    R1,XFRCST                                                        
         ST    R1,0(RE)                                                         
         MVI   0(RE),APENSDR       DR - 1C                                      
         LA    RE,4(RE)                                                         
         LA    R1,XSICST                                                        
         ST    R1,0(RE)                                                         
         MVI   0(RE),0             CR - 12                                      
         MVI   4(RE),FF            E-O-L                                        
*&&                                                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
         MVC   POSTCAC+(ACTKACT-ACTKEY),PTATJOB                                 
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   POSTOFFC,XTOOFC                                                  
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,0          CREDIT SI                                    
         ZAP   POSTAMNT,XPLUS                                                   
*&&US                                                                           
         GOTO1 AADDWMDT,BOPARM,PTAELD,PTATJOB ADD MEDIA XFER ELEMENT            
         LA    R1,PTATWRK                                                       
         GOTO1 AADDRFL             ADD WORKCODE FILTER ELEMENT                  
*                                                                               
         LA    RE,PBPTRS           PICK UP LIST OF ANALYSIS POINTERS            
         CLC   =C'1R',TRNKULC                                                   
         BNE   *+8                                                              
         LA    RE,4(RE)                                                         
         LA    R1,XTOCST           SET A(NEW 1C ACCOUNT)                        
         ST    R1,0(RE)                                                         
         MVI   0(RE),APENSDR                                                    
*&&                                                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVI   PBPTRS,FF                                                        
*                                                                               
UPDX500  CLC   XTOCST,XFRCST                                                    
         BE    UPDX700             SAME JOB COST CODE - NO COSTING              
         CLC   XSIACC,BCSPACES                                                  
         BE    UPDX600             NO SI ACCOUNT - NO COSTING                   
         MVC   POSTACT,XFRCST                                                   
         MVC   POSTCAC+(ACTKUNT-ACTKEY),XSICST                                  
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   POSTOFFC,XFROFC                                                  
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,TRNSDR     DEBIT 1C/12                                  
         ZAP   POSTAMNT,XMINUS                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
         XC    POSTACT,POSTCAC+1                                                
         XC    POSTCAC+1(L'POSTACT),POSTACT                                     
         XC    POSTACT,POSTCAC+1                                                
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVI   POSTSTAT,0          CREDIT 12/1C                                 
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
         MVC   POSTACT,XTOCST                                                   
         MVC   POSTCAC+(ACTKUNT-ACTKEY),XSICST                                  
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   POSTOFFC,XTOOFC                                                  
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,TRNSDR     DEBIT 1C/12                                  
         ZAP   POSTAMNT,XPLUS                                                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
         XC    POSTACT,POSTCAC+1                                                
         XC    POSTCAC+1(L'POSTACT),POSTACT                                     
         XC    POSTACT,POSTCAC+1                                                
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVI   POSTSTAT,0          CREDIT 12/1C                                 
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
UPDX600  TM    BCCPYST7,CPYSTMSY   TEST IF TMS USER                             
         BO    UPDX700                                                          
         CLC   =C'1R',TRNKULC      TEST IF 1R/1C POSTINGS REQUIRED              
         BNE   UPDX700             ONLY FOR 1R CONTRA                           
         OC    PTAHOURS,PTAHOURS                                                
         BZ    UPDX700             IF NO HOURS DON'T BOTHER                     
         MVC   POSTACT,TRNKULC                                                  
         MVC   POSTCAC+(ACTKUNT-ACTKEY),XFRCST                                  
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   POSTOFFC,XFROFC                                                  
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,TRNSDR     DEBIT 1R/1C                                  
         ZAP   POSTAMNT,XMINUS                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
         MVC   POSTCAC+(ACTKUNT-ACTKEY),XTOCST                                  
         MVC   POSTOFFC,XTOOFC                                                  
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         ZAP   POSTAMNT,XPLUS      DEBIT 1R/1C                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
UPDX700  GOTO1 AXFRTMS,BOPARM,TRNRECD,(C'T',PTAELD),POSTVALS                    
*                                                                               
UPDX800  ICM   R4,15,PBLSTDA       GET NEXT FROM D/A LIST                       
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     UPDX100                                                          
*                                                                               
UPDX900  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*              FIND SK OR SI ACCOUNT POINTER FOR TRANSFER POSTINGS    *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
GETSKSI  NTR1                                                                   
         LA    RF,TRNRFST                                                       
         SR    R0,R0                                                            
         USING APEELD,RF                                                        
GSKSI10  CLI   APEEL,0             FIND ANALYSIS POINTER ELEMENT                
         BE    EXIT                                                             
         CLI   APEEL,APEELQ                                                     
         BE    GSKSI30                                                          
         CLI   APEEL,SPDELQ                                                     
         BE    GSKSI70                                                          
GSKSI20  IC    R0,APELN                                                         
         AR    RF,R0                                                            
         B     GSKSI10                                                          
GSKSI30  IC    R0,APENUM           R0=NUMBER OF SUB-ELEMENTS                    
         LA    RE,APENTRY                                                       
         SR    RF,RF                                                            
         USING APENTRY,RE                                                       
GSKSI40  IC    RF,APENLEN                                                       
         SH    RF,=Y(APENACT-APENLEN+1)                                         
         CLC   =C'SK',APENACT      TEST SK POINTER                              
         BNE   GSKSI50                                                          
         EX    RF,*+4                                                           
         MVC   XSKACC(0),APENACT                                                
         B     EXIT                                                             
GSKSI50  CLC   =C'SI',APENACT      TEST SI POINTER                              
         BNE   GSKSI60                                                          
         CLC   =C'1R',TRNKULC      ONLY IF 1R CONTRA                            
         BNE   GSKSI60                                                          
         EX    RF,*+4                                                           
         MVC   XSIACC(0),APENACT                                                
         B     EXIT                                                             
GSKSI60  IC    RF,APENLEN                                                       
         AR    RE,RF                                                            
         BCT   R0,GSKSI40                                                       
         B     GSKSI20                                                          
*                                                                               
         USING SPDELD,RF                                                        
GSKSI70  SR    RE,RE                                                            
         IC    RE,SPDLN                                                         
         SH    RE,=Y(SPDACCS-SPDELD+1)                                          
         CLC   =C'SK',SPDACCS                                                   
         BNE   GSKSI80                                                          
         EX    RE,*+4                                                           
         MVC   XSKACC(0),SPDACCS                                                
         B     EXIT                                                             
GSKSI80  CLC   =C'SI',SPDACCS      TEST SI MEMO                                 
         BNE   GSKSI20                                                          
         CLC   =C'1R',TRNKULC      ONLY USE IF 1R CONTRA                        
         BNE   GSKSI20                                                          
         EX    RE,*+4                                                           
         MVC   XSIACC(0),SPDACCS                                                
         B     EXIT                                                             
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
*              GET TO JOB COSTING ACCOUNT                             *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R3                                                        
GETJBCST NTR1  ,                                                                
         L     R1,XSAVAC                                                        
         USING PBACTABD,R1                                                      
         MVC   PBACKEY(L'BCCPYPRD),BCCPYPRD                                     
         MVC   PBACKEY+L'BCCPYPRD(L'PTATJOB),PTATJOB                            
         ICM   RE,15,AIO1                                                       
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         LR    R1,RF                                                            
         ICM   R0,15,AIO2                                                       
         MVCL  R0,RE               SAVE TRANSACTION IN IO2                      
         S     R3,AIO1                                                          
         A     R3,AIO2                                                          
*                                                                               
         USING ACTRECD,RF                                                       
         LA    RF,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PTATJOB(0),BCCLICOD TEST SAME CLIENT                             
         BNE   GJBC10                                                           
         L     R1,XSAVAC                                                        
         MVC   PBACANC,BCCLIPRF+(PPRCOSTU-PPRELD)                               
         MVC   PBACOFF,BCCLIPRF+(PPRGAOFF-PPRELD)                               
         IC    RE,BCPROLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PTATJOB(0),BCCLICOD TEST SAME PRODUCT                            
         BNE   GJBC20                                                           
         OC    BCPROPRF+(PPRCOST-PPRELD)(L'PPRCOST),BCPROPRF+(PPRCOST-PX        
               PRELD)                                                           
         BZ    *+10                                                             
         MVC   PBACANC,BCPROPRF+(PPRCOSTU-PPRELD)                               
         OC    BCPROPRF+(PPRGAOFF-PPRELD)(L'PPRGAOFF),BCPROPRF+(PPRGAOFX        
               F-PPRELD)                                                        
         BZ    GJBC30                                                           
         MVC   PBACOFF,BCCLIPRF+(PPRGAOFF-PPRELD)                               
         B     GJBC30              NOW READ JOB                                 
*                                                                               
GJBC10   EX    RE,*+4              READ FOR CLIENT                              
         MVC   ACTKACT(0),PTATJOB                                               
         GOTO1 AGETACT,0                                                        
         BE    GJBC12                                                           
         DC    H'0'                                                             
GJBC12   ICM   RE,15,ACAPPR        PICK UP PROFILE                              
         L     R1,XSAVAC                                                        
         MVC   PBACANC,(PPRCOSTU-PPRELD)(RE)                                    
         MVC   PBACOFF,(PPRGAOFF-PPRELD)(RE)                                    
*                                                                               
GJBC20   LA    RF,IOKEY            READ FOR PRODUCT                             
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),PTATJOB                                               
         GOTO1 AGETACT,0                                                        
         BE    GJBC22                                                           
         DC    H'0'                                                             
GJBC22   ICM   RE,15,ACAPPR        PICK UP PROFILE                              
         BZ    GJBC30                                                           
         L     R1,XSAVAC                                                        
         OC    (PPRCOST-PPRELD)(L'PPRCOST,RE),(PPRCOST-PPRELD)(RE)              
         BZ    *+10                                                             
         MVC   PBACANC,(PPRCOSTU-PPRELD)(RE)                                    
         CLC   (PPRGAOFF-PPRELD)(L'PPRGAOFF,RE),BCSPACES                        
         BNH   GJBC30                                                           
         MVC   PBACOFF,(PPRGAOFF-PPRELD)(RE)                                    
*                                                                               
GJBC30   LA    RF,IOKEY            READ FOR JOB                                 
         SR    RE,RE                                                            
         IC    RE,BCJOBLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),PTATJOB                                               
         GOTO1 AGETACT,0                                                        
         BE    GJBC32                                                           
         DC    H'0'                                                             
GJBC32   L     R1,XSAVAC                                                        
         ICM   RE,15,ACAPPR        PICK UP PROFILE                              
         BZ    GJBC40                                                           
         OC    (PPRCOST-PPRELD)(L'PPRCOST,RE),(PPRCOST-PPRELD)(RE)              
         BZ    *+10                                                             
         MVC   PBACANC,(PPRCOSTU-PPRELD)(RE)                                    
         CLC   (PPRGAOFF-PPRELD)(L'PPRGAOFF,RE),BCSPACES                        
         BNH   GJBC40                                                           
         MVC   PBACOFF,(PPRGAOFF-PPRELD)(RE)                                    
GJBC40   MVC   PBACNAM,ACNAME                                                   
*                                                                               
         ICM   RE,15,AIO2          RESTORE TRANSACTION TO AIO1                  
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         LR    R1,RF                                                            
         ICM   R0,15,AIO1                                                       
         MVCL  R0,RE                                                            
         L     RF,XSAVAC                                                        
*                                                                               
         XIT1  REGS=(RF)                                                        
         DROP  R1,RF                                                            
         EJECT                                                                  
**********************************************************************          
*              ALLOCATE A TRANSFER POSTING                           *          
**********************************************************************          
         SPACE 1                                                                
ALLXFR   NTR1  ,                                                                
*                                                                               
X        USING TRNKEY,BOWORK2                                                   
         USING POSTVALS,R4         R4=A(POSTING VALUES BLOCK)                   
         MVC   X.TRNKEY,BCSPACES   SET KEY OF NEW POSTING                       
         MVC   X.TRNKCPY,CUABIN                                                 
         MVC   X.TRNKULA,POSTACT                                                
         MVC   X.TRNKWORK,POSTOFFC                                              
         MVC   X.TRNKCULC,POSTCAC                                               
         L     R0,ATIA             DO NOT ALLOW GETOPT TO USE TIA               
         XC    ATIA,ATIA                                                        
         L     RF,AGOPBLK          CLEAR GETOPT OWN SAVED STORAGE               
         XC    0(GOADM-GOBLOCK,RF),0(RF)                                        
         GOTO1 AGETOPT,BODMCB,X.TRNKEY                                          
         ST    R0,ATIA                                                          
         DROP  X                                                                
         USING PTAELD,R3                                                        
X        USING PTAELD,BOELEM                                                    
         XC    X.PTAEL(PTARLN1Q),X.PTAEL                                        
         L     RE,AGOPBLK          GET DESTINATION CLIENT RECORD                
         ICM   RF,15,GOACLI-GOBLOCK(RE)                                         
         AH    RF,=Y(ACCORFST)     NOTE RECORD IN ACCFIL FORMAT                 
         SR    R0,R0                                                            
         USING LOKELD,RF                                                        
ALLX02   IC    R0,LOKLN            LOKEL CANNOT BE FIRST ELEMENT                
         AR    RF,R0                                                            
         CLI   LOKEL,0             FIND LOCK ELEMENT                            
         BE    ALLX04                                                           
         CLI   LOKEL,LOKELQ                                                     
         BNE   ALLX02                                                           
         TM    LOKSTAT,LOKSLOCK    TEST RECORD IS LOCKED                        
         BO    ALLXFRX             YES - DO NOT ADD PTAEL                       
         DROP  RF                                                               
*                                                                               
ALLX04   MVI   X.PTAEL,PTAELQ                                                   
         MVI   X.PTALN,PTARLN1Q                                                 
         MVC   X.PTADATE,BCTODAYC                                               
         MVC   X.PTAPERS,PTAPERS                                                
         MVI   X.PTATYPE,PTATRAL                                                
         MVI   X.PTASTAT1,PTASPEND+PTASCASH                                     
         ZAP   X.PTANET,PTANET                                                  
         ZAP   X.PTANETF,PTANETF                                                
         ZAP   X.PTACDSC,PTACDSC                                                
         MVC   X.PTACUR,PTACUR                                                  
         MVC   X.PTAHOURS,PTAHOURS                                              
         MVC   X.PTAOFFC,PTAOFFC                                                
         L     RE,AGOPBLK                                                       
         ZAP   X.PTARCORT,GOAGYCOM-GOBLOCK(L'GOAGYCOM,RE)                       
         ZAP   X.PTARCOM,BCPZERO                                                
         TM    PTASTAT2,PTASXCOM   TEST TRANSFER IS COMMISSIONABLE              
         BNO   ALLXFRX                                                          
         ZAP   BOPL81(16),X.PTANET SET NET ALLOCATED                            
         AP    BOPL81(16),X.PTACDSC  ADD IN CASH DISCOUNT                       
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),X.PTARCORT                                            
         SRP   BOPL81(16),64-8,5                                                
         ZAP   X.PTARCOM,BOPL81+8(8)                                            
ALLXFRX  B     EXIT                                                             
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
         SPACE 1                                                                
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
XWORKD   DSECT                                                                  
*                                                                               
XSAVAC   DS    A                                                                
*                                                                               
XPLUS    DS    PL8                 + PTANET                                     
XMINUS   DS    PL8                 - PTANET                                     
*                                                                               
XFRCST   DS    XL(L'ACTKULA)       TRANSFER FROM JOB COSTING CODE               
XTOCST   DS    CL(L'ACTKULA)       TRANSFER TO JOB COSTING CODE                 
XSIACC   DS    CL(L'ACTKULA)       TRANSFER SIACCOUNT                           
XSICST   DS    CL(L'ACTKULA)       TRANSFER SIACCOUNT ANALYSIS CODE             
XSKACC   DS    CL(L'ACTKULA)       TRANSFER SKACCOUNT                           
XFROFC   DS    CL2                 TRANSFER FROM OFFICE CODE                    
XTOOFC   DS    CL2                 TRANSFER TO OFFICE CODE                      
*                                                                               
XPBXTRA  DS    XL(L'PBXTRA)        EXTRA ELS SAVE/RESTORE AREA                  
*                                                                               
XWORKL   EQU   *-XWORKD                                                         
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACCLB21   08/16/00'                                      
         END                                                                    
