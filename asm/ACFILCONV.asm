*          DATA SET ACFILCONV  AT LEVEL 026 AS OF 03/19/15                      
*PHASE ACCCONVA                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE DMACCEMU                                                               
*INCLUDE CONVMOS                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
ACCCONV  TITLE 'CONVERT ACCOUNT FILE TO NEW FORMAT'                             
ACCCONV  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACNV**,R9,R8                                                 
         USING ACCWORKD,R6         R6=A(GLOBAL W/S)                             
         L     RA,VCPRINT                                                       
         USING DPRINT,RA           RA=A(PRINT CSECT)                            
*                                                                               
         CLI   OVSWITCH,0                                                       
         BNE   ACC2                                                             
         MVI   OVSWITCH,1                                                       
         MVI   OFATAB,OFATEOTQ                                                  
         GOTO1 VSORTER,DMCB,SORTSRT,SORTREC,0,0                                 
         OPEN  (OUTAPE,(OUTPUT))                                                
         GOTO1 VDATCON,DMCB,(5,0),(2,CTODAY)                                    
         GOTO1 (RF),(R1),(5,0),(10,DTODAY)                                      
*&&US*&& MVI   CTRY,CTRYUSA                                                     
*&&UK*&& MVI   CTRY,CTRYGBR                                                     
*                                                                               
ACCHDR   CLI   PROCREC,HEADER      BUILD OUTPUT FILE HEADER REC                 
         BNE   ACCX                                                             
         LA    R3,IOL              R3=A(OUTPUT RECORD)                          
         USING ACCRECD,RF                                                       
         LA    RF,IO               RF=A(OUTPUT KEY AREA)                        
         MVI   ACCKEY+L'ACCKEY-1,1 SET OUTPUT KEY                               
         L     RE,AIOAREA          RE=A(INPUT RECORD)                           
         LA    RE,ACCORFST+4(RE)                                                
         USING HDRELD,RE                                                        
         SR    R0,R0                                                            
ACCHDR2  CLI   HDREL,0             FIND INPUT FILE HEADER ELEMENT               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   HDREL,HDRELQ                                                     
         BE    *+14                                                             
         IC    R0,HDRLN                                                         
         AR    RE,R0                                                            
         B     ACCHDR2                                                          
         USING FHDELD,R1                                                        
         LA    R1,ACCRFST          BUILD OUTPUT FILE HEADER ELEMENT             
         MVI   FHDEL,FHDELQ                                                     
         MVI   FHDLN,FHDLNQ                                                     
         MVC   FHDLDAT,DTODAY                                                   
         MVC   FHDFNAM,=C'ACCMST1 '                                             
         MVI   FHDCNTR+2,1                                                      
         MVC   FHDLOAD(L'FHDLOAD+L'FHDUPDT+L'FHDGOOD),HDRLOAD                   
         LA    RE,FHDLNQ(R1)                                                    
         MVI   0(RE),0             SET EOR                                      
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STCM  RE,3,ACCRLEN        SET OUTPUT RECLEN                            
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,0(R3)         AND TOTAL OUTPUT LENGTH                      
         CLC   0(2,R3),MINLEN      TEST MINIMUM LENGTH                          
         BNL   *+6                                                              
         DC    H'0'                                                             
         PUT   OUTAPE,(R3)                                                      
         B     ACCX                                                             
         DROP  R1,RE,RF                                                         
         EJECT                                                                  
ACC2     CLI   OVSWITCH,1          TEST PROCESS CALL                            
         BNE   ACCG                                                             
         CLI   WRITE,X'FF'         DON'T PUT DELETED RECORDS                    
         BE    ACCX                                                             
         L     R3,AIOAREA          R3=A(INPUT RECORD)                           
         LA    R4,4(R3)            R4=A(KEY)                                    
         GOTO1 VRECTYP,DMCB,(0,(R4))                                            
         CLI   0(R1),0             TEST UNKNOWN RECORD                          
         BE    ACCIDJ                                                           
         AP    INPUT,=P'1'                                                      
         SPACE 2                                                                
         USING IDJRECD,R4                                                       
ACCIDJ   CLI   IDJKTYP,IDJKTYPQ    DELETE INT AGY JOURNAL POINTERS              
         BNE   ACCIDJX                                                          
         CLI   IDJKSUB,IDJKSUBQ                                                 
         BE    ACCPURGE                                                         
ACCIDJX  DS    0H                                                               
         SPACE 2                                                                
         USING ACTRECD,R4                                                       
ACCCHK   CLC   ACTRLEN,=Y(ACCORFST+2)                                           
         BNH   ACCPURGE                                                         
ACCCHKX  DS    0H                                                               
         SPACE 2                                                                
         USING TRNRECD,R4                                                       
ACCCHG   CLI   PROCREC,TRNSACTN    TEST ACCOUNT HIERARCHY RECORD                
         BH    ACCCHGX                                                          
         CLC   LASTACT,TRNKCULA    TEST CHANGE OF ACCOUNT                       
         BE    *+16                                                             
         BAS   RE,PUTCHD           PUT LAST CONTRA-HEADERS                      
         BAS   RE,PUTOFA           PUT LAST OFFICE/ACCOUNT RECORDS              
         B     ACCCHGX                                                          
         CLC   LASTCAC,TRNKCULC    TEST CHANGE OF CONTRA-ACCOUNT                
         BE    *+8                                                              
         BAS   RE,PUTCHD           PUT LAST CONTRA-HEADERS                      
ACCCHGX  DS    0H                                                               
         SPACE 2                                                                
         USING CPYRECD,R4                                                       
ACCCPY   CLI   PROCREC,COMPANY     PROCESS COMPANY RECORD                       
         BNE   ACCCPYX                                                          
         MVI   CPYFLAG,0                                                        
         LA    R1,CPYRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING CPYELD,R1                                                        
ACCCPY2  CLI   CPYEL,0             TEST E-O-R                                   
         BE    ACCW                                                             
         CLI   CPYEL,CPYELQ                                                     
         BE    *+14                                                             
         IC    R0,CPYLN                                                         
         AR    R1,R0                                                            
         B     ACCCPY2                                                          
         TM    CPYSTAT1,CPYSOROE                                                
         BZ    ACCCPYX                                                          
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    ACCCPYX                                                          
         OI    CPYFLAG,CPYFNEWO    SET NEW OFFICES IN USE                       
         B     ACCW                                                             
ACCCPYX  DS    0H                                                               
         SPACE 2                                                                
         USING ACTRECD,R4                                                       
ACCACT   CLI   PROCREC,ACCHIGH     PROCESS ACCOUNT RECORDS                      
         BL    ACCACTX                                                          
         CLI   PROCREC,ACCLOW                                                   
         BH    ACCACTX                                                          
         MVC   LASTACT,ACTKEY      SAVE CURRENT ACCOUNT KEY                     
         XC    LASTCAC,LASTCAC                                                  
         LA    R1,ACTRECD+ACCORFST                                              
         SR    R0,R0                                                            
ACCACT18 CLI   0(R1),0             LOCATE ACCOUNT STATUS ELEMENT                
         BE    ACCACT20                                                         
         CLI   0(R1),RSTELQ                                                     
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     ACCACT18                                                         
         USING RSTELD,R1                                                        
         CLI   RSTLN,RSTLN2Q                                                    
         BE    ACCACT20                                                         
         MVC   ELEMENT(RSTLN1Q),RSTELD                                          
         MVI   RSTEL,X'FF'                                                      
         LA    R1,ELEMENT                                                       
         MVI   RSTLN,RSTLN2Q                                                    
         XC    RSTSTAT2(RSTLN2Q-RSTLN1Q),RSTSTAT2                               
         BAS   RE,DELPUT                                                        
*                                                                               
ACCACT20 XC    IOL,IOL             BUILD SORT RECORD IN IO                      
         MVC   IO(L'ACTKEY),ACTKEY                                              
         MVC   IO+L'ACTKEY(L'COMPMASK),COMPMASK                                 
         MVC   IO+L'ACTKEY+L'COMPMASK(L'RECNAME),RECNAME                        
         MVC   IOL(2),ACCTLEN                                                   
         GOTO1 VSORTER,DMCB,SORTPUT,IOL                                         
ACCACT25 B     ACCW                                                             
ACCACTX  DS    0H                                                               
         SPACE 2                                                                
         USING TRNRECD,R4                                                       
ACCCHD   CLI   PROCREC,HISTORY     PROCESS HISTORY RECORDS                      
         BNE   ACCCHDX                                                          
         CLC   TRNKDATE,SPACES                                                  
         BNE   ACCX                                                             
         LA    RE,TRNKREF+1        ENSURE A GOOD KEY                            
         LA    R0,L'TRNKREF-1                                                   
         CLI   0(RE),C' '                                                       
         BL    ACCX                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         MVC   LASTCAC,TRNKCULC    SAVE CONTRA-ACCOUNT                          
         MVI   CHDFLAG,0           SET RECORD FLAG                              
         LA    R2,IO                                                            
         USING CHDRECD,R2                                                       
         MVC   CHDKEY,TRNRECD      BUILD CONTRA-HEADER RECORD                   
         XC    CHDKNULL,CHDKNULL                                                
         XC    CHDKSTA(CHDRFST-CHDKSTA),CHDKSTA                                 
         MVC   CHDRSTAT,TRNRECD+ACCOSTAT                                        
*                                                                               
         LA    RE,TRNRECD+ACCORFST RE=A(INPUT RECORD)                           
         LA    RF,CHDRFST          RF=A(OUTPUT RECORD)                          
         SR    R1,R1                                                            
ACCCHD2  CLI   0(RE),0                                                          
         BE    ACCCHD8                                                          
*                                                                               
         CLI   0(RE),CACELQ        TEST CONTRA HEADER ELEMENT                   
         BNE   ACCCHD4                                                          
         OI    CHDFLAG,CHDFCACF    SET CONTRA HEADER ELEMENT COPIED             
         IC    R1,1(RE)            MOVE CONTRA HEADER TO NEW RECORD             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)                                                    
         LA    RF,1(RF,R1)         POINT TO NEXT OUTPUT ELEMENT                 
         MVI   0(RE),X'FF'         SET TO DELETE ON BUCKET RECORD               
         B     ACCCHD6                                                          
*                                                                               
ACCCHD4  CLI   0(RE),BUKELQ        TEST BUCKET OR PRIOR BUCKET ELEMENT          
         BE    *+12                                                             
         CLI   0(RE),PBKELQ                                                     
         BNE   *+12                                                             
         OI    CHDFLAG,CHDFBUKS    YES - SET BUCKET FOUND FLAG                  
         B     ACCCHD6                                                          
         OI    CHDFLAG,CHDFSPEC    SET SPECIAL ELEMENT FOUND FLAG               
         IC    R1,1(RE)            COPY SPECIAL ELEMENT TO NEW HEADER           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)                                                    
         LA    RF,1(RF,R1)         POINT TO NEXT OUTPUT ELEMENT                 
         MVI   0(RE),X'FF'         DELETE SPECIAL ELEMENT FROM BUCKET           
*                                                                               
ACCCHD6  CLI   0(RE),X'FF'         TEST ELEMENT DELETED                         
         BNE   *+8                                                              
         OI    CHDFLAG,CHDFDELS    YES - SET ELEMENT DELETE FLAG                
         IC    R1,1(RE)                                                         
         AR    RE,R1               BUMP TO NEXT ELEMENT                         
         B     ACCCHD2                                                          
*                                                                               
ACCCHD8  CLI   CHDFLAG,0           TEST ANY ACTION HERE                         
         BNE   *+6                                                              
         DC    H'0'                THIS IS AN ALIEN RECORD                      
         MVI   0(RF),0             SET END OF NEW RECORD                        
         LA    RE,1(RF)                                                         
         LA    R0,IO                                                            
         SR    RE,R0                                                            
         STCM  RE,3,CHDRLEN        SET NEW RECORD LENGTH                        
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL           SET TOTAL RECORD LENGTH                      
*                                                                               
         TM    CHDFLAG,CHDFDELS    TEST ELEMENTS DELETED FROM INPUT REC         
         BZ    ACCCHD10                                                         
         GOTO1 VHELLO,DMCB,(C'D',ACCOUNT),(X'FF',TRNRECD),0,0                   
         SR    RE,RE                                                            
         ICM   RE,3,TRNRLEN                                                     
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,0(R3)                                                      
*                                                                               
* DO NOT WRITE ANY CONTRA-HEADERS OUT TO SORT FILE FOR US SINCE                 
* CONTRA NAMES FIX WILL BE OMITTED                                              
*                                                                               
ACCCHD10 DS    0H                                                               
*&&UK                                                                           
         CLC   CHDKCPY,CHDKCCPY    TEST SPECIAL CONTRA                          
         BNE   *+12                                                             
         TM    CHDFLAG,CHDFSPEC    OR WERE SPECIAL ELEMENTS FOUND               
         BZ    ACCCHD12                                                         
*&&                                                                             
ACCCHD11 CLC   IOL(2),MINLEN       TEST MINIMUM LENGTH                          
         BNL   *+6                                                              
         DC    H'0'                                                             
         PUT   OUTAPE,IOL          YES - PUT CONTRA HEADER TO OUTPUT            
         AP    OUTPUT,=P'1'                                                     
         B     ACCCHD14                                                         
*                                                                               
ACCCHD12 XC    CHDKCULA,CHDKCULC   INVERT ACCOUNT AND CONTRA                    
         XC    CHDKCULC,CHDKCULA                                                
         XC    CHDKCULA,CHDKCULC                                                
         GOTO1 VSORTER,DMCB,SORTPUT,IOL                                         
         OI    CHDFLAG,CHDFSORT    SET CONTRA PUT TO SORT                       
*                                                                               
ACCCHD14 CLI   CHDKBTYP,C' '       TEST DEFAULT BUCKET RECORD                   
         BNE   ACCCHD16                                                         
         MVC   CHDREC,CHDRECD      SAVE CONTRA HEADER RECORD IN CHDREC          
         LA    R1,CHDREC                                                        
         LA    R1,CHDRFST-CHDRECD(R1)                                           
         CLI   0(R1),CACELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,CACLN-CACELD(R1)                                              
         AR    R1,R0                                                            
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         LA    R0,CHDREC                                                        
         SR    R1,R0                                                            
         STCM  R1,3,CHDREC+(CHDRLEN-CHDRECD)                                    
         LA    R1,4(R1)                                                         
         SLL   R1,16                                                            
         STCM  R1,15,CHDRECL                                                    
*                                                                               
ACCCHD16 TM    CHDFLAG,CHDFBUKS    TEST ANY CONTRA BUCKET ELEMENTS              
         BZ    ACCX                                                             
         B     ACCW                                                             
ACCCHDX  DS    0H                                                               
         SPACE 2                                                                
         USING TRNRECD,R4                                                       
ACCTRN   CLI   PROCREC,TRNSACTN    TEST TRANSACTION RECORD                      
         BNE   ACCTRNX                                                          
         OC    TRNKREF,SPACES                                                   
         CLI   TRNRECD+ACCORFST,TRNELQ                                          
         BNE   ACCPURGE                                                         
         GOTO1 VCONVMOS,DMCB,(X'FE',TRNRECD+ACCORFST),PMOS                      
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
         LA    R1,TRNRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING TRNELD,R1                                                        
         OC    TRNREF,SPACES                                                    
         TM    CPYFLAG,CPYFNEWO    TEST COMPANY USING NEW OFFICES               
         BZ    ACCTRN2                                                          
         CLC   PRODUL,TRNKUNT                                                   
         BE    ACCTRN2                                                          
         CLC   PCTLUL,TRNKUNT                                                   
         BE    ACCTRN2                                                          
*                                                                               
ACCTRN1  CLI   TRNOFFC,C' '        FORCE MISSING OFFICE TO '00'                 
         BH    *+10                                                             
         MVC   TRNOFFC,=C'00'                                                   
         MVC   TRNKOFF,TRNOFFC     SET OFFICE CODE IN KEY                       
*                                                                               
ACCTRN2  CLI   0(R1),0                                                          
         BNE   ACCTRN4                                                          
         LA    R1,ELEMENT                                                       
         USING TRSELD,R1                                                        
         MVI   TRSEL,TRSELQ                                                     
         B     ACCTRN6                                                          
ACCTRN4  CLI   0(R1),TRSELQ                                                     
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     ACCTRN2                                                          
         CLI   TRSLN,TRSLNQ                                                     
         BE    ACCTRN7                                                          
         MVC   ELEMENT(TRSPMOS-TRSELD),TRSELD                                   
         MVI   TRSEL,X'FF'                                                      
         LA    R1,ELEMENT                                                       
ACCTRN6  MVI   TRSLN,TRSLNQ                                                     
         OC    TRSDATE,TRSDATE                                                  
         BNZ   *+10                                                             
         MVC   TRSDATE,CTODAY                                                   
         MVC   TRSPMOS,PMOS                                                     
         BAS   RE,DELPUT                                                        
*                                                                               
ACCTRN7  TM    CPYFLAG,CPYFNEWO    TEST COMPANY USING NEW OFFICES               
         BZ    ACCW                                                             
         CLC   PRODUL,TRNKUNT      BUT NOT FOR PRODUCTION                       
         BE    ACCW                                                             
         CLC   PCTLUL,TRNKUNT      AND PROJECT CONTROL                          
         BE    ACCW                                                             
         LA    R1,TRNRECD+ACCORFST                                              
         USING TRNELD,R1                                                        
         CLC   TRNOFFC,SPACES      DON'T GENERATE IF SPACES OR LESS             
         BNH   ACCW                                                             
         CLC   TRNOFFC,=C'**'      DON'T GENERATE FOR ORDERS                    
         BE    ACCW                                                             
*                                                                               
         LA    RE,CHDTAB           POST OFFICE TO CONTRA HEADER TABLE           
ACCTRN8  CLC   TRNOFFC,0(RE)                                                    
         BE    ACCTRN12                                                         
         OC    0(L'TRNOFFC,RE),0(RE)                                            
         BZ    *+12                                                             
         LA    RE,L'TRNOFFC(RE)                                                 
         B     ACCTRN8                                                          
         MVC   0(L'TRNOFFC,RE),TRNOFFC                                          
         XC    L'TRNOFFC(L'TRNOFFC,RE),L'TRNOFFC(RE)                            
*                                                                               
ACCTRN12 LA    RE,OFATAB                                                        
         USING OFATABD,RE                                                       
ACCTRN14 CLI   OFATABD,OFATEOTQ    POST TRNSACTION TO OFFICE TABLE              
         BE    ACCTRN16                                                         
         CLC   OFATOFFC,TRNOFFC    MATCH ON OFFICE CODE                         
         BE    ACCTRN18                                                         
         LA    RE,OFATABL(RE)      BUMP TO NEXT TABLE ENTRY                     
         B     ACCTRN14                                                         
ACCTRN16 MVC   OFATOFFC,TRNOFFC    CREATE AN OFFICE TABLE ENTRY                 
         ZAP   OFATTOTD,=P'0'                                                   
         ZAP   OFATTOTC,=P'0'                                                   
         MVC   OFATLMOS,EFFS                                                    
         XC    OFATHMOS,OFATHMOS                                                
         MVI   OFATABD+OFATABL,OFATEOTQ                                         
ACCTRN18 LA    RF,OFATTOTD         UPDATE OFFICE TABLE ENTRY                    
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    RF,OFATTOTC                                                      
         AP    0(L'OFATTOTD,RF),TRNAMNT                                         
         CLC   OFATLMOS,PMOS                                                    
         BL    *+10                                                             
         MVC   OFATLMOS,PMOS                                                    
         CLC   OFATHMOS,PMOS                                                    
         BH    *+10                                                             
         MVC   OFATHMOS,PMOS                                                    
         B     ACCW                                                             
ACCTRNX  DS    0H                                                               
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT RECORD TO NEW FORMAT AND PUT TO OUTPUT TAPE                 *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R4                                                       
ACCW     GOTO1 VDMACEMU,DMCB,=C'OLDN',ACCOUNT,ACCRECD,ACCRECD                   
         ORG   *-2                                                              
         LR    R2,R1               POINT R2 TO PARAMETER LIST                   
         BASR  RE,RF                                                            
         SR    RE,RE                                                            
         ICM   RE,3,ACCRLEN                                                     
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,0(R3)         SET NEW RECORD LENGTH                        
         CLC   0(2,R3),MINLEN      TEST MINIMUM LENGTH                          
         BNL   *+6                                                              
         DC    H'0'                                                             
         PUT   OUTAPE,(R3)                                                      
         AP    OUTPUT,=P'1'                                                     
         B     ACCX                                                             
         SPACE 2                                                                
ACCPURGE MVI   WRITE,X'FF'         DROP RECORD AND EXIT                         
         B     ACCX                                                             
         EJECT                                                                  
***********************************************************************         
* GET RECORDS BACK FROM SORT AND BUILD CONTRA-HEADERS                 *         
***********************************************************************         
         SPACE 1                                                                
ACCG     BAS   RE,PUTOFA           PUT LAST OFFICE/ACCOUNT RECORDS              
         BAS   RE,PUTCHD           PUT LAST CONTRA HEADER RECORDS               
         LA    R2,IO                                                            
         XC    LASTACT,LASTACT                                                  
         USING CHDRECD,R2                                                       
ACCG2    GOTO1 VSORTER,DMCB,SORTGET                                             
         ICM   RE,15,4(R1)                                                      
         BZ    ACCGX                                                            
         SR    RF,RF               MOVE SORT RECORD TO ADDRESSABLE AREA         
         ICM   RF,3,0(RE)                                                       
         LA    R0,IOL                                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLC   COMPMASK,CHDKSTA                                                 
         BNE   ACCG3                                                            
         MVC   LASTACT,CHDKCULA    SAVE ACCOUNT CODE OF REAL ACCOUNT            
         MVC   LASTNAME,CHDKSTA+L'COMPMASK                                      
         B     ACCG2                                                            
*                                                                               
ACCG3    XC    CHDKCULC,CHDKCULA   INVERT CONTRA AND ACCOUNT                    
         XC    CHDKCULA,CHDKCULC                                                
         XC    CHDKCULC,CHDKCULA                                                
         CLC   LASTACT,CHDKCULC    TEST CONTRA IS A REAL ACCOUNT                
         BNE   ACCG4                                                            
*                                                                               
         AP    REALACC,=P'1'                                                    
         BAS   RE,DMPGET                                                        
         XC    ELEMENT,ELEMENT     REBUILD CONTRA-ACCOUNT ELEMENT               
         LA    R5,ELEMENT                                                       
         USING CACELD,R5                                                        
         MVI   CACEL,CACELQ                                                     
         MVC   CACCNT,CHDKCULC                                                  
         MVC   CACNAME,LASTNAME    INSERT ACCOUNT NAME                          
         LA    R1,L'CACNAME        COMPUTE ELEMENT LENGTH                       
         LA    RE,CACNAME+L'CACNAME-1                                           
         CLI   0(RE),C' '          TEST FOR LAST SIGNIFICANT BYTE               
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,CACLN1Q(R1)                                                   
         STC   R1,CACLN                                                         
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('CACELQ',CHDRECD),0,0                 
         GOTO1 (RF),(R1),(C'P',ACCMST),CHDRECD,CACELD,0                         
         SR    RE,RE                                                            
         ICM   RE,3,CHDRLEN        COMPUTE NEW RECORD LENGTH                    
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,IOL                                                           
         BAS   RE,DMPPUT                                                        
*                                                                               
ACCG4    CLC   IOL(2),MINLEN       TEST MINIMUM LENGTH                          
         BNL   *+6                                                              
         DC    H'0'                                                             
         PUT   OUTAPE,IOL                                                       
         AP    OUTPUT,=P'1'                                                     
         B     ACCG2                                                            
*                                                                               
ACCGX    CLOSE (OUTAPE)                                                         
         MVC   P(L'INFMSG1),INFMSG1                                             
         OI    REALACC+L'REALACC-1,X'0F'                                        
         UNPK  P+L'INFMSG1(8),REALACC                                           
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(L'INFMSG2),INFMSG2                                             
         OI    INPUT+L'INPUT-1,X'0F'                                            
         UNPK  P+L'INFMSG2(9),INPUT                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(L'INFMSG3),INFMSG3                                             
         OI    OUTPUT+L'OUTPUT-1,X'0F'                                          
         UNPK  P+L'INFMSG2(9),OUTPUT                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         B     ACCX                                                             
*                                                                               
ACCX     XIT1  ,                                                                
         EJECT                                                                  
DELPUT   LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB,(C'D',ACCOUNT),(X'FF',ACCRECD),0,0                   
         GOTO1 (RF),(R1),(C'P',ACCOUNT),ACCRECD,ELEMENT,0                       
DELPUTX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD OFFICE/ACCOUNT RECORDS FROM OFFICE TABLE           *         
***********************************************************************         
         SPACE 1                                                                
PUTOFA   NTR1  ,                                                                
         LA    R3,OFATAB           BUILD OFFICE/ACCOUNT RECORDS                 
         USING OFATABD,R3                                                       
         LA    R2,IO                                                            
         USING OFARECD,R2                                                       
         MVC   OFAKEY,SPACES                                                    
         MVC   OFAKCULA,LASTACT                                                 
PUTOFA2  CLI   OFATABD,OFATEOTQ    TEST END OF OFFICE TABLE                     
         BE    PUTOFAX                                                          
         MVC   OFAKOFF,OFATOFFC    BUILD REST OF OFFICE/ACCOUNT KEY             
         XC    OFARSTA(256),OFARSTA                                             
         MVC   OFARLMOS,OFATLMOS                                                
         MVC   OFARHMOS,OFATHMOS                                                
         LA    R1,OFARFST                                                       
         USING ABLELD,R1           BUILD BALANCE ELEMENT                        
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,OFATTOTD                                                   
         ZAP   ABLCR,OFATTOTC                                                   
         XC    ABLTXS,ABLTXS                                                    
         LA    R1,ABLLN3Q(R1)                                                   
         MVI   0(R1),0             SET END OF RECORD                            
         LA    R1,1(R1)                                                         
         LA    R0,OFARECD                                                       
         SR    R1,R0                                                            
         STCM  R1,3,OFARLEN        SET RECORD LENGTH                            
         LA    R1,4(R1)                                                         
         SLL   R1,16                                                            
         STCM  R1,15,IOL           SET OUTPUT RECORD LENGTH                     
         CLC   IOL(2),MINLEN       TEST MINIMUM LENGTH                          
         BNL   *+6                                                              
         DC    H'0'                                                             
         PUT   OUTAPE,IOL                                                       
         LA    R3,OFATABL(R3)      BUMP TO NEXT TABLE ENTRY                     
         B     PUTOFA2                                                          
PUTOFAX  MVI   OFATAB,OFATEOTQ     CLEAR OFFICE TABLE                           
         B     ACCX                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD OFFICE CONTRA HEADER RECORDS FROM CHDTAB           *         
***********************************************************************         
         SPACE 1                                                                
PUTCHD   NTR1  ,                                                                
         LA    R2,CHDREC                                                        
         USING CHDRECD,R2                                                       
         LA    R3,CHDTAB                                                        
PUTCHD2  OC    0(L'CHDKOFF,R3),0(R3)                                            
         BZ    PUTCHDX                                                          
         MVC   CHDKOFF,0(R3)                                                    
         TM    CHDFLAG,CHDFSORT                                                 
         BZ    PUTCHD4                                                          
         GOTO1 VSORTER,DMCB,SORTPUT,CHDRECL                                     
         B     PUTCHD6                                                          
PUTCHD4  CLC   CHDRECL(2),MINLEN      TEST MINIMUM LENGTH                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         PUT   OUTAPE,CHDRECL                                                   
PUTCHD6  LA    R3,L'CHDKOFF(R3)                                                 
         B     PUTCHD2                                                          
PUTCHDX  XC    CHDTAB(L'CHDKOFF),CHDTAB                                         
         B     ACCX                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
DMPGET   NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   ACCX                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    ACCX                                                             
         LA    R7,=C'GET'                                                       
         LA    R5,IO                                                            
         MVC   HALF,ACCRLEN-ACCRECD(R5)                                         
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1  ,                                                                
         CP    PDUMP,MAXDUMP                                                    
         BH    ACCX                                                             
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   ACCX                                                             
         LA    R7,=C'PUT'                                                       
         LA    R5,IO                                                            
         MVC   HALF,ACCRLEN-ACCRECD(R5)                                         
         SPACE 1                                                                
DUMP     LH    R8,HALF                                                          
         GOTO1 VPRNTBL,DMCB,(3,(R7)),(R5),C'DUMP',(R8),=C'2D'                   
         B     ACCX                                                             
         SPACE 1                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'100'                                                         
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'100'                                                         
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
CTRY     DC    X'00'                                                            
PRODUL   DC    C'SJ'                                                            
PCTLUL   DC    C'1J'                                                            
ACCTLEN  DC    Y(L'ACTKEY+L'RECNAME+L'COMPMASK+4)                               
COMPMASK DC    X'FFFEFDFCFBFAF9F8'                                              
         SPACE 1                                                                
VRECTYP  DC    V(ACRECTYP)                                                      
VSORTER  DC    V(SORTER)                                                        
VCONVMOS DC    V(CONVMOS)                                                       
VDMACEMU DC    V(DMACCEMU)                                                      
VPRNTBL  DC    V(PRNTBL)                                                        
ACCOUNT  DC    C'ACCOUNT'                                                       
ACCMST   DC    C'ACCMST  '                                                      
         SPACE 1                                                                
INFMSG1  DC    C'CONTRA HEADERS POINTING TO REAL ACCOUNTS='                     
INFMSG2  DC    C'TOTAL RECORDS PROCESSED='                                      
INFMSG3  DC    C'TOTAL RECORDS OUTPUT='                                         
EFFS     DC    X'FFFFFFFFFFFFFFFF'                                              
MINLEN   DC    H'50'                                                            
REALACC  DC    PL6'0'                                                           
INPUT    DC    PL6'0'                                                           
OUTPUT   DC    PL6'0'                                                           
LASTACT  DC    XL15'00'                                                         
LASTNAME DC    CL36' '                                                          
LASTCAC  DC    XL15'00'                                                         
CPYFLAG  DC    X'00'               COMPANY FLAG BYTE                            
CPYFNEWO EQU   X'80'               NEW OFFICES IN USE                           
CHDFLAG  DS    XL1                 CONTRA-HEADER FLAG BYTE                      
CHDFDELS EQU   X'80'               ELEMENT SET FOR DELETION                     
CHDFSPEC EQU   X'40'               SPECIAL ELEMENTS FOUND                       
CHDFCACF EQU   X'20'               CACEL COPIED                                 
CHDFBUKS EQU   X'10'               BUKEL/PBKEL FOUND                            
CHDFSORT EQU   X'08'               CONTRA-HEADER PUT TO SORT                    
CTODAY   DS    XL2                                                              
DTODAY   DS    XL8                                                              
PMOS     DS    PL2                                                              
ELEMENT  DS    XL256                                                            
CHDRECL  DS    F                                                                
CHDREC   DS    XL256                                                            
IOL      DS    F                                                                
IO       DS    2000X                                                            
         SPACE 1                                                                
SORTSRT  DC    C'SORT FIELDS=(5,42,A),FORMAT=BI,WORK=1 '                        
SORTREC  DC    C'RECORD TYPE=V,LENGTH=(1100,,,,) '                              
SORTGET  DC    C'GET'                                                           
SORTPUT  DC    C'PUT'                                                           
         SPACE 1                                                                
OUTAPE   DCB   DDNAME=OUTAPE,DSORG=PS,RECFM=VB,LRECL=4004,MACRF=PM,    *        
               BUFNO=2,BLKSIZE=32760                                            
         SPACE 1                                                                
CHDTAB   DS    (OFATMAXN)XL(L'CHDKOFF)                                          
OFATAB   DS    (OFATMAXN)XL(OFATABL)                                            
         EJECT                                                                  
OFATABD  DSECT                     ** OFFICE TABLE **                           
OFATEOTQ EQU   X'FF'               END OF TABLE INDICATOR                       
OFATOFFC DS    CL(L'OFAKOFF)       OFFICE CODE                                  
OFATTOTD DS    PL(L'ABLDR)         TOTAL DEBITS                                 
OFATTOTC DS    PL(L'ABLCR)         TOTAL CREDITS                                
OFATLMOS DS    PL(L'OFAKLMOS)      LOWEST MOS POSTED                            
OFATHMOS DS    PL(L'OFAKHMOS)      HIGHEST MOS POSTED                           
OFATABL  EQU   *-OFATABD           LENGTH OF OFFICE TABLE ENTRY                 
OFATMAXN EQU   512                 MAXIMUM NUMBER OF OFFICES/ACCOUNT            
         SPACE 2                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* ACACCWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACACCWORKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACFILCONV 03/19/15'                                      
         END                                                                    
