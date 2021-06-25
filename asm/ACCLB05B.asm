*          DATA SET ACCLB05B   AT LEVEL 050 AS OF 12/22/99                      
*PHASE T62105B                                                                  
         TITLE '- BILL PROGRAM - SUMMARY'                                       
CLB05    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL SUMMWRKX-SUMMWRKD,**CLB5**,R8,R7,CLEAR=YES,RR=RE                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R6,RC                                                            
         USING SUMMWRKD,R6                                                      
         L     RC,AOVERWRK         RC=A(LOCAL W/S)                              
         USING EWORKD,RC                                                        
         USING PRORATAD,PRATBLK                                                 
         ST    RE,BORELO                                                        
         A     RE,=A(ADDCOL)                                                    
         ST    RE,BOADDR1                                                       
*                                                                               
         MVC   BOPARM+4(4),=X'D9000A85'                                         
         GOTO1 VCOLY,BOPARM,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VJOBBER,0(R1)                                                    
*                                                                               
         L     RE,=A(ALOVAL)       A(OVERLAY VALIDATION ROUTINE)                
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,BOSVALS2   A(OVERLAY VAL RTN OUTPUT AREA)               
         XC    BOBYTE1,BOBYTE1                                                  
         GOTO1 AVALOPT,ALOTAB      VALIDATE OPTIONS                             
         BNE   EXITL                                                            
*                                                                               
         LA    RE,EWORKD           CLEAR LOCAL WORKING STORAGE                  
         LA    RF,EWORKL                                                        
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLC   CSCPYCUR,CSBILCUR   TEST BILLING IN AGENCY CURRENCY              
         BNE   *+8                                                              
         OI    EINDS,EIAGYCUR                                                   
         L     RF,BOSVALS2                                                      
         CLI   ESWHOLE-OSVALS2(RF),YES                                          
         BNE   *+8                                                              
         OI    EINDS,EIWHOLE                                                    
*                                                                               
         CLC   CSSCRN,TWASCRN                                                   
         BE    *+8                                                              
         OI    EINDS,EIFST                                                      
         GOTO1 AOVRSCR,BOPARM,(CSSCRN,BASOLAYH)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ENQJOBC,BCJOBCOD                                                 
         MVC   ENQJOBN,BCJOBNAM                                                 
         MVCDD BODUB1(2),AC#WC,L                                                
         GOTO1 VDICTAT,BOPARM,C'SL  ',BODUB1                                    
         MVC   ENQWCW,BODUB1                                                    
         MVCDD BODUB1(2),AC#WC,LU                                               
         GOTO1 (RF),(R1)                                                        
         MVC   ENQWCU,BODUB1                                                    
*                                                                               
         BAS   RE,SAVETSAR         SAVE CURRENT TSAR                            
         BAS   RE,READ                                                          
         BAS   RE,SORTBUFF                                                      
         BAS   RE,SCROLLER                                                      
         BAS   RE,DISSCR                                                        
         GOTO1 ATSARIO,TSARES      RESTORE TSAR                                 
         LA    RF,BASACTH                                                       
         ST    RF,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,X'FF'                                                          
         B     EXIT                                                             
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* READ TRANSACTION RECORDS AND BUILD UNIVERSAL W/C BUFFER ENTRY      *          
**********************************************************************          
         SPACE 1                                                                
READ     ST    RE,RETURN1                                                       
         BAS   RE,UBSET            INITIALISE BUFFER                            
         LA    R2,IOKEY            READ JOB RECORD                              
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   ACTKACT,BCJOBCOD                                                 
         MVC   SIOKEY,IOKEY                                                     
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,ACTKDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETOPT,BOPARM,AIO1 ESTABLISH JOB OPTIONS                        
         BAS   RE,JESTER           GET JOB ESTIMATES                            
         DROP  R2                                                               
*                                                                               
         MVC   IOKEY,SIOKEY                                                     
         GOTO1 AIO,IOREAD+IOACCDIR      RE-READ TO ESTABLISH SEQUENCE           
         BE    *+6                                                              
         DC    H'0'                                                             
READ02   GOTO1 AIO,IOSEQ+IOACCDIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'ACTKCULA),SIOKEY                                         
         BNE   READX                                                            
         MVC   IODAOVER,IOKEY+(TRNKDA-TRNRECD)                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,FILTTRN          APPLY TRANSACTION FILTERS                    
         BL    READ02                                                           
         BE    READ04                                                           
*                                                                               
         GOTO1 AGETOPT,BOPARM,AIO1 ESTABLISH W/C OPTIONS                        
         L     RF,AGOPBLK                                                       
         MVC   AGYCOMM,GOAGYCOM-GOBLOCKD(RF)                                    
READ04   XR    R0,R0                                                            
         TM    EINDS,EIAGYCUR                                                   
         BO    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 APRORATA,BOPARM,AIO1,AGOPBLK,ACOM,(R0),PRATBLK,0                 
*                                                                               
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         BAS   RE,UBSET            R1=A(UTILITY BUFFER)                         
         USING WBWORKD,R1                                                       
         MVC   WBWC,TRNKWORK       USE TX KEY W/C UNLESS AN ORDER               
         CLC   TRNKWORK,=C'**'                                                  
         BNE   READ10                                                           
         LA    R3,TRNRFST                                                       
         XR    R0,R0               ESTABLISH W/C FOR ORDERS                     
READ06   CLI   0(R3),0                                                          
         BE    READ12                                                           
         CLI   0(R3),OAMELQ                                                     
         BE    READ08                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     READ06                                                           
READ08   MVC   WBWC,OAMWORK-OAMELD(R3)                                          
         AP    WBORDERS,PA$NET     NET (ORDERS)                                 
         B     *+10                                                             
READ10   AP    WBCHARGE,PA$NET     NET (CHARGES)                                
         AP    WBBILNET,PA$NETBL   NET BILLING (PRIOR BILLS)                    
         AP    WBBILGRS,PA$GRSBL   GROSS BILLING (TOTAL BILLING)                
         AP    WBALLNET,PP$AALLO   ALLOCATED NET                                
         AP    WBALLCOM,PP$ACOMM   ALLOCATED COMMISSION                         
         AP    WBUNBNET,PA$NETUB   UNBILLED NET                                 
         AP    WBUNBGRS,PA$GRSUB   UNBILLED GROSS                               
         AP    WBBILCOM,PA$COMBL   BILLED COMMISSION                            
         AP    WBUNBCOM,PA$COMUB   UNBILLED COMMISSION                          
         BAS   RE,CRCV             INCOME VARIANCE                              
         AP    WBINVAMT,BODUB1                                                  
         BAS   RE,CRCO             % INCOME VARIANCE (USE OPTIONS COMM)         
         AP    WBINVPCT,BODUB1                                                  
         AP    WBUPDWOF,PA$WOFAM   UPDATED WRITE-OFFS                           
         BAS   RE,UPBUFF           UPDATE UNIVERSAL W/C BUFFER                  
         BAS   RE,UPACC            UPDATE ACCUMULATORS                          
READ12   L     RF,AIO1             RE-READ TO ESTABLISH SEQUENCE                
         MVC   IOKEY,0(RF)                                                      
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     READ02                                                           
READX    L     RE,RETURN1                                                       
         BR    RE                                                               
         DROP  R1,R2                                                            
         EJECT                                                                  
**********************************************************************          
* FILTER TRANSACTIONS                                                *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
FILTTRN  ST    RE,RETURN2                                                       
         L     R2,AIO1                                                          
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   FILTLO                                                           
         CLC   =C'99',TRNKWORK                                                  
         BE    FILTLO              EXCLUDE BILLING                              
*                                                                               
         TM    TRNRSTAT,TRNSDRFT   IGNORE DRAFT TRANSACTIONS                    
         BZ    *+12                                                             
         CLI   TRNTYPE,99          ALLOCATION VEHICLE                           
         BNE   FILTLO                                                           
*                                                                               
         TM    TRNSTAT,TRNSHOLD    IGNORE HELD ITEMS                            
         BO    FILTLO                                                           
         TM    TRNSTAT,TRNSREV     IGNORE REVERSALS                             
         BO    FILTLO                                                           
*                                                                               
         CLC   TRNKWORK,=C'**'                                                  
         BE    FILTHI                                                           
         CLC   TRNKWORK,SWC                                                     
         BE    FILTEQ                                                           
*                                                                               
FILTHI   CLI   *,0                 WANTED (CALL GETOPT)                         
         B     *+14                                                             
FILTEQ   CR    RE,RE               WANTED (DON'T CALL GETOPT)                   
         B     *+8                                                              
FILTLO   CLI   *,255               NOT WANTED                                   
         L     RE,RETURN2                                                       
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* GET JOB ESTIMATE VALUES FROM JOBBER                                *          
**********************************************************************          
         SPACE 1                                                                
JESTER   NTR1                                                                   
         LA    RE,COLIST           INITIALISE JOBBER                            
         ST    RE,ACOLIST                                                       
*&&UK*&& GOTO1 VJOBCOL,BOPARM,(X'FF',LOOKFLDH),ACOLIST,ACOM                     
*&&US*&& GOTO1 VJOBCOL,BOPARM,(4,LOOKFLDH),ACOLIST,ACOM                         
*                                                                               
         SR    RE,RE               ENSURE JOBBER BLOCK IS INITIALISED           
         SR    RF,RF                                                            
         L     R0,AJOBBLK                                                       
         LA    R1,JBLOCKL                                                       
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,AJOBBLK          R5=A(JOBBER BLOCK)                           
         USING JOBLOCKD,R5                                                      
         MVC   JBAJOB,AIOA         A(JOB RECORD) - OLD FILE                     
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOM                                                      
         L     RF,AGOPBLK          CLEAR OUT A(BILL EXTENSION BLOCK)            
         XC    (GOABEXT-GOBLOCK)(L'GOABEXT,RF),(GOABEXT-GOBLOCK)(RF)            
         MVC   JBAGOBLK,AGOPBLK                                                 
         MVC   JBGETOPT,VGETOPT                                                 
         MVC   JBAIO,AIO3                                                       
*                                                                               
         LA    RE,SUMMCOLT         COLUMN TABLE                                 
         STCM  RE,15,JBACOLTB                                                   
         LA    RE,SUMMCOLL                                                      
         STCM  RE,15,JBLCOLTB                                                   
         LA    RE,SUMMOPVT         OPERAND VALUE TABLE                          
         STCM  RE,15,JBAOPVTB                                                   
         LA    RE,SUMMOPVL                                                      
         STCM  RE,15,JBLOPVTB                                                   
         GOTO1 VJOBBER,BOPARM,(R5)                                              
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,JBACOLTB         R4=A(COLUMN OUTPUT TABLE)                    
         USING JBCOLD,R4                                                        
*                                                                               
         LR    RE,R4               FIND A PRIMARY WC                            
         USING PRIMED,RE                                                        
         XR    R1,R1                                                            
         ICM   R1,3,JBNROWS                                                     
JEST02   CLI   PRITYPE,JBCOLTWC                                                 
         BNE   JEST08                                                           
         LR    R0,R1                                                            
         BCT   R0,*+8                                                           
         B     JEST10                                                           
         LR    RF,RE               FIND A SECONDARY WC                          
         AH    RF,JBLCOL                                                        
         USING SECOND,RF                                                        
JEST04   CLI   SECTYPE,JBCOLTWC    MATCH PRIMARY AND SECONDARY WCS              
         BNE   JEST06                                                           
         CLC   PRIWC,SECWC                                                      
         BNE   JEST06                                                           
         AP    PRIOE,SECOE         ADD WC VALUES AT PRIMARY LEVEL               
         AP    PRICE,SECCE                                                      
         MVI   SECTYPE,X'FF'                                                    
JEST06   AH    RF,JBLCOL           BUMP SECONDARY POINTER                       
         BCT   R0,JEST04                                                        
JEST08   AH    RE,JBLCOL           BUMP PRIMARY POINTER                         
         BCT   R1,JEST02                                                        
         DROP  RE,RF                                                            
*                                                                               
JEST10   XR    R2,R2                                                            
         ICM   R2,3,JBNROWS                                                     
         LH    R3,JBLCOL                                                        
         BZ    JESTX                                                            
JEST12   CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   JEST14                                                           
         BAS   RE,UBSET            R1=A(UTILITY BUFFER)                         
         USING WBWORKD,R1                                                       
         MVC   WBWC,JBCOLWC                                                     
         AP    WBOESNET,JBCOLVAL                                                
         AP    WBOESGRS,JBCOLVAL+6                                              
         AP    WBCESNET,JBCOLVAL+12                                             
         AP    WBCESGRS,JBCOLVAL+18                                             
         BAS   RE,UPBUFF           UPDATE UNIVERSAL W/C BUFFER                  
         BAS   RE,UPACC            UPDATE ACCUMULATORS                          
JEST14   AR    R4,R3                                                            
         BCT   R2,JEST12                                                        
JESTX    XIT1                                                                   
         DROP  R1,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCROLLER ST    RE,RETURN1                                                       
         TM    EINDS,EIFST                                                      
         BZ    SCR02                                                            
         OI    EINDS,EIFWD                                                      
         XC    COLNUM,COLNUM                                                    
         XC    RECNUM,RECNUM                                                    
         B     SCRX                                                             
*                                                                               
SCR02    MVC   EPAG#LO,CSPAG#LO    SAVE LAST LOW & HIGH RECORDS                 
         MVC   EPAG#HI,CSPAG#HI                                                 
         XC    CSPAG#LO,CSPAG#LO   CLEAR LOW & HIGH VALUES FOR PAGE             
         XC    CSPAG#HI,CSPAG#HI                                                
*                                                                               
         MVC   EVERSCR,BCSCRNUM    SET SCROLL MAGNITUDES = # ENTERED            
         MVC   EHORSCR,BCSCRNUM                                                 
         TM    BCSCRNUM,PFKIMAXN+PFKIPAGE+PFKIHALF                              
         BZ    SCR04                                                            
*                                                                               
         MVI   EVERSCR,MAXROWS     SET SCROLL MAGNITUDES = 1 PAGE               
         IC    RF,CSRHSDIS                                                      
         IC    RE,CSLHSDIS                                                      
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,EHORSCR                                                       
*                                                                               
         TM    BCSCRNUM,PFKIHALF   TEST HALF PAGE SCORLL                        
         BZ    SCR04                                                            
         XR    RE,RE                                                            
         IC    RE,EVERSCR                                                       
         TM    BCSCROLL,PFKIUPDN                                                
         BNZ   *+8                                                              
         IC    RE,MAXROWS                                                       
         SRL   RE,1                                                             
         STC   RE,EVERSCR                                                       
         IC    RE,EHORSCR                                                       
         SRA   RE,1                                                             
         BNZ   *+8                                                              
         LA    RE,1                                                             
         STC   RE,EHORSCR                                                       
*                                                                               
SCR04    TM    BCSCROLL,PFKIHORZ   TEST HORIZONTALLY SCROLLING                  
         BZ    SCR18                                                            
         XR    RF,RF               RF=HORIZONTAL SCROLLING MAGNITUDE            
         IC    RF,EHORSCR                                                       
*                                                                               
         TM    BCSCROLL,PFKIUPDN   TEST SCROLLING RIGHT                         
         BO    SCR06                                                            
         TM    BCSCRNUM,PFKIMAXN   TEST MAXIMUM SCROLL                          
         BO    SCR14                                                            
         XR    R1,R1                                                            
         IC    R1,CSRHSDIS                                                      
         AR    R1,RF                                                            
         CLM   R1,1,BOBYTE1        TEST SCROLLED OVER END                       
         BCTR  R1,0                                                             
         BL    SCR16                                                            
         CLC   BOBYTE1,CSRHSDIS    TEST LAST COLUMN CURRENTLY DISPLAYED         
         BE    SCR10               YES - DISPLAY FIRST PAGE                     
         B     SCR14               NO - DISPLAY LAST PAGE                       
*                                                                               
SCR06    TM    BCSCRNUM,PFKIMAXN   TEST MAXIMUM SCROLL                          
         BO    SCR10                                                            
         XR    R1,R1               SCROLL LEFT                                  
         IC    R1,CSLHSDIS                                                      
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         BP    SCR12                                                            
*                                                                               
SCR10    XR    R1,R1               DISPLAY FIRST PAGE                           
SCR12    STC   R1,COLNUM                                                        
         OI    EINDS,EIFWD                                                      
         B     SCRX                                                             
SCR14    IC    R1,BOBYTE1          DISPLAY LAST PAGE                            
         BCTR  R1,0                                                             
SCR16    STC   R1,COLNUM                                                        
         OI    EINDS,EIBACK                                                     
         B     SCRX                                                             
*                                                                               
SCR18    TM    BCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    SCR20                                                            
*                                                                               
         XC    RECNUM,RECNUM                                                    
         TM    BCSCRNUM,PFKIMAXN   TEST SCROLL FIRST                            
         BNZ   SCRX                                                             
         XR    RF,RF               SCROLL UP (BACKWARDS)                        
         IC    RF,EVERSCR                                                       
         XR    RE,RE                                                            
         ICM   RE,3,EPAG#LO                                                     
         SR    RE,RF               BACK-UP TO RECORD NUMBER-1                   
         BM    SCRX                                                             
         STCM  RE,3,RECNUM                                                      
         B     SCRX                                                             
*                                                                               
SCR20    XC    RECNUM,RECNUM       SET TO DISPLAY FIRST PAGE                    
         XR    RE,RE                                                            
         ICM   RE,3,EPAG#LO                                                     
         LA    RE,1(RE)                                                         
         XR    RF,RF                                                            
         ICM   RF,3,EPAG#HI                                                     
         SR    RF,RE               NUMBER OF ENTRIES ON PAGE                    
         BNP   SCRX                                                             
         CLM   RF,1,EVERSCR        TEST SCROLL EXCEEDS ACTUAL AMOUNT            
         BL    SCRX                                                             
         ICM   RF,1,EVERSCR                                                     
         BZ    SCRX                                                             
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         STCM  RE,3,RECNUM                                                      
*                                                                               
SCRX     L     RE,RETURN1                                                       
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* DISPLAY SCREEN                                                     *          
**********************************************************************          
         SPACE 1                                                                
DISSCR   ST    RE,RETURN1                                                       
         XC    LOC1,LOC1                                                        
         XC    LOC2,LOC2                                                        
         LA    RE,ENQHD1H          DISPLAY COLUMN HEADINGS                      
         XR    R1,R1                                                            
         IC    R1,FHLN-FHD(RE)                                                  
         SH    R1,=Y(FHDAD)                                                     
         LA    RE,ENQHD1                                                        
         LA    RF,ENQHD2                                                        
         ST    RE,ALINE1           A(FIRST HEADLINE START POINT)                
         ST    RF,ALINE2           A(SECOND HEADLINE START POINT)               
         ST    RE,ASTART1                                                       
         ST    RF,ASTART2                                                       
         AR    RE,R1                                                            
         ST    RE,AEND             SAVE A(END OF FIRST LINE)                    
         TM    EINDS,EIBACK                                                     
         BZ    DSC01                                                            
         AR    RF,R1                                                            
         ST    RE,ALINE1                                                        
         ST    RF,ALINE2                                                        
         SR    RE,R1                                                            
         ST    RE,AEND                                                          
DSC01    L     R3,BOSVALS2                                                      
         AH    R3,=Y(ESDIS-OSVALS2)                                             
         XR    R4,R4                                                            
         IC    R4,BOBYTE1          R4=COLUMNS REMAINING (BACKWARDS)             
         XR    RE,RE                                                            
         IC    RE,COLNUM                                                        
         MH    RE,=H'2'                                                         
         AR    R3,RE               R3=A(START COLUMN)                           
         TM    EINDS,EIBACK                                                     
         BO    DSC02                                                            
         MVC   CSLHSDIS,0(R3)                                                   
         B     DSC04                                                            
DSC02    MVC   CSRHSDIS,0(R3)                                                   
         SR    R4,RE               R4=COLUMNS REMAINING (FORWARDS)              
DSC04    GOTO1 ASETCLM,BOPARM,('CLMSUMMQ',0)                                    
         L     R2,ACLMHEAD                                                      
         USING CLMTABD,R2                                                       
         XR    RE,RE                                                            
         ICM   RE,3,CLMLEN                                                      
         AR    RE,R2                                                            
         BCTR  RE,0                                                             
         ST    RE,AEOT             SAVE A(END OF TABLE)                         
         LA    R2,CLMHEADL(R2)                                                  
DSC06    CLC   CLMCHAR,1(R3)                                                    
         BNE   DSC10                                                            
         LA    R5,COLRTNS          R5=A(COLUMN ROUTINES TABLE)                  
         USING COLRTND,R5                                                       
DSC08    CLC   CLMRTN,CRNUM        MATCH ON COLUMN ROUTINE                      
         BE    DSC18                                                            
         LA    R5,COLRTNQ(R5)                                                   
         CLI   CRNUM,0             NOT AN ENQUIRY COLUMN IF NO RTN              
         BNE   DSC08                                                            
         B     DSC12                                                            
DSC10    LA    R2,CLMDATAL(R2)                                                  
         C     R2,AEOT                                                          
         BL    DSC06                                                            
DSC12    TM    EINDS,EIBACK                                                     
         BO    DSC14                                                            
         MVC   CSRHSDIS,0(R3)                                                   
         LA    R3,2(R3)                                                         
         B     DSC16                                                            
DSC14    MVC   CSLHSDIS,0(R3)                                                   
         SH    R3,=H'2'                                                         
DSC16    BCT   R4,DSC04                                                         
         B     DSC36               NO MORE HEADINGS - GET COLUMN DATA           
*                                                                               
DSC18    XR    R1,R1                                                            
         IC    R1,CLMNHWDH         USE NARROW COLUMNS AS DEFAULT                
         TM    EINDS,EIWHOLE                                                    
         BO    *+8                                                              
         IC    R1,CLMHWDTH         USE WIDE COLUMNS IF SHOWING PENNIES          
         L     RE,ALINE1           NEXT SPACE ON HEADLINES (FORWARDS)           
         L     RF,ALINE2                                                        
         TM    EINDS,EIBACK        WILL NEXT HEADING FIT -                      
         BZ    DSC20                                                            
         SR    RE,R1                - BACKWARDS?                                
         SR    RF,R1                                                            
         C     RE,AEND                                                          
         BNH   DSC36                                                            
         B     DSC22                                                            
DSC20    LA    RE,0(R1,RE)          - FORWARDS?                                 
         C     RE,AEND                                                          
         BNL   DSC36                                                            
DSC22    TM    EINDS,EIBACK        YES                                          
         BZ    *+12                                                             
         ST    RE,ALINE1           NEXT SPACE ON HEADLINES (BACKWARDS)          
         ST    RF,ALINE2                                                        
         ST    R1,RETURN2                                                       
*                                                                               
         LA    R0,L'LOC1           SAVE LOCATION OF COLUMN END/START            
         LA    RF,LOC1                                                          
         CLI   0(RF),0                                                          
         BE    DSC24                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
DSC24    LA    RE,LOC1                                                          
         CR    RF,RE                                                            
         BNH   DSC26                                                            
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         LA    RE,1(R1,RE)                                                      
         LA    RF,1(RF)                                                         
         STC   RE,0(RF)                                                         
         B     *+8                                                              
DSC26    STC   R1,0(RF)                                                         
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         ICM   RF,15,CLMHEAD1      DATA DICTIONARY REFERENCE                    
         XR    RE,RE               IS ENTRY WIDER THAN COLUMN?                  
         IC    RE,CLMHEAD1+3                                                    
         ST    RE,RETURN3                                                       
         CR    R1,RE                                                            
         BNL   DSC28               NO                                           
         STCM  RF,15,BOWORK1                                                    
         GOTO1 VDICTAT,BOPARM,C'SL  ',BOWORK1                                   
         L     R1,RETURN2          SCAN BACKWARDS FOR A SPACE                   
         LR    R0,R1                                                            
         L     RE,RETURN3                                                       
         LA    RE,BOWORK1(RE)                                                   
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNH   *+12                                                             
         BCT   R0,*-10                                                          
         B     DSC28               NO SPACE - MOVE AS MUCH AS POSSIBLE          
         L     RF,RETURN2                                                       
         LA    RF,BOWORK2(RF)      MOVE RIGHT SIDE TO RIGHT OF 2ND LINE         
         SR    R1,R0                                                            
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),1(RE)                                                    
         OI    0(RF),X'40'                                                      
         L     RF,RETURN2          MOVE LEFT SIDE TO RIGHT OF 1ST LINE          
         LA    RF,BOWORK1(RF)                                                   
         LA    R1,BOWORK1                                                       
         LR    R0,RE                                                            
         SR    R0,R1                                                            
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         MVC   0(1,RF),0(RE)                                                    
         BCT   R0,*-10                                                          
         LR    R0,RF               SPACE FILL REST OF 1ST LINE                  
         SR    R0,RE                                                            
         BNP   DSC28                                                            
         BCTR  RF,0                                                             
         MVI   0(RF),C' '                                                       
         BCT   R0,*-6                                                           
         B     DSC34                                                            
*                                                                               
DSC28    L     R1,RETURN2                                                       
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         ICM   RF,15,CLMHEAD1      DATA DICTIONARY REFERENCE                    
         ICM   RE,15,=X'02000000'  RIGHT JUSTIFY                                
         OR    RE,R1               AND FORCE LENGTH IF >3                       
         TM    CLMHEAD1,X'02'      CURRENTLY LEFT JUSTIFIED                     
         BZ    DSC30                                                            
         SRL   RF,8                                                             
         SLL   RF,8                                                             
         AR    RF,RE                                                            
DSC30    STCM  RF,15,BOWORK1                                                    
         ICM   RF,15,CLMHEAD2                                                   
         TM    CLMHEAD2,X'02'                                                   
         BZ    DSC32                                                            
         SRL   RF,8                                                             
         SLL   RF,8                                                             
         AR    RF,RE                                                            
DSC32    STCM  RF,15,BOWORK2                                                    
         GOTO1 VDICTAT,BOPARM,C'SL  ',BOWORK1                                   
         GOTO1 (RF),(R1),,BOWORK2                                               
DSC34    L     R1,RETURN2                                                       
         BCTR  R1,0                                                             
         L     RE,ALINE1                                                        
         EX    R1,*+4                                                           
         MVC   0(0,RE),BOWORK1                                                  
         BCTR  RE,0                                                             
         TM    EINDS,EIBACK                                                     
         BO    *+8                                                              
         LA    RE,3(R1,RE)                                                      
         ST    RE,ALINE1                                                        
         L     RE,ALINE2                                                        
         EX    R1,*+4                                                           
         MVC   0(0,RE),BOWORK2                                                  
         BCTR  RE,0                                                             
         TM    EINDS,EIBACK                                                     
         BO    *+8                                                              
         LA    RE,3(R1,RE)                                                      
         ST    RE,ALINE2                                                        
         B     DSC12                                                            
*                                                                               
DSC36    BAS   RE,NEWCOLS          CALCULATE NEW COLUMN LOCATIONS               
*                                                                               
         GOTO1 TYPESET,ASTART1     TYPE-SET COLUMNS (FIRST HEADLINE)            
         GOTO1 TYPESET,ASTART2                      (SECOND HEADLINE)           
*                                                                               
         MVC   CSPAG#LO,RECNUM     SET LOW RECORD NUMBER FOR PAGE               
         LA    RE,ENQWC1H          DISPLAY COLUMN DATA                          
DSC38    ST    RE,ACLINE           SAVE A(CURRENT LINE)                         
         XR    R0,R0                                                            
         IC    R0,FHLN-FHD(RE)                                                  
         AR    RE,R0                                                            
         XR    R1,R1                                                            
         IC    R1,FHLN-FHD(RE)                                                  
         SH    R1,=Y(FHDAD)                                                     
         L     RE,ACLINE                                                        
         AR    RE,R0                                                            
         LA    RE,FHDAD(RE)                                                     
         ST    RE,ALINE1                                                        
         ST    RE,ASTART1                                                       
         AR    RE,R1                                                            
         ST    RE,AEND                                                          
         TM    EINDS,EIBACK                                                     
         BZ    DSC40                                                            
         ST    RE,ALINE1                                                        
         SR    RE,R1                                                            
         ST    RE,AEND                                                          
DSC40    TM    EINDS,EITOTS        DATALINE SET FOR TOTALS                      
         BO    *+12                                                             
         BAS   RE,EXBUFF           EXTRACT DATALINE FROM W/C BUFFER             
         BNE   DSC54                                                            
         BAS   RE,CRCVP            PERFORM SUBSIDIARY COLUMN PROCESSING         
         L     R3,BOSVALS2                                                      
         AH    R3,=Y(ESDIS-OSVALS2)                                             
         XR    R4,R4                                                            
         IC    R4,BOBYTE1          R4=COLUMNS REMAINING (BACKWARDS)             
         XR    RE,RE                                                            
         IC    RE,COLNUM                                                        
         MH    RE,=H'2'                                                         
         AR    R3,RE               R3=A(START COLUMN)                           
         TM    EINDS,EIBACK                                                     
         BO    DSC42                                                            
         SR    R4,RE               R4=COLUMNS REMAINING (FORWARDS)              
DSC42    GOTO1 ASETCLM,BOPARM,('CLMSUMMQ',0)                                    
         L     R2,ACLMHEAD                                                      
         LA    R2,CLMHEADL(R2)                                                  
DSC44    CLC   CLMCHAR,1(R3)                                                    
         BNE   DSC48                                                            
         LA    R5,COLRTNS          R5=A(COLUMN ROUTINES TABLE)                  
DSC46    CLC   CLMRTN,CRNUM        MATCH ON COLUMN ROUTINE                      
         BE    DSC58                                                            
         LA    R5,COLRTNQ(R5)                                                   
         CLI   CRNUM,0                                                          
         BNE   DSC46                                                            
         B     DSC50                                                            
DSC48    LA    R2,CLMDATAL(R2)                                                  
         C     R2,AEOT                                                          
         BL    DSC44                                                            
DSC50    SH    R3,=H'2'                                                         
         TM    EINDS,EIBACK                                                     
         BO    *+8                                                              
         LA    R3,4(R3)                                                         
         BCT   R4,DSC42                                                         
DSC52    L     RE,ACLINE           END OF DATALINE                              
         LA    RF,BOELEM                                                        
         USING WBWORKD,RF                                                       
         MVC   8(L'WBWC,RE),WBWC                                                
         GOTO1 TYPESET,ASTART1     TYPE-SET COLUMNS                             
         L     RE,ACLINE                                                        
         LA    RF,ENQTOTSH         ANY ROOM FOR ANY MORE DATALINES?             
         CR    RE,RF                                                            
         BNL   DSC54                                                            
         LA    RE,ENQWC2H-ENQWC1H(RE)  YES - GET NEXT DATALINE                  
         B     DSC38                                                            
DSC54    MVC   CSPAG#HI,RECNUM     SET HIGH RECORD NUMBER FOR PAGE              
         LA    RE,ENQTOTSH         DISPLAY JOB TOTALS                           
         TM    EINDS,EITOTS                                                     
         BZ    DSC56                                                            
         NI    EINDS,X'FF'-EITOTS                                               
         B     DSCX                                                             
DSC56    MVC   BOELEM,ACCBLOCK                                                  
         OI    EINDS,EITOTS                                                     
         B     DSC38                                                            
         DROP  RF                                                               
*                                                                               
DSC58    XR    R1,R1                                                            
         IC    R1,CLMNHWDH         USE NARROW COLUMNS AS DEFAULT                
         TM    EINDS,EIWHOLE                                                    
         BO    *+8                                                              
         IC    R1,CLMHWDTH         USE WIDE COLUMNS IF SHOWING PENNIES          
         L     RE,ALINE1           NEXT ENTRY ON FIRST DATALINE                 
         TM    EINDS,EIBACK        WILL NEXT ENTRY FIT -                        
         BZ    DSC60                                                            
         SR    RE,R1                - BACKWARDS?                                
         C     RE,AEND                                                          
         BNH   DSC52               YES - SAVE A)NEXT START POINT)               
         LR    RF,RE                                                            
         BCTR  RF,0                                                             
         ST    RF,ALINE2                                                        
         B     DSC62                                                            
DSC60    LA    RF,0(R1,RE)          - FORWARDS?                                 
         C     RF,AEND                                                          
         BNL   DSC52                                                            
         LA    RF,1(RF)            YES - SAVE A(NEXT START POINT)               
         ST    RF,ALINE2                                                        
DSC62    XR    RF,RF                                                            
         ICM   RF,3,CRDISP                                                      
         LA    RF,BOELEM(RF)                                                    
         LR    R0,R1                                                            
         TM    EINDS,EIWHOLE       TEST IF WE WANT TO SEE PENNIES               
         BO    DSC64                                                            
         CURED (P8,(RF)),((R0),(RE)),2,DMCB=BOPARM,FLOAT=-                      
         B     DSC66                                                            
DSC64    CURED (P8,(RF)),((R0),(RE)),2,DMCB=BOPARM,FLOAT=-,DECS=ROUND           
DSC66    MVC   ALINE1,ALINE2                                                    
         B     DSC50                                                            
DSCX     L     RE,RETURN1                                                       
         BR    RE                                                               
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* CALCULATE NEW COLUMN LOCATIONS                                     *          
**********************************************************************          
         SPACE 1                                                                
NEWCOLS  ST    RE,RETURN2                                                       
         L     RE,ALINE1           ESTABLISH LINE SPACES REMAINING              
         L     RF,AEND                                                          
         TM    EINDS,EIBACK                                                     
         BO    *+14                                                             
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         B     *+10                                                             
         SR    RE,RF                                                            
         LA    RF,1(RE)                                                         
         STC   RF,SPACERS                                                       
*                                                                               
         XR    RE,RE               ESTABLISH NUMBER OF COLUMNS ON PAGE          
         XR    RF,RF                                                            
         IC    RE,CSLHSDIS                                                      
         IC    RF,CSRHSDIS                                                      
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,COLPAGE                                                       
*                                                                               
         XR    RF,RF                                                            
         XR    R1,R1                                                            
         IC    R1,SPACERS                                                       
         MVC   LOC2,LOC1           COPY COLUMN LOCATION DATA                    
NEWC02   LTR   R1,R1               R1=SPACES REMAINING                          
         BZ    NEWCX                                                            
         IC    RF,COLPAGE                                                       
         BCTR  RF,0                RF=NUMBER OF COLUMNS INVOLVED                
*                                                                               
         TM    EINDS,EIBACK                                                     
         BO    NEWC04                                                           
         LR    R0,RF               R0=LOOP CONTROLLER                           
         LR    RE,RF               RE=COLUMN INCREMENT                          
         CR    R1,RF                                                            
         BH    *+8                                                              
         LR    R0,R1                                                            
         LR    RE,R1                                                            
         B     NEWC08                                                           
*                                                                               
NEWC04   CR    R1,RF                                                            
         BNH   NEWC06                                                           
         LR    R0,RF                                                            
         LR    RE,RF                                                            
         B     NEWC08                                                           
NEWC06   OI    EINDS,EILAST                                                     
         SR    RF,R1                                                            
         LA    RF,1(RF)                                                         
         LR    R0,RF                                                            
         LR    RE,R1                                                            
         IC    RF,COLPAGE                                                       
         BCTR  RF,0                                                             
*                                                                               
NEWC08   LTR   R0,R0                                                            
         BZ    NEWCX                                                            
NEWC10   LA    R3,LOC2             INCREMENT END/START LOCATIONS                
         AR    R3,RF                                                            
         XR    R4,R4                                                            
         IC    R4,0(R3)                                                         
         AR    R4,RE                                                            
         STC   R4,0(R3)                                                         
         BCTR  RF,0                                                             
         TM    EINDS,EIBACK                                                     
         BZ    *+12                                                             
         TM    EINDS,EILAST                                                     
         BO    *+8                                                              
         BCTR  RE,0                                                             
         BCTR  R1,0                                                             
         BCT   R0,NEWC08                                                        
         TM    EINDS,EIBACK                                                     
         BZ    NEWC02                                                           
         TM    EINDS,EILAST                                                     
         BZ    NEWC02                                                           
         NI    EINDS,X'FF'-EILAST                                               
         BCTR  R1,0                                                             
         BCTR  RE,0                                                             
         LTR   R0,RF                                                            
         BNZ   NEWC10                                                           
NEWCX    L     RE,RETURN2                                                       
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* COLUMN TYPE-SETTING                                                *          
* ENTRY - P1=A(SCREEN LINE)                                          *          
**********************************************************************          
         SPACE 1                                                                
TYPESET  NTR1                                                                   
         L     RE,0(R1)                                                         
         LR    R1,RE                                                            
         MVC   BOWORK1,BCSPACES                                                 
         SH    RE,=Y(FHDAD)                                                     
         XR    RF,RF                                                            
         IC    RF,FHLN-FHD(RE)                                                  
         SH    RF,=Y(FHDAD+1)                                                   
         EX    RF,*+4                                                           
         MVC   BOWORK1(0),0(R1)    MOVE SCREEN LINE TO STORAGE                  
         EX    RF,*+4                                                           
         MVC   0(0,R1),BCSPACES    CLEAR SCREEN LINE                            
*                                                                               
         XR    RE,RE                                                            
         IC    RE,COLPAGE                                                       
         LR    R0,RE               R0=NUMBER OF COLUMNS ON PAGE                 
         LA    R2,LOC1             R2=OLD COLUMN LOCATIONS                      
         LA    R3,LOC2             R3=NEW COLUMN LOCATIONS                      
         LA    R4,BOWORK1          R4=A(OLD LINE)                               
         ST    R4,BOADDR1                                                       
         LR    R5,R1               R5=A(NEW LINE)                               
         ST    R5,BOADDR2                                                       
         IC    RE,0(R2)            RE=L'COLUMN                                  
         TM    EINDS,EIBACK                                                     
         BZ    TYPE02                                                           
         LA    R4,1(RF,R4)                                                      
         ST    R4,BOADDR1                                                       
         SR    R4,RE                                                            
         LA    R5,1(RF,R5)                                                      
         ST    R5,BOADDR2                                                       
         SR    R5,RE                                                            
*                                                                               
TYPE02   BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R5),0(R4)                                                    
         XR    R1,R1                                                            
         XR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         IC    RE,1(R2)                                                         
         SR    RE,RF                                                            
         BCTR  RE,0                RE=L'NEXT COLUMN                             
*                                                                               
         TM    EINDS,EIBACK                                                     
         BO    TYPE04                                                           
         L     R4,BOADDR1                                                       
         IC    R1,1(R2)                                                         
         SR    R1,RE                                                            
         AR    R4,R1               R4=A(NEXT OLD COLUMN) - FORWARDS             
         L     R5,BOADDR2                                                       
         IC    R1,1(R3)                                                         
         SR    R1,RE                                                            
         AR    R5,R1               R5=A(NEXT NEW COLUMN) - FORWARDS             
         B     TYPE06                                                           
*                                                                               
TYPE04   L     R4,BOADDR1                                                       
         IC    R1,1(R2)                                                         
         SR    R4,R1               R4=A(NEXT OLD COLUMN) - BACKWARDS            
         L     R5,BOADDR2                                                       
         IC    R1,1(R3)                                                         
         SR    R5,R1               R5=A(NEXT NEW COLUMN) - BACKWARDS            
*                                                                               
TYPE06   LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,TYPE02                                                        
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* INITIALISE UTILITY BUFFER AND ACCUMULATORS                         *          
* EXIT - R1=A(UTILITY BUFFER)                                        *          
**********************************************************************          
         SPACE 1                                                                
UBSET    LA    R1,BOELEM                                                        
         USING WBWORKD,R1                                                       
         XC    BOELEM,BOELEM                                                    
         LA    RF,WBTSADLN                                                      
         LR    R0,RF                                                            
         LA    RF,WBTSADAT                                                      
         AR    R0,RF                                                            
         ZAP   0(WBELM,RF),=P'0'                                                
         LA    RF,WBELM(RF)                                                     
         CR    RF,R0                                                            
         BL    *-12                                                             
         TM    EINDS,EIACCS                                                     
         BO    *+14                                                             
         MVC   ACCBLOCK,BOELEM                                                  
         OI    EINDS,EIACCS                                                     
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
**********************************************************************          
* UPDATE ACCUMULATORS                                                *          
* ENTRY - R1=A(UTILITY BUFFER)                                       *          
**********************************************************************          
         SPACE 1                                                                
UPACC    ST    RE,RETURN2                                                       
         USING WBWORKD,R1                                                       
         LA    RE,WBTSADAT                                                      
         LA    R0,WBTSADX                                                       
         LA    RF,ACCBLOCK+WBTSAKLN                                             
UPA02    CLC   0(WBELM,RE),PL8ZERO                                              
         BE    *+10                                                             
         AP    0(WBELM,RF),0(WBELM,RE)                                          
         LA    RE,WBELM(RE)                                                     
         LA    RF,WBELM(RF)                                                     
         CR    RE,R0                                                            
         BL    UPA02                                                            
         L     RE,RETURN2                                                       
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* COLUMN ROUTINES (PRE-SORT)                                         *          
**********************************************************************          
         SPACE 1                                                                
CRCO     ST    RE,RETURN2          COMM (OPTS) = B.NET * COMM RATE              
         ZAP   BOPL81,AGYCOMM                                                   
         ZAP   MYPACK,PA$NETBL                                                  
         SRP   MYPACK,2,0                                                       
         MP    MYPACK,BOPL81                                                    
         SRP   MYPACK,64-8,5                                                    
         ZAP   BODUB1,MYPACK                                                    
CRCOX    L     RE,RETURN2                                                       
         BR    RE                                                               
*                                                                               
CRCV     ST    RE,RETURN3          VAR = B.COMM (ACT) - COMM (OPTS)             
         BAS   RE,CRCO                                                          
         ZAP   MYPACK,PA$COMBL                                                  
         SP    MYPACK,BODUB1                                                    
         MVC   BODUB1,MYPACK+(L'BODUB1)                                         
CRCVX    L     RE,RETURN3                                                       
         BR    RE                                                               
         SPACE 1                                                                
**********************************************************************          
* COLUMN ROUTINES  (POST-SORT)                                       *          
**********************************************************************          
         SPACE 1                                                                
CRCVP    ST    RE,RETURN2          %VAR = VAR / COMM (OPTS)                     
         ZAP   BODUB1,PL8ZERO                                                   
         LA    R1,BOELEM                                                        
         USING WBWORKD,R1                                                       
         CP    WBINVPCT,PL8ZERO                                                 
         BE    CRCVPX                                                           
         ZAP   MYPACK,WBINVAMT                                                  
         SRP   MYPACK,4,5                                                       
         DP    MYPACK,WBINVPCT                                                  
         ZAP   WBINVPCT,MYPACK(L'WBINVPCT)                                      
CRCVPX   L     RE,RETURN2                                                       
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* SAVE CURRENT TSAR BUFFER TO DISK                                   *          
**********************************************************************          
         SPACE 1                                                                
SAVETSAR NTR1                                                                   
         USING TSARD,R2                                                         
         GOTO1 ATSARIO,TSASAV                                                   
         L     R2,ATSABLK                                                       
         MVI   TSRECI,0            INITIALISE FOR W/C BUFFER                    
         MVI   TSINDS,TSINODSK                                                  
         MVI   TSIND2,0                                                         
         LA    R1,WBWORK                                                        
         ST    R1,TSAREC                                                        
         MVI   TSKEYL,WBTSAKLN                                                  
         MVC   TSRECL,=Y(WBTSARLN)                                              
         MVI   TSACTN,TSAINI                                                    
         GOTO1 VTSAR,(R2)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* EXTRACT DATALINE FROM UNIVERSAL W/C BUFFER                         *          
**********************************************************************          
         SPACE 1                                                                
EXBUFF   NTR1                                                                   
         USING WBWORKD,R3                                                       
         L     R2,ATSABLK                                                       
         LA    R3,WBWORK                                                        
         XR    RF,RF                                                            
         ICM   RF,3,RECNUM                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,3,RECNUM                                                      
         CLM   RF,3,TSPRECN        ALL RECORDS PROCESSED?                       
         BH    EXITN                                                            
         STH   RF,TSRNUM                                                        
         MVI   TSACTN,TSAGET       NO - GET DATALINE                            
         GOTO1 VTSAR,(R2)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BOELEM(WBTSARLN),WBTSAREC                                        
         B     EXITY                                                            
         SPACE 1                                                                
**********************************************************************          
* SORT UNIVERSAL W/C BUFFER                                          *          
**********************************************************************          
         SPACE 1                                                                
SORTBUFF NTR1                                                                   
         L     R2,ATSABLK                                                       
         MVI   TSACTN,TSASRT       ACTION SORT                                  
         MVI   TSRTKSEQ,0          ASCENDING SORT                               
         MVI   TSRTKDSP,0          DISPLACEMENT TO SORT KEY                     
         MVI   TSRTKLEN,WBTSAKLN   LENGTH OF SORT KEY                           
         GOTO1 VTSAR,(R2)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* UPDATE UNIVERSAL W/C BUFFER                                        *          
**********************************************************************          
         SPACE 1                                                                
UPBUFF   NTR1                                                                   
         L     R2,ATSABLK                                                       
         XC    TSRNUM,TSRNUM                                                    
         LA    R3,WBWORK                                                        
*                                                                               
UPB02    XR    R1,R1                                                            
         ICM   R1,3,TSRNUM                                                      
         LA    R1,1(R1)            INCREMENT TSAR RECORD NUMBER                 
         CLM   R1,3,TSPRECN        ALL RECORDS PROCESSED?                       
         BNH   UPB04                                                            
         MVC   WBTSAREC(WBTSARLN),BOELEM  YES - ADD NEW BUFFER ENTRY            
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,(R2)                                                       
         BE    UPBX                                                             
         DC    H'0'                                                             
UPB04    STH   R1,TSRNUM                                                        
         MVI   TSACTN,TSAGET       GET NEXT BUFFER ENTRY                        
         GOTO1 VTSAR,(R2)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   WBWC,BOELEM         MATCH ON W/C                                 
         BNE   UPB02                                                            
         LA    RE,BOELEM+(WBTSADAT-WBTSAREC)                                    
         LA    RF,WBTSADLN                                                      
         LR    R0,RF                                                            
         LA    RF,WBTSADAT                                                      
         AR    R0,RF                                                            
UPB06    AP    0(WBELM,RF),0(WBELM,RE)                                          
         LA    RE,WBELM(RE)                                                     
         LA    RF,WBELM(RF)                                                     
         CR    RF,R0                                                            
         BL    UPB06                                                            
         MVI   TSACTN,TSAWRT       WRITE BACK BUFFER ENTRY                      
         GOTO1 VTSAR,(R2)                                                       
         BE    UPBX                                                             
         DC    H'0'                                                             
UPBX     XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL #1                                                    *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* OPTION TABLE                                                       *          
**********************************************************************          
         SPACE 1                                                                
ALOTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,ESDISQ,ESDISQ)                                   
         DC    AL1(1)                                                           
         DC    AL2(1,ESCOLS-OSVALS2)                                            
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  WHOLE - Y/N                                  
         DC    AL2(UC8WHOLE-TWAD,UC3WHOLE-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,0,L'ESWHOLE,L'ESWHOLE,L'ESWHOLE)                     
         DC    AL1(2)                                                           
         DC    AL2(2,ESWHOLE-OSVALS2)                                           
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
ALOTABX  DC    AL1(0)                                                           
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP  R7,R8,RB                                                         
ALOVAL   NMOD1 250,**ALOV**,CLEAR=YES                                           
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     ALOVDIS             1 DIS=ABC                                    
         B     ALOVYN              2 OPT=Y/N                                    
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISPLAY=ABC OR +ABC OR ABC+ OR + (DEFAULT COLUMNS)         *         
***********************************************************************         
         SPACE 1                                                                
ALOVDIS  MVI   BCWORK,0                                                         
         MVI   BOBYTE1,0                                                        
         SR    RE,RE                                                            
         ICM   RE,1,FVXLEN         TEST MORE THAN ONE CHARACTER INPUT           
         BNZ   VDIS00                                                           
         CLI   FVIFLD,C'+'         TEST FOR DIS=+ (DEFAULT COLUMNS)             
         BNE   VDIS06                                                           
         L     RF,AGOPBLK          LOOK FOR USER DEFAULT COLUMNS                
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         LA    R1,GOCBDSUM-GOBBLOCK(RF)                                         
         LA    R0,L'GOCBDSUM                                                    
         OC    0(L'GOCBDSUM,R1),0(R1)                                           
         BNZ   *+12                                                             
         LA    R1,ESCOLD           USE PROGRAM DEFAULT COLUMNS                  
         LA    R0,ESCOL2                                                        
         L     RF,BOADDR1                                                       
         BASR  RE,RF               CALL ADDCOL                                  
         LA    R1,1(R1)                                                         
         BCT   R0,*-6                                                           
         B     VDISY                                                            
*                                                                               
VDIS00   CLI   FVIFLD,C'+'         TEST FOR LEADING + SIGN (SUFFIX)             
         BNE   VDIS04                                                           
         CLI   FVILEN,2            TEST FOR DIS=++                              
         BNE   VDIS02                                                           
         CLI   FVIFLD+1,C'+'                                                    
         BNE   VDIS02                                                           
         LA    R0,ESCOL2                                                        
         BAS   RE,VDISR            ADD ALL ENQUIRY COLUMNS TO LIST              
         B     VDISY                                                            
*                                                                               
VDIS02   MVI   BCWORK,ESCSUFF                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),FVIFLD+1                                               
         STC   RE,FVILEN                                                        
         LA    R0,ESCOL1                                                        
         BAS   RE,VDISR            ADD DEFAULT BEFORE USER COLUMNS              
         B     VDIS06                                                           
*                                                                               
VDIS04   LA    RF,FVIFLD(RE)       POINT TO END OF INPUT STRING                 
         CLI   0(RF),C'+'          TEST FOR TRAILING + SIGN (PREFIX)            
         BNE   VDIS06                                                           
         MVI   BCWORK,ESCPREF                                                   
         MVI   0(RF),C' '                                                       
         STC   RE,FVILEN                                                        
*                                                                               
VDIS06   SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
         L     RF,BOADDR1                                                       
VDIS08   BASR  RE,RF               ADD USER COLUMNS TO LIST                     
         BE    *+14                                                             
         MVC   FVXTRA(1),0(R1)                                                  
         B     VDISN                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VDIS08                                                        
*                                                                               
         CLI   BCWORK,ESCPREF                                                   
         BNE   VDISY                                                            
         LA    R0,ESCOL1                                                        
         BAS   RE,VDISR            ADD DEFAULT AFTER USER COLUMNS               
         B     VDISY                                                            
*                                                                               
VDISY    MVC   BOWORK1(ESDISQ),BCWORK+1                                         
         XC    BCWORK,BCWORK                                                    
         MVC   BCWORK(ESDISQ),BOWORK1                                           
         B     VDISX                                                            
*                                                                               
VDISN    MVC   FVMSGNO,=AL2(AE$INCOL)                                           
*                                                                               
VDISX    XMOD1                                                                  
*                                                                               
VDISR    ST    RE,BOFULL1                                                       
         LA    R1,ESCOLD           ADD NORMAL DISPLAY COLUMNS                   
         L     RF,BOADDR1                                                       
         BASR  RE,RF                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-6                                                           
         L     RE,BOFULL1                                                       
         BR    RE                                                               
*                                                                               
ESCOLD   DC    AL1(SUM#CEN,SUM#CHA,SUM#ODS,SUM#BIN,SUM#ANT)                     
         DC    AL1(SUM#ACO,SUM#BIG,SUM#UNN)                                     
ESCOL1   EQU   *-ESCOLD                                                         
         DC    AL1(SUM#CEG,SUM#OEN,SUM#OEG,SUM#BIC,SUM#UNC)                     
         DC    AL1(SUM#UNG,SUM#IVA,SUM#IVP,SUM#UWO)                             
ESCOL2   EQU   *-ESCOLD                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE Y/N OPTIONS                                                *         
***********************************************************************         
         SPACE 1                                                                
ALOVYN   SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         MVI   BCWORK,C'Y'                                                      
         LH    RE,=Y(UC@YES-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VYNX                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'N'                                                      
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VYNX                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ICODO)                                           
VYNX     XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD A COLUMN TO LIST OF DISPLAY COLUMNS                             *         
*                                                                     *         
* NTRY - R1=A(DISPLAY COLUMN CHARACTER)                               *         
* EXIT - CC=EQUAL IF OK, NOT EQUAL ON ERROR                           *         
***********************************************************************         
         SPACE 1                                                                
ADDCOL   NTR1                                                                   
         CLI   0(R1),0             IGNORE NULL COLUMNS                          
         BE    ADDCOLY                                                          
         LR    R0,R1                                                            
         GOTO1 ASETCLM,BOPARM,('CLMSUMMQ',0)                                    
         LR    R1,R0                                                            
         L     RE,ACLMHEAD                                                      
         USING CLMTABD,RE          RE=A(DISPLAY COLUMN TABLE)                   
         XR    R0,R0                                                            
         ICM   R0,3,CLMLEN                                                      
         AR    R0,RE                                                            
         SH    R0,=H'1'                                                         
         STCM  R0,15,BOADDR2                                                    
         LA    RE,CLMHEADL(RE)                                                  
ADDCOL02 C     RE,BOADDR2          TEST EOT                                     
         BNL   ADDCOLN                                                          
         CLC   CLMCHAR,0(R1)       MATCH CHARACTER TO TABLE                     
         BE    *+12                                                             
         LA    RE,CLMDATAL(RE)                                                  
         B     ADDCOL02                                                         
         TM    CLMINDS1,CLMIENQ    MUST BE AN ENQUIRY COLUMN                    
         BZ    ADDCOLN                                                          
*                                                                               
         LA    R1,BCWORK+1                                                      
         LA    R0,ESDISQ                                                        
ADDCOL04 CLI   1(R1),0             TEST THIS IS A NEW COLUMN                    
         BE    ADDCOL06                                                         
         CLC   CLMCHAR,1(R1)       IGNORE DUPLICATE COLUMNS                     
         BE    ADDCOLY                                                          
         LA    R1,2(R1)                                                         
         BCT   R0,ADDCOL04                                                      
         B     ADDCOLN                                                          
ADDCOL06 XR    RF,RF                                                            
         IC    RF,BOBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BOBYTE1                                                       
         MVC   0(1,R1),BOBYTE1     SET RELATIVE COLUMN NUMBER                   
         MVC   1(L'CLMCHAR,R1),CLMCHAR  SET COLUMN CHARATER                     
*                                                                               
ADDCOLY  CR    RE,RE                                                            
         B     ADDCOLX                                                          
ADDCOLN  CLI   *,0                                                              
*                                                                               
ADDCOLX  XIT1                                                                   
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL #2                                                    *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CONSTANTS AND EQUATES                                              *          
**********************************************************************          
         SPACE 1                                                                
*&&UK                                                                           
LOOKFLDH DC    AL2(AC#OEN,AC#OEG,AC#CEN,AC#CEG,0)                               
*&&                                                                             
*&&US                                                                           
LOOKFLDH DC    XL5'00'                                                          
         DC    AL1(L'LOOKFLD)                                                   
         DC    XL3'00'                                                          
LOOKFLD  DC    C'OEN,OEG,CEN,CEG'                                               
*&&                                                                             
PL8ZERO  DC    PL8'0'                                                           
YES      EQU   C'Y'                                                             
MAXROWS  EQU   17                                                               
         EJECT                                                                  
**********************************************************************          
* COLUMN ROUTINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
COLRTNS  DC    AL1(20),AL2(WBOESGRS-WBTSAREC)                                   
         DC    AL1(21),AL2(WBOESNET-WBTSAREC)                                   
         DC    AL1(22),AL2(WBCESGRS-WBTSAREC)                                   
         DC    AL1(23),AL2(WBCESNET-WBTSAREC)                                   
         DC    AL1(24),AL2(WBCHARGE-WBTSAREC)                                   
         DC    AL1(25),AL2(WBORDERS-WBTSAREC)                                   
         DC    AL1(26),AL2(WBBILGRS-WBTSAREC)                                   
         DC    AL1(27),AL2(WBBILNET-WBTSAREC)                                   
         DC    AL1(28),AL2(WBALLCOM-WBTSAREC)                                   
         DC    AL1(29),AL2(WBALLNET-WBTSAREC)                                   
         DC    AL1(30),AL2(WBUNBGRS-WBTSAREC)                                   
         DC    AL1(31),AL2(WBUNBNET-WBTSAREC)                                   
         DC    AL1(32),AL2(WBBILCOM-WBTSAREC)                                   
         DC    AL1(33),AL2(WBUNBCOM-WBTSAREC)                                   
         DC    AL1(34),AL2(WBINVAMT-WBTSAREC)                                   
         DC    AL1(35),AL2(WBINVPCT-WBTSAREC)                                   
         DC    AL1(36),AL2(WBUPDWOF-WBTSAREC)                                   
         DC    AL1(0)                                                           
         EJECT                                                                  
**********************************************************************          
* MY EXTRA WORKING STORAGE                                           *          
**********************************************************************          
         SPACE 1                                                                
SUMMWRKD DSECT                                                                  
SUMMOPVT DS    (SUMMOPVL)X                                                      
SUMMCOLT DS    (SUMMCOLL)X                                                      
SUMMOPVL EQU   4000                                                             
SUMMCOLL EQU   4000                                                             
SUMMWRKX EQU   *                                                                
         EJECT                                                                  
**********************************************************************          
* LOCAL WORKING STORAGE                                              *          
**********************************************************************          
         SPACE 1                                                                
EWORKD   DSECT                                                                  
RETURN1  DS    A                   RETURN ADDRESSES                             
RETURN2  DS    A                                                                
RETURN3  DS    A                                                                
ACOLIST  DS    A                   A(JOBBER COLUMN LIST)                        
AEOT     DS    A                   A(END OF TABLE)                              
AEND     DS    A                   A(END OF LINE)                               
ASTART1  DS    A                   A(START OF FIRST LINE)                       
ASTART2  DS    A                   A(START OF SECOND LINE)                      
ALINE1   DS    A                   A(NEXT ENTRY FIRST LINE)                     
ALINE2   DS    A                   A(NEXT ENTRY SECOND LINE)                    
ACLINE   DS    A                   A(CURRENT LINE HEADER)                       
EPAG#LO  DS    XL2                                                              
EPAG#HI  DS    XL2                                                              
EHORSCR  DS    XL1                                                              
EVERSCR  DS    XL1                                                              
RECNUM   DS    XL2                 RECORD NUMBER                                
COLNUM   DS    XL1                 COLUMN NUMBER                                
EINDS    DS    XL1                 INDICATOR BYTE                               
EIAGYCUR EQU   X'80'               BILLING IN AGENCY CURRENCY                   
EIWHOLE  EQU   X'40'               WHOLE UNITS                                  
EITOTS   EQU   X'20'               TOTALS DISPLAYED                             
EIACCS   EQU   X'10'               ACCUMULATORS INITIALISED                     
EIFST    EQU   X'08'               FIRST FOR SCREEN                             
EILAST   EQU   X'04'               LAST PASS                                    
EIFWD    EQU   X'02'               FORWARD                                      
EIBACK   EQU   X'01'               BACKWARD                                     
AGYCOMM  DS    PL4                 AGENCY COMMISSION RATE                       
MYPACK   DS    PL16                                                             
SWC      DS    XL2                 SAVED WORKCODE                               
SPACERS  DS    XL1                 SPACES REMAINING ON LINE                     
COLPAGE  DS    XL1                 NUMBER OF COLUMNS ON PAGE                    
LOC1     DS    XL40                COLUMN LOCATOR (BEFORE TYPE-SETTING)         
LOC2     DS    XL40                COLUMN LOCATOR (AFTER TYPE-SETTING)          
SIOKEY   DS    XL(L'IOKEY)                                                      
PRATBLK  DS    (PR$LNQ)C                                                        
*                                                                               
COLIST   DS    XL200                                                            
*                                                                               
ACCBLOCK DS    XL(WBTSARLN)        ACCUMULATORS                                 
*                                                                               
WBWORK   DS    XL(WBTSARLN)        W/C BUFFER TSAR RECORD                       
*                                                                               
EWORKL   EQU   *-EWORKD                                                         
         EJECT                                                                  
**********************************************************************          
* LOCAL DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
WBWORKD  DSECT                     >> UNIVERSAL W/C BUFFER <<                   
WBTSAREC DS    0X                  TSAR RECORD                                  
WBTSAKEY DS    0X                  TSAR KEY                                     
WBWC     DS    XL2                 WORKCODE                                     
WBTSAKLN EQU   *-WBTSAKEY          LENGTH OF TSAR KEY                           
WBTSADAT DS    0X                  TSAR COLUMNS                                 
WBCESNET DS    PL8                 CURRENT ESTIMATE NET                         
WBCHARGE DS    PL8                 CHARGES                                      
WBORDERS DS    PL8                 ORDERS                                       
WBBILNET DS    PL8                 NET BILLING (PRIOR BILLS)                    
WBALLNET DS    PL8                 ALLOCATION NET                               
WBALLCOM DS    PL8                 ALLOCATION COMMISSION                        
WBBILGRS DS    PL8                 GROSS BILLING (TOTAL BILLING)                
WBUNBNET DS    PL8                 UNBILLED                                     
WBCESGRS DS    PL8                 CURRENT ESTIMATE GROSS                       
WBOESNET DS    PL8                 ORGINAL ESTIMATE NET                         
WBOESGRS DS    PL8                 ORIGINAL ESTIMATE GROSS                      
WBUNBGRS DS    PL8                 UNBILLED GROSS                               
WBBILCOM DS    PL8                 BILLED COMMISSION                            
WBUNBCOM DS    PL8                 UNBILLED COMMISSION                          
WBINVAMT DS    PL8                 INCOME VARIANCE                              
WBINVPCT DS    PL8                 % INCOME VARIANCE                            
WBUPDWOF DS    PL8                 UPDATED WRITE-OFFS                           
WBTSADX  EQU   *                   END OF TSAR DATA AREA                        
WBELM    EQU   *-WBUPDWOF          LENGTH OF TSAR DATA ELEMENT                  
WBTSADLN EQU   *-WBTSADAT          LENGTH OF TSAR DATA AREA                     
WBTSARLN EQU   *-WBTSAREC          LENGTH OF TSAR RECORD                        
*                                                                               
COLRTND  DSECT                     >> COLUMN ROUTINES <<                        
CRNUM    DS    XL1                 ROUTINE NUMBER                               
CRDISP   DS    XL2                 DISPLACEMENT TO BUFFER DATA                  
COLRTNQ  EQU   *-COLRTND                                                        
*                                                                               
PRIMED   DSECT                     >> PRIMARY COLUMN VALUES <<                  
         DS    XL2                                                              
PRITYPE  DS    XL1                 COLUMN TYPE                                  
PRIWC    DS    XL2                 WORKCODE                                     
         DS    XL7                                                              
PRIOE    DS    PL6                 ORIGINAL ESTIMATE                            
PRICE    DS    PL6                 CURRENT ESTIMATE                             
         SPACE 1                                                                
SECOND   DSECT                     >> SECONDARY COLUMN VALUES <<                
         DS    XL2                                                              
SECTYPE  DS    XL1                 COLUMN TYPE                                  
SECWC    DS    XL2                 WORKCODE                                     
         DS    XL7                                                              
SECOE    DS    PL6                 ORIGINAL ESTIMATE                            
SECCE    DS    PL6                 CURRENT ESTIMATE                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* INCLUDED DSECTS                                                    *          
**********************************************************************          
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLS                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
CLMTABD  DSECT                                                                  
         ORG   CLMCODE                                                          
         DS    XL1                 N/D                                          
CLMCHAR  DS    CL1                 1 CHARACTER COLUMN CODE                      
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBFAD                                                       
         ORG   OSVALS2                                                          
ESDIS    DS    0C                  >> DISPLAY COLUMNS <<                        
ESCOLS   DS    XL(2*ESCOL2)        COLUMN LIST (RELATIVE NO./COL CODE)          
ESDISQ   EQU   *-ESDIS                                                          
ESCSUFF  EQU   X'40'               EXTRA COLUMNS ARE SUFFIXED                   
ESCPREF  EQU   X'80'               EXTRA COLUMNS ARE PREFIXED                   
*                                                                               
ESOPS    DS    0C                  >> OPTIONS <<                                
ESWHOLE  DS    CL1                 WHOLE UNITS - Y/N                            
ESOPL    EQU   *-ESOPS                                                          
         ORG   OSVALS2+L'OSVALS2                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050ACCLB05B  12/22/99'                                      
         END                                                                    
