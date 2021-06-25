*          DATA SET ACREPNB03B AT LEVEL 022 AS OF 10/09/14                      
*PHASE ACNB03BC                                                                 
ACNB03   TITLE 'Bill formating'                                                 
         PRINT NOGEN                                                            
         REQUS                                                                  
ACNB03   CSECT ,                                                                
         J     *+12                                                             
         DC    CL8'**NB03**'                                                    
         USING NBILD,R8                                                         
         USING ACWORKD,RC          RC=A(ACWORKD)                                
         SRL   RF,32-8                                                          
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         LARL  R0,NB03ACT                                                       
         AR    RF,R0                                                            
         BR    RF                                                               
                                                                                
NB03ACT  J     BFMT                RECORDS TO SORT                              
         J     BPRT                RECORDS FROM SORT                            
         J     FMTTX               SET THE TEXT IN PARAGRAPHS                   
         J     GENB                GENERATE THE BILL FORMAT RECORDS             
         EJECT                                                                  
***********************************************************************         
* READ FORMAT RECORDS - CONTROL BUILDING OF BILL RECORDS              *         
***********************************************************************         
                                                                                
BFMT     NTR1  LABEL=*,WORK=(R7,FWSLNQ)                                         
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         USING FWSD,R7                                                          
         LA    R0,FWSD                                                          
         LHI   R1,FWSLNQ                                                        
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LARL  RF,SOFTAB                                                        
         MVI   2(RF),0             INITIALIZE TABLE                             
         LA    RF,L'SOFTAB(RF)                                                  
         CLI   0(RF),EOT                                                        
         JNE   *-12                                                             
         USING TABD,R5                                                          
         LA    R5,SFPARM           CLEAR NUMBER IN SECTION FILTER TABLE         
         XC    TABNUM,TABNUM                                                    
         LA    R5,SSPARM           AND SECTION SORT TABLE                       
         XC    TABNUM,TABNUM                                                    
         LA    R5,PDPARM           AND PARAGRAPH DETAIL TABLE                   
         XC    TABNUM,TABNUM                                                    
         XC    FORMNO,FORMNO       CLEAR FORMAT NUMBER                          
         MVI   STATUS,0                                                         
                                                                                
         LA    R5,SFPARM           SET FILTER TABLE PARAMETERS                  
         LA    RE,SFWK             A(RECORD TO ADD)                             
         ST    RE,TABREC                                                        
         MVC   TABADR,ASFTAB       A(KEY TABLE)                                 
         LA    RE,SFRLNQ           RECORD LENGTH                                
         ST    RE,TABLEN                                                        
         LA    RE,SFKLNQ           KEY LENGTH                                   
         ST    RE,TABKYD                                                        
         LA    RE,MXSEC                                                         
         ST    RE,TABMAX                                                        
                                                                                
         LA    R5,SSPARM           SET SECTION SORT PARAMETERS                  
         LA    RE,SSWK             A(RECORD TO ADD)                             
         ST    RE,TABREC                                                        
         MVC   TABADR,ASSTAB       A(SECTION SORT KEY TABLE)                    
         LA    RE,SSRLNQ           RECORD LENGTH                                
         ST    RE,TABLEN                                                        
         LA    RE,SSKLNQ           KEY LENGTH                                   
         ST    RE,TABKYD                                                        
         LA    RE,MXSEC            MAXIMUM                                      
         ST    RE,TABMAX                                                        
                                                                                
         LA    R5,PDPARM           SET PARAGRAPH DETAIL PARAMETERS              
         LA    RE,PDWK             A(RECORD TO ADD)                             
         ST    RE,TABREC                                                        
         MVC   TABADR,APDTAB       A(SECTION SORT KEY TABLE)                    
         LA    RE,PDRLNQ           RECORD LENGTH                                
         ST    RE,TABLEN                                                        
         LA    RE,PDKLNQ           KEY LENGTH                                   
         ST    RE,TABKYD                                                        
         LHI   RE,MXPDL            MAXIMUM                                      
         ST    RE,TABMAX                                                        
         DROP  R5                                                               
                                                                                
         TM    PGMAQ,PGMALL27      TEST A27 GROUP BILL                          
         JZ    BFMT0               NO,                                          
         TM    GBRC,GBRCHDR        TEST HEADER WRITTEN                          
         JO    BFMT1               YES,                                         
         OI    GBRC,GBRCHDR        SET HEADER WRITTEN                           
                                                                                
BFMT0    GOTOR BHDR                ADD A BILL HEADER                            
                                                                                
BFMT1    XC    ADMGR,AGENB         LET FORM ROUTINE BUILD RECORDS               
         XC    AGENB,ADMGR                                                      
         XC    ADMGR,AGENB                                                      
                                                                                
         GOTOR PCFRM               READ AND SAVE FORMAT AND FILTER DATA         
                                                                                
         XC    ADMGR,AGENB         SWITCH BACK TO DATAMGR                       
         XC    AGENB,ADMGR                                                      
         XC    ADMGR,AGENB                                                      
                                                                                
         TM    PGMAQ,PGMALL27      TEST A27 GROUP BILL                          
         JZ    BFMT2               NO,                                          
         CLC   FORMTRN,PGLV        TEST TIME TO PROCESS TRANSACTIONS            
         JNE   XIT                 NO, OK TO EXIT                               
                                                                                
BFMT2    GOTOR ATSET,SSPARM        SET SEARCH PARMS FOR SORT SECTION            
                                                                                
         ICM   R0,15,NBTRN         R0=NUMBER OF TRANSACTIONS IN TABLE           
         JZ    XIT                                                              
         L     R2,ABTRN            R3=A(TRANSACTION DETAIL)                     
                                                                                
         USING BTRND,R2                                                         
BFMT3    ST    R2,CBTRN            SAVE ADDRESS OF THE CURRENT ENTRY            
         CLC   BTRNWC,WCBILLQ      SKIP WORKCODE 99                             
         JE    BFMT11                                                           
         TM    PGMSTA,PGMUNB+PGMRRB IS IT UNBILLING OR RERUN?                   
         JNZ   BFMT4                                                            
         TM    BILTTYP,PROG        TEST PROGRESSIVE                             
         JNO   BFMT4               NO,                                          
         OC    BTRNUSD,BTRNUSD     TEST ALREADY FULLY BILLED                    
         JNZ   BFMT11              YES, SKIP                                    
                                                                                
BFMT4    MVC   WRKCCUR,BTRNWC      GET WORK CODE VALUES                         
         CLC   WRKCCUR,SPACES      BLANK WORKCODE FOR SPECIAL BILLS             
         JE    BFMT5                                                            
         GOTOR AWRKM,WRKMGTQ                                                    
                                                                                
*                                  BUILD THE PARAGRAPH DETAILS                  
BFMT5    LA    R1,SFPARM           R1=A(SECTION FILTER TABLE PARAMS)            
         USING TABD,R1                                                          
         ICM   R3,15,TABNUM        R3=NUMBER OF SECTION FILTER ENTRIES          
         JZ    BFMT11                                                           
         ICM   R5,15,TABADR        R5=A(SECTION FILTER TABLE)                   
         USING SFKD,R5                                                          
         DROP  R1                                                               
                                                                                
BFMT7    GOTOR GETSECT,DMCB,BTRND,SFKD   GET SECTION # FOR THIS TRAN.           
         JNE   BFMT9                                                            
         LA    R4,BTRNBK                                                        
         USING JOBD,R4                                                          
         AP    SFNET,JOBNET        ADD TO PARAGRAPH ACCUMS                      
         AP    SFCOM,JOBCOM                                                     
         AP    SFCSD,JOBCD                                                      
         GOTOR BLDPAR,DMCB,SFKD    BUILD THE PARAGRAPH RECORD                   
                                                                                
BFMT9    LA    R5,SFRLNQ(R5)       GET NEXT SECTION FILTER                      
         JCT   R3,BFMT7                                                         
                                                                                
BFMT11   LA    R2,BTRNLQ(R2)       NEXT TRANSACTION                             
         JCT   R0,BFMT3                                                         
                                                                                
*                                  BUILD FILE RECORDS                           
         XC    PDOLD(PDKLNQ),PDOLD CLEAR OLD DETAIL RECORD                      
         LA    R1,PDPARM                                                        
         USING TABD,R1                                                          
         XC    TABLST,TABLST       CLEAR ADDRESS OF LAST RECORD                 
                                                                                
BFMT13   GOTOR ATNXT,PDPARM        GET FIRST/NEXT PARAGRAPH DETAIL              
         JNE   BFMT19                                                           
         L     R5,TABREC                                                        
         USING PDD,R5                                                           
         CLC   PDKLVL,PDOLD+(PDKLVL-PDD) TEST CHANGE OF LEVEL                   
         JE    BFMT15                                                           
         XC    SECSEQ,SECSEQ       CLEAR SECTION SEQUENCE                       
         DROP  R1                                                               
                                                                                
BFMT15   CLC   PDKSEC,PDOLD+(PDKSEC-PDD) TEST CHANGE OF SECTION                 
         JE    BFMT17                                                           
         GOTOR SECHDR              ADD SECTION HEADER                           
         XC    PARSEQ,PARSEQ                                                    
                                                                                
BFMT17   GOTOR SECPAR              ADD SECTION PARAGRAPH RECORD                 
         LA    R0,PDOLD                                                         
         LA    RE,PDWK                                                          
         LA    R1,PDRLNQ                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE PDWK IN PDOLD                           
         J     BFMT13              GET NEXT PARAGRAPH DETAIL                    
                                                                                
BFMT19   L     R2,AELMNT           ADD TOTALS TO BILL HEADER                    
         USING SCIELD,R2                                                        
         LA    R6,AMTS                                                          
         USING AMTD,R6                                                          
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN2Q                                                    
         MVI   SCITYPE,SCITCBAP                                                 
         ZAP   SCIAMNT,DUENET                                                   
         ZAP   SCIADMN,DUECOM                                                   
         GOTOR ADDEL,DMCB,ABLH,AELMNT                                           
                                                                                
         TM    PGMAQ,PGMALL27      TEST A27 GROUP BILL                          
         JNZ   BFMT21              YES,                                         
         GOTOR SORT,DMCB,('BILKRBHD',ABLH)         BILL HEADER TO SORT          
         GOTOR SORT,DMCB,('BILKRCLI',ADHEIRA)      CLIENT                       
         GOTOR SORT,DMCB,('BILKRPRD',ADHEIRB)      PRODUCT                      
         GOTOR SORT,DMCB,('BILKRJOB',ADACC)        JOB                          
         J     XIT                                                              
                                                                                
BFMT21   J     XIT                                                              
         DROP  R2,R4,R5,R6                                                      
         EJECT                                                                  
***********************************************************************         
* Add a new bill header record                                        *         
***********************************************************************         
                                                                                
BHDR     NTR1  LABEL=*                                                          
         LA    R2,DKEY                                                          
         USING BEDRECD,R2                                                       
         XC    DKEY,DKEY           Get next sequence number                     
         MVI   BEDKTYP,BEDKTYPQ                                                 
         MVC   BEDKCPY,RCCOMPFL    COMPANY CODE                                 
         MVI   BEDKSUB,BEDKSUBQ    SUB TYPE                                     
         MVC   BEDKJOB,CPJ         CLIENT PRODUCT JOB                           
         GOTOR ADMGR,ACCHIQ                                                     
         CLC   BEDKEY(BEDKJSEQ-BEDKEY),DIR                                      
         JNE   *+10                                                             
         MVC   BEDKJSEQ,DIR+(BEDKJSEQ-BEDRECD)                                  
         LLH   RE,BEDKJSEQ                                                      
         BCTR  RE,0                                                             
         STCM  RE,3,BEDKJSEQ                                                    
                                                                                
         L     R2,ABLH             Set key in record                            
         XC    BEDKEY(BEDRFST+2-BEDKEY),BEDKEY                                  
         MVC   BEDKEY,DKEY                                                      
                                                                                
         USING BLHELD,R3                                                        
         L     R3,AELMNT           Add bill header element                      
         XC    BLHEL(BLHLN2Q),BLHEL                                             
         MVI   BLHEL,BLHELQ                                                     
         MVI   BLHLN,BLHLN2Q                                                    
         MVC   BLHJOB,CPJ          Client/product/job code                      
         MVC   BLHBLNO,BILNUM      Bill number                                  
         MVC   BLHUSER,ORIGINUM    User                                         
         MVC   BLHCRED,TODAY2      Compressed date                              
         MVC   BLHFORM,FORMNO      Format code                                  
         MVC   BLHLANG,RCLANG      Language                                     
         MVC   BLHCUR,RCCURR       Currency                                     
         GOTOR GETBF                                                            
         USING BFTABD,RF                                                        
         MVC   BLHRINDS,BFTRINDS   ACREPORT indicators                          
         MVC   BLHFLAG1,BFTFLAG1   Format flags                                 
         DROP  RF                                                               
         GOTOR DATCON,DMCB,(2,TODAY2),(0,WORK)                                  
         LA    RF,7                Default is 7 retention days                  
         GOTOR ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         GOTOR DATCON,DMCB,(0,WORK+6),(2,BLHEXPD)                               
         MVC   BLHTRND,TODAY2      Default bill date is today                   
         CLC   QSTART,SPACES                                                    
         JE    BHDR3                                                            
         GOTOR DATCON,DMCB,(0,QSTART),(2,BLHTRND)                               
                                                                                
BHDR3    GOTOR DATCON,DMCB,(2,BLHTRND),(0,WORK)                                 
         MVC   BLHDUED,DUEDT2      Save compressed due date                     
         GOTOR ADDEL,DMCB,ABLH,AELMNT   Add bill header element                 
         LA    R4,BEDRFST          Set address of element                       
         ST    R4,ABLHEL                                                        
         J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* READ PC BILLING FORMAT RECORDS                                     *          
**********************************************************************          
                                                                                
PCFRM    NTR1  LABEL=*                                                          
         L     R0,ABFM             CLEAR BFM RECORD AREA                        
         LHI   R1,BFMRLNQ                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         SAM31 ,                   SET 31 BIT MODE                              
         L     R0,ABFMB            CLEAR BFM BUFFER AREA                        
         L     R1,=AL4(BFMBLNQ)                                                 
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SAM24 ,                   RETURN TO 24 BIT MODE                        
                                                                                
         LA    R2,DKEY             BUILD KEY FOR FORMAT CONTROL RECORD          
         USING BFMRECD,R2                                                       
         XC    BFMKEY,BFMKEY                                                    
         MVI   BFMKTYP,BFMKTYPQ    RECORD TYPE                                  
         MVC   BFMKCPY,RCCOMPFL    COMPANY                                      
         MVI   BFMKSUB,BFMKSUBQ    SUB-TYPE                                     
         MVC   BFMKLANG,RCLANG     LANGUAGE                                     
         MVC   BFMKFMT,FORMNO      FORMAT NUMBER                                
                                                                                
         GOTOR ADMGR,ACCRDQ        READ BILL FORMAT RECORD                      
         MVC   ANXTBFM,ABFMB       SET A(NEXT AREA FOR BFM RECORD)              
                                                                                
PCFRM3   LA    R2,DIR                                                           
         CLC   0(BFMKLVL-BFMRECD,R2),DKEY                                       
         JNE   XIT                                                              
         L     R2,ABFMR                                                         
         ST    R2,AIO              SET IO AREA                                  
         GOTOR ADMGR,ACCGETQ                                                    
         TM    UPSI,UPBFM          TRACE BFM RECORDS?                           
         JNO   PCFRM4                                                           
         GOTOR ATRCE,TRCBFMQ                                                    
                                                                                
PCFRM4   OC    BFMKWHER,BFMKWHER   TEST FORMAT DETAIL                           
         JNZ   PCFRM7                                                           
         GOTOR SORT,DMCB,('BILKRBFM',ABFMR)   FORMAT RECORD TO SORT             
         J     PCFRM11                                                          
                                                                                
PCFRM7   LARL  R3,PCFRTAB          FIND ROUTINE TO PROCESS RECORD               
         USING PCFRTABD,R3                                                      
PCFRM8   CLC   BFMKWHER,PCFRTWHR   TEST WHERE                                   
         JNE   PCFRM9                                                           
         OC    PCFRTWIN,PCFRTWIN   TEST ANY WHERE WITHIN                        
         JZ    *+14                                                             
         CLC   BFMKSWHR,PCFRTWIN   TEST WHERE WITHIN                            
         JNE   PCFRM9                                                           
         LLH   RF,PCFRTROU         GET ROUTINE DISPLACEMENT                     
         LARL  RE,BFMT                                                          
         AR    RF,RE                                                            
         SR    R1,R1               R1=0 INDICATES NOT A HOOK ROUTINE            
         GOTOR (RF)                PROCESS RECORD                               
         J     PCFRM11                                                          
                                                                                
PCFRM9   LA    R3,PCFRTABL(R3)     BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R3),EOT           TEST END OF TABLE                            
         JNE   PCFRM8              NO - GO PROCESS                              
                                                                                
PCFRM11  GOTOR ADMGR,ACCSEQQ       READ FOR NEXT IN SEQUENCE                    
         J     PCFRM3                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT PAGE HEADER/FOOTER                                           *         
***********************************************************************         
                                                                                
JCM      DS    0H                                                               
PGEHDR   DS    0H                                                               
PGEFOT   NTR1  LABEL=*                                                          
         L     R2,AIO                                                           
         USING BFMRECD,R2                                                       
         L     R3,ABEDR                                                         
         USING BEDRECD,R3          BUILD KEY FOR BEDREC                         
         XC    BEDKEY(BEDRFST-BEDRECD+2),BEDKEY                                 
         L     RF,ABLH                                                          
         MVC   BEDKEY,0(RF)        FROM BILL HEADER                             
         MVC   BEDKLVL,BFMKLVL     LEVEL                                        
         MVC   BEDKWHER,BFMKWHER   WHERE                                        
                                                                                
         GOTOR UPPAN,DMCB,BFMRECD,BEDRECD                                       
         GOTOR FMTTX,DMCB,('FTACTSIZ',BEDRECD)                                  
                                                                                
         LARL  R1,PGEHTAB                                                       
PGEHDR3  CLC   BEDKWHER,0(R1)      FIND RECORD TYPE FOR SORT                    
         JE    PGEHDR5                                                          
         LA    R1,L'PGEHTAB(R1)                                                 
         CLI   0(R1),EOT                                                        
         JNE   PGEHDR3                                                          
         J     XIT                                                              
                                                                                
PGEHDR5  LLC   R0,2(R1)                                                         
         GOTOR SORT,DMCB,((R0),ABEDR)                                           
         J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT BODY OF BILL - SET FILTER AND SECTION DATA                   *         
***********************************************************************         
                                                                                
BDYBIL   NTR1  LABEL=*                                                          
         L     R0,ABFM             CLEAR BFM RECORD AREA                        
         LHI   R1,BFMRLNQ                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R2,AIO                                                           
         USING BFMRECD,R2                                                       
         L     R3,ABFM             CONCATENATE FORMAT RECORD(S)                 
BIG      USING BFMRECD,R3                                                       
                                                                                
BDYBIL3  MVC   BIG.BFMKEY,BFMKEY   KEY                                          
         MVC   BIG.BFMRSTA,BFMRSTA STATUS                                       
         SR    RF,RF                                                            
         ICM   RF,3,BIG.BFMRLEN    LENGTH - SO FAR                              
         JNZ   *+8                 TEST FIRST TIME                              
         LA    RF,BFMRFST-BFMRECD+1 SET FIRST LENGTH                            
         BCTR  RF,0                                                             
         L     R0,ABFM                                                          
         AR    R0,RF               R0=A(NEXT AVAILABLE AREA)                    
         LLH   R1,BFMRLEN          R1=LENGTH OF NEW DATA                        
         SHI   R1,BFMRFST-BFMRECD                                               
         AR    RF,R1                                                            
         STCM  RF,3,BIG.BFMRLEN    SAVE NEW LENGTH                              
         LR    RF,R1               RF=LENGTH OF NEW DATA                        
         LA    RE,BFMRFST                                                       
         MVCL  R0,RE               SAVE NEW RECORD DATA                         
                                                                                
         TM    BFMRSTAT,BFMSCONT   RECORD CONTINUED?                            
         JNO   BDYBIL5                                                          
                                                                                
         GOTOR ADMGR,ACCSEQQ       GET CONTINUATION                             
         GOTOR ADMGR,ACCGETQ                                                    
                                                                                
         CLC   BIG.BFMKEY(BFMKSEQ-BFMKEY),BFMKEY                                
         JE    BDYBIL3                                                          
         DC    H'0'                EXPECTED CONTINUATION                        
         DROP  BIG                                                              
                                                                                
BDYBIL5  L     R2,ABFM                                                          
         USING BFMRECD,R2                                                       
         OC    BFMKSWHR,BFMKSWHR   TEST SECTION MASTER RECORD                   
         JNZ   BDYBIL7             NO,                                          
         GOTOR SFLT                YES, SET SECTION FILTERS                     
         J     BDYBIL9                                                          
                                                                                
BDYBIL7  GOTOR SSRT                SET SECTION SORT DATA                        
                                                                                
BDYBIL9  SAM31 ,                                                                
         L     R0,ANXTBFM          MOVE RECORD TO BUFFER                        
         LLH   R1,BFMRLEN                                                       
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SAM24 ,                                                                
                                                                                
         L     R0,ANXTBFM          SET ADDRESS FOR NEXT                         
         LLH   R1,BFMRLEN                                                       
         AR    R0,R1                                                            
         AHI   R0,4                                                             
         ST    R0,ANXTBFM                                                       
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET SECTION FILTER DATA                                             *         
***********************************************************************         
                                                                                
SFLT     NTR1  LABEL=*                                                          
         LA    R0,SFWK             CLEAR SECTION FILTER AREA                    
         LHI   R1,SFRLNQ                                                        
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R2,ABFM                                                          
         USING BFMRECD,R2                                                       
                                                                                
         LA    R5,SFWK             SET SECTION FILTER KEY                       
         USING SFKD,R5                                                          
                                                                                
         MVC   SFKLVL,BFMKLVL      LEVEL                                        
         MVC   SFKSEC,BFMKSECT     SECTION                                      
                                                                                
         ZAP   SFNET,PZERO         INITIALIZE ACCUMS                            
         ZAP   SFCOM,PZERO                                                      
         ZAP   SFCSD,PZERO                                                      
                                                                                
         MVI   ELCODE,BSDELQ       GET SECTION DEFINITION ELEMENT               
         GOTOR GETELN                                                           
         JE    *+6                                                              
         DC    H'0'                MISSING FILTER ELEMENTS                      
                                                                                
         USING BSDELD,R2                                                        
         LA    R4,SFEL             SECTION FILTER ELEMENT DATA                  
         USING SFED,R4                                                          
         CLI   BSDSTYP,BSDSALL     TEST CATCH ALL                               
         JNE   SFLT3                                                            
         MVI   SFKALL,SFKALLQ                                                   
         OI    STATUS,CATCHALL     SET BILL HAS CATCH ALL PARAGRAPH             
         J     SFLT9                                                            
                                                                                
SFLT3    MVC   SFETYP,BSDSTYP      SET TYPE                                     
         MVI   SFELN,SFELNQ        LENGTH                                       
         MVI   SFENUM,0                                                         
         LA    R3,SFEDATA                                                       
         LLC   R6,BSDSTYP          FILTER TYPE                                  
         BCTR  R6,0                                                             
         MHI   R6,FLTTABL                                                       
         LARL  RF,FLTTAB                                                        
         AR    R6,RF               GET FILTER ENTRY                             
         USING FLTTABD,R6                                                       
         LLC   R0,FLTLEN           R0=FILTER LENGTH                             
                                                                                
SFLT5    LLC   R1,BSDLN            GET LENGTH OF DATA                           
         SHI   R1,BSDLN1Q+1                                                     
         JNM   *+6                                                              
         DC    H'0'                                                             
         EXRL  R1,SFLTMVC                                                       
         LLC   RE,SFELN                                                         
         LA    RE,1(R1,RE)                                                      
         STC   RE,SFELN            ADJUST ELEMENT LENGTH FOR DATA               
         LLC   R1,SFENUM                                                        
         AHI   R1,1                                                             
         STC   R1,SFENUM           COUNT NUMBER OF ENTRIES                      
         AR    R3,R0               BUMP R3 TO NEXT DATA AREA                    
         MVI   0(R3),0                                                          
         LA    RE,SFWKX                                                         
         CR    R3,RE                                                            
         JNH   *+6                                                              
         DC    H'0'                RECORD IS TOO BIG                            
                                                                                
         GOTOR NEXTEL                                                           
         JNE   SFLT9                                                            
         CLC   BSDSTYP,SFETYP      TEST SAME TYPE                               
         JE    SFLT5                                                            
         LLC   R1,SFELN            BUMP R4 FOR NEXT ELEMENT                     
         AR    R4,R1                                                            
         J     SFLT3                                                            
                                                                                
SFLT9    GOTOR ATBADD,SFPARM       ADD TO TABLE                                 
         J     XIT                                                              
                                                                                
SFLTMVC  MVC   0(0,R3),BSDDATA                                                  
         DROP  R2,R4,R5,R6                                                      
         EJECT                                                                  
*********************************************************************           
* SET SECTION SORT AND TOTAL DATA                                   *           
*********************************************************************           
                                                                                
SSRT     NTR1  LABEL=*                                                          
         LA    R0,SSWK             CLEAR SECTION SORT AREA                      
         LHI   R1,SSRLNQ                                                        
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    SRTSSEL,SRTSSEL     CLEAR SORT DATA ELEMENT                      
         XC    TOTSSEL,TOTSSEL     AND TOTAL DATA  ELEMENT                      
                                                                                
         L     R2,ABFM                                                          
         USING BFMRECD,R2                                                       
         LA    R5,SSWK             SET SECTION SORT KEY                         
         USING SSKD,R5                                                          
         MVC   SSKLVL,BFMKLVL      LEVEL                                        
         MVC   SSKSEC,BFMKSECT     SECTION                                      
         MVC   SSKWHR,BFMKSWHR     WHERE                                        
         MVC   SSDA,ANXTBFM        SAVE ADDRESS OF BFM RECORD                   
                                                                                
         MVI   ELCODE,BFPELQ       GET FORMAT PARAGRAPH ELEMENT                 
         GOTOR GETELN                                                           
         JNE   XIT                                                              
                                                                                
         USING BFPELD,R2                                                        
         CLI   BFPHGTMN,0          TEST MINIMUM HEIGHT                          
         JE    XIT                                                              
                                                                                
         L     R2,ABFM                                                          
         MVI   ELCODE,BFSELQ       GET FORMAT SORT ELEMENT                      
         GOTOR GETELN                                                           
         JNE   SSRT5                                                            
         USING BFSELD,R2                                                        
         MVC   SSKSUB,BFSLVL       SUB-SECTION                                  
         LA    R6,SRTSSEL                                                       
         USING SSELD,R6                                                         
         MVI   SSEEL,SSESRTQ       SORT LIST                                    
                                                                                
         LLC   R0,BFSLN            GET NUMBER OF BFSBODYS                       
         SHI   R0,(BFSBODYS-BFSELD)                                             
         JZ    SSRT5                                                            
         LA    R4,BFSBODYS                                                      
                                                                                
SSRT3    GOTOR ADDSRT,DMCB,(R4),SRTSSEL                                         
         LA    R4,L'BFMBODY(R4)                                                 
         JCT   R0,SSRT3                                                         
                                                                                
SSRT5    L     R2,ABFM                                                          
         USING BFMRECD,R2                                                       
         LA    R6,TOTSSEL                                                       
         MVI   SSEEL,SSETOTQ       TOTAL LIST                                   
         CLC   BFMKSWHR,=AL2(BFMKSPRQ)  IF NOT A HEADING, ADD TOTALS            
         JL    SSRT7                                                            
         MVI   SSENUM,3                                                         
         MVI   SSELIST+0,PCQNET    NET                                          
         MVI   SSELIST+1,PCQCOM    COMMISSION                                   
         MVI   SSELIST+2,PCQGRS    GROSS                                        
                                                                                
SSRT7    MVI   ELCODE,BFMELQ       GET FORMAT PARAGRAPH ELEMENT                 
         GOTOR GETELN                                                           
         J     SSRT92                                                           
                                                                                
SSRT9    GOTOR NEXTEL                                                           
SSRT92   JNE   SSRT11                                                           
                                                                                
         USING BFMELD,R2                                                        
         CLI   BFMTYPE,BFMTDATA                                                 
         JNE   SSRT9                                                            
         XR    R4,R4                                                            
         ICM   R4,1,BFMBODY        GET FIELD TABLE ENTRY                        
         JZ    SSRT9                                                            
         MHI   R4,FLDTABL                                                       
         LARL  RF,FLDTAB                                                        
         AR    R4,RF                                                            
         USING FLDTABD,R4                                                       
         TM    FLDINDS,FLDITOT     TEST TOTALLED AMOUNT                         
         JZ    SSRT9                                                            
         GOTOR ADDSRT,DMCB,BFMBODY,TOTSSEL                                      
         J     SSRT9                                                            
         DROP  R4                                                               
                                                                                
SSRT11   LA    R3,SSEL                                                          
         LA    R6,SRTSSEL          ADD SORT ELEMENT TO RECORD                   
         GOTOR SSRT99                                                           
         LA    R6,TOTSSEL          ADD TOTAL ELEMENT TO RECORD                  
         GOTOR SSRT99                                                           
         GOTOR ATBADD,SSPARM       ADD SORT RECORD TO TABLE                     
         J     XIT                                                              
                                                                                
SSRT99   SR    RF,RF                                                            
         ICM   RF,1,SSENUM                                                      
         BZR   RE                                                               
         AHI   RF,SSELNQ           SET ELEMENT LENGTH                           
         STC   RF,SSELN                                                         
         BCTR  RF,0                                                             
         EXRL  RF,SSRTMVC          MOVE ELEMENT TO RECORD                       
         LA    R3,1(RF,R3)                                                      
         BR    RE                                                               
                                                                                
SSRTMVC  MVC   0(0,R3),SSEEL                                                    
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ADD BODY FIELD TO SSEL                                              *         
* NTRY: P1=A(BODY FIELD)                                              *         
*       P2=A(SORT ELEMENT)                                            *         
***********************************************************************         
                                                                                
ADDSRT   NTR1  LABEL=*                                                          
         LM    R2,R3,0(R1)                                                      
         USING SSELD,R3                                                         
         LA    RF,SSELIST          RF TO LIST                                   
         XR    R0,R0                                                            
         ICM   R0,1,SSENUM         NUMBER IN LIST SO FAR                        
         JZ    ADDSRT5                                                          
                                                                                
ADDSRT3  CLC   0(L'BFMBODY,RF),0(R2) TEST ALREADY IN LIST                       
         JE    XIT                                                              
         LA    RF,L'BFMBODY(RF)                                                 
         JCT   R0,ADDSRT3                                                       
                                                                                
ADDSRT5  MVC   0(L'BFMBODY,RF),0(R2) ADD NEW ITEM TO LIST                       
         LLC   RE,SSENUM                                                        
         AHI   RE,1                                                             
         STC   RE,SSENUM           UPDATE THE COUNT                             
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET SECTION NUMBER (IF ANY) FOR CURRENT TRANSACTION                 *         
*  NTRY: P1=A(TRANSACTION    TABLE ENTRY)                             *         
*        P2=A(SECTION FILTER TABLE)                                   *         
***********************************************************************         
                                                                                
GETSECT  NTR1  LABEL=*                                                          
         LM    R3,R4,0(R1)                                                      
         USING BTRND,R3                                                         
         USING SFKD,R4                                                          
         CLI   SFKALL,SFKALLQ      TEST CATCH ALL SECTION                       
         JE    XITY                                                             
         CLI   SFEL,0              TEST SECTION HAS NO FILTERS                  
         JE    XITN                                                             
                                                                                
         LA    R4,SFEL                                                          
         USING SFED,R4                                                          
GETSECT3 XR    R6,R6                                                            
         ICM   R6,1,SFETYP         TEST ANY FILTER TYPE                         
         JZ    XITY                NO,                                          
         BCTR  R6,0                                                             
         MHI   R6,FLTTABL                                                       
         LARL  RF,FLTTAB                                                        
         AR    R6,RF               GET FILTER ENTRY                             
         USING FLTTABD,R6                                                       
         LLC   R0,SFENUM           NUMBER OF ENTRIES FOR THIS TYPE              
         LA    R2,SFEDATA          START OF FILTER DATA                         
         LLC   RF,FLTLEN           LENGTH OF FILTER FIELD                       
         BCTR  RF,0                                                             
                                                                                
GETSECT5 LLH   R5,FLTDSP           R5=DISP. TO DATA                             
         LA    RE,BTRND                                                         
         CLI   FLTBAS,1            TEST BASE IS TRANSACTION                     
         JE    *+8                 YES,                                         
         LA    RE,NBILD            NO, USE GLOBAL                               
         AR    R5,RE                                                            
         CLC   0(0,R5),0(R2)       DATA VS. FILTER                              
         JE    GETSECT7                                                         
         LA    R2,1(RF,R2)                                                      
         JCT   R0,GETSECT5         TRY NEXT FILTER                              
         J     XITN                                                             
                                                                                
GETSECT7 LLC   RF,SFELN                                                         
         AR    R4,RF                                                            
         J     GETSECT3            TRY NEXT TYPE                                
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD PARAGRAPH RECORD                                              *         
*  NTRY: P1=A(SECTION FILTER ENTRY)                                   *         
***********************************************************************         
                                                                                
BLDPAR   NTR1  LABEL=*                                                          
         L     R5,0(R1)                                                         
         USING SFKD,R5             R5=A(FILTER RECORD)                          
         LA    R1,SSPARM                                                        
         USING TABD,R1                                                          
         XC    TABLST,TABLST       CLEAR ADDRESS OF LAST RECORD                 
         DROP  R1                                                               
                                                                                
BLDPAR3  GOTOR ATNXT,SSPARM        GET FIRST/NEXT SECTION SORT DATA             
         JNE   XIT                 END OF TABLE                                 
         LA    R4,SSWK                                                          
         USING SSKD,R4                                                          
                                                                                
         CLC   SSKD(L'SSKLVL+L'SSKSEC),SFKD  MATCH SECTION + LEVEL              
         JNE   BLDPAR3                                                          
                                                                                
         LA    R0,PDWK             CLEAR PARAGRAPH DETAIL WORK                  
         LA    R1,PDRLNQ                                                        
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R6,PDWK             SET PARAGRAPH KEY                            
         USING PDD,R6                                                           
         MVC   PDKLVL,SSKLVL       LEVEL                                        
         MVC   PDKSEC,SSKSEC       SECTION                                      
         MVC   PDKWHR,SSKWHR       WHERE                                        
         MVC   PDKSUB,SSKSUB       SUBSECTION                                   
         LA    R3,PDKSORT          R3=A(NEXT KEY SORT FIELD)                    
         LA    R2,PDDATA                                                        
         USING PARELD,R2                                                        
                                                                                
         LA    R4,SSEL                                                          
         USING SSELD,R4                                                         
BLDPAR5  CLI   SSEEL,0             TEST END OF SORT/TOTAL ELEMENTS              
         JE    BLDPAR13                                                         
         CLI   SSEEL,SSESRTQ       TEST SORT DATA                               
         JE    *+12                                                             
         CLI   SSEEL,SSETOTQ       TEST TOTAL DATA                              
         JNE   BLDPAR11                                                         
         LLC   R0,SSENUM           NUMBER IN LIST                               
         LA    R6,SSELIST                                                       
                                                                                
*                                  EXPAND PARAGRAPH DATA                        
BLDPAR7  LLC   RE,0(R6)            BFMFLD VALUE                                 
         MHI   RE,FLDTABL                                                       
         LARL  RF,FLDTAB                                                        
         AR    RF,RE                                                            
         USING FLDTABD,RF                                                       
         MVC   PARFLD,0(R6)        DATA CODE                                    
         MVC   PARDLEN,FLDGLEN     DATA LENGTH                                  
         MVI   PAREL,PARSRTQ       DATA TYPE - SORT                             
         TM    FLDINDS,FLDITOT                                                  
         JZ    *+8                                                              
         MVI   PAREL,PARTOTQ       DATA TYPE - TOTAL                            
         DROP  RF                                                               
                                                                                
         GOTOR GETDATA,DMCB,PARELD                                              
         LLC   RE,PARDLEN          DATA LENGTH                                  
         LA    RE,PARLNQ(RE)       + FIXED ELEMENT LENGTH                       
         STC   RE,PARLN            SET TOTAL ELEMENT LENGTH                     
                                                                                
         SR    RF,RF                                                            
         CLI   PAREL,PARSRTQ       TEST SORT DATA                               
         JNE   BLDPAR9                                                          
         ICM   RF,1,PARDLEN        DATA LENGTH                                  
         BCTR  RF,0                                                             
         EXRL  RF,BLDPMV3P         SORT DATA TO KEY                             
         LA    R3,1(RF,R3)                                                      
                                                                                
BLDPAR9  LLC   RF,PARLN            BUMP TO NEXT PAREL                           
         AR    R2,RF                                                            
         MVI   0(R2),0                                                          
         LA    R6,1(R6)                                                         
         JCT   R0,BLDPAR7                                                       
                                                                                
BLDPAR11 LLC   R0,SSELN            BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         J     BLDPAR5                                                          
                                                                                
BLDPAR13 LA    R6,PDWK                                                          
         CLC   PDKWHR,=AL2(BFMKSSTQ)                                            
         JL    *+8                                                              
         MVI   0(R3),X'FF'         FORCE TOTALS TO SORT LAST                    
*                                   ADD PARAGRAPH RECORD TO TABLE               
         GOTOR ATBADD,PDPARM         PARAGRAPH DETAIL - BINARY ADD              
         JNE   BLDPAR3               NOT FOUND(ADDED)                           
         ICM   R3,15,TABRTN-TABD(R1) R3=A(RECORD ALREADY IN TABLE)              
         LA    R3,PDDATA-PDD(R3)     R3=A(DATA AREA OF OLD RECORD)              
         LA    R2,PDWK+(PDDATA-PDD)  R2=A(DATA AREA OF NEW RECORD)              
         SR    R0,R0                                                            
                                                                                
BLDPAR15 CLI   PAREL,0                                                          
         JE    BLDPAR3                                                          
         CLI   PAREL,PARTOTQ       TEST TOTAL ELEMENT ON NEW                    
         JE    BLDPAR19                                                         
BLDPAR17 LLC   R0,PARLN                                                         
         AR    R2,R0                                                            
         J     BLDPAR15                                                         
                                                                                
BLDPAR19 LR    R4,R3               FIND MATCH ON OLD                            
OLD      USING PARELD,R4                                                        
BLDPAR21 CLI   OLD.PAREL,0                                                      
         JE    BLDPAR17                                                         
         CLC   PAREL(PARLNQ),OLD.PAREL                                          
         JE    BLDPAR23                                                         
         LLC   R0,OLD.PARLN                                                     
         AR    R4,R0                                                            
         J     BLDPAR21                                                         
                                                                                
BLDPAR23 AP    OLD.PARAMT,PARAMT   ADD NEW TO OLD                               
         J     BLDPAR17                                                         
                                                                                
BLDPMV3P MVC   0(0,R3),PARDATA                                                  
         DROP  R2,R4,R5,R6,OLD                                                  
         EJECT                                                                  
**********************************************************************          
* ADD SECTION HEADER                                                 *          
**********************************************************************          
                                                                                
SECHDR   NTR1  LABEL=*                                                          
         LA    R5,PDWK                                                          
         USING PDD,R5                                                           
         LA    R1,SFPARM           R1=A(SECTION FILTER TABLE PARAMS)            
         USING TABD,R1                                                          
         ICM   R0,15,TABNUM        R0=NUMBER OF SECTION FILTER ENTRIES          
         ICM   R4,15,TABADR        R4=A(SECTION FILTER TABLE)                   
         DROP  R1                                                               
                                                                                
         USING SFKD,R4                                                          
SECHDR3  CLC   SFKLVL,PDKLVL       TEST LEVEL                                   
         JNE   SECHDR7                                                          
         CLC   SFKSEC,PDKSEC       AND SECTION                                  
         JE    SECHDR11                                                         
                                                                                
SECHDR7  LA    R4,SFRLNQ(R4)       GET NEXT SECTION FILTER                      
         JCT   R0,SECHDR3                                                       
         J     XIT                 NO SECTION HEADER                            
                                                                                
SECHDR11 L     R2,ABFMR                                                         
         USING BEDRECD,R2                                                       
         XC    BEDRECD(BEDRFST-BEDRECD+2),BEDRECD                               
         L     RF,ABLH             KEY FROM BILL HEADER                         
         MVC   BEDKEY,0(RF)                                                     
         MVC   BEDKLVL,SFKLVL                                                   
         MVC   BEDKWHER,=AL2(BEDKWBDQ)                                          
                                                                                
         LLC   RF,SECSEQ           SET SECTION SEQ#                             
         AHI   RF,1                                                             
         STC   RF,SECSEQ                                                        
         STC   RF,BEDKSSEQ                                                      
                                                                                
         MVC   BEDRSECT,PDKSEC     SECTION                                      
         MVC   BEDRSLVL,PDKWHR     LEVEL                                        
         MVC   BEDRSSUB,PDKSUB     SUB                                          
                                                                                
         L     R3,AELMNT                                                        
         USING PGHELD,R3           ADD PARAGRAPH HEADER EL                      
         XC    PGHELD(PGHLNQ),PGHELD                                            
         MVI   PGHEL,PGHELQ                                                     
         MVI   PGHLN,PGHLNQ                                                     
         ZAP   PGHNET,SFNET                                                     
         ZAP   PGHCOM,SFCOM                                                     
         MVC   PGHHTYP,BEDRSECT                                                 
         GOTOR ADDEL,DMCB,ABFMR,AELMNT                                          
                                                                                
         GOTOR SORT,DMCB,('BILKRBED',ABFMR)                                     
         J     XIT                                                              
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* ADD SECTION PARAGRAPH                                               *         
***********************************************************************         
                                                                                
SECPAR   NTR1  LABEL=*                                                          
         LA    R2,PDWK             FOR THIS DETAIL                              
         USING PDD,R2                                                           
         LA    R3,WORK                                                          
         USING SSKD,R3             BUILD SECTION SORT KEY                       
         MVC   SSKLVL,PDKLVL       LEVEL                                        
         MVC   SSKSEC,PDKSEC       SECTION                                      
         MVC   SSKWHR,PDKWHR       WHERE                                        
         MVC   SSKSUB,PDKSUB       SUB-SECTION                                  
         LA    R1,SSPARM           SEARCH FOR SORT SECTION ENTRY                
                                                                                
         USING TABD,R1                                                          
         ST    R3,TABARG                                                        
         GOTOR ATSRCH,SSPARM                                                    
         JNE   XIT                                                              
         L     R5,TABRTN                                                        
         DROP  R1,R3                                                            
                                                                                
         USING SSKD,R5                                                          
         SAM31 ,                                                                
         L     R0,ABFM             MOVE RECORD FROM BUFFER                      
         LHI   R1,BFMRLNQ                                                       
         ICM   RE,15,SSDA          ADDRESS OF RECORD IN BUFFER                  
         LLH   RF,BFMRLEN-BFMRECD(RE)                                           
         MVCL  R0,RE                                                            
         SAM24 ,                                                                
                                                                                
         L     R3,ABEDR                                                         
         USING BEDRECD,R3          BUILD KEY FOR BEDREC                         
         XC    BEDRECD(BEDRFST-BEDRECD+2),BEDRECD                               
         L     RF,ABLH                                                          
         MVC   BEDKEY,0(RF)        FROM BILL HEADER                             
         MVC   BEDKLVL,PDKLVL      LEVEL                                        
         MVC   BEDKWHER,=AL2(BEDKWBDQ)  BODY                                    
         MVC   BEDKSSEQ,SECSEQ     SECTION SEQUENCE                             
         LLH   RF,PARSEQ           UPDATE PARAGRAPH NUMBER                      
         LA    RF,1(RF)                                                         
         STCM  RF,3,PARSEQ                                                      
         STCM  RF,3,BEDKPARA       SET PARAGRAPH NUMBER                         
                                                                                
         MVC   BEDRSECT,PDKSEC     SECTION                                      
         MVC   BEDRSLVL,PDKWHR     LEVEL                                        
         MVC   BEDRSSUB,PDKSUB     SUB                                          
                                                                                
         L     R6,AELMNT           ADD AMOUNT BUCKETS                           
         USING FWTELD,R6                                                        
         XC    FWTEL(FWTLNQ),FWTEL                                              
         MVI   FWTEL,FWTELQ                                                     
         MVI   FWTLN,FWTLNQ                                                     
                                                                                
         MVI   AMTFLG,0                                                         
         ZAP   CSD,PZERO                                                        
         LA    R4,PDDATA                                                        
                                                                                
         USING PARELD,R4                                                        
SECPAR3  CLI   PAREL,0                                                          
         JE    SECPAR7                                                          
         CLI   PAREL,PARSRTQ                                                    
         JE    *+12                                                             
         CLI   PAREL,PARTOTQ                                                    
         JNE   SECPAR5                                                          
         LLC   R5,PARFLD                                                        
         MHI   R5,FLDTABL                                                       
         LARL  RE,FLDTAB                                                        
         AR    R5,RE                                                            
                                                                                
         USING FLDTABD,R5                                                       
         TM    FLDINDS,FLDIAMT     TEST AMOUNT FIELD                            
         JZ    SECPAR5                                                          
         MVC   FWTFLD,PARFLD                                                    
         ZAP   FWTAMT,PARAMT                                                    
         GOTOR SECEL,DMCB,AELMNT                                                
         CLI   PARFLD,PCQCD        SAVE CD                                      
         JNE   *+10                                                             
         ZAP   CSD,PARAMT                                                       
         CLI   PARFLD,PCQNET       SAVE NET                                     
         JNE   *+10                                                             
         ZAP   NET,PARAMT                                                       
         CLI   PARFLD,PCQHRS       SAVE HOURS                                   
         JNE   *+14                                                             
         ZAP   HRS,PARAMT                                                       
         OI    AMTFLG,AMTHRS                                                    
         CLI   PARFLD,PCQRAT       SAVE RATE                                    
         JNE   *+14                                                             
         ZAP   RTE,PARAMT                                                       
         OI    AMTFLG,AMTRTE                                                    
                                                                                
SECPAR5  LLC   RF,PARLN                                                         
         AR    R4,RF                                                            
         J     SECPAR3                                                          
                                                                                
SECPAR7  TM    AMTFLG,AMTHRS+AMTRTE TEST HAVE JUST ONE OF HOURS/RATE            
         JNM   SECPAR11                                                         
         MVI   FWTFLD,PCQRAT       SET TO CALCULATE RATE                        
         LA    R1,HRS                                                           
         TM    AMTFLG,AMTRTE                                                    
         JZ    *+12                                                             
         LA    R1,RTE                                                           
         MVI   FWTFLD,PCQHRS       SET TO CALCULATE HOURS                       
                                                                                
         ZAP   PK16,NET            NET                                          
         SRP   PK16,4,0            X 1000                                       
         CP    0(L'HRS,R1),PZERO   TEST ABOUT TO DIVIDE BY ZERO                 
         JNE   *+14                                                             
         ZAP   PK8,PZERO           MAKE THE OTHER ZERO                          
         J     SECPAR9                                                          
         DP    PK16,0(L'HRS,R1)    DIVIDE BY HOURS OR RATE                      
         SRP   PK16(8),64-2,5                                                   
                                                                                
SECPAR9  ZAP   FWTAMT,PK8         =RATE OR HOURS                                
         GOTOR SECEL,DMCB,AELMNT                                                
                                                                                
SECPAR11 L     R5,ABFM                                                          
         USING BFMRECD,R5          BUILD KEY FOR BEDREC                         
         LA    R5,BFMRFST                                                       
         USING BFPELD,R5                                                        
                                                                                
SECPAR13 CLI   BFPEL,0                                                          
         JE    SECPARX                                                          
         CLI   BFPEL,BFPELQ        TEST FOR PARA ELEMENT                        
         JNE   SECPAR15                                                         
         CLC   PDKWHR,=AL2(BEDKSPRQ)                                            
         JNE   SECPAR17                                                         
         OC    PDNTRN,PDNTRN       TEST PARA BUILT FROM 1 ADVANCE               
         JNZ   SECPAR17                                                         
         CLC   PDNADV,=AL2(1)                                                   
         JNE   SECPAR17                                                         
         MVC   BFPADV,PD1STADV     SAVE WHICH ADVANCE                           
         J     SECPAR17                                                         
         DROP  R5                                                               
                                                                                
         USING BFMELD,R5                                                        
SECPAR15 CLI   BFMEL,BFXELQ        ADD EXTRA TEXT ELEMENT                       
         JE    SECPAR17                                                         
         CLI   BFMEL,BFMELQ        SKIP ALL OTHERS, EXCEPT FORMAT               
         JNE   SECPAR19                                                         
         CLI   BFMTYPE,BFMTDATA                                                 
         JNE   SECPAR17                                                         
         CLI   BFMBODY,0           TEST FOR PANEL FIELD                         
         JNE   SECPAR18                                                         
         OI    BEDRSTAT,BEDSPAN                                                 
         J     SECPAR17                                                         
                                                                                
SECPAR18 GOTOR TSTREQ,DMCB,BFMELD,PDWK,PDOLD                                    
         JNE   SECPAR19                                                         
         GOTOR CNVBFM,DMCB,PDWK,BFMELD                                          
         J     SECPAR19                                                         
                                                                                
SECPAR17 GOTOR SECEL,DMCB,BFMELD   COPY ELEMENT TO BEDRECD                      
                                                                                
SECPAR19 LLC   RF,BFMLN                                                         
         AR    R5,RF                                                            
         J     SECPAR13                                                         
                                                                                
SECPARX  GOTOR UPPAN,DMCB,ABEDR,ABEDR                                           
         GOTOR FMTTX,DMCB,('FTACTSIZ',ABEDR)                                    
         GOTOR SORT,DMCB,('BILKRBED',ABEDR)                                     
         J     XIT                                                              
         DROP  R2,R3,R4,R5,R6                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST IF FIELD IS REQUIRED FOR PARAGRAPH                  *         
*                                                                     *         
* NTRY1: P1=A(BFMELD)                                                 *         
*        P2=A(CURRENT DETAIL RECORD)                                  *         
*        P3=A(PREVIOUS DETAIL RECORD)                                 *         
***********************************************************************         
                                                                                
TSTREQ   NTR1  LABEL=*                                                          
         LM    R3,R5,0(R1)                                                      
         USING BFMELD,R3                                                        
NREC     USING PDD,R4                                                           
OREC     USING PDD,R5                                                           
         CLI   BFMDPER,BFMDPTRN    REQUIRED IF DISPLAY PER TRANSACTION          
         JE    XITY                YES, ALWAYS DISPLAY                          
                                                                                
*                                  TEST FOR NORMAL PARAGRPAH                    
         CLC   NREC.PDKWHR,=AL2(BEDKSPRQ)                                       
         JNE   XITY                NO - MUST BE (SUB)-TOTAL                     
*                                  TEST FOR SAME LEVEL / SECTION                
         CLC   NREC.PDKLVL,OREC.PDKLVL                                          
         JNE   XITY                                                             
         CLC   NREC.PDKSEC,OREC.PDKSEC                                          
         JNE   XITY                                                             
                                                                                
         CLI   BFMDPER,0                                                        
         JNE   *+10                                                             
         MVC   BFMDPER,BFMBODY     DEFAULT IS PER ITSELF                        
                                                                                
         LA    R6,NREC.PDDATA                                                   
N        USING PARELD,R6                                                        
         LA    R2,OREC.PDDATA                                                   
O        USING PARELD,R2                                                        
         XR    RF,RF                                                            
TREQ3    CLI   N.PAREL,0                                                        
         JE    XITY                                                             
         CLI   N.PAREL,PARTOTQ     TOTALS NEED TO BE DISPLAYED                  
         JNE   TREQ5                                                            
         CLC   N.PARFLD,BFMDPER                                                 
         JE    XITY                                                             
         J     TREQ7                                                            
                                                                                
TREQ5    CLI   N.PAREL,PARSRTQ                                                  
         JNE   TREQ7                                                            
         CLI   N.PARFLD,PCQREF                                                  
         JE    XITY                                                             
         LLC   RF,N.PARLN                                                       
         BCTR  RF,0                                                             
         EXRL  RF,TREQCLNO        REQUIRED IF CHANGE OF DATA                    
         JNE   XITY                                                             
                                                                                
         CLC   N.PARFLD,BFMDPER   IF SAME UP TO THIS DATA - NOT REQ.            
         JE    XITN                                                             
                                                                                
TREQ7    LLC   RF,N.PARLN                                                       
         AR    R6,RF                                                            
         LLC   RF,O.PARLN                                                       
         AR    R2,RF                                                            
         J     TREQ3                                                            
                                                                                
TREQCLNO CLC   N.PARELD(0),O.PARELD                                             
         DROP  R3,NREC,OREC,O,N                                                 
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT BFMELD FOR BEDRECD                               *         
*  NTRY: P1=A(PARAGRAPH DETAIL RECORD)                                *         
*        P2=A(BFMELD)                                                 *         
*  EXIT: BFMELD ADDED TO BEDRECD                                      *         
***********************************************************************         
                                                                                
CNVBFM   NTR1  LABEL=*                                                          
         LM    R2,R3,0(R1)                                                      
         USING PDD,R2                                                           
         USING BFMELD,R3                                                        
                                                                                
         LA    R4,PDDATA                                                        
         USING PARELD,R4                                                        
         XR    RF,RF                                                            
CNVBFM3  CLI   PAREL,0             FIND PARELD FOR FIELD                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PAREL,PARSRTQ       SORT                                         
         JE    *+12                                                             
         CLI   PAREL,PARTOTQ       OR TOTAL                                     
         JNE   CNVBFM5                                                          
         CLC   PARFLD,BFMBODY      MATCH FIELD (BFMFLD)                         
         JE    CNVBFM7                                                          
CNVBFM5  IC    RF,PARLN                                                         
         AR    R4,RF                                                            
         J     CNVBFM3                                                          
                                                                                
CNVBFM7  MVI   TEXT,C' '                                                        
         MVC   TEXT+1(L'TEXT-1),TEXT                                            
         LLC   R5,PARFLD           GET FIELD TABLE ENTRY                        
         MHI   R5,FLDTABL                                                       
         LARL  RE,FLDTAB                                                        
         AR    R5,RE                                                            
                                                                                
         USING FLDTABD,R5                                                       
         XR    RF,RF                                                            
         ICM   RF,1,FLDCNV                                                      
         JNZ   CNVBFM9                                                          
         LLC   RE,PARLN            NO CONVERT ROUTINE - JUST COPY               
         SHI   RE,PARLNQ                                                        
         STC   RE,TEXTLEN                                                       
         JNP   XIT                                                              
         BCTR  RE,0                                                             
         EXRL  RE,CNVBMVTP                                                      
         J     CNVBFM11                                                         
                                                                                
CNVBFM9  GOTOR CNVDATA,DMCB,PARELD,BFMELD    CONVERT DATA TO TEXT               
                                                                                
CNVBFM11 TM    FLDINDS,FLDIRMTS    TEST REMOVE TRAILING SPACES                  
         JZ    CNVBFM17                                                         
         XR    RE,RE                                                            
         ICM   RE,1,TEXTLEN                                                     
         JZ    CNVBFM17                                                         
         LA    RF,TEXT-1(RE)       FIX LENGTH FOR TRAILING SPACES               
CNVBFM13 CLI   0(RF),C' '                                                       
         JH    CNVBFM15                                                         
         BCTR  RF,0                                                             
         JCT   RE,CNVBFM13                                                      
CNVBFM15 STC   RE,TEXTLEN          SET NEW LENGTH                               
                                                                                
CNVBFM17 L     RE,AELMNT           COPY BFMEL AND ADD TEXT                      
         LLC   RF,BFMLN                                                         
         EXRL  RF,CNVBMVEB                                                      
         LR    R3,RE               BUILD NEW ELEMENT                            
         XR    R5,R5                                                            
         ICM   R5,1,TEXTLEN        LENGTH OF TEXT                               
         JZ    XIT                                                              
         STCM  R5,3,BFMTLEN        SET LENGTH IN ELEMENT                        
         CHI   R5,BFMTMAXL         TEST HIGHER THAN MAX                         
         JNH   *+8                                                              
         LA    R5,BFMTMAXL         USE MAX                                      
         EXRL  R5,CNVBMVTX                                                      
         AR    RF,R5               ADD TO ELEMENT LENGTH                        
         STC   RF,BFMLN            SET ELEMENT LENGTH                           
         GOTOR SECEL,DMCB,AELMNT                                                
                                                                                
         CLM   R5,1,TEXTLEN        TEST BFMELD NOT BIG ENOUGH FOR TEXT          
         JNL   XIT                                                              
         LLC   R5,TEXTLEN                                                       
         SHI   R5,BFMTMAXL         GET REMAINING LENGTH                         
         USING BFXELD,R3                                                        
         MVI   BFXEL,BFXELQ        BUILD EXTRA TEXT ELEMENT                     
         LA    RE,BFXLNQ(R5)                                                    
         STC   RE,BFXLN            SET ELEMENT LENGTH                           
         EXRL  R5,CNVBMVTT         MOVE EXTRA TEXT                              
         GOTOR SECEL,DMCB,AELMNT                                                
         J     XIT                                                              
                                                                                
CNVBMVTP MVC   TEXT(0),PARDATA                                                  
         USING BFMELD,R3                                                        
CNVBMVEB MVC   0(0,RE),BFMELD                                                   
CNVBMVTX MVC   BFMTEXT(0),TEXT                                                  
         USING BFXELD,R3                                                        
CNVBMVTT MVC   BFXTEXT(0),TEXT+BFMTMAXL                                         
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ELEMENT TO BEDRECD                                   *         
*  NTRY: P1=A(ELEMENT TO ADD)                                         *         
***********************************************************************         
                                                                                
SECEL    NTR1  LABEL=*                                                          
         L     R3,0(R1)                                                         
         USING BFMELD,R3                                                        
         CLI   BFMEL,BFMELQ                                                     
         JNE   SECEL1                                                           
         CLI   BFMBODY,PCQNIF    TEST NEW ITEM                                  
         JE    *+12                                                             
         CLI   BFMBODY,PCQXFR    TRANSFER DATA                                  
         JNE   SECEL0                                                           
         CLI   BFMTEXT,C' '      SKIP BLANK ELEMENT                             
         JE    XIT                                                              
         J     SECEL1                                                           
                                                                                
SECEL0   CP    CSD,PZERO           IF CD=ZERO                                   
         JNE   SECEL1                                                           
         TM    BFMINDS1,BFMICDO    REMOVE CD DEPENDENT FIELDS                   
         JO    XIT                                                              
         DROP  R3                                                               
                                                                                
SECEL1   L     R2,ABEDR                                                         
         USING BEDRECD,R2                                                       
                                                                                
SECEL3   LLH   RF,BEDRLEN          TEST RECORD WILL BE TOO BIG                  
         LLC   RE,1(R1)            + LENGTH OF NEW ELEMENT                      
         AR    RF,RE                                                            
         CHI   RF,IOLNQ                                                         
         JNL   SECEL5              RECORD WILL BE TOO BIG                       
         GOTOR ADDEL,DMCB,ABEDR,(R3)                                            
         J     XIT                                                              
                                                                                
SECEL5   OI    BEDRSTAT,BEDSCONT   SET RECORD CONTINUED                         
         GOTOR UPPAN,DMCB,ABEDR,ABEDR                                           
         GOTOR FMTTX,DMCB,('FTACTSIZ',ABEDR)                                    
         GOTOR SORT,DMCB,('BILKRBED',ABEDR)                                     
         LLC   RE,BEDKSEQ          INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)                                                         
         STC   RE,BEDKSEQ                                                       
         NI    BEDRSTAT,ALL-(BEDSCONT) TURNOFF CONTINUED FOR NEXT               
         XC    BEDRFST(3),BEDRFST                                               
         LA    RF,BEDRFST+1-BEDRECD                                             
         STCM  RF,3,BEDRLEN                                                     
         J     SECEL3                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS MAIN BILL TOTALS                                            *         
***********************************************************************         
                                                                                
MBT      NTR1  LABEL=*                                                          
         LTR   R1,R1                                                            
         JNZ   MBTHK               MAIN BILL HOOK ROUTINE                       
                                                                                
         MVI   PSTIND,0                                                         
         L     R3,AIO                                                           
         USING BFMRECD,R3                                                       
         LA    R3,BFMRFST                                                       
         USING BFMELD,R3                                                        
         SR    R0,R0               NO, DROP CD ELEMENTS                         
                                                                                
MBT3     CLI   BFMEL,0                                                          
         JE    MBT9                                                             
         CLI   BFMEL,BFMELQ                                                     
         JNE   MBT7                                                             
         TM    BFMINDS1,BFMICDO    REMOVE CD DEPENDENT FIELDS                   
         JNO   MBT5                                                             
         CP    CSD$,PZERO          ANY CD?                                      
         JNE   MBT7                YES, KEEP ELEMENT                            
         MVI   BFMEL,X'FF'         DELETE ELEMENT                               
         J     MBT5                                                             
                                                                                
MBT5     CLI   BFMPNLC,PCQTXTYP    TEST PST TYPE REQUIRED                       
         JNE   MBT6                                                             
         OI    PSTIND,PSTIDUE      SET PST DUE                                  
         J     MBT7                                                             
                                                                                
MBT6     TM    PSTIND,PSTIDUE      TEST PST DUE                                 
         JNO   MBT7                                                             
         CLI   BFMPNLC,PCQJTPST    TEST PST TOTAL(ABT) PST                      
         JNE   MBT7                                                             
         OI    PSTIND,PSTIABT      SET PST(AMOUNT BEFORE TAX)                   
                                                                                
MBT7     LLC   R0,BFMLN                                                         
         AR    R3,R0                                                            
         J     MBT3                                                             
                                                                                
MBT9     GOTOR DELEL,DMCB,AIO                                                   
         DROP  R3                                                               
                                                                                
         CLI   PSTIND,0            NEED PST AMOUNTS                             
         JE    MBT19               NO,                                          
         L     R6,APROTYP          GET PROVINCE TOTALS BY TYPE                  
         USING CPROD,R6                                                         
         CLI   CPRODES,0                                                        
         JE    XIT                                                              
                                                                                
MBT11    LA    R4,CPROTXB                                                       
         USING CTAXD,R4                                                         
         ZAP   SAVDPST,PZERO                                                    
         ZAP   SAVTPST,PZERO                                                    
         CP    CTAXGRS,PZERO                                                    
         JE    MBT25               SKIP IF ZERO GROSS                           
         MVC   PRVDES,CPRODES      PROVINCE DESCRIPTION                         
         LA    RF,AMTS                                                          
         USING AMTD,RF                                                          
         ZAP   SAVDPST,DUEPST      SAVE DUE PST                                 
         ZAP   SAVTPST,JOTOTPST    SAVE TOTAL PST                               
         LA    RE,CTAXPST          PST TYPE                                     
         USING CTXD,RE                                                          
         MVC   CTXPRVD,CPRODES     PROVINCE                                     
         ZAP   DUEPST,CTAXTAX      TAX                                          
         ZAP   JOTOTPST,CTAXTAX                                                 
         AP    JOTOTPST,CTAXPB     REMOVE PREVIOUS AMOUNT                       
                                                                                
         OC    CRTLR,CRTLR         TEST RETAILER                                
         JZ    MBT17               NO,                                          
         ZAP   DUEPST,CTAXRTX      YES, USE RETAILER PORTION                    
         ZAP   JOTOTPST,CTAXTAX                                                 
                                                                                
MBT17    LA    R1,DUEPST           SKIP ZERO AMOUNTS                            
         TM    PSTIND,PSTIABT                                                   
         JNO   *+8                                                              
         LA    R1,JOTOTPST                                                      
         CP    0(L'DUEPST,R1),PZERO                                             
         JE    MBT25                                                            
         DROP  R4,RE,RF                                                         
                                                                                
MBT19    L     RE,AIO              MOVE FORMAT RECORD                           
         L     R0,ABFM                                                          
         LLH   R1,BFMRLEN-BFMRECD(RE)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    BTTWRK,BTTWRK                                                    
         L     R2,ABFM                                                          
         MVI   ELCODE,BTTELQ                                                    
         GOTOR GETELN                                                           
         JNE   MBT23                                                            
         USING BTTELD,R2                                                        
         LLC   R1,BTTEL                                                         
         BCTR  R1,0                                                             
         EXRL  R1,MBTMVCWS                                                      
                                                                                
MBT23    L     R2,ABFM                                                          
         USING BFMRECD,R2                                                       
         L     R3,ABEDR                                                         
         USING BEDRECD,R3          BUILD KEY FOR BEDREC                         
         XC    BEDRECD(BEDRFST-BEDRECD+2),BEDRECD                               
         L     RF,ABLH                                                          
         MVC   BEDKEY,0(RF)        FROM BILL HEADER                             
         MVC   BEDKLVL,BFMKLVL     LEVEL                                        
         MVC   BEDKWHER,BFMKWHER   WHERE                                        
         MVC   BEDKSEQ,RECNO                                                    
                                                                                
         GOTOR UPPAN,DMCB,ABFM,ABEDR    EXPAND SOME PANELS                      
         GOTOR FMTTX,DMCB,('FTACTSIZ',ABEDR)                                    
         GOTOR SORT,DMCB,('BILKRBED',ABEDR)                                     
                                                                                
MBT25    CLI   PSTIND,0            TEST NEED PST                                
         JE    XIT                 NO,                                          
                                                                                
         LA    RF,AMTS                                                          
         USING AMTD,RF                                                          
         ZAP   DUEPST,SAVDPST      RESTORE PST                                  
         ZAP   JOTOTPST,SAVTPST                                                 
                                                                                
         LA    R6,CPROLNQ(R6)      GET NEXT PST ENTRY                           
         CLI   CPRODES,0                                                        
         JNE   MBT11                                                            
         MVI   PSTIND,0                                                         
         J     XIT                                                              
                                                                                
         USING BTTELD,R2                                                        
MBTMVCWS MVC   BTTWRK(0),BTTEL     SAVE BTTEL                                   
         DROP  R2,R3,R6,RF                                                      
         EJECT                                                                  
**********************************************************************          
* MAIN BILL HOOK ROUTINES                                            *          
*  PARM 1 = A(BFMEL)                                                 *          
*       2 = A(FLBTAB ENTRY)                                          *          
**********************************************************************          
                                                                                
MBTHK    LM    R2,R3,0(R1)                                                      
         USING BFMELD,R2                                                        
         USING FLDTABD,R3                                                       
         LA    R6,BTTWRK                                                        
         USING BTTELD,R6                                                        
                                                                                
         LARL  R4,MBTHTAB          FIND ROUTINE FOR THIS ELEMENT DATA           
MBTHK3   CLI   0(R4),EOT                                                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BFMPNLC,0(R4)       MATCH PANEL CODE                             
         JE    *+12                                                             
         LA    R4,L'MBTHTAB(R4)                                                 
         J     MBTHK3                                                           
         LLH   RF,2(R4)                                                         
         LARL  RE,MBT                                                           
         AR    RF,RE                                                            
         BR    RF                                                               
                                                                                
MBTNET   TM    BTTINDS1,BTTINET    * NET *                                      
         JZ    XITN                                                             
         LA    R0,L'NET$                                                        
         LA    RF,NET$                                                          
         J     MBTFMT                                                           
                                                                                
MBTCOM   TM    BTTINDS1,BTTICOM    * COMMISSION *                               
         JZ    XITN                                                             
         LA    R0,L'COM$                                                        
         LA    RF,COM$                                                          
         J     MBTFMT                                                           
                                                                                
MBTGRS   TM    BTTINDS1,BTTIGRS    * GROSS *                                    
         JZ    XITN                                                             
         LA    R0,L'GRS$                                                        
         LA    RF,GRS$                                                          
         J     MBTFMT                                                           
                                                                                
MBTCD    CP    CSD$,PZERO          * DISCOUNT *                                 
         JE    XITN                                                             
         LA    R0,L'CSD$                                                        
         LA    RF,CSD$                                                          
                                                                                
MBTFMT   GOTOR FMTAMT,DMCB,BFMEL,((R0),(RF))                                    
         J     XITY                                                             
                                                                                
MBTPAY   L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         TM    PGMAQ,PGMALL27      TEST CLIENT BILL                             
         JNZ   *+12                YES,  SKIP GROUP TEST                        
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         JO    XITN                YES, SKIP THE 'PAY ABOVE...'                 
         TM    BTTINDS2,BTTIPPAY   * "PAY ABOVE AMOUNT" *                       
         JZ    XITN                                                             
         L     R5,ADICO                                                         
         LA    R4,AMTS                                                          
         USING AMTD,R4                                                          
         USING DICD,R5                                                          
         TM    PGMAQ,PGMALL27      TEST CLIENT BILL                             
         JZ    MBTPAY3             NO,                                          
         LA    RF,DD@PPTA          "PLEASE PAY...                               
         LA    R1,L'DD@PPTA                                                     
         LA    RE,DUEGRS                                                        
         J     MBTPAY5                                                          
                                                                                
MBTPAY3  LA    RF,DD@PAA                                                        
         LA    R1,L'DD@PAA                                                      
         LA    RE,DUEGRS                                                        
         TM    JXSTAT3,JXS3PNET    TEST PAY=NET                                 
         JNO   MBTPAY5                                                          
         LA    RE,DUENET                                                        
                                                                                
MBTPAY5  TM    RQLV,LVA+LVB        TEST A27 GROUP                               
         JZ    *+8                                                              
         LA    RE,LVGRCV                                                        
         CP    0(L'DUEGRS,RE),PZERO                                             
         JNL   *+12                                                             
         LA    RF,DD@CRAM          CREDIT AM0UNT                                
         LA    R1,L'DD@CRAM                                                     
         BCTR  R1,0                                                             
         EXRL  R1,MBTPMVTX                                                      
         LA    R5,BFMTEXT+1(R1)                                                 
         GOTOR GETPDLN                                                          
         J     XITY                                                             
                                                                                
MBTXDES  MVC   BFMTEXT(L'PRVDES),PRVDES                                         
         LA    R5,BFMTEXT+L'PRVDES                                              
         GOTOR GETPDLN                                                          
         J     XITY                                                             
                                                                                
MBTPMVTX MVC   BFMTEXT(0),0(RF)                                                 
         DROP  R2,R3,R4,R5,R6,R9                                                
         EJECT                                                                  
***********************************************************************         
* PROCESS GST /PST TAX ANALYSIS                                       *         
***********************************************************************         
                                                                                
TAX      NTR1  LABEL=*                                                          
         LTR   R1,R1                                                            
         JNZ   TAXHK               TAX HOOK ROUTINE                             
                                                                                
         TM    FRSTIND,FRSTTAX     TEST ALREADY HAD FIRST                       
         JO    *+12                                                             
         OI    FRSTIND,FRSTTAX     SET FIRST                                    
         MVI   RECNO,0             RESET SEQUENCE FOR OUTPUT                    
                                                                                
         TM    CTAXOPT,CTAXOGST+CTAXOPST  TEST WANT GST                         
         JZ    XIT                                                              
         L     R6,AGSTBUF          GST ENTRY                                    
         TM    CTAXOPT,CTAXOGRP                                                 
         JNO   *+8                                                              
         L     R6,AGSTGRP          GST GROUP                                    
         USING CTDLD,R6                                                         
         CLI   0(R6),0             NO TAX ITEMS                                 
         JE    XIT                                                              
                                                                                
         L     RE,AIO              MOVE FORMAT RECORD                           
         L     R0,ABFM                                                          
         LLH   R1,BFMRLEN-BFMRECD(RE)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R2,ABFM                                                          
         USING BFMRECD,R2                                                       
         L     R3,ABEDR                                                         
         USING BEDRECD,R3          BUILD KEY FOR BEDREC                         
         XC    BEDRECD(BEDRFST-BEDRECD+2),BEDRECD                               
         L     RF,ABLH                                                          
         MVC   BEDKEY,0(RF)        FROM BILL HEADER                             
         MVC   BEDKLVL,BFMKLVL     LEVEL                                        
         MVC   BEDKWHER,BFMKWHER   TAX RECORDS                                  
                                                                                
         GOTOR DETL,DMCB,TAXHTAB   CREATE DETAIL AND TRAILER RECORDS            
                                                                                
         MVC   RECNO,BEDKSEQ                                                    
         GOTOR UPPAN,DMCB,ABFM,ABEDR EXPAND SOME PANELS                         
         GOTOR FMTTX,DMCB,('FTACTSIZ',ABEDR)                                    
         GOTOR SORT,DMCB,('BILKRBED',ABEDR)                                     
                                                                                
         MVI   TXFLG,C'G'          SET FOR GST                                  
         L     R6,AGSTBUF          GST ENTRY                                    
         TM    CTAXOPT,CTAXOGRP                                                 
         JNO   *+8                                                              
         L     R6,AGSTGRP          GST GROUP                                    
TAX2     LA    RF,CTDLTXB                                                       
         USING CTAXD,RF                                                         
         CP    CTAXGRS,PZERO                                                    
         JE    TAX3                SKIP IF ZERO GROSS                           
         LA    RE,CTAXG                                                         
         CLI   TXFLG,C'G'                                                       
         JE    *+8                                                              
         LA    RE,CTAXP                                                         
         MVC   0(CTDLLNQ,RE),0(R6)                                              
         GOTOR DETBFM,DMCB,ADETR   ADD TAX DETAIL RECORD                        
TAX3     LA    R6,CTDLLNQ(R6)                                                   
TAX5     CLI   0(R6),0             NO TAX ITEMS                                 
         JNE   TAX2                                                             
                                                                                
         CLI   TXFLG,C'P'          TEST FINISHED PST                            
         JE    TAX13               YES,                                         
         MVI   TXFLG,C'P'          SET FOR PST                                  
         L     R6,APSTBUF          PST ENTRY                                    
         TM    CTAXOPT,CTAXOGRP                                                 
         JNO   *+8                                                              
         L     R6,APSTGRP          GST GROUP                                    
         J     TAX5                                                             
                                                                                
TAX13    TM    DETLFLG,DETLTRL     TEST ANY BFMED'S IN TRAILER                  
         JZ    XIT                                                              
         GOTOR DETBFM,DMCB,ATRLR   ADD TRAILER RECORD                           
         J     XIT                                                              
         DROP  R2,R3,R6,RF                                                      
         EJECT                                                                  
**********************************************************************          
* TAX HOOK ROUTINES                                                  *          
*  PARM 1 = A(BFMEL)                                                 *          
*       2 = A(FLBTAB ENTRY)                                          *          
**********************************************************************          
                                                                                
TAXHK    LM    R2,R3,0(R1)                                                      
         USING BFMELD,R2                                                        
         USING FLDTABD,R3                                                       
                                                                                
         LA    R4,TAXHTAB          FIND ROUTINE FOR THIS ELEMENT DATA           
TAXHK3   CLI   0(R4),EOT                                                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BFMPNLC,0(R4)       MATCH PANEL CODE                             
         JE    *+12                                                             
         LA    R4,L'TAXHTAB(R4)                                                 
         J     TAXHK3                                                           
         LLH   RF,2(R4)                                                         
         LARL  RE,TAX                                                           
         AR    RF,RE                                                            
         BR    RF                                                               
                                                                                
TXTYP    CLI   TXFLG,C'G'                                                       
         JNE   TXTYPP                                                           
         L     R5,ADICO                                                         
         USING DICD,R5                                                          
         MVC   BFMTEXT(L'DD@VAT1),DD@VAT1  TYPE                                 
         LA    R1,L'DD@VAT1                                                     
         J     TXTYPX                                                           
                                                                                
TXTYPP   LA    R3,CTAXPST          PST TYPE                                     
         USING CTXD,R3                                                          
         MVC   BFMTEXT(L'CTXPRVD),CTXPRVD                                       
         LA    R1,L'CTXPRVD                                                     
TXTYPX   LA    R5,BFMTEXT(R1)                                                   
         GOTOR GETPDLN                                                          
         J     XITY                                                             
                                                                                
         USING CTXD,R3                                                          
TXREG    LA    R3,CTAXGST          REGISTRATION                                 
         CLI   TXFLG,C'G'                                                       
         JE    *+8                                                              
         LA    R3,CTAXPST                                                       
         MVC   BFMTEXT(L'CTXREG),CTXREG                                         
         LA    R5,BFMTEXT+L'CTXREG                                              
         GOTOR GETPDLN                                                          
         J     XITY                                                             
                                                                                
TXRAT    LA    R3,CTAXGST          RATE                                         
         CLI   TXFLG,C'G'                                                       
         JE    *+8                                                              
         LA    R3,CTAXPST                                                       
         LLH   RE,CTXRATE                                                       
         CVD   RE,DUB                                                           
         GOTOR FMTRAT,DMCB,BFMEL,(L'DUB,DUB)                                    
         J     XITY                                                             
         DROP  R3                                                               
                                                                                
         USING CTAXD,R3                                                         
TXBAS    LA    R3,CTAXGST$         BASIS                                        
         CLI   TXFLG,C'G'                                                       
         JE    *+8                                                              
         LA    R3,CTAXPST$                                                      
         ZAP   DUB,CTAXGRS         GROSS                                        
         SP    DUB,CTAXTAX         LESS TAX=BASIS                               
         ICM   RF,15,CRTLR         TEST RETAILER                                
         JZ    TXBAS3              NO,                                          
         ZAP   DUB,CTAXRTG         USE RETAILER NUMBERS                         
         SP    DUB,CTAXRTX                                                      
                                                                                
TXBAS3   LA    R0,FAPINOC          NO COMMAS                                    
         GOTOR FMTAMT,DMCB,((R0),BFMEL),(L'DUB,DUB)                             
         J     XITY                                                             
                                                                                
TXAMT    LA    R3,CTAXGST$         TAX                                          
         CLI   TXFLG,C'G'                                                       
         JE    *+8                                                              
         LA    R3,CTAXPST$                                                      
         ZAP   DUB,CTAXTAX                                                      
         ICM   RF,15,CRTLR         TEST RETAILER                                
         JZ    TXAMT7              NO,                                          
         ZAP   DUB,CTAXRTX         YES, USE RETAILER PORTION                    
                                                                                
TXAMT7   LA    R0,FAPIPZA+FAPINOC                                               
         GOTOR FMTAMT,DMCB,((R0),BFMEL),(L'DUB,DUB)                             
         J     XITY                                                             
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS PREVIOUS BILLS                                              *         
***********************************************************************         
                                                                                
PRV      NTR1  LABEL=*                                                          
         LTR   R1,R1                                                            
         JNZ   PRVHK               PREVIOUS BILL HOOK ROUTINE                   
                                                                                
         TM    FRSTIND,FRSTPRV     TEST ALREADY HAD FIRST                       
         JO    *+12                                                             
         OI    FRSTIND,FRSTPRV     SET FIRST                                    
         MVI   RECNO,0             RESET SEQUENCE FOR OUTPUT                    
                                                                                
         L     R3,AIO              FIND TAX ELEMENTS                            
         USING BFMRECD,R3                                                       
         LA    R3,BFMRFST                                                       
         USING BFMELD,R3                                                        
         SR    R0,R0                                                            
                                                                                
PRV03    CLI   BFMEL,0             TEST EOR                                     
         JE    PRV07                                                            
         CLI   BFMEL,BFMELQ        TEST BFM ELEMENT                             
         JNE   PRV05                                                            
         CLI   BFMPNLC,PCQTXTYP    TEST PST/GST DUE LINES                       
         JNE   PRV05                                                            
         XR    R1,R1               SET 'NOT HOOK' FLAG                          
         GOTOR MBT                 PROCESS TAX LINES                            
         J     XIT                                                              
                                                                                
PRV05    LLC   R0,BFMLN                                                         
         AR    R3,R0                                                            
         J     PRV03                                                            
         DROP  R3                                                               
                                                                                
PRV07    L     RE,AIO              MOVE FORMAT RECORD                           
         L     R0,ABFM                                                          
         LLH   R1,BFMRLEN-BFMRECD(RE)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    BTTWRK,BTTWRK                                                    
         L     R2,ABFM                                                          
         MVI   ELCODE,BTTELQ                                                    
         GOTOR GETELN                                                           
         JE    *+6                                                              
         DC    H'0'                NEED BTTEL                                   
         USING BTTELD,R2                                                        
         LLC   R1,BTTEL                                                         
         BCTR  R1,0                                                             
         EXRL  R1,PRVMVCWS                                                      
                                                                                
         MVI   RTLPB,C'Y'          ASSUME RETAILER P.B.                         
         ICM   R5,15,CRTLR         TEST RETAILER ENTRY                          
         JZ    PRV11               NO,                                          
         USING RTLD,R5                                                          
         ICM   R0,15,NPRVB         NUMBER OF PREVIOUS BILLS                     
         JZ    PRV11               NO,                                          
         ICM   R6,15,FPRVB         A(FIRST PREVIOUS BILL)                       
         USING BTRND,R6                                                         
PRV09    CLC   RTLDIS,BTRNCA       ONLY USE BILLS FOR THIS RETAILER             
         JE    PRV11               YES, PREVIOUS BILLS FOR THIS RET             
         LA    R6,BTRNLQ(R6)       NEXT PREVIOUS BILL                           
         JCT   R0,PRV09                                                         
         MVI   RTLPB,C'N'          SET NO P.B. FOR THIS RETAILER                
         J     PRV11                                                            
                                                                                
PRV11    L     R2,ABFM                                                          
         USING BFMRECD,R2                                                       
         L     R3,ABEDR                                                         
         USING BEDRECD,R3          BUILD KEY FOR BEDREC                         
         XC    BEDRECD(BEDRFST-BEDRECD+2),BEDRECD                               
         L     RF,ABLH                                                          
         MVC   BEDKEY,0(RF)                                                     
         MVC   BEDKLVL,BFMKLVL     LEVEL                                        
         MVC   BEDKWHER,=AL2(BEDKWPBQ)  PREVIOUS BILLS                          
                                                                                
         LARL  R0,PRVHTAB                                                       
         GOTOR DETL,DMCB,(R0)      CREATE DETAIL AND TRAILER RECORDS            
         TM    DETLFLG,DETLDET     TEST RECORD CONTAIN DETAILS                  
         JO    PRV13               YES, PROCESS PREVIOUS BILLS                  
                                                                                
         MVC   BEDKSEQ,RECNO                                                    
         GOTOR UPPAN,DMCB,BFMRECD,BEDRECD       EXPAND COMMENTS                 
         GOTOR FMTTX,DMCB,('FTACTSIZ',BEDRECD)                                  
         GOTOR SORT,DMCB,('BILKRBED',BEDRECD)   PUT HEADER TO OUTPUT            
         J     XIT                                                              
                                                                                
PRV13    OC    NPRVB,NPRVB         TEST ANY PREVIOUS                            
         JZ    XIT                 NO PREVIOUS BILLS                            
         CLI   RTLPB,C'N'          TEST P.B. FOR THIS RETAILER                  
         JE    XIT                 NO PREVIOUS BILLS                            
                                                                                
         GOTOR UPPAN,DMCB,BFMRECD,BEDRECD       EXPAND SOME PANELS              
         GOTOR FMTTX,DMCB,('FTACTSIZ',BEDRECD)                                  
                                                                                
         LR    R2,R3                                                            
         MVI   ELCODE,BFPELQ       GET PARAGRAPH ELEMENT                        
         GOTOR GETELN                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         USING BFPELD,R2                                                        
         MVI   BFPSPLS,1           SPACE 1 AFTER HEADER                         
                                                                                
         L     R2,ABFM                                                          
         USING BFMRECD,R2                                                       
                                                                                
         MVC   BEDKSEQ,RECNO                                                    
         GOTOR SORT,DMCB,('BILKRBED',BEDRECD)   PUT HEADER TO OUTPUT            
                                                                                
         ICM   R0,15,NPRVB         NUMBER OF PREVIOUS BILLS                     
         ICM   R6,15,FPRVB         A(FIRST PREVIOUS BILL)                       
                                                                                
         USING BTRND,R6                                                         
PRV15    ICM   R5,15,CRTLR         TEST RETAILER ENTRY                          
         JZ    PRV17                                                            
         USING RTLD,R5                                                          
         CLC   RTLDIS,BTRNCA       ONLY USE BILLS FOR THIS RETAILER             
         JNE   PRV19                                                            
PRV17    ST    R6,APRVB                                                         
         GOTOR DETBFM,DMCB,ADETR   PROCESS DETAIL ITEM                          
PRV19    LA    R6,BTRNLQ(R6)       NEXT PREVIOUS BILL                           
         JCT   R0,PRV15                                                         
                                                                                
         TM    DETLFLG,DETLTRL     TEST ANY BFMED'S IN TRAILER                  
         JZ    XIT                                                              
         GOTOR DETBFM,DMCB,ATRLR   ADD TRAILER RECORD                           
         J     XIT                                                              
         DROP  R6                                                               
                                                                                
         USING BTTELD,R2                                                        
PRVMVCWS MVC   BTTWRK(0),BTTEL     SAVE BTTEL                                   
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PREVIOUS BILL HOOK ROUTINES                                        *          
*  PARM 1 = A(BFMEL)                                                 *          
*       2 = A(FLBTAB ENTRY)                                          *          
**********************************************************************          
                                                                                
PRVHK    LM    R2,R3,0(R1)                                                      
         USING BFMELD,R2                                                        
         USING FLDTABD,R3                                                       
         L     R5,APRVB                                                         
         USING BTRND,R5                                                         
         LA    R6,BTRNBK                                                        
         USING JOBD,R6                                                          
                                                                                
         LARL  R4,PRVHTAB          FIND ROUTINE FOR THIS ELEMENT DATA           
PRVHK3   CLI   0(R4),EOT                                                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BFMPNLC,0(R4)       MATCH PANEL CODE                             
         JE    *+12                                                             
         LA    R4,L'PRVHTAB(R4)                                                 
         J     PRVHK3                                                           
         LLH   RF,2(R4)                                                         
         LARL  RE,PRV                                                           
         AR    RF,RE                                                            
         BR    RF                                                               
                                                                                
PBTYP    MVC   BFMTEXT(L'BTRNBTYP),BTRNBTYP                                     
         LA    R5,BFMTEXT+L'BTRNBTYP                                            
         LA    R1,BFMTEXT                                                       
         LA    R0,7                                                             
         CLC   0(6,R1),=C'PC EST'                                               
         JE    *+16                                                             
         LA    R1,1(R1)                                                         
         JCT   R0,*-14                                                          
         J     PBTYPX                                                           
         MVC   0(12,R1),=C'PCT ESTIMATE'                                        
         LA    R5,BFMTEXT+15                                                    
PBTYPX   GOTOR GETPDLN                                                          
         J     XITY                                                             
                                                                                
PBDAT    GOTOR DATCON,DMCB,(1,BTRNDTE),(8,BFMTEXT)                              
         LA    R5,BFMTEXT+10                                                    
         GOTOR GETPDLN                                                          
         J     XITY                                                             
                                                                                
PBREF    MVC   BFMTEXT(L'BTRNREF),BTRNREF                                       
         LA    R5,BFMTEXT+L'BTRNREF                                             
         GOTOR GETPDLN                                                          
         J     XITY                                                             
                                                                                
PBNET    ZAP   DUB,JOBNET                                                       
         AP    DUB,JOBCD                                                        
         LA    R0,L'DUB                                                         
         LA    RF,DUB                                                           
         J     PBFMT                                                            
                                                                                
PBCOM    LA    R0,L'JOBCOM                                                      
         LA    RF,JOBCOM                                                        
         J     PBFMT                                                            
                                                                                
PBGRS    ZAP   DUB,JOBGRS                                                       
         AP    DUB,JOBCD                                                        
         LA    R0,L'DUB                                                         
         LA    RF,DUB                                                           
         J     PBFMT                                                            
                                                                                
PBCD     LA    R0,L'JOBCD                                                       
         LA    RF,JOBCD                                                         
         J     PBFMT                                                            
                                                                                
PBFMT    LA    R3,FAPINOC          ASSUME NO COMMAS                             
         TM    OPT8,OPT8PAWC       TEST WITH COMMAS                             
         JNO   *+6                                                              
         XR    R3,R3               LEAVE COMMAS                                 
         GOTOR FMTAMT,DMCB,((R3),BFMEL),((R0),(RF))                             
         J     XITY                                                             
                                                                                
DUGST    LA    R0,L'DUEGST                                                      
         LA    R4,AMTS                                                          
         USING AMTD,R4                                                          
         LA    RF,DUEGST                                                        
         LA    R3,FAPIPZA+FAPINOC                                               
         GOTOR FMTAMT,DMCB,((R3),BFMEL),((R0),(RF))                             
         J     XITY                                                             
                                                                                
DUPST    LA    R0,L'DUEPST                                                      
         LA    R4,AMTS                                                          
         LA    RF,DUEPST                                                        
         LA    R3,FAPIPZA+FAPINOC                                               
         GOTOR FMTAMT,DMCB,((R3),BFMEL),((R0),(RF))                             
         J     XITY                                                             
                                                                                
DUNTX    LA    R0,L'DUENTX                                                      
         LA    R4,AMTS                                                          
         LA    RF,DUENTX                                                        
         GOTOR FMTAMT,DMCB,BFMEL,((R0),(RF))                                    
         J     XITY                                                             
                                                                                
PBDUE    LA    R4,AMTS                                                          
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         JO    XITN                YES, SKIP THE 'PAY ABOVE...'                 
         LA    R6,BTTWRK                                                        
         USING BTTELD,R6                                                        
         TM    BTTINDS2,BTTIPPAY   * "PAY ABOVE AMOUNT" *                       
         JZ    XITN                                                             
         L     R5,ADICO                                                         
         USING DICD,R5                                                          
         LA    RF,DD@PAA                                                        
         LA    R1,L'DD@PAA                                                      
         LA    RE,DUEGRS                                                        
         TM    RQLV,LVA+LVB       TEST A27 GROUP                                
         JZ    *+8                                                              
         LA    RE,LVGRCV                                                        
         CP    0(L'DUEGRS,RE),PZERO                                             
         JNL   *+12                                                             
         LA    RF,DD@CRAM          CREDIT AM0UNT                                
         LA    R1,L'DD@CRAM                                                     
         BCTR  R1,0                                                             
         EXRL  R1,PBDMVCTX                                                      
         LA    R5,BFMTEXT+1(R1)                                                 
         GOTOR GETPDLN                                                          
         J     XITY                                                             
                                                                                
PBDMVCTX MVC   BFMTEXT(0),0(RF)                                                 
         DROP  R2,R3,R4,R5,R6                                                   
         EJECT                                                                  
***********************************************************************         
* BUILD DETAIL AND TRAILER RECORDS  FOR P.B./ TAX ETC.                *         
*  PARM 1=A(LIST OF PANEL CODES FOR THIS DETAIL)                      *         
***********************************************************************         
                                                                                
DETL     NTR1  LABEL=*                                                          
         L     R6,0(R1)            SAVE ADDRESS OF THE LIST                     
         MVI   DETLFLG,0                                                        
                                                                                
         L     R4,ABEDR            MOVE KEY TO DETAIL AND TRAILER               
         L     R3,ADETR                                                         
         USING BEDRECD,R3                                                       
         MVC   BEDRECD(BEDRFST-BEDRECD+2),0(R4)                                 
         L     R3,ATRLR                                                         
         MVC   BEDRECD(BEDRFST-BEDRECD+2),0(R4)                                 
         DROP  R3                                                               
                                                                                
         L     R2,ABFM             COPY BFMEL'S THAT ARE IN LIST                
         USING BFMRECD,R2          TO DETAIL                                    
         LA    R3,BFMRFST                                                       
         SR    R0,R0                                                            
DETL3    CLI   0(R3),0                                                          
         JE    DETL17                                                           
         CLI   0(R3),BFPELQ        COPY BFPELD TO BOTH                          
         JE    DETL6                                                            
         CLI   0(R3),BTTELQ        COPY BTTELD TO BOTH                          
         JE    DETL6                                                            
         CLI   0(R3),BFMELQ                                                     
         JE    DETL7                                                            
DETL5    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         J     DETL3                                                            
                                                                                
DETL6    GOTOR ADDEL,DMCB,ADETR,(C'E',0(R3))                                    
         GOTOR ADDEL,DMCB,ATRLR,(C'E',0(R3))                                    
         J     DETL5                                                            
                                                                                
         USING BFMELD,R3                                                        
DETL7    LR    R4,R6               MATCH PANEL CODE TO LIST                     
DETL9    CLI   0(R4),EOT                                                        
         JE    DETL13                                                           
         CLI   1(R4),1             IS IT A DETAIL?                              
         JNE   *+14                SKIP IT, IF NOT DETAIL                       
         CLC   BFMPNLC,0(R4)                                                    
         JE    DETL11                                                           
         LA    R4,L'TAXHTAB(R4)                                                 
         J     DETL9                                                            
                                                                                
DETL11   GOTOR ADDEL,DMCB,ADETR,(C'E',0(R3))                                    
         MVI   0(R3),X'FF'         DELETE FROM INPUT RECORD                     
         OI    DETLFLG,DETLDET     SET - DETAILS FOUND                          
         J     DETL5                                                            
                                                                                
DETL13   TM    DETLFLG,DETLDET     TEST ANY DETAILS YET                         
         JZ    DETL5               NO, MUST BE A HEADER                         
         GOTOR ADDEL,DMCB,ATRLR,(C'E',0(R3))                                    
         MVI   0(R3),X'FF'         DELETE FROM INPUT RECORD                     
         OI    DETLFLG,DETLTRL     SET - TRAILER FOUND                          
         J     DETL5                                                            
                                                                                
DETL17   GOTOR HELLO,DMCB,(C'D',ACCMST),(X'FF',BFMRECD),0,0                     
         J     XITY                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CONVERT  DETAIL/TRAILER BFM RECORDS TO BEDRECS                      *         
*  PARM 1=A(DETAIL OR TRAILER RECORD BFMREC)                          *         
***********************************************************************         
                                                                                
DETBFM   NTR1  LABEL=*                                                          
         L     R6,0(R1)            R6=A(INPUT RECORD)                           
                                                                                
         LR    RE,R6               MOVE DETAIL/TRAILER RECORD                   
         L     R0,ABEDR                                                         
         LLH   R1,BFMRLEN-BFMRECD(RE)                                           
         LA    R1,1(R1)            PICK UP TRAILING ZERO                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R2,ABEDR                                                         
         USING BEDRECD,R2                                                       
         MVC   BEDKSEQ,RECNO       SET RECORD NUMBER                            
                                                                                
         MVC   BEDKSEQ,RECNO                                                    
         GOTOR UPPAN,DMCB,ABEDR,ABEDR  EXPAND SOME PANELS                       
         GOTOR FMTTX,DMCB,('FTACTSIZ',ABEDR)                                    
         GOTOR SORT,DMCB,('BILKRBED',ABEDR)  PUT HEADER TO OUTPUT               
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE FIELD PANELS (BFMELDS WITH BFMPANEL SET)                     *         
* NTRY: P1=A(INPUT RECORD)                                            *         
*       P2=A(OUTPUT RECORD)                                           *         
***********************************************************************         
                                                                                
UPPAN    NTR1  LABEL=*                                                          
         LM    R5,R6,0(R1)                                                      
                                                                                
         L     R3,ABEDN            IO FOR NEW RECORD                            
         USING BEDRECD,R3                                                       
         XC    BEDRECD(BEDRFST-BEDRECD+2),BEDRECD                               
         MVC   BEDKEY,0(R6)                  KEY FROM OUTPUT RECORD             
         MVC   BEDRSTA,BEDRSTA-BEDRECD(R6)   STATUS FROM OUTPUT                 
                                                                                
         LA    R2,BEDRFST-BEDRECD(R5)  LOOP THRU INPUT RECORD                   
         USING BFMELD,R2                                                        
         XR    R0,R0                                                            
UPPAN3   CLI   BFMEL,0                                                          
         JE    UPPAN11                                                          
         LR    R4,R2                                                            
         CLI   BFMEL,BFMELQ                                                     
         JNE   UPPAN7                                                           
         CLI   BFMPNLC,0           TEST ANY PANEL CODE                          
         JE    UPPAN7              NONE, SKIP GET PANEL FIELD ROUTINE           
                                                                                
         L     R4,AELMNT                                                        
         XC    0(255,R4),0(R4)     COPY BFMEL                                   
         LLC   RF,BFMLN                                                         
         BCTR  RF,0                                                             
         EXRL  RF,UPPAMV4B                                                      
                                                                                
N        USING BFMELD,R4                                                        
         MVI   N.BFMTEXT,C' '                                                   
         MVC   N.BFMTEXT+1(BFMTMAXL-1),N.BFMTEXT                                
         GOTOR GETPD,DMCB,N.BFMELD     EXTRACT DATA FOR THIS PANEL              
         CLI   N.BFMTOP,0              TEST TOP IS ZERO                         
         JE    UPPAN9                  YES, SKIP IT                             
         XR    RF,RF                                                            
         ICM   RF,3,N.BFMTLEN          TEST LENGTH OF DATA                      
         JNZ   *+6                     YES,                                     
         DC    H'0'                    LENGTH NOT SET                           
         AHI   RF,BFMLNQ                                                        
         STC   RF,N.BFMLN              SET ELEMENT LENGTH                       
         OI    BEDRSTAT,BEDSPAN        SET RECORD HAS PANELS                    
                                                                                
UPPAN7   GOTOR ADDEL,DMCB,ABEDN,N.BFMELD                                        
UPPAN9   IC    R0,BFMLN                                                         
         AR    R2,R0                                                            
         J     UPPAN3                                                           
                                                                                
UPPAN11  LR    R0,R6                   MOVE NEW RECORD TO OUTPUT                
         L     RE,ABEDN                                                         
         LLH   RF,BEDRLEN-BEDRECD(RE)                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         J     XIT                                                              
                                                                                
UPPAMV4B MVC   0(0,R4),BFMELD                                                   
         DROP  R2,R3,N                                                          
         EJECT                                                                  
***********************************************************************         
* EXTRACT ROUTINES                                                    *         
* PARM 1=A(PARELD)                                                    *         
***********************************************************************         
                                                                                
GETDATA  NTR1  LABEL=*                                                          
         L     R4,0(R1)                                                         
         USING PARELD,R4                                                        
         L     R3,CBTRN            ADDRESS OF TRANSACTION ENTRY                 
         USING BTRND,R3                                                         
                                                                                
         LLC   R5,PARFLD                                                        
         MHI   R5,FLDTABL                                                       
         LARL  RE,FLDTAB                                                        
         AR    R5,RE               RF=A(FIELD ENTRY)                            
                                                                                
         USING FLDTABD,R5                                                       
         LLH   RF,FLDRADR          GET DISPLACEMENT TO ROUTINE                  
         XR    R1,R1                                                            
         ICM   R1,1,FLDGLEN        TEST DATA LENGTH                             
         JNZ   GETDATA2                                                         
         LARL  RE,GETDATA                                                       
         AR    RF,RE                                                            
         BR    RF                                                               
                                                                                
GETDATA2 LA    R6,AMTS                                                          
         USING AMTD,R6                                                          
         XR    RE,RE                                                            
         SLDL  RE,20               BASE INTO RE                                 
         SRL   RF,20               DISPL IN RF                                  
         EXRL  RE,GETDLRE0         BASE WILL BE IN RE                           
         AR    RF,RE               ADD TO DISPL                                 
                                                                                
         BCTR  R1,0                                                             
         EXRL  R1,GETDMVDF                                                      
         TM    FLDINDS,FLDIAMT     TEST AMOUNT FIELD                            
         JNO   XIT                                                              
         MVI   PARDLEN,L'PARAMT                                                 
         EXRL  R1,GETDZAPF                                                      
         J     XIT                                                              
                                                                                
GETDLRE0 LR    RE,0                                                             
GETDMVDF MVC   PARDATA(0),0(RF)                                                 
GETDZAPF ZAP   PARAMT,0(0,RF)                                                   
                                                                                
GETNEW   MVI   PARDLEN,1           NEW ITEM TXFLG                               
         MVI   PARDATA,C' '                                                     
         TM    BTRNSTA,BTRNSNEW                                                 
         JNO   *+8                                                              
         MVI   PARDATA,C'*'                                                     
         J     XIT                                                              
                                                                                
GETVNDN  GOTOR AXTRT,GCONNMQ       CONTRA NAME                                  
         TM    PGMAQ,PGMALL27      TEST 27                                      
         JNZ   GETEXT              YES, SET FIX SWITCH                          
**********************************************************************          
         MVI   TEMPCHOP,C'Y'       **** TEMP FIX TO MATCH OLD 21***             
**********************************************************************          
         J     GETEXT                                                           
                                                                                
GETNAR   GOTOR AXTRT,GTRNNRQ       NARRATIVE                                    
         L     RE,DMCB                                                          
         CLC   0(2,RE),=C'**'      NARRATIVE STARTS WITH **                     
         JNE   GETEXT                                                           
         MVI   DMCB,0              IGNORE IT                                    
         J     GETEXT                                                           
                                                                                
GETWCN   GOTOR AXTRT,GWCNMEQ       WORKCODE NAME                                
         J     GETEXT                                                           
                                                                                
GETWCD   GOTOR AXTRT,GWCDSCQ       WORKCODE DESCRIPTION                         
         J     GETEXT                                                           
                                                                                
GETXFR   GOTOR AXTRT,GXFRQ         GET TRANSFER DATA                            
         J     GETEXT                                                           
                                                                                
GETLIN   GOTOR AXTRT,GLINQ         GET LONG INVOICE NUMBER                      
         J     GETEXT                                                           
                                                                                
GETEXT   MVI   PARDLEN,1           DEFAULT 1 SPACE                              
         MVI   PARDATA,C' '                                                     
         SR    RE,RE               BYTE 0   OF DMCB HAS LENGTH                  
         ICM   RE,1,DMCB                1-3             A(DATA)                 
         JZ    XIT                                                              
         STC   RE,PARDLEN                                                       
         BCTR  RE,0                                                             
         L     RF,DMCB                                                          
         BASR  RB,0                                                             
         MVC   PARDATA(0),0(RF)                                                 
         EX    RE,0(RB)                                                         
**********************************************************************          
         CLI   TEMPCHOP,C'Y'       TEMP FIX TO MATCH OLD 21                     
         JNE   XIT                                                              
         MVI   TEMPCHOP,C'N'                                                    
         LLC   R0,PARDLEN                                                       
         LARL  RF,CHOPBLK                                                       
         GOTOR CHOPPER,DMCB,((R0),PARDATA),(15,(RF)),(16,3),0                   
         ICM   R0,15,DMCB+8                                                     
         CHI   R0,1                                                             
         JE    XIT                 ONE LINE - PARDATA IS GOOD                   
         MVI   PARDATA,C' '                                                     
         MVC   PARDATA+1(L'PARDATA-1),PARDATA                                   
         LARL  RF,CHOPBLK                                                       
         MVC   PARDATA(47),0(RF)                                                
         MVI   PARDLEN,31                                                       
         CHI   R0,2                                                             
         JE    XIT                                                              
         MVI   PARDLEN,47                                                       
         J     XIT                                                              
CHOPBLK  DS    CL50                                                             
**********************************************************************          
         J     XIT                                                              
                                                                                
GETH@R   MVI   PARDLEN,1           HOURS @                                      
         MVI   PARDATA,C' '                                                     
         ZAP   DUB,BTRNBK+(JOBHRS-JOBD)(L'JOBHRS) HOURS                         
         JZ    GETH@R2                                                          
         CURED (P8,DUB),(7,PARDATA),2,MINUS=YES,ALIGN=LEFT                      
         LA    R6,PARDATA+1                                                     
         AR    R6,R0                                                            
         L     RF,ADICO                                                         
         MVC   0(L'DD@HOURS,R6),DD@HOURS-DICD(RF)                               
         LA    R6,L'DD@HOURS(R6)                                                
         CLI   0(R6),C' '                                                       
         JH    *+8                                                              
         JCT   R6,*-8                                                           
         MVI   2(R6),C'@'                                                       
         LA    R6,4(R6)                                                         
         ZAP   DUB,BTRNBILR                                                     
         CURED (P8,DUB),(7,0(R6)),2,ALIGN=LEFT                                  
         AR    R6,R0                                                            
         LA    RF,PARDATA                                                       
         SR    R6,RF                                                            
         STC   R6,PARDLEN                                                       
         J     XIT                                                              
                                                                                
GETH@R2  ZAP   DUB,BTRNUNTS        UNITS OR HOURS                               
         JZ    XIT                                                              
         TM    BTRNSTA,BTRNSUQH                                                 
         JZ    GETH@R4                                                          
         CURED (P8,DUB),(7,PARDATA),2,MINUS=YES,ALIGN=LEFT                      
         LA    R6,PARDATA+1                                                     
         AR    R6,R0                                                            
         L     RF,ADICO                                                         
         MVC   0(L'DD@HOURS,R6),DD@HOURS-DICD(RF)                               
         LA    R6,L'DD@HOURS(R6)                                                
         J     GETH@R6                                                          
                                                                                
GETH@R4  CURED (P8,DUB),(7,PARDATA),0,MINUS=YES,ALIGN=LEFT                      
         LA    R6,PARDATA+1                                                     
         AR    R6,R0                                                            
         L     RF,ADICO                                                         
         MVC   0(L'DD@UNITS,R6),DD@UNITS-DICD(RF)                               
         LA    R6,L'DD@UNITS(R6)                                                
                                                                                
GETH@R6  CLI   0(R6),C' '                                                       
         JH    *+8                                                              
         JCT   R6,*-8                                                           
         MVC   2(2,R6),=C'@ '                                                   
         LA    R6,4(R6)                                                         
         ZAP   DUB,BTRNUPRC                                                     
         CURED (P8,DUB),(7,0(R6)),2,ALIGN=LEFT                                  
         AR    R6,R0                                                            
         LA    RF,PARDATA                                                       
         SR    R6,RF                                                            
         STC   R6,PARDLEN                                                       
         J     XIT                                                              
                                                                                
GETWCGN  MVC   PARDATA(40),SPACES      SET WC GROUP NAME                        
         CLI   WRKCGRP,C' '            TEST ANY GROUP                           
         JNH   GETWCGN3                NO, USE 'PROFESSIONAL                    
         MVI   PARDLEN,L'WRKCGRPN+4                                             
         MVC   PARDATA+4(L'WRKCGRPN),WRKCGRPN                                   
         J     XIT                                                              
                                                                                
GETWCGN3 L     R6,ADICO                                                         
         USING DICD,R6                                                          
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         MVC   0(L'DD@PROFS,RF),DD@PROFS                                        
         LA    RF,L'DD@PROFS+1(RF)                                              
         TM    OPT8,OPT8OSTF       TEST 'OMIT STAFF'                            
         JO    GETWCGN4            YES,                                         
         MVC   0(L'DD@STAFF,RF),DD@STAFF                                        
         LA    RF,L'DD@STAFF+1(RF)                                              
                                                                                
GETWCGN4 MVC   0(L'DD@SERVE,RF),DD@SERVE                                        
         GOTOR ADSQUASH,DMCB,WORK,L'WORK                                        
         LLC   R1,DMCB+7                                                        
         BCTR  R1,0                                                             
         EXRL  R1,GETWMVDW                                                      
         LA    RF,PARDATA+2(R1)                                                 
                                                                                
         GOTOR SETTDA              SET DATE INFO                                
         LA    R1,PARDATA-1                                                     
         SR    RF,R1                                                            
         STC   RF,PARDLEN                                                       
         J     XIT                                                              
                                                                                
GETWMVDW MVC   PARDATA(0),WORK                                                  
                                                                                
GETVNDCN MVI   PARDLEN,1           VENDOR CODE & NAME                           
         MVI   PARDATA,C' '                                                     
         MVC   WORK,SPACES                                                      
         LA    R2,WORK                                                          
         TM    OPT5,OPT5PVCO       TEST PRINT VENDOR CODE                       
         JNO   GTVNDC1                                                          
         MVC   0(12,R2),BTRNSUP+2                                               
         LA    R2,13(R2)                                                        
GTVNDC1  TM    OPT5,OPT5PVDE       TEST PRINT VENDOR DESCRIPTION                
         JNO   GTVNDCN3                                                         
         GOTOR AXTRT,GCONNMQ       CONTRA NAME                                  
         XR    R1,R1               BYTE 0   OF DMCB HAS LENGTH                  
         ICM   R1,1,DMCB                1-3             A(DATA)                 
         JZ    GTVNDCN3                                                         
         BCTR  R1,0                                                             
         L     RE,DMCB                                                          
         EXRL  R1,GTVNMV2E                                                      
         LA    R2,1(R1,R2)                                                      
GTVNDCN3 CLI   WORK,C' '                                                        
         JNH   XIT                                                              
         LA    R1,WORK                                                          
         SR    R2,R1                                                            
         STC   R2,PARDLEN                                                       
         BCTR  R2,0                                                             
         EXRL  R2,GTVNMVDW                                                      
         J     XIT                                                              
                                                                                
GTVNMV2E MVC   0(0,R2),0(RE)                                                    
GTVNMVDW MVC   PARDATA(0),WORK                                                  
                                                                                
GETWRKCN MVC   WRKCCUR,BTRNWC                                                   
         CLC   WRKCCUR,SPACES                                                   
         JE    GTWRKCN1                                                         
         GOTOR AWRKM,WRKMGTQ                                                    
GTWRKCN1 MVI   PARDLEN,1           WORK CODE & NAME                             
         MVI   PARDATA,C' '                                                     
         MVC   WORK,SPACES                                                      
         LA    R2,WORK                                                          
         TM    OPT5,OPT5WCCO       TEST PRINT WORKCODE                          
         JNO   *+14                                                             
         MVC   0(2,R2),BTRNWC                                                   
         LA    R2,3(R2)                                                         
         TM    OPT5,OPT5WCDE       TEST PRINT WC DESCRIPTION                    
         JNO   GTWRKCN3                                                         
         TM    OPT3,OPT3PWCL       PRINT LONG WC NAME                           
         JZ    GTWRKCN2            NO, USE SHORT                                
         MVC   0(L'WRKCNME,R2),WRKCNME                                          
         LA    R2,L'WRKCNME+1(R2)                                               
         J     GTWRKCN3                                                         
GTWRKCN2 MVC   0(L'WRKCDSC,R2),WRKCDSC                                          
         LA    R2,L'WRKCDSC+1(R2)                                               
GTWRKCN3 CLI   WORK,C' '                                                        
         JNH   XIT                                                              
         LA    R1,WORK                                                          
         SR    R2,R1                                                            
         STC   R2,PARDLEN                                                       
         BCTR  R2,0                                                             
         EXRL  R2,GTWRMVDW                                                      
         J     XIT                                                              
                                                                                
GTWRMVDW MVC   PARDATA(0),WORK                                                  
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
***********************************************************************         
* PUT RECORDS TO SORT                                                 *         
* P1= BYTE 0       -  RECORD TYPE                                     *         
*       BYTE 1-3   -  A(IO)                                           *         
***********************************************************************         
                                                                                
SORT     NTR1  LABEL=*                                                          
         MVC   BYTE,0(R1)          RECORD TYPE                                  
         SR    R2,R2                                                            
         ICM   R2,7,1(R1)          A(IO)                                        
         USING BEDRECD,R2                                                       
                                                                                
         L     R0,ASRTWK           CLEAR SORT AREA                              
         LHI   R1,SRDLNQ                                                        
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R3,ASRTWK           SET SORT RECORD KEY                          
         USING SORTD,R3                                                         
         MVC   SKRQN,REQNUM        REQUEST NUMBER                               
         MVI   SKRQRT,SKRQBLQ      REPORT TYPE - BILLS                          
         MVI   BILKBIL,BILKBILQ                                                 
         MVC   BILKCLI,CLI         CLIENT                                       
         MVC   BILKPRD,PRD         PRODUCT                                      
         MVC   BILKJOB,JOB         JOB                                          
         MVC   BILKRET,RETLSQN     RETAILER SEQUENCE NUMBER                     
         MVC   BILKRT,BYTE         RECORD TYPE                                  
         MVC   BILKRKEY,BEDRECD    RECORD KEY                                   
                                                                                
         TM    PGMAQ,PGMALL27      TEST 27                                      
         JZ    SORT9               NO,                                          
                                                                                
         CLI   BILKRT,BILKRBHD     FOR HEADER THRU END                          
         JL    SORT3                                                            
         CLI   BILKRT,BILKRBED                                                  
         JH    SORT3                                                            
         MVC   BILKLVL,BEDKLVL     ADD LEVEL TO SORT                            
         MVC   BILKGRP,CPJ         CLIENT/PRODUCT/JOB                           
         TM    RQLV,LVA+LVB        TEST REQUEST LEVEL A OR B                    
         JZ    SORT3               NO,                                          
         XC    BILKGRP+6(6),BILKGRP+6  CLEAR JOB                                
         TM    RQLV,LVB            TEST REQUEST LEVEL B                         
         JO    SORT3                                                            
         XC    BILKGRP+3(3),BILKGRP+3                                           
                                                                                
SORT3    TM    PGLV,LVA            TEST LEVEL A                                 
         JO    SORT5               YES,                                         
         TM    PGLV,LVB            LAST FOR LEVEL B                             
         JNO   SORT9                                                            
         XC    BILKJOB,BILKJOB     PRODUCT HEADER                               
         CLC   BEDKWHER,=AL2(BEDKWBTQ)                                          
         JE    SORT4                                                            
         CLC   BEDKWHER,=AL2(BEDKWGSQ)                                          
         JNE   SORT9                                                            
SORT4    MVI   BILKJOB,X'FF'                                                    
         J     SORT9                                                            
                                                                                
SORT5    XC    BILKPRD,BILKPRD     END OF LEVEL A                               
         XC    BILKJOB,BILKJOB                                                  
         CLC   BEDKWHER,=AL2(BEDKWBTQ)                                          
         JE    SORT6                                                            
         CLC   BEDKWHER,=AL2(BEDKWGSQ)                                          
         JNE   SORT9                                                            
SORT6    MVI   BILKPRD,X'FF'                                                    
                                                                                
SORT9    LLH   R1,ACCRLEN-ACCRECD(R2)   GET RECORD LENGTH                       
         LA    RF,SRLNQ(R1)                                                     
         STCM  RF,3,SRLEN                                                       
                                                                                
         LA    R0,SRDATA           MOVE RECORD TO SORT AREA                     
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR ASORT,SORTPUTQ                                                   
         LLC   R0,RECNO                                                         
         AHI   R0,1                                                             
         STC   R0,RECNO                                                         
         J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET PANEL DATA FOR A BFMELD                                         *         
*  PARM 1=A(BFMELD)                                                   *         
***********************************************************************         
                                                                                
GETPD    NTR1  LABEL=*                                                          
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         L     R2,0(R1)                                                         
         USING BFMELD,R2                                                        
                                                                                
         LLC   R5,BFMPNLC          FIELD NUMBER                                 
         MHI   R5,FLDTABL                                                       
         LARL  RE,FLDTAB                                                        
         AR    R5,RE               R5=A(FIELD ENTRY)                            
                                                                                
         USING FLDTABD,R5                                                       
         ICM   RF,15,FLDHOOK       TEST SPECIAL HOOK CODE                       
         JZ    GETPD3                                                           
         GOTOR (RF),DMCB,BFMELD,FLDTABD                                         
         JE    XIT                                                              
         J     GETPDNO                                                          
                                                                                
GETPD3   LLH   RF,FLDRADR          GET DISPLACEMENT TO ROUTINE                  
         XR    R1,R1                                                            
         ICM   R1,1,FLDGLEN        TEST DATA LENGTH                             
         JZ    GETPD9              NO LENGTH, MUST BE ROUTINE                   
         LA    R6,AMTS                                                          
         XR    RE,RE                                                            
         SLDL  RE,20               BASE INTO RE                                 
         SRL   RF,20               DISPL IN RF                                  
         EXRL  RE,GETPLRE0         BASE ADDRESS NOW IN RE                       
         AR    RF,RE               ADD TO DISPL                                 
                                                                                
         CLI   FLDCNV,0            TEST CONVERSION ROUTINE                      
         JNE   GETPD7                                                           
         BCTR  R1,0                                                             
         EXRL  R1,GETPMVTF                                                      
         LA    R5,BFMTEXT+1(R1)                                                 
         GOTOR GETPDLN                                                          
         J     XIT                                                              
                                                                                
GETPD7   LA    R4,WORK             MAKE DUMMY PARELD FOR CONVERT                
         USING PARELD,R4                                                        
         MVC   PARFLD,BFMPNLC                                                   
         BCTR  R1,0                                                             
         EXRL  R1,GETPMVDF                                                      
         GOTOR CNVDATA,DMCB,PARELD,BFMELD                                       
         SR    R1,R1                                                            
         ICM   R1,1,TEXTLEN                                                     
         JNZ   *+12                                                             
         LA    R1,1                                                             
         MVI   TEXT,C' '                                                        
         BCTR  R1,0                                                             
         EXRL  R1,GETPMVTT                                                      
         LA    R5,BFMTEXT+1(R1)                                                 
         GOTOR GETPDLN                                                          
         J     XIT                                                              
                                                                                
GETPD9   L     R6,ADICO            R6=A(DICTIONARY)                             
         USING DICD,R6                                                          
         LA    R5,BFMTEXT          R5 TO TEXT AREA                              
         LARL  RE,GETPD                                                         
         AR    RF,RE               GO TO SPECIAL ROUTINE                        
         BR    RF                                                               
                                                                                
GETPMVTF MVC   BFMTEXT(0),0(RF)                                                 
GETPLRE0 LR    RE,0                                                             
GETPMVDF MVC   PARDATA(0),0(RF)                                                 
GETPMVTT MVC   BFMTEXT(0),TEXT                                                  
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                  SPECIAL ROUTINES FOR PANEL CODES             
AGYJ     CLI   AGYLCPJ,C' '        AGENCY JOB                                   
         JNH   GETPDNO             SKIP FIELD                                   
         MVC   0(L'DD@AGJOB,R5),DD@AGJOB                                        
         AHI   R5,L'DD@AGJOB                                                    
         CLI   0(R5),C' '                                                       
         JH    *+10                                                             
         BCTR  R5,0                                                             
         J     *-10                                                             
         MVC   2(L'AGYLCPJ,R5),AGYLCPJ                                          
         AHI   R5,2+L'AGYLCPJ                                                   
         GOTOR GETPDLN             GET DATA LENGTH                              
         GOTOR SOFTCOL             GET COLUMN/ROW                               
         J     XIT                                                              
*                                  BILLING ADDRESS                              
BA       TM    RQLV,LVA+LVB        TEST 27 REQUEST @ CLI OR PROD LEVEL          
         JNZ   *+12                YES, SKIP JOB ADDRESS                        
         ICM   R4,15,ADACCADD      FROM JOB                                     
         JNZ   BA02                                                             
         TM    RQLV,LVA            TEST 27 REQUEST @ CLI LEVEL                  
         JNZ   *+12                YES, SKIP PRODUCT LEVEL                      
         ICM   R4,15,ADLVBADD      OR PRODUCT                                   
         JNZ   BA02                                                             
         ICM   R4,15,ADLVAADD      OR CLIENT                                    
         JZ    GETPDNO                                                          
         USING ADRELD,R4                                                        
BA02     LLC   R0,ADRNUM                                                        
         LA    RE,ADRADD1                                                       
BA03     MVC   0(L'ADRADD1,R5),0(RE)                                            
         AHI   RE,L'ADRADD1                                                     
         AHI   R5,L'ADRADD1+1                                                   
         JCT   R0,BA03                                                          
         J     GETPDX              SET LENGTH                                   
         DROP  R4                                                               
                                                                                
BD       DS    0H                  BILL DATE                                    
         TM    OPT4,OPT4SDAT       USE MMMDD/YY FORMAT?                         
         JO    BD8                 YES                                          
         GOTOR DATCON,DMCB,(2,BILDT2),(10,0(R5))                                
         J     GETPDL                                                           
                                                                                
BD8      DS    0H                  BILL DATE                                    
         GOTOR DATCON,DMCB,(2,BILDT2),(8,0(R5))                                 
         J     GETPDL                                                           
                                                                                
BST      TM    PGMSTA,PGMDFT       BILL STATUS                                  
         JZ    GETPDNO                                                          
         MVC   0(L'DD@DRAFT,R5),DD@DRAFT                                        
         AHI   R5,L'DD@DRAFT                                                    
         J     GETPDX                                                           
                                                                                
BTYP     MVC   0(L'JXBLNME,R5),JXBLNME       BILL TYPE NAME                     
         AHI   R5,L'JXBLNME                                                     
         CLI   BILTCDE,JXBLPEST              TEST % ESTIMATE                    
         JNE   GETPDX                                                           
         LA    R1,BFMTEXT                                                       
         CLC   0(6,R1),=C'PC EST'                                               
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-14                                                             
         MVC   0(12,R1),=C'PCT ESTIMATE'                                        
         LA    R5,15(R1)                                                        
         J     GETPDX                                                           
                                                                                
BMN      TM    PGMAQ,PGMALL27           TEST CLIENT BILL                        
         JNZ   BMN5                     YES,                                    
         USING PMDELD,R4                                                        
         L     R4,APMDEL                                                        
         TM    PMDSTAT,PMDSPNIH         TEST MEDIA NAME IN HEADLINE             
         JNO   BMN3                     NO,                                     
         MVC   0(L'MEDNS,R5),MEDNS      MEDIA NAME                              
         AHI   R5,L'MEDNS-1                                                     
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         MVC   2(L'DD@BIL,R5),DD@BIL                                            
         AHI   R5,2+L'DD@BIL                                                    
         J     GETPDX                                                           
                                                                                
BMN3     MVC   2(L'DD@PRDBL,R5),DD@PRDBL  PRODUCTION BILL                       
         AHI   R5,L'DD@PRDBL                                                    
         J     GETPDX                                                           
                                                                                
BMN5     L     R3,ABEDR                                                         
         USING BEDRECD,R3                                                       
         CLC   BEDKLVL,=AL2(BFMKLSMQ)          TEST SUMMARY                     
         JNE   BMN7                            NO,                              
         MVC   2(L'DD@INV,R5),DD@INV          'INVOICE'                         
         AHI   R5,2+L'DD@INV                                                    
         J     GETPDX                                                           
                                                                                
BMN7     MVC   2(L'DD@INVDE,R5),DD@INVDE      'INVOICE DETAIL'                  
         AHI   R5,2+L'DD@INVDE                                                  
         J     GETPDX                                                           
         DROP  R3,R4                                                            
                                                                                
CD       MVC   0(4,R5),=C'*CD*'                                                 
         J     GETPDL                                                           
                                                                                
CDE      MVC   0(3,R5),=C'CD='                                                  
         J     GETPDL                                                           
                                                                                
CON      L     R4,ADCMPNAM         COMPANY NAME                                 
         USING NAMELD,R4                                                        
         LLC   RF,NAMLN                                                         
         SHI   RF,(NAMLN1Q+1)                                                   
         EXRL  RF,CONMV5N                                                       
         LA    R5,1(RF,R5)                                                      
         J     GETPDX                                                           
                                                                                
CONMV5N  MVC   0(0,R5),NAMEREC                                                  
         DROP  R4                                                               
                                                                                
COA      ICM   R4,15,ADCMPADD      COMPANY ADDRESS                              
         JZ    GETPDNO                                                          
         USING ADRELD,R4                                                        
         LLC   RF,ADRLN                                                         
         SHI   RF,ADRADD1+1-ADRELD                                              
         EXRL  RF,COAMV5A                                                       
         LA    R5,1(RF,R5)                                                      
         J     GETPDX                                                           
                                                                                
COAMV5A  MVC   0(0,R5),ADRADD1                                                  
         DROP  R4                                                               
                                                                                
DC       MVC   0(14,R5),RECVAC+1                                                
         J     GETPDL                                                           
                                                                                
DD       DS    0H                  DUE DATE                                     
         GOTOR DATCON,DMCB,(X'02',DUEDT2),(17,0(R5))                            
         J     GETPDL                                                           
                                                                                
EST      DS    0H                  ESTIMATE AMOUNT                              
         MVC   0(L'DD@EST,R5),DD@EST  ESTIMATE(NET)= $$$.00                     
         AHI   R5,L'DD@EST             OR                                       
         MVI   0(R5),C'('                                                       
         LA    R5,1(R5)                                                         
         LA    R4,DD@NET           ESTIMATE(GROSS)= $$$.00                      
         LA    RE,L'DD@NET                                                      
         LA    RF,JXCUNEST         CURRENT NET ESTIMATE                         
         TM    OPT1,OPT1ESTN                                                    
         JO    *+16                                                             
         LA    R4,DD@GROSS                                                      
         LA    RE,L'DD@GROSS                                                    
         LA    RF,JXCUGEST         CURRENT GROSS ESTIMATE                       
         BCTR  RE,0                                                             
         EXRL  RE,ESTMV54          'NET' OR 'GROSS'                             
         LA    R5,3(RE,R5)                                                      
         CLI   0(R5),C' '                                                       
         JH    *+10                                                             
         BCTR  R5,0                                                             
         J     *-10                                                             
         MVC   1(2,R5),=C')='                                                   
         ZAP   DUB,0(L'JXCUNEST,RF)                                             
         CURED (P8,DUB),(10,4(R5)),2,ALIGN=LEFT                                 
         LA    R5,15(R5)                                                        
         J     GETPDX                                                           
                                                                                
ESTMV54  MVC   0(0,R5),0(R4)                                                    
                                                                                
CL       DS    0H                  JOB CLOSE DATE                               
         GOTOR DATCON,DMCB,(X'01',JXCLSDT),(17,0(R5))                           
         J     GETPDL                                                           
                                                                                
CPJSEP   MVC   0(L'CLI,R5),CLI     JOB CODE (CLI/PRO/JOB)                       
         GOTOR CPJSEP3                                                          
         MVC   0(L'PRD,R5),PRD                                                  
         GOTOR CPJSEP3                                                          
         MVC   0(L'JOB,R5),JOB                                                  
         GOTOR CPJSEP3                                                          
         J     GETPDX                                                           
CPJSEP3  AHI   R5,1                                                             
         CLI   0(R5),C' '                                                       
         JH    CPJSEP3                                                          
         MVI   0(R5),C'/'                                                       
         AHI   R5,1                                                             
         BR    RE                                                               
                                                                                
JC2      MVI   LINNUM,11           EXTRA JOB COMMENTS                           
         L     R3,AXTRA            EXTRA JOB COMMENT BUFFER                     
         J     JCO3                                                             
                                                                                
JCO      MVC   LINNUM,BFMTOP       JOB COMMENTS                                 
         L     R3,ACMNT            COMMENT BUFFER(1 BYTE LEN,DATA)              
                                                                                
JCO3     CLI   0(R3),0             TEST END OF BUFFER                           
         JNE   JCO7                                                             
         J     GETPDNO             DON'T ADD ELEMENT                            
                                                                                
JCO5     LLC   RF,0(R3)                                                         
         LA    R3,1(RF,R3)                                                      
         LLC   RF,LINNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,LINNUM                                                        
         J     JCO3                                                             
                                                                                
JCO7     MVC   BFMTOP,LINNUM       SET NEXT LINE                                
         LLC   R5,0(R3)                                                         
         STCM  R5,3,BFMTLEN        SET LENGTH OF TEXT                           
         CHI   R5,BFMTMAXL         TEST HIGHER THAN MAX                         
         JNH   *+8                                                              
         LA    R5,BFMTMAXL         USE MAX                                      
         EXRL  R5,JCOMVTX                                                       
         LA    RF,BFMLNQ                                                        
         AR    RF,R5               ADD TO ELEMENT LENGTH                        
         STC   RF,BFMLN            SET ELEMENT LENGTH                           
         GOTOR ADDEL,DMCB,ABEDN,BFMELD   ADD ELEMENT TO OUTPUT RECORD           
                                                                                
         CLM   R5,3,BFMTLEN        TEST BFMELD NOT BIG ENOUGH FOR TEXT          
         JNL   JCO5                                                             
         ICM   R5,3,BFMTLEN                                                     
         SHI   R5,BFMTMAXL         GET REMAINING LENGTH                         
E        USING BFXELD,BFSWRK                                                    
         MVI   E.BFXEL,BFXELQ      BUILD EXTRA TEXT ELEMENT                     
         LA    RE,BFXLNQ(R5)                                                    
         STC   RE,E.BFXLN          SET ELEMENT LENGTH                           
         EXRL  R5,JCOMVTXX         MOVE EXTRA TEXT                              
         GOTOR ADDEL,DMCB,ABEDN,E.BFXELD  ADD ELEMENT TO OUTPUT RECORD          
         J     JCO5                                                             
                                                                                
JCOMVTX  MVC   BFMTEXT(0),1(R3)                                                 
JCOMVTXX MVC   E.BFXTEXT(0),BFMTEXT+BFMTMAXL                                    
         DROP  E                                                                
                                                                                
OP       DS    0H                  JOB OPEN DATE                                
         GOTOR DATCON,DMCB,(X'01',JXOPNDT),(17,0(R5))                           
         J     GETPDL                                                           
                                                                                
AN       L     RF,ADMASTC          ORIGIN NAME                                  
         MVC   0(L'MCORIGIN,R5),MCORIGIN-MASTD(RF)                              
         AHI   R5,L'MCORIGIN                                                    
         J     GETPDX                                                           
                                                                                
AA       L     RF,ADMASTC          ORIGIN ADDRESS                               
         MVC   0(L'MCORIGAD,R5),MCORIGAD-MASTD(RF)                              
         AHI   R5,L'MCORIGAD                                                    
         J     GETPDX                                                           
                                                                                
AMTDU    DS    0H                  AMOUNT DUE                                   
         TM    PGMSTA,PGMUNB       TEST UNBILL?                                 
         JO    AMTREV                                                           
         TM    OPTC,OPTCUSD                                                     
         JNO   AMTDU2                                                           
         LA    R1,L'DD@TAMTU        'TOTAL AMOUNT USD'                          
         LA    RF,DD@TAMTU                                                      
         BCTR  R1,0                                                             
         EXRL  R1,AMTDMV5F                                                      
         LA    R5,3(R1,R5)                                                      
         J     AMTDU4                                                           
*                                                                               
AMTDU2   LA    R1,L'DD@TAMTD       'TOTAL AMOUNT DUE'                           
         LA    RF,DD@TAMTD                                                      
         BCTR  R1,0                                                             
         EXRL  R1,AMTDMV5F         SET TEXT IN ELEMENT                          
         LA    R5,3(R1,R5)                                                      
*                                                                               
AMTDU4   CLI   BILTCDE,JXBLPEST    TEST % ESTIMATE                              
         JNE   GETPDX                                                           
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,2                                                             
         MVC   0(L'JXBLNME,R5),JXBLNME                                          
         CLC   0(6,R5),=C'PC EST'                                               
         JE    *+12                                                             
         AHI   R5,1                                                             
         J     *-14                                                             
         MVC   0(12,R5),=C'PCT ESTIMATE'                                        
         J     GETPDL                                                           
                                                                                
AMTDMV5F MVC   0(0,R5),0(RF)                                                    
                                                                                
AMTREV   MVC   0(L'DD@TOREV,R5),DD@TOREV                                        
         AHI   R5,L'DD@TOREV-1                                                  
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,2                                                             
         MVC   0(2,R5),JXRRNUM                                                  
         MVI   2(R5),C'-'                                                       
         MVC   3(4,R5),JXRRNUM+2                                                
         AHI   R5,8                                                             
         GOTOR DATCON,DMCB,(2,JXRRDT2),(8,0(R5))                                
         J     GETPDL                                                           
                                                                                
AUTH     DS    0H                   AUTHORIZATION                               
         TM    JXSTA2,JOBSFUN       TEST JOB FUNDED                             
         JNO   GETPDNO              SKIP IT, IF NOT                             
         L     R4,ADGOBLOC                                                      
         L     R4,GOAEXT-GOBLOCKD(R4)                                           
         USING GOXBLKD,R4                                                       
         CLI   GOPAB,YES           TEST PRINT AUTHORIZATION                     
         JNE   GETPDNO             NO, SKIP IT                                  
         MVC   0(L'GOANT,R5),GOANT                                              
         AHI   R5,L'GOANT-1        FIND LENGTH OF DESCRIPTION                   
         LA    R0,L'GOANT                                                       
         CLI   0(R5),C' '                                                       
         JH    *+14                                                             
         BCTR  R5,0                                                             
         JCT   R0,*-10                                                          
         J     GETPDNO             NO DESCRIPTION, SKIP IT                      
                                                                                
         ICM   RF,15,AJFNEL        GET AUTHORIZATION NUMBER                     
         JZ    AUTH3                                                            
         USING JFNELD,RF                                                        
         MVI   1(R5),C' '                                                       
         MVC   2(L'JFNNUM,R5),JFNNUM                                            
         AHI   R5,1+L'JFNNUM                                                    
         LA    R0,1+L'JFNNUM(R1)                                                
         CLI   0(R5),C' '                                                       
         JH    AUTH3                                                            
         BCTR  R5,0                                                             
         JCT   R0,*-10                                                          
         DROP  R4,RF                                                            
                                                                                
AUTH3    GOTOR GETPDLN             SET TEXT LENGTH                              
         GOTOR SOFTCOL                                                          
         J     XIT                                                              
                                                                                
SHIP     DS    0H                  SHIP DATE                                    
         GOTOR DATCON,DMCB,(X'02',SHPDT2),(17,0(R5))                            
         J     GETPDL                                                           
                                                                                
PG       MVC   0(L'DD@PAGE,R5),DD@PAGE  PAGE NUMBER                             
         AHI   R5,L'DD@PAGE                                                     
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         MVC   2(3,R5),=C'???'                                                  
         J     GETPDL                                                           
                                                                                
PONBJ    L     R3,ADLVCSUP         JOB LEVEL PRINT ON BILLS                     
         J     PONB2                                                            
                                                                                
PONB     L     R3,ADPROFIL         PRINT ON BILLS                               
         TM    RQLV,LVA            TEST A27 CLIENT LEVEL                        
         JNO   *+8                 NO,                                          
         L     R3,ADLVASUP         YES, USE CLIENT PONB                         
         TM    RQLV,LVB            TEST A27 PRODUCT LEVEL                       
         JNO   *+8                 NO,                                          
         L     R3,ADLVBSUP         YES,USE PRODUCT PONB                         
         USING PPRELD,R3                                                        
PONB2    MVC   0(L'PPRBILLP,R5),PPRBILLP                                        
         OC    0(L'PPRBILLP,R5),SPACES                                          
         AHI   R5,L'PPRBILLP-1                                                  
         LA    R0,L'PPRBILLP       GET LENGTH OF PRINT ON BILLS                 
         CLI   0(R5),C' '                                                       
         JH    GETPDX                                                           
         BCTR  R5,0                                                             
         JCT   R0,*-10                                                          
         J     GETPDNO                                                          
*        LARL  RF,SOFTAB           TAKE FIRST SOFT FIELD                        
*        OI    2(RF),X'80'                                                      
*        LA    RF,L'SOFTAB(RF)     LOOK AT NEXT FIELD                           
*        LLC   R1,BFMLEFT          START OF THIS FIELD                          
*        AR    R0,R1               ADDED TO LENGTH                              
*        ICM   R1,1,1(RF)          START OF NEXT FIELD                          
*        CR    R0,R1               IF LONGER THAN WIDTH                         
*        JL    *+8                                                              
*        OI    2(RF),X'80'                                                      
         J     GETPDX                                                           
         DROP  R3                                                               
                                                                                
TD       DS    0H                  * TODAY'S DATE *                             
         GOTOR DATCON,DMCB,(X'01',TODAY1),(17,0(R5))                            
         J     GETPDL                                                           
                                                                                
RTLRN    L     R3,CRTLR            RETAILER NAME                                
         USING RTLD,R3                                                          
         MVC   0(L'RTLNME,R5),RTLNME                                            
         AHI   R5,L'RTLNME                                                      
         J     GETPDX                                                           
                                                                                
RTLRA    L     R3,CRTLR            RETAILER ADDRESS                             
         CLI   RTLADDR,C' '        TEST ANY RETAILER ADDRESS                    
         JNH   BA                  NO, USING BILLING ADDRESS                    
         MVC   0(L'RTLADDR,R5),RTLADDR                                          
         AHI   R5,L'RTLADDR        IS THERE AN ADDRESS?                         
         J     GETPDX                                                           
                                                                                
RTLUNTS  DS    0H                  RETAILER UNITS   (NN.NN UNITS)               
         TM    JXRTLSTA,JXRTLSUN   TEST UNITS                                   
         JNO   GETPDNO                                                          
         TM    PGMSTA,PGMUNB       IS IT UNBILLING?                             
         JO    GETPDNO                                                          
         L     R3,CRTLR                                                         
         CP    RTLUNT,PZERO                                                     
         JE    GETPDNO                                                          
         ZAP   DUB,RTLUNT                                                       
         MVI   0(R5),C'('                                                       
         AHI   R5,1                                                             
         CURED (P8,DUB),(10,0(R5)),2,ALIGN=LEFT                                 
         AHI   R5,1                                                             
         AR    R5,R0                                                            
         MVC   0(L'DD@UNITS,R5),DD@UNITS                                        
         AHI   R5,L'DD@UNITS+2                                                  
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         MVI   1(R5),C')'                                                       
         AHI   R5,1                                                             
         J     GETPDX                                                           
         DROP  R3                                                               
                                                                                
UF1      LA    R0,1                USER FIELD 1-10                              
         J     UFX                                                              
UF2      LA    R0,2                                                             
         J     UFX                                                              
UF3      LA    R0,3                                                             
         J     UFX                                                              
UF4      LA    R0,4                                                             
         J     UFX                                                              
UF5      LA    R0,5                                                             
         J     UFX                                                              
UF6      LA    R0,6                                                             
         J     UFX                                                              
UF7      LA    R0,7                                                             
         J     UFX                                                              
UF8      LA    R0,8                                                             
         J     UFX                                                              
UF9      LA    R0,9                                                             
         J     UFX                                                              
UF10     LA    R0,10                                                            
         J     UFX                                                              
                                                                                
UFX      L     R4,JXAUSRF          R4=A(USER FIELD TABLE)                       
         USING USRFD,R4                                                         
         SR    RF,RF                                                            
UFX3     CLI   0(R4),EOT           TEST EOT                                     
         JE    GETPDNO                                                          
         LA    R3,USRFLM                                                        
         USING UFSELD,R3                                                        
         TM    UFSSTAT,UFSSSHBI    SHOW ON BILLS?                               
         JNO   UFX5                                                             
         CLI   USRFDLEN,0          TEST DATA LENGTH                             
         JE    UFX5                                                             
         JCT   R0,UFX5                                                          
         J     UFX7                                                             
UFX5     LA    R4,USRFLNQ(R4)                                                   
         J     UFX3                                                             
                                                                                
UFX7     MVC   0(L'UFSDESC,R5),UFSDESC                                          
         AHI   R5,L'UFSDESC                                                     
         MVI   0(R5),C' '                                                       
         AHI   R5,1                                                             
         SR    RF,RF                                                            
         ICM   RF,1,USRFDLEN                                                    
         JNP   GETPDNO                                                          
         BCTR  RF,0                                                             
         EXRL  RF,UFXMV5U                                                       
         LA    R5,1(RF,R5)                                                      
         GOTOR GETPDLN                                                          
         GOTOR SOFTCOL                                                          
         J     XIT                                                              
                                                                                
UFXMV5U  MVC   0(0,R5),UFSDATA                                                  
         DROP  R3                                                               
                                                                                
PREBN    DS    0H                  GET PREBILL NAME                             
         TM    OPT9,OPT9UWDP       USE WORKCODE DESCRIPTION                     
         JNO   PREBN3              NO, USE STANDARD                             
         CLI   PRENAME,C' '        TEST ANY PREBILL NAME                        
         JE    PREBN3              NO, USE STANDARD                             
         MVC   0(L'PRENAME,R5),PRENAME                                          
         AHI   R5,L'PRENAME                                                     
         J     GETPDX                                                           
                                                                                
PREBN3   MVC   0(L'DD@MMPS,R5),DD@MMPS  "MONTHLY MINIMUM...."                   
         AHI   R5,L'DD@MMPS                                                     
         J     GETPDX                                                           
                                                                                
PROFS    L     R6,ADICO            PROFESSIONAL SERV.                           
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         MVC   0(L'DD@PROFS,RF),DD@PROFS                                        
         AHI   RF,L'DD@PROFS+1                                                  
         TM    OPT8,OPT8OSTF       TEST 'OMIT STAFF'                            
         JO    PROFS3              YES,                                         
         MVC   0(L'DD@STAFF,RF),DD@STAFF                                        
         AHI   RF,L'DD@STAFF+1                                                  
                                                                                
PROFS3   MVC   0(L'DD@SERVE,RF),DD@SERVE                                        
         AHI   RF,L'DD@SERVE                                                    
         GOTOR ADSQUASH,DMCB,WORK,L'WORK                                        
         LLC   R1,DMCB+7                                                        
         BCTR  R1,0                                                             
         EXRL  R1,PROFMV5W                                                      
         LA    R5,2(R1,R5)                                                      
         J     GETPDX                                                           
                                                                                
PROFMV5W MVC   0(0,R5),WORK                                                     
                                                                                
TTLDA    DS    0H                  TITLE DATE                                   
         TM    OPT9,OPT9SPSD       TEST SUPPRESS DATE                           
         JO    GETPDNO             YES,                                         
         LA    RF,0(R5)                                                         
         GOTOR SETTDA              SET TITLE DATES                              
         AHI   R5,50                                                            
         J     GETPDX                                                           
                                                                                
SETTDA   MVC   0(L'TTLSDAY8,RF),TTLSDAY8                                        
         OC    0(L'TTLSDAY8,RF),SPACES                                          
         LA    RF,1+L'TTLSDAY8(RF)                                              
         OC    TTLEDAY8,TTLEDAY8                                                
         BZR   RE                                                               
         LA    RF,L'TTLSDAY8-1(RF)                                              
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-10                                                             
         LA    RF,2(RF)                                                         
         MVC   0(L'DD@TO,RF),DD@TO                                              
         LA    RF,L'DD@TO-1(RF)                                                 
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-10                                                             
         LA    RF,2(RF)                                                         
         MVC   0(L'TTLEDAY8,RF),TTLEDAY8                                        
         OC    0(L'TTLEDAY8,RF),SPACES                                          
         LA    RF,L'TTLEDAY8(RF)                                                
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-10                                                             
         BR    RE                                                               
                                                                                
HANDF    TM    OPT7,OPT7PFEE        PRINT 'FEE'                                 
         JNO   HANDF1                                                           
         LA    RF,DD@FEE                                                        
         LA    R1,L'DD@FEE                                                      
         J     HANDF3                                                           
HANDF1   TM    OPT7,OPT7PCOM        PRINT 'COMMISSION'                          
         JNO   HANDF2                                                           
         LA    RF,DD@CMN                                                        
         LA    R1,L'DD@CMN                                                      
         JNO   HANDF3                                                           
HANDF2   LA    RF,DD@HAND           DEFAULT IS HANDLING CHARGE                  
         LA    R1,L'DD@HAND                                                     
HANDF3   BCTR  R1,0                                                             
         EXRL  R1,HANDMV5F                                                      
         LA    R5,2(R1,R5)                                                      
         J     GETPDX                                                           
                                                                                
HANDMV5F MVC   0(0,R5),0(RF)                                                    
                                                                                
GETPDL   LA    R5,15(R5)           SET FOR SMALL TEXT                           
GETPDX   GOTOR GETPDLN                                                          
         J     XIT                                                              
                                                                                
GETPDLN  LA    R0,BFMTEXT                                                       
GETPDLN1 CLI   0(R5),C' '          GET LENGTH OF TEXT FIELD                     
         JH    GETPDLN2                                                         
         BCTR  R5,0                                                             
         CR    R5,R0                                                            
         JL    GETPDNO                                                          
         J     GETPDLN1                                                         
GETPDLN2 LA    R5,1(R5)                                                         
         SR    R5,R0                                                            
         STCM  R5,3,BFMTLEN        SET  LENGTH IN ELEMENT                       
         BR    RE                                                               
                                                                                
GETPDNO  MVI   BFMTOP,0            SKIP IT                                      
         J     XIT                                                              
         DROP  R2,R4,R6,R9                                                      
         EJECT                                                                  
***********************************************************************         
* GET NEXT AVAILABLE COL/ROW FROM TABLE                               *         
***********************************************************************         
                                                                                
         USING BFMELD,R2                                                        
SOFTCOL  MVI   BFMLEFT,0           CLEAR COLUMN                                 
         MVI   BFMTOP,0            AND ROW                                      
         LARL  RF,SOFTAB           FIND NEXT AVAILABLE ENTRY                    
SOFTCOL1 CLI   2(RF),0             TEST AREA ALREADY USED                       
         JE    SOFTCOL2                                                         
         LA    RF,L'SOFTAB(RF)                                                  
         CLI   0(RF),EOT                                                        
         JNE   SOFTCOL1                                                         
         BR    RE                                                               
                                                                                
SOFTCOL2 OI    2(RF),X'80'         SET USED TXFLG                               
         MVC   BFMTOP,0(RF)        SET ROW                                      
         MVC   BFMLEFT,1(RF)       AND COLUMN                                   
         LA    R1,BFMTEXT+SOFTWTH+1 TEST ITEM IS 2 WIDE                         
         CLC   0(SOFTWTH,R1),SPACES                                             
         BER   RE                                                               
         LA    RF,L'SOFTAB(RF)                                                  
         CLI   0(RF),EOT                                                        
         BER   RE                                                               
         OI    2(RF),X'80'         SET NEXT IS USED TXFLG                       
         BR    RE                                                               
         DROP  R2                                                               
                                                                                
SOFTWTH  EQU   44                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT AMOUNT ONTO BFMELD                                *         
*                                                                     *         
* NTRY: P1 BYTE 0=INDICATOR BYTE (FAPINDS)                            *         
*             1-3=A(BFMELD)                                           *         
*       P2 BYTE 0=LENGTH OF PACKED INPUT FIELD                        *         
*             1-3=A(PACKED INPUT FIELD)                               *         
*                                                                     *         
* EXIT:   BFMTEXT=OUTOUT                                              *         
*         BFMTLEN=L(OUTPUT)                                           *         
*         BFMWTH =WIDTH OF FIELD                                      *         
*         BFMTLN =NEW L(ELEMENT)                                      *         
***********************************************************************         
                                                                                
FMTAMT   STM   RE,R7,SVREG                                                      
         MVC   FAPARMS,0(R1)                                                    
                                                                                
         XR    R3,R3                                                            
         ICM   R3,7,FAPABFM        R3=A(BFMEL)                                  
         USING BFMELD,R3                                                        
         MVC   BFMTEXT(50),SPACES                                               
         XR    RF,RF                                                            
         ICM   RF,7,FAPAAMT        RF=A(AMOUNT)                                 
         SR    R1,R1                                                            
         ICM   R1,1,FAPINPL        R1=LENGTH OF INPUT                           
         BCTR  R1,0                                                             
         EXRL  R1,FMTAZAP                                                       
         TM    BFMINDS1,BFMIREV    REVERSE THE SIGN                             
         JNO   *+10                                                             
         MP    FAPL8,PNEG1                                                      
         TM    BFMINDS2,BFMIPZA    TEST PRINT ZERO AMOUNT                       
         JNO   *+8                                                              
         OI    FAPINDS,FAPIPZA                                                  
                                                                                
         CURED (P8,FAPL8),(19,BFMTEXT),2,MINUS=YES,                    *        
               COMMAS=Y,ALIGN=LEFT,ZERO=BLANK                                   
         ORG   *-2                                                              
                                                                                
         TM    FAPINDS,FAPIPZA     PRINT ZERO AMOUNTS?                          
         JNO   *+8                                                              
         NI    CURPEDT1-CURPARMD(R1),ALL-(CURPZERB)                             
         TM    FAPINDS,FAPINOC     NO COMMAS?                                   
         JNO   *+8                                                              
         NI    CURPEDT1-CURPARMD(R1),ALL-(CURPCOMY)                             
         ZAP   DUB,FAPL8                                                        
         OI    DUB+7,X'0F'                                                      
         CP    DUB,P1MIL           TEST OVER ONE MILLION                        
         JL    *+8                                                              
         NI    CURPEDT1-CURPARMD(R1),ALL-(CURPCOMY)                             
                                                                                
         GOTOR (RF)                NOW GO TO CUREDIT                            
                                                                                
         LTR   R1,R0               TEST LENGTH OF OUTPUT                        
         JNZ   *+8                 NOT ZERO                                     
         MVI   BFMTOP,0            SET TO IGNORE ELEMENT                        
         STCM  R1,3,BFMTLEN                                                     
         J     LMX                                                              
                                                                                
FMTAZAP  ZAP   FAPL8,0(0,RF)       GET AMOUNT                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT RATE ONTO BFMELD                                  *         
*                                                                     *         
* NTRY: P1 BYTE 0=INDICATOR BYTE                                      *         
*             1-3=A(BFMELD)                                           *         
*       P2 BYTE 0=LENGTH OF PACKED INPUT FIELD                        *         
*             1-3=A(PACKED INPUT FIELD)                               *         
*                                                                     *         
* EXIT:   BFMTEXT=OUTOUT                                              *         
*         BFMTLEN=L(OUTPUT)                                           *         
*         BFMTLN= NEW L(ELEMENT)                                      *         
***********************************************************************         
                                                                                
FMTRAT   STM   RE,R7,SVREG                                                      
         MVC   FAPARMS,0(R1)                                                    
                                                                                
         XR    R3,R3                                                            
         ICM   R3,7,FAPABFM        R3=A(BFMEL)                                  
         USING BFMELD,R3                                                        
         XR    RF,RF                                                            
         ICM   RF,7,FAPAAMT        RF=A(AMOUNT)                                 
         SR    R1,R1                                                            
         ICM   R1,1,FAPINPL        R1=LENGTH OF INPUT                           
         BCTR  R1,0                                                             
         EXRL  R1,FMTRZAP                                                       
                                                                                
         MVC   BFMTEXT(12),SPACES                                               
         CURED FAPL8,(8,BFMTEXT),2,ALIGN=LEFT,MINUS=YES,TRAIL=%                 
         STCM  R0,3,BFMTLEN        SET TEXT LENGTH                              
         AHI   R0,BFMLNQ                                                        
         STC   R0,BFMLN            SET ELEMENT LENGTH                           
         J     LMX                                                              
                                                                                
FMTRZAP  ZAP   FAPL8,0(0,RF)       GET AMOUNT                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT PERCENT AMOUNT ONTO BFMELD                        *         
*                                                                     *         
* NTRY: P1 BYTE 0=INDICATOR BYTE                                      *         
*             1-3=A(BFMELD)                                           *         
*       P2 BYTE 0=LENGTH OF PACKED INPUT FIELD                        *         
*             1-3=A(PACKED INPUT FIELD)                               *         
*                                                                     *         
* EXIT: BFMTEXT=OUTOUT                                                *         
*       BFMTLEN=L(OUTPUT)                                             *         
*       BFMWTH =WIDTH OF FIELD                                        *         
*       BFMTLN =NEW L(ELEMENT)                                        *         
***********************************************************************         
                                                                                
FMTPCT   STM   RE,R7,SVREG                                                      
         MVC   FAPARMS,0(R1)                                                    
                                                                                
         XR    R3,R3                                                            
         ICM   R3,7,FAPABFM        R3=A(BFMEL)                                  
         USING BFMELD,R3                                                        
         XR    RF,RF                                                            
         ICM   RF,7,FAPAAMT        RF=A(AMOUNT)                                 
         SR    R1,R1                                                            
         ICM   R1,1,FAPINPL        R1=LENGTH OF INPUT                           
         BCTR  R1,0                                                             
         EXRL  R1,FMTRZAP                                                       
                                                                                
         MVC   BFMTEXT(12),SPACES                                               
         LA    R2,BFMTEXT                                                       
         CURED FAPL8,(8,(R2)),2,ALIGN=LEFT                                      
         AR    R2,R0                                                            
         MVI   0(R2),C'%'                                                       
         LA    RE,BFMTEXT-1                                                     
         SR    R2,RE                                                            
         STCM  R2,3,BFMTLEN        SET TEXT LENGTH                              
         LA    R2,1(R2)                                                         
         STC   R2,BFMWTH           WIDTH IS 1 MORE FOR EXTRA SPACE              
         LA    R2,BFMLNQ-1(R2)                                                  
         STC   R2,BFMLN            SET ELEMENT LENGTH                           
         J     LMX                                                              
                                                                                
FMTPZAP  ZAP   FAPL8,0(0,RF)       GET AMOUNT                                   
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* CONVERSION ROUTINES                                                *          
*  PARM 1=A(PARELD)                                                  *          
*       2=A(BFMELD)                                                  *          
**********************************************************************          
                                                                                
CNVDATA  STM   RE,R7,SVREG                                                      
         LM    R2,R3,0(R1)                                                      
         USING PARELD,R2                                                        
         USING BFMELD,R3                                                        
                                                                                
         LLC   R5,PARFLD                                                        
         MHI   R5,FLDTABL                                                       
         LARL  RE,FLDTAB                                                        
         AR    R5,RE                                                            
                                                                                
         USING FLDTABD,R5                                                       
         LLC   RF,FLDCNV                                                        
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         LARL  RE,CNVDACT                                                       
         AR    RF,RE                                                            
         BR    RF                                                               
                                                                                
CNVDACT  J     CNVDATE                                                          
         J     CNVAMT                                                           
         J     CNVHRS                                                           
         J     CNVRATE                                                          
         J     CNVCMR                                                           
                                                                                
CNVDATE  DS    0H                  CONVERT DATE                                 
         GOTOR DATCON,DMCB,(X'41',PARDATA),(17,TEXT)                            
         LLC   RE,TEXTLEN          REMOVE TRAILING SPACES                       
         LA    RF,TEXT-1(RE)                                                    
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         JCT   RE,*-10                                                          
         STC   RE,TEXTLEN                                                       
         J     LMX                                                              
                                                                                
CNVAMT   ZAP   DUB,PARAMT          CONVERT BILLING AMOUNT                       
         TM    BFMINDS1,BFMIREV    REVERSE THE SIGN                             
         JNO   *+10                                                             
         MP    DUB,PNEG1                                                        
         CURED (P8,DUB),(30,TEXT),2,MINUS=YES,                         *        
               ALIGN=LEFT,ZERO=BLANK                                            
         ORG   *-2                                                              
         TM    BFMINDS2,BFMIPZA    TEST PRINT ZERO AMOUNT                       
         JNO   *+8                                                              
         NI    CURPEDT1-CURPARMD(R1),ALL-(CURPZERB)                             
                                                                                
         ZAP   DOUBLE,DUB                                                       
         OI    DOUBLE+7,X'0F'                                                   
         CP    DOUBLE,P1MIL                                                     
         JNL   CNVAMT3                                                          
         TM    OPT8,OPT8PAWC       PRINT WITH COMMAS                            
         JNO   *+8                                                              
         OI    CURPEDT1-CURPARMD(R1),CURPCOMY                                   
                                                                                
         TM    BFMINDS2,BFMIDOL    ARE DOLLAR SIGNS ALLOWED?                    
         JNO   CNVAMT3             NO                                           
                                                                                
         TM    OPT7,OPT7PDOL       YES, BUT DO WE WANT THEM?                    
         JNO   CNVAMT3             NO                                           
         NI    CURPEDT1-CURPARMD(R1),ALL-CURPSYMN                               
         MVI   CURPCSYM-CURPARMD(R1),C'$'                                       
         MVI   CURPCIND-CURPARMD(R1),CURPCPFX+1                                 
                                                                                
CNVAMT3  GOTOR (RF)                                                             
                                                                                
         STC   R0,TEXTLEN                                                       
         TM    BFMINDS1,BFMICRY    TEST CR=YES                                  
         JZ    LMX                                                              
         LA    RE,TEXT-1                                                        
         AR    RE,R0                                                            
         CLI   0(RE),C'-'                                                       
         JNE   LMX                                                              
         MVI   0(RE),C'C'          SET CR                                       
         MVI   1(RE),C'R'                                                       
         AHI   R0,1                                                             
         STC   R0,TEXTLEN                                                       
         J     LMX                                                              
                                                                                
CNVRATE  DS    0H                  RATE                                         
         CURED (P8,PARAMT),(30,TEXT),2,MINUS=YES,                      *        
               COMMAS=YES,ALIGN=LEFT                                            
         STC   R0,TEXTLEN                                                       
         J     LMX                                                              
                                                                                
CNVHRS   DS    0H                  HOURS                                        
         CURED (P8,PARAMT),(30,TEXT),2,MINUS=YES,                      *        
               COMMAS=YES,ALIGN=LEFT                                            
         STC   R0,TEXTLEN                                                       
         J     LMX                                                              
                                                                                
CNVCMR   DS    0H                  COMMISSION RATE                              
         CURED (P8,PARAMT),(30,TEXT),4,MINUS=YES,                      *        
               COMMAS=YES,ALIGN=LEFT                                            
         LA    R1,TEXT                                                          
         AR    R1,R0                                                            
         MVI   0(R1),C'%'                                                       
         AHI   R0,1                                                             
         STC   R0,TEXTLEN                                                       
                                                                                
LMX      LM    RE,R7,SVREG                                                      
         BR    RE                                                               
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
* CONTROL PRINTING OF REPORTS                                         *         
***********************************************************************         
                                                                                
BPRT     NMOD1 PWSLNQ,**BPRT                                                    
         DROP  RB                                                               
         LARL  RA,GLOBALS                                                       
         LR    R7,RC                                                            
         USING PWSD,R7                                                          
         L     RC,BASERC                                                        
         LA    R0,PWSD                                                          
         LHI   R1,PWSLNQ                                                        
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LARL  RF,HHOOK                                                         
         STCM  RF,15,HEADHOOK                                                   
         MVC   PAGE,=H'1'                                                       
                                                                                
BPRT3    L     R6,ASRTWK                                                        
         USING SORTD,R6                                                         
                                                                                
         LARL  RE,BPRTAB           LOOK UP EDIT ROUTINE IN TABLE                
         LHI   R0,BPRTABN          R0=NUMBER OF ENTRIES IN TABLE                
BPRT4    CLC   BILKRT,0(RE)        MATCH ON SORT RECORD TYPE                    
         JE    BPRT5                                                            
         LA    RE,L'BPRTAB(RE)     BUMP TO NEXT ENTRY                           
         JCT   R0,BPRT4                                                         
         J     XIT                 INVALID EDIT ROUTINE - EXIT                  
                                                                                
BPRT5    LLH   RF,1(RE)            GET DISPLACEMENT TO ROUTINE                  
         LARL  RE,BPRT                                                          
         AR    RF,RE               ADD ON WHERE I AM                            
         BR    RF                                                               
                                                                                
PWSD     DSECT ,                                                                
                                                                                
PWSSVKEY DS    CL(L'BEDKEY)                                                     
                                                                                
PTPAGFLN DS    X                   PAGE FOOTER LINE NUMBER                      
                                                                                
PSVRE    DS    F                                                                
                                                                                
         DS    0D                                                               
MXPLQ    EQU   25                  MAX PRINT LINES                              
PBLKLQ   EQU   (MXPLQ*L'P)         SIZE OF PRINT BLOCK                          
PBLK     DS    (PBLKLQ)C           PRINT BLOCK                                  
                                                                                
PAGENUM  DS    CL5                                                              
FOOTIO   DS    (IOLNQ)X            IO FOR FOOTER                                
PWSLNQ   EQU   *-PWSD                                                           
ACNB03   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* MOVE RECORDS TO LOCAL AREAS                                         *         
***********************************************************************         
                                                                                
CLIR     L     R0,ADHEIRA          SAVE CLIENT RECORD                           
         ST    R0,AIO                                                           
         GOTOR MOVER                                                            
         ST    R0,ADHEIRB                                                       
         GOTOR ASETEL,SETLINI      INITIALIZE                                   
         GOTOR ASETEL,SETLCLQ                                                   
         MVC   CBTRN,ABTRN         SET START OF TABLE                           
         XC    NPRVB,NPRVB         NUMBER OF PREVIOUS BILLS                     
         J     XITY                                                             
                                                                                
PRDR     L     R0,ADHEIRB          SAVE PRODUCT RECORD                          
         ST    R0,AIO                                                           
         GOTOR MOVER                                                            
         ST    R0,ADACC                                                         
         GOTOR ASETEL,SETLPRQ                                                   
         J     XITY                                                             
                                                                                
JOBR     L     R0,ADACC            SAVE ACCOUNT RECORD                          
         ST    R0,AIO                                                           
         GOTOR MOVER                                                            
         GOTOR APARCPJ             PARSE CLIENT/PRODUCT/JOB                     
         GOTOR ASETEL,SETLJBQ                                                   
         J     XITY                                                             
                                                                                
PRVB     L     R0,CBTRN            MOVE PREVIOUS BILL ITEM TO TABLE             
         GOTOR MOVER                                                            
         ST    R0,CBTRN            SET FOR NEXT                                 
         ICM   RF,15,NPRVB         COUNT PREVIOUS BILLS                         
         AHI   RF,1                                                             
         ST    RF,NPRVB                                                         
         J     XITY                                                             
                                                                                
BFMR     L     R0,ABFM             SAVE BILL FORMAT RECORD                      
         ST    R0,AIO                                                           
         GOTOR MOVER                                                            
         GOTOR ASETEL,SETLBFQ      SET ELEMENT ADDRESSES                        
         J     XITY                                                             
                                                                                
BLHR     L     R0,ABLH             SAVE BILL HEADER RECORD                      
         ST    R0,AIO                                                           
         GOTOR MOVER                                                            
         GOTOR ASETEL,SETLBHQ      SET ELEMENT ADDRESSES                        
         MVI   PTIND,0                                                          
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,YES                                                     
         MVI   FORCEMID,NO                                                      
         MVC   PAGE,=H'1'                                                       
         L     RF,ABLHEL                                                        
         USING BLHELD,RF                                                        
         MVC   REPTINDS,BLHRINDS                                                
         MVC   REPFLAG1,BLHFLAG1                                                
         DROP  RF                                                               
         J     XITY                                                             
                                                                                
UPPR     L     R0,AUPPR            UPPER                                        
         GOTOR MOVER                                                            
         OI    PTIND,PTIUPPR                                                    
         J     XITY                                                             
                                                                                
PGHR     L     R0,APGHR            PAGE HEADER RECORD                           
         GOTOR MOVER                                                            
         MVC   PTPAGFLN,MAXLINES                                                
         MVI   FORCEHED,YES                                                     
         MVI   FORCEMID,NO                                                      
         MVC   PAGE,=H'1'                                                       
         J     XITY                                                             
                                                                                
PGH2     L     R0,APGH2            PAGE HEADER 2 RECORD                         
         GOTOR MOVER                                                            
         OI    PTIND,PTIHPG2                                                    
         J     XITY                                                             
                                                                                
PGFR     LA    R0,FOOTIO           PAGE FOOTER RECORD                           
         GOTOR MOVER                                                            
         OI    PTIND,PTIFOOT                                                    
         LLC   RE,MAXLINES                                                      
         LA    RE,1(RE)                                                         
         SR    RE,R4                                                            
         STC   RE,PTPAGFLN                                                      
         J     XITY                                                             
         EJECT                                                                  
***********************************************************************         
* GET FIRST EDIT RECORD AND PRINT THE BILL                            *         
***********************************************************************         
                                                                                
BEDR     L     R0,ABEDR            SAVE BILL HEADER                             
         ST    R0,AIO                                                           
         GOTOR MOVER                                                            
                                                                                
         XC    LASTX(LASTXLQ),LASTX                                             
         GOTOR PROC                PROCESS BILL                                 
         L     R6,ASRTWK                                                        
         USING SORTD,R6                                                         
         CLI   SKRQRT,X'FF'        TEST END REQUEST/RUN                         
         JE    XITN                                                             
         CLI   BILKBIL,BILKBILQ    TEST BILL RECORD                             
         JNE   XITN                YES,                                         
         TM    RQLV,LVA+LVB        TEST A27 GROUP BILL                          
         JNZ   BPRT3               YES                                          
         MVI   FORCEHED,YES                                                     
         MVI   FORCEMID,NO                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         J     BPRT3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS BILL RECORDS                                     *         
***********************************************************************         
                                                                                
PROC     NTR1  LABEL=*                                                          
         L     R3,ABEDR                                                         
         USING BEDRECD,R3                                                       
         MVC   PWSSVKEY,BEDKEY                                                  
         J     PRECS03                                                          
                                                                                
PRECS02  GOTOR ASORT,SORTGETQ                                                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R6,ASRTWK                                                        
PRECS03  CLI   BILKRT,BILKRBED     TEST EDIT RECORD                             
         JNE   XIT                                                              
                                                                                
PRECS04  XC    LASTX(LASTXLQ),LASTX                                             
         MVC   WORK,BEDKEY         SAVE LAST KEY                                
         L     R0,ABEDR            MOVE TO IO AREA                              
         GOTOR MOVER                                                            
         L     R2,ABEDR                                                         
         MVI   ELCODE,BFMELQ       SKIP RECORDS WITHOUT ELEMENTS                
         GOTOR GETELN                                                           
         JNE   PRECS02                                                          
         MVC   PWSSVKEY,WORK       OK, TO REPLACE SAVED KEY                     
                                                                                
         LARL  R5,ROUTAB           FIND ROUTINE FOR THIS SECTION                
PRECS06  CLC   BEDKWHER,0(R5)                                                   
         JE    PRECS08                                                          
         LA    R5,L'ROUTAB(R5)                                                  
         CLI   0(R5),EOT                                                        
         JNE   PRECS06                                                          
         DC    H'0'                                                             
                                                                                
PRECS08  LLH   RF,2(R5)                                                         
         LARL  RE,BPRT                                                          
         AR    RF,RE                                                            
         GOTOR (RF)                GOTO PRINT ROUTINE                           
         J     PRECS02                                                          
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS BODY OF BILL                                                *         
***********************************************************************         
                                                                                
PBODY    NTR1  LABEL=*                                                          
         L     R2,ABEDR                                                         
         USING BEDRECD,R2                                                       
                                                                                
         CLC   BEDKLVL,PWSSVKEY+(BEDKLVL-BEDRECD) TEST CHANGE OF LEVEL          
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   BEDRSLVL,=AL2(BEDKSHPQ)    HEADING PER PAGE?                     
         JNE   PBODY3                                                           
         L     R0,AHPPR                                                         
         GOTOR MOVER                                                            
         OI    PTIND,PTIHPPG                                                    
         NI    PTIND,ALL-(PTISECH)                                              
         J     XIT                                                              
                                                                                
PBODY3   CLC   BEDRSLVL,=AL2(BEDKSHSQ)    HEADING PER SECTION?                  
         JNE   PBODY5                                                           
         L     R0,ASCHR            SAVE SECTION HEADER                          
         GOTOR MOVER                                                            
         OI    PTIND,PTISECH                                                    
         GOTOR FMTTX,DMCB,('FTACTPRT',ASCHR),MID1                               
         MVI   FORCEMID,YES                                                     
                                                                                
         LLC   RF,LINE                                                          
         AHI   RF,5                                                             
         CLM   RF,1,MAXLINES                                                    
         JL    *+8                                                              
         MVI   FORCEHED,YES                                                     
         J     XIT                                                              
                                                                                
PBODY5   GOTOR CLRPBK              CLEAR THE PRINT BLOCK                        
         GOTOR FMTTX,DMCB,('FTACTPRT',BEDRECD),PBLK                             
         GOTOR PRTBK               PRINT THE BLOCK                              
         NI    PTIND,X'FF'-PTISECH DON'T REPEAT SECTION HEADER                  
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS BILL TOTALS                                                 *         
***********************************************************************         
                                                                                
PGSTS    DS    0H                  REMOVE REPEATING CODES                       
PPRVB    DS    0H                                                               
PBTTL    NTR1  LABEL=*                                                          
         CLI   CTAXOPT,0           FOR GST/PST SUMMARIES                        
         JE    *+8                                                              
         GOTOR REMDUP              REMOVE REPEATING CODES                       
                                                                                
         MVC   MID1,SPACES         CLEAR SECTION HEADER                         
         MVC   MID2,SPACES                                                      
         MVI   FORCEMID,NO                                                      
         NI    PTIND,ALL-(PTISECH)                                              
                                                                                
PBTTL3   L     R2,ABEDR                                                         
         USING BEDRECD,R2                                                       
         GOTOR CLRPBK              CLEAR THE PRINT BLOCK                        
         GOTOR FMTTX,DMCB,('FTACTPRT',BEDRECD),PBLK                             
         GOTOR PRTBK               PRINT THE BLOCK                              
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
REMDUP   NTR1  LABEL=*             ** REMOVE DUPLICATE CODES **                 
         L     R2,ABEDR                                                         
         MVI   ELCODE,BFMELQ                                                    
         GOTOR GETELN                                                           
         J     REMDUP4                                                          
REMDUP3  GOTOR NEXTEL                                                           
REMDUP4  JNE   XIT                                                              
         USING BFMELD,R2                                                        
         CLI   BFMPNLC,PCQTXTYP    TAX TYPE                                     
         JNE   REMDUP5                                                          
         CLC   BFMTEXT(L'CTXPRVD),LASTPRVD                                      
         MVC   LASTPRVD,BFMTEXT                                                 
         JNE   *+10                                                             
         MVC   BFMTEXT(L'CTXPRVD),SPACES                                        
         J     REMDUP3                                                          
                                                                                
REMDUP5  CLI   BFMPNLC,PCQTXREG    TAX REGISTRATION                             
         JNE   REMDUP3                                                          
         CLC   LASTREGL,BFMTLEN    TEST SAME LENGTH                             
         JNE   REMDUP7                                                          
         XR    R1,R1                                                            
         ICM   R1,3,BFMTLEN                                                     
         JZ    REMDUP7                                                          
         BCTR  R1,0                                                             
         EXRL  R1,REMDCLC                                                       
         JNE   REMDUP7                                                          
         EXRL  R1,REMDMVC                                                       
         J     REMDUP3                                                          
                                                                                
REMDUP7  MVC   LASTREG,BFMTEXT                                                  
         MVC   LASTREGL,BFMTLEN                                                 
         J     REMDUP3                                                          
                                                                                
REMDCLC  CLC   BFMTEXT(0),LASTREG                                               
REMDMVC  MVC   BFMTEXT(0),SPACES                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT THE BLOCK                                                     *         
***********************************************************************         
                                                                                
PRTBK    NTR1  LABEL=*                                                          
         MVI   SVSPA,0                                                          
         L     R2,ABEDR            SET SPACING                                  
         MVI   ELCODE,BFPELQ                                                    
         GOTOR GETELN                                                           
         JNE   PRTBK1                                                           
                                                                                
         USING BFPELD,R2           GET SPACING "AFTER"                          
         LLC   RE,BFPSPLS                                                       
         LA    RE,1(RE)                                                         
         STC   RE,SVSPA            SAVE SPACING                                 
                                                                                
         CLI   BFPSPBP,1           TEST SPACE "BEFORE"                          
         JNE   PRTBK1                                                           
         GOTOR ACREPORT            PRINT A BLANK LINE                           
         DROP  R2                                                               
                                                                                
PRTBK1   LA    R2,PBLK                                                          
         AHI   R2,((MXPLQ-1)*(L'P)) R2=LAST LINE OF BLOCK                       
         LA    R0,MXPLQ            GET NUMBER OF LINES TO PRINT                 
         CLC   0(L'P,R2),SPACES                                                 
         JNE   *+12                                                             
         SHI   R2,L'P                                                           
         JCT   R0,*-14                                                          
                                                                                
         LA    R2,PBLK                                                          
                                                                                
PRTBK3   CHI   R0,4                TEST LAST SET                                
         JH    *+10                                                             
         MVC   SPACING,SVSPA       SET NUMBER OF SPACES AFTER                   
         LA    RF,4                MAX PRINT LINES                              
         CR    R0,RF               TEST LESS THAN 4 TO PRINT                    
         JNL   *+6                                                              
         LR    RF,R0               USE LOWER NUMBER                             
         SR    R0,RF               R0=NUMBER REMAINING                          
         LTR   RF,RF                                                            
         JNP   XIT                                                              
         LA    R3,P                                                             
         MVC   0(L'P,R3),0(R2)     MOVE TO PRINT LINE                           
         LA    R2,L'P(R2)                                                       
         LA    R3,L'P(R3)                                                       
         JCT   RF,*-14                                                          
                                                                                
         GOTOR ACREPORT            PRINT UP TO 4 LINES                          
         LTR   R0,R0               ANY MORE?                                    
         JNZ   PRTBK3                                                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MISCELLANEOUS                                                       *         
***********************************************************************         
                                                                                
MOVER    ST    RE,PSVRE                                                         
         L     RE,ASRTWK                                                        
         LLH   RF,0(RE)            MOVE RECORD TO LOCAL IO (R0)                 
         SHI   RF,SRDATA-SORTD                                                  
         LR    R1,RF                                                            
         LA    RE,SRDATA-SORTD(RE)                                              
         MVCL  R0,RE                                                            
         L     RE,PSVRE                                                         
         BR    RE                                                               
                                                                                
CLRPBK   ST    RE,PSVRE            CLEAR PRINT BLOCK TO SPACES                  
         LA    R0,PBLK                                                          
         LA    R1,PBLKLQ                                                        
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     RE,PSVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* HEAD HOOK                                                           *         
***********************************************************************         
                                                                                
HHOOK    NTR1  LABEL=*                                                          
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         MVC   UPPER1,SPACES       CLEAR UPPERS                                 
         MVC   UPPER2,SPACES                                                    
         MVI   UPPER1+1,X'00'      BUT FORCE 2 LINES AT TOP                     
         MVI   UPPER2+1,X'00'                                                   
         L     RF,ADBXAREA                                                      
         USING BOXD,RF                                                          
         MVI   BOXYORN,NO          INITIALISE                                   
         MVI   BOXOFF,YES                                                       
         DROP  RF                                                               
         LAY   R0,XHEADS                                                        
         LHI   R1,L'XHEADS*XHEADSN                                              
         LA    RF,C' '                                                          
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
         XC    XHEADA,XHEADA                                                    
         MVI   XHEADN,0                                                         
                                                                                
         CLI   RCSUBPRG,0          HOOK FOR BILLS ONLY                          
         JNE   XIT                                                              
         MVI   ELCODE,BFMELQ       LOOK FOR PAGE ELEMENT                        
         L     R2,APGHR                                                         
         SR    R4,R4                                                            
         GOTOR GETELN                                                           
         J     HHOOK4                                                           
HHOOK3   GOTOR NEXTEL                                                           
HHOOK4   JNE   HHOOK5                                                           
         USING BFMELD,R2                                                        
         CLI   BFMPNLC,PCQPG       TEST PAGE?                                   
         JNE   HHOOK3                                                           
         LA    R4,BFMTEXT          FIND SPACE FOR NUMBER                        
         CLC   0(3,R4),=C'???'                                                  
         JE    *+12                                                             
         LA    R4,1(R4)                                                         
         J     *-14                                                             
         MVC   PAGENUM,SPACES                                                   
         EDITR PAGE,(3,0(R4)),ALIGN=LEFT                                        
         DROP  R2                                                               
                                                                                
HHOOK5   LA    R0,HEAD1                                                         
         TM    REPFLAG1,BFTFXHDS   TEST USE XHEADS ONLY                         
         JZ    HHOOK6                                                           
         LAY   R0,XHEADS                                                        
         STCM  R0,7,XHEADA         SET ADDRESS OF XHEADS FOR ACREPORT           
         MVI   XHEADN,XHEADSN      SET NUMBER OF XHEADS FOR ACREPORT            
                                                                                
HHOOK6   GOTOR FMTTX,DMCB,('FTACTPRT',APGHR),(R0)                               
         LTR   R4,R4                                                            
         JZ    *+10                                                             
         MVC   0(3,R4),=C'???'     FOR NEXT TIME                                
         TM    REPFLAG1,BFTFXHDS   TEST USING XHEADS                            
         JZ    HHOOK6X             NO - WE ARE DONE                             
                                                                                
         LAY   R1,XHEADS+((XHEADSN-1)*L'XHEADS)                                 
         LHI   R0,XHEADSN-1                                                     
HHOOK62  CLC   0(L'XHEADS,R1),SPACES                                            
         JNE   HHOOK64                                                          
         SHI   R1,L'XHEADS                                                      
         JCT   R0,HHOOK62                                                       
         DC    H'0'                                                             
HHOOK64  MVC   BFSWRK(L'XHEADS),0(R1)                                           
         MVC   0(L'XHEADS,R1),SPACES                                            
         CLC   PAGE,=H'1'          TEST ON FIRST PAGE                           
         JNE   HHOOK65                                                          
         TM    PTIND,PTIHPG2       TEST PAGE HEADER 2 PRESENT                   
         JZ    HHOOK65                                                          
         LAY   R0,XHEADS                                                        
         GOTOR FMTTX,DMCB,('FTACTPRT',APGH2),(R0)                               
HHOOK65  LAY   R1,XHEADS+((XHEADSN-1)*L'XHEADS)                                 
         LHI   R0,XHEADSN-1                                                     
HHOOK66  CLC   0(L'XHEADS,R1),SPACES                                            
         JNE   HHOOK68                                                          
         SHI   R1,L'XHEADS                                                      
         JCT   R0,HHOOK66                                                       
HHOOK68  AHI   R1,L'XHEADS*2                                                    
         MVC   0(L'XHEADS,R1),BFSWRK                                            
                                                                                
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
                                                                                
         MVI   BOXYORN,YES         INITIALISE BOX                               
         MVI   BOXOFF,NO                                                        
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         OI    BOXDDCTL,BOXDDUL+BOXDDLC                                         
         LA    R1,BOXROWS                                                       
         AR    R1,R0                                                            
         MVI   4(R1),BOXTOP                                                     
         MVI   6(R1),BOXBOT                                                     
         MVI   BOXCOLS,BOXLEFT                                                  
         LA    R1,BFSWRK+L'XHEADS-1                                             
         LA    RF,BOXCOLS+L'XHEADS                                              
         CLI   0(R1),C' '                                                       
         JH    *+10                                                             
         BCTR  R1,0                                                             
         JCT   RF,*-10                                                          
         MVI   0(RF),BOXRIGHT                                                   
         DROP  R4                                                               
                                                                                
HHOOK6X  TM    PTIND,PTIUPPR       ANY UPPER?                                   
         JNO   HHOOK7                                                           
         L     R4,VEXTRAS                                                       
         USING RUNXTRAD,R4                                                      
         GOTOR FMTTX,DMCB,('FTACTPRT',AUPPR),UPPER1                             
         DROP  R4                                                               
                                                                                
HHOOK7   TM    0(R5),PTISECH       SECTION HEADER?                              
         JNO   XIT                                                              
         GOTOR FMTTX,DMCB,('FTACTPRT',ASCHR),MID1                               
         MVI   FORCEMID,YES                                                     
         J     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* TO SET SIZE OF BFMELDS TO SIZE OF TEXT                              *         
*                                                                     *         
* NTRY: P1 BYTE 0=C'S'                                                *         
*             1-3=A(BFMREC/BEDREC RECORD)                             *         
*                                                                     *         
* TO FILL A PRINT BLOCK                                               *         
*                                                                     *         
* NTRY: P1 BYTE 0=C'P'                                                *         
*             1-3=A(BFMREC/BEDREC RECORD)                             *         
*       P2       =A(OUTPUT PRINT BLOCK (132 COLS. PER LINE)           *         
***********************************************************************         
                                                                                
FMTTX    NTR1  LABEL=*,WORK=(R7,FTWORKL)                                        
         USING FTWORKD,R7                                                       
         LARL  RA,GLOBALS                                                       
         LR    R2,R1                                                            
         LA    R0,FTWORKD                                                       
         LHI   R1,FTWORKL                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ST    R2,FTAPARMS                                                      
         MVC   FTACT,0(R2)                                                      
         MVC   FTAREC,0(R2)                                                     
         MVI   FTAREC,0                                                         
         CLI   FTACT,FTACTSIZ                                                   
         JE    FTXT02                                                           
         CLI   FTACT,FTACTPRT                                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   FTAPBLK,4(R2)                                                    
         MVI   FTAPBLK,0                                                        
                                                                                
FTXT02   L     R2,FTAREC                                                        
         USING BEDRECD,R2                                                       
         LA    R4,BEDRFST                                                       
         USING BFPELD,R4                                                        
         XR    RF,RF                                                            
                                                                                
FTXT04   CLI   BFPEL,BFPELQ                                                     
         JE    FTXT06                                                           
         CLI   BFPEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         LLC   RF,BFPLN                                                         
         JXH   R4,RF,FTXT04                                                     
                                                                                
FTXT06   CLI   FTACT,FTACTSIZ                                                   
         JNE   *+8                                                              
         MVI   BFPHGTMN,0          RESET PARAGRAPH MINIMUM HEIGHT               
                                                                                
* THIS LOGIC ADDED TO STOP THE WORKCODE AND NAME FROM BEING PRINTED             
* IF THE AMOUNT NETS TO ZERO                                                    
* IF CODE IS ONLY EXECUTED IF NOT SHOWING WORKCODE DETAILS AND IF THE           
* GROSS AMOUNT IT ZERO.                                                         
                                                                                
         L     R6,ADGOBLOC                                                      
         USING GOBLOCKD,R6                                                      
         CLI   GOBILDET,NO         PRINTING DETAIL?                             
         JNE   FTXT16              YES, SKIP THIS THEN                          
                                                                                
         ZAP   DUB,PZERO           CLEAR AMOUNT SAVEAREA                        
         LA    R3,BEDRFST                                                       
                                                                                
         USING FWTELD,R3                                                        
FTXT08   CLI   FWTEL,0                                                          
         JE    FTXT16                                                           
         CLI   FWTEL,FWTELQ        FIND THE X'F6' ELEMENT                       
         JE    FTXT11                                                           
                                                                                
FTXT10   LLC   RF,FWTLN                                                         
         AR    R3,RF                                                            
         J     FTXT08                                                           
                                                                                
FTXT11   CLI   FWTFLD,8            IS IT THE GROSS FIELD?                       
         JNE   FTXT10              NO, GET NEXT                                 
         ZAP   DUB,FWTAMT          YES, SAVE AMOUNT                             
         JNZ   FTXT16              IF NOT ZERO, SKIP ALL THIS                   
                                                                                
         LA    R3,BEDRFST                                                       
         USING BFMELD,R3                                                        
FTXT12   CLI   BFMEL,0             AMOUNT IS ZERO                               
         JE    FTXT16                                                           
         CLI   BFMEL,BFMELQ        FIND THE X'F7' ELEMENT NOW                   
         JNE   FTXT14                                                           
         CLI   BFMTYPE,BFMTDATA    MAKE SURE IT IS TYPE DATA                    
         JNE   FTXT14                                                           
         CLI   BFMFLD,BFMFWCQ      IF IT IS A WORKCODE, DON'T PRINT             
         JE    FTXT26                                                           
                                                                                
FTXT14   LLC   RF,BFMLN                                                         
         AR    R3,RF                                                            
         J     FTXT12                                                           
                                                                                
FTXT16   LA    R3,BEDRFST                                                       
         USING BFMELD,R3                                                        
FTXT18   CLI   BFMEL,0                                                          
         JE    FTXT26                                                           
         CLI   BFMEL,BFMELQ                                                     
         JNE   FTXT24                                                           
                                                                                
         ST    R3,FTABFM                                                        
         GOTOR GETTXT              EXTRACT THE TEXT                             
         GOTOR BLDROWS             BUILD THE ROWS                               
         JNE   FTXT24                                                           
                                                                                
         CLI   FTACT,FTACTSIZ                                                   
         JNE   FTXT22                                                           
         TM    BFMSTAT1,BFMSNUM    LEAVE IF NUMERICAL                           
         JO    FTXT20                                                           
         MVC   BFMHGT,FTHGTMIN     SET HEIGHT TO MINIMUM REQUIRED               
         CLI   BFMALN,BFMALFTQ     ONLY DO WIDTH IF LEFT ALIGNED                
         JNE   FTXT20                                                           
         MVC   BFMWTH,FTWTHMIN     SET WIDTH TO MINIMUM                         
                                                                                
FTXT20   LLC   RE,BFMTOP                                                        
         LLC   RF,BFMHGT                                                        
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         CLM   RE,1,BFPHGTMN                                                    
         JNH   *+8                                                              
         STC   RE,BFPHGTMN                                                      
         J     FTXT24                                                           
                                                                                
FTXT22   GOTOR PRTROWS                                                          
                                                                                
FTXT24   LLC   RF,BFMLN                                                         
         JXH   R3,RF,FTXT18                                                     
                                                                                
FTXT26   J     XITE                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT TEXT FROM BFMELD AND BFXELDS                     *         
*                                                                     *         
* EXIT: FTDATA  =TEXT                                                 *         
*       FTINDS,FTIEXSPC SET IF EXTRA SPACE REQUIRED AT END OF FIELD   *         
***********************************************************************         
                                                                                
GETTXT   NTR1  LABEL=*                                                          
         L     R3,FTABFM                                                        
         USING BFMELD,R3                                                        
                                                                                
         LA    R5,FTDATA           COPY TEXT TO FTDATA                          
         LLC   RF,BFMLN                                                         
         LR    RE,RF                                                            
         SHI   RE,BFMLNQ+1                                                      
         JM    GTXT02                                                           
         EXRL  RE,GTXTMVCM                                                      
         LA    R5,1(R5,RE)                                                      
                                                                                
GTXT02   LR    R4,R3                                                            
         USING BFXELD,R4           TEST FOR ANY BFXELDS                         
GTXT04   AR    R4,RF                                                            
         CLI   BFXEL,BFXELQ                                                     
         JNE   GTXT06                                                           
         LLC   RF,BFXLN                                                         
         LR    RE,RF                                                            
         SHI   RE,BFXLNQ+1                                                      
         JNP   GTXT04                                                           
         EXRL  RE,GTXTMVCM                                                      
         LA    R5,1(R5,RE)                                                      
         J     GTXT04                                                           
         DROP  R4                                                               
                                                                                
GTXT06   ST    R5,FTADATAX         SAVE A(END OF TEXT LINE +1)                  
                                                                                
         NI    FTINDS,FF-(FTIEXSPC+FTIEXLFT)                                    
         TM    BFMSTAT1,BFMSNUM    TEST RIGHT ALIGNED NUMBER                    
         JZ    GTXT10                                                           
         CLI   BFMALN,BFMARGTQ                                                  
         JNE   GTXT10                                                           
         TM    BFMSTAT1,BFMSHEAD   TEST HEADLINE OF                             
         JO    GTXT08                                                           
         BCTR  R5,0                                                             
         CLI   0(R5),C'-'          TEST NEGATIVE NUMBER                         
         JE    GTXT10                                                           
         BCTR  R5,0                                                             
         CLC   0(2,R5),=C'CR'      TEST NEGATIVE NUMBER                         
         JNE   GTXT08                                                           
         OI    FTINDS,FTIEXLFT     MOVE TO THE RIGHT ONE POSITION               
         J     GTXT10                                                           
                                                                                
GTXT08   OI    FTINDS,FTIEXSPC     PUT EXTRA SPACE ON RHS OF FIELD              
                                                                                
GTXT10   DS    0H                                                               
         J     XITE                                                             
                                                                                
GTXTMVCM MVC   0(0,R5),BFMTEXT                                                  
         USING BFXELD,R4           TEST FOR ANY BFXELDS                         
GTXTMVCX MVC   0(0,R5),BFXTEXT                                                  
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD ROWS FOR TEXT                                      *         
*                                                                     *         
* NTRY: FTABFM  =A(BFMELD)                                            *         
*       FTDATA  =TEXT                                                 *         
*       FTADATAX=A(END OF TEXT)                                       *         
* EXIT: FTROWS  =LIST OF ROWD ENTRIES                                 *         
*       FTWTHMIN=MINIMUM WIDTH OF BLOCK                               *         
*       FTHGTMIN=MINIMUM HEIGHT OF BLOCK                              *         
*             CC=NOT EQUAL IF NOTHING TO PRINT                        *         
***********************************************************************         
                                                                                
BLDROWS  NTR1  LABEL=*                                                          
         L     R3,FTABFM                                                        
         USING BFMELD,R3                                                        
         MVI   FTHGTMIN,0          CLEAR HEIGHT MINIMUM                         
                                                                                
         LLC   RE,BFMWTH                                                        
         TM    FTINDS,FTIEXSPC     TEST FOR EXTRA SPACE AT END                  
         JZ    *+6                                                              
         BCTR  RE,0                YES - REDUCE THE WIDTH BY 1                  
         STC   RE,FTWTHMAX                                                      
                                                                                
         LLC   RE,BFMHGT           SET MAXIMUM HEIGHT                           
         TM    BFMINDS1,BFMIUND+BFMIUND2    ADJUST IF UNDERLINED                
         JZ    *+8                                                              
         LA    RE,1                                                             
         STC   RE,FTHGTMAX                                                      
                                                                                
         LA    R5,FTDATA           R5=POSITION OF TEXT TO COPY                  
         LA    R4,FTROWS                                                        
         USING ROWD,R4             R4=A(ROW ENTRY)                              
                                                                                
BROWS02  L     R2,FTADATAX                                                      
         SR    R2,R5               R2=L(TEXT) LEFT TO BE COPIED                 
         JNP   BROWS32                                                          
         STCM  R5,15,ROWADDR                                                    
                                                                                
         LLC   RE,FTHGTMIN         BUMP UP HEIGHT                               
         LA    RE,1(RE)                                                         
         STC   RE,FTHGTMIN                                                      
                                                                                
         LLC   R0,FTWTHMAX                                                      
         AHI   R0,1                                                             
         CR    R0,R2                                                            
         JH    *+6                                                              
         LR    R0,R2                                                            
         LR    RF,R5                                                            
         XR    RE,RE                                                            
                                                                                
BROWS04  CLC   =C'\|',0(RF)        TEST NEW LINE                                
         JE    BROWS06                                                          
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         JCT   R0,BROWS04                                                       
         J     BROWS10             NEW LINE CHARACTER NOT FOUND                 
                                                                                
BROWS06  LA    R5,2(RF)            BUMP R5 TO AFTER NEW LINE                    
         J     BROWS28                                                          
                                                                                
BROWS10  CLM   R2,1,FTWTHMAX       TEST L(TEXT LEFT) <= WIDTH                   
         JH    BROWS14                                                          
         STC   R2,ROWLEN                                                        
         J     BROWS30                                                          
                                                                                
BROWS14  LLC   RE,FTWTHMAX         FIND LAST SPACE IN LINE                      
         LA    R6,0(R5,RE)         R6=A(START OF NEXT LINE)                     
         LA    RE,1(RE)                                                         
                                                                                
BROWS16  CLI   0(R6),C' '                                                       
         JNH   BROWS18                                                          
         BCTR  R6,0                                                             
         JCT   RE,BROWS16                                                       
         MVC   ROWLEN,FTWTHMAX     NO SPACES - SET TO MAX WIDTH                 
         J     BROWS20                                                          
                                                                                
BROWS18  BCTR  RE,0                                                             
         STC   RE,ROWLEN                                                        
                                                                                
BROWS20  DS    0H                                                               
         CLC   FTHGTMIN,FTHGTMAX   TEST ALREADY GOT TO MAX HEIGHT               
         JNL   BROWS30                                                          
                                                                                
         LLC   RE,ROWLEN                                                        
         AR    R5,RE                                                            
BROWS22  CLI   0(R5),C' '          BUMP PASS PRECEDING SPACES                   
         JH    BROWS28                                                          
         LA    R5,1(R5)                                                         
         JCT   R2,BROWS22                                                       
         LA    R4,ROWL(R4)                                                      
         J     BROWS30             NO MORE TEXT SO FINISHED                     
                                                                                
BROWS28  DS    0H                                                               
         LA    R4,ROWL(R4)         BUMP TO NEXT ROW                             
         J     BROWS02                                                          
                                                                                
BROWS30  LA    R4,ROWL(R4)                                                      
                                                                                
BROWS32  MVI   ROWD,ROWEOTQ                                                     
         DROP  R4                                                               
                                                                                
         MVC   FTWTHMIN,FTWTHMAX                                                
         CLI   FTHGTMIN,1                                                       
         JNE   *+10                                                             
         MVC   FTWTHMIN,FTROWS+(ROWLEN-ROWD)                                    
                                                                                
         CLC   FTHGTMIN,FTHGTMAX   RESET MIN TO MAX IF HIGHER                   
         JNH   *+10                                                             
         MVC   FTHGTMIN,FTHGTMAX                                                
                                                                                
         TM    FTINDS,FTIEXSPC     TEST EXTRA-SPACE AT END                      
         JZ    BROWS34                                                          
         LLC   RE,FTWTHMIN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,FTWTHMIN                                                      
                                                                                
BROWS34  TM    BFMINDS1,BFMIUND+BFMIUND2   UNDERLINED?                          
         JZ    *+8                                                              
         MVI   FTHGTMIN,2                                                       
                                                                                
BLDROWSX DS    0H                                                               
         CLI   FTHGTMIN,0                                                       
         JNE   XITE                                                             
         J     XITL                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COPY ROWS TO PRINT BLOCK                                 *         
***********************************************************************         
                                                                                
PRTROWS  NTR1  LABEL=*                                                          
         L     R3,FTABFM                                                        
         USING BFMELD,R3                                                        
         LLC   R2,BFMTOP                                                        
         BCTR  R2,0                                                             
         MHI   R2,L'P                                                           
         A     R2,FTAPBLK                                                       
         LLC   RF,BFMLEFT                                                       
         TM    FTINDS,FTIEXLFT     EXTRA SPACE OF LEFT                          
         JO    *+6                                                              
         BCTR  RF,0                                                             
         AR    R2,RF               R2=A(TOP LEFT HAND CORNER OF ITEM)           
                                                                                
         LA    R4,FTROWS                                                        
         USING ROWD,R4                                                          
PROWS02  CLI   ROWD,ROWEOTQ                                                     
         JE    PROWS20                                                          
         XR    RE,RE                                                            
         ICM   RE,1,ROWLEN         RE=L(TEXT TO BE COPIED)                      
         JZ    PROWS18                                                          
         XR    RF,RF                                                            
         ICM   RF,1,BFMWTH         RF=L(SLOT FOR TEXT)                          
         LR    R5,R2                                                            
         CLI   BFMALN,BFMALFTQ                                                  
         JE    PROWS10                                                          
         CLI   BFMALN,BFMACTRQ                                                  
         JE    PROWS04                                                          
         CLI   BFMALN,BFMARGTQ                                                  
         JE    PROWS06                                                          
         J     PROWS10                                                          
                                                                                
PROWS04  DS    0H                  CALCULATE CENTR POSITION                     
         SR    RF,RE                                                            
         SRL   RF,1                                                             
         AR    R5,RF                                                            
         J     PROWS10                                                          
                                                                                
PROWS06  DS    0H                  CALCULATE RIGHT ALIGNED POSITION             
         SR    RF,RE                                                            
         AR    R5,RF                                                            
         TM    FTINDS,FTIEXSPC     TEST FOR EXTRA SPACE AT END                  
         JZ    *+6                                                              
         BCTR  R5,0                                                             
                                                                                
PROWS10  DS    0H                                                               
         ICM   R1,15,ROWADDR       COPY TEXT TO BLOCK                           
         BCTR  RE,0                                                             
         LR    R6,R5                                                            
         TM    BFMINDS1,BFMILFT+BFMILFT0  TEST FIELD CAN BE MOVED LEFT          
         JZ    PROWS12                                                          
         BCTR  R6,0                                                             
         CLI   0(R6),C' '                                                       
         JNH   *-6                                                              
         LA    R6,1(R6)                                                         
         TM    BFMINDS1,BFMILFT0   TEST NO SPACES                               
         JO    *+8                                                              
         LA    R6,1(R6)            LEAVE ONE SPACE                              
                                                                                
PROWS12  TM    BFMINDS1,BFMIFUP    TEST DATA CAN FLOAT UP                       
         JNO   PROWS17             NO,                                          
         ICM   R0,15,FTAPBLK       R0=TO TOP OF BLOCK                           
         LR    RF,R6                                                            
                                                                                
PROWS13  SHI   RF,L'P              BACK UP ONE LINE                             
         CR    RF,R0               TEST AT TOP                                  
         JL    PROWS15             YES                                          
         EXRL  RE,PROWCLFS         TEST AREA IS AVAILABLE                       
         JNH   PROWS13             YES, BACKUP AGAIN                            
PROWS15  LA    R6,L'P(RF)          BUMP TO NEXT AVAILABLE LINE                  
PROWS17  EXRL  RE,PROWMV61                                                      
PROWS18  LA    R4,ROWL(R4)                                                      
         AHI   R2,L'P                                                           
         J     PROWS02                                                          
                                                                                
PROWS20  TM    BFMINDS1,BFMIUND+BFMIUND2   TEST FOR UNDERLINE                   
         JZ    PRTROWSX                                                         
         LR    RF,R5                                                            
         AHI   RF,L'P              RF=A(UNDERLINING)                            
         LA    RE,1(RE)                                                         
         TM    BFMINDS1,BFMIUND2   TEST FOR UNDERLINE TO PREVIOUS               
         JNO   PROWS22                                                          
                                                                                
PROWS21  BCTR  RF,0                                                             
         BCTR  R5,0                                                             
         LA    RE,1(RE)                                                         
         CLI   0(RF),C'-'          BACKUP TO LAST -                             
         JE    PROWS24                                                          
         J     PROWS21                                                          
                                                                                
PROWS22  CLI   0(R5),C' '          FIND FIRST NON-SPACE CHARACTER               
         JH    PROWS24                                                          
         LA    R5,1(R5)                                                         
         LA    RF,1(RF)                                                         
         JCT   RE,PROWS22                                                       
         J     PRTROWSX                                                         
PROWS24  MVI   0(RF),C'-'          UNDERLINE REST OF FIELD                      
         LA    RF,1(RF)                                                         
         JCT   RE,PROWS24                                                       
                                                                                
PRTROWSX J     XITE                                                             
                                                                                
PROWCLFS CLC   0(0,RF),SPACES                                                   
PROWMV61 MVC   0(0,R6),0(R1)                                                    
         DROP  R3,R4,R7                                                         
         EJECT                                                                  
***********************************************************************         
* GENERATE FORMAT RECORDS FROM TABLE                                  *         
***********************************************************************         
                                                                                
GENB     NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                SIMULATE DATAMGR ROUTINES                    
         LARL  RF,GENBACT                                                       
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
GENBACT  J     GENRD               READ                                         
         J     GENHI               HIGH                                         
         DC    AL2(0,0)            HIGH (DELETED)                               
         J     GENSEQ              SEQUENTIAL                                   
         DC    AL2(0,0)            NO WRITES                                    
         J     GENGET              GET RECORD                                   
                                                                                
GENHI    DS    0H                                                               
GENRD    DS    0H                                                               
         LA    R7,GTTAB                                                         
         USING AGT,R7                                                           
         XC    AGT(AGTLNQ),AGT     FIRST TIME - CLEAR ADDRESS LIST              
         XC    DIR,DIR                                                          
         MVI   LOOPD,0             SET DEPTH OF LOOP                            
                                                                                
         L     R6,ADGOBLOC                                                      
         USING GOBLOCKD,R6                                                      
         MVC   ESWCOPT,GOWCONES    DETAIL OPTION FOR %EST. BILLS                
                                                                                
         MVI   HPP,1               SET HEADING PER PAGE (DETAIL)                
         CLI   BILTCDE,JXBLSPCL                                                 
         JE    GENHI3                                                           
         CLI   BILTCDE,JXBLPEST                                                 
         JE    GENHI3                                                           
                                                                                
         CLI   GOBILDET,NO         USELESS CLIENT OPTION IS SET TO NO           
         JNE   *+8                                                              
         MVI   HPP,2               SET HEADING PER PAGE (NO DETAIL)             
         CLI   GO1LINBL,YES        IS IT ONE-LINE PER W/C                       
         JNE   *+8                                                              
         MVI   HPP,2               SET HEADING PER PAGE (NO DETAIL)             
         DROP  R6                                                               
                                                                                
GENHI3   GOTOR GETBF                                                            
         USING BFTABD,RF                                                        
         LLH   R3,BFTTDISP         ASSUME POSITIVE DISPLACEMENT                 
         LA    R3,BFTTDISP(R3)                                                  
         DROP  RF                                                               
         GOTOR TLOOP,DMCB,0        LOOP THRU TABLE                              
         CLI   DIR,EOT             TEST EOF SET                                 
         JE    XITN                YES, NO RECORD                               
         J     GENX                                                             
                                                                                
GENSEQ   LA    R7,GTTAB                                                         
         XR    R0,R0               RESET PROPER DEPTH                           
         ICM   R0,1,LOOPD                                                       
         JZ    GENSEQ3                                                          
                                                                                
GENSEQ1  NTR1  LABEL=*             RETURN TO ORIGINAL LIST                      
         LA    R7,AGTLNQ(R7)                                                    
         JCT   R0,GENSEQ1                                                       
                                                                                
         ICM   R0,1,LOOPD          RETURN TO ORIGINAL LIST                      
GENSEQ2  GOTOR TLOOP,DMCB,1                                                     
         SHI   R7,AGTLNQ                                                        
         L     RD,4(RD)                                                         
         JCT   R0,GENSEQ2                                                       
                                                                                
GENSEQ3  L     R3,AGTEF                                                         
         GOTOR TLOOP,DMCB,1                                                     
         CLI   DIR,EOT             TEST EOF SET                                 
         JE    XITN                YES, NO RECORD                               
                                                                                
GENX     L     R2,ABFMR                                                         
         MVC   DIR(L'BFMKEY),0(R2) RETURN KEY OF RECORD                         
         J     XITY                                                             
                                                                                
         USING BFMRECD,R2                                                       
GENGET   ICM   R2,15,ABFMR         RETURN RECORD TO CALLER                      
         ICM   R0,15,AIO                                                        
         XR    R3,R3                                                            
         ICM   R3,3,BFMRLEN                                                     
         LA    R1,IOLNQ                                                         
         MVCL  R0,R2                                                            
         J     XITY                                                             
         DROP  R2                                                               
                                                                                
GETBF    LARL  RF,BFTAB            LOCATE BFTAB ENTRY - RETURN IN RF            
         USING BFTABD,RF                                                        
GETBF02  LLC   R1,BFTPGMAQ         MATCH PROGRAM TYPE                           
         EXRL  R1,GENHITM                                                       
         JZ    GETBF04                                                          
         CLC   FORMCDE,BFTFCODE    MATCH FORMAT CODE                            
         BER   RE                                                               
GETBF04  AHI   RF,BFTLNQ           BUMP TO NEXT TABLE ENTRY                     
         CLI   0(RF),EOT                                                        
         JNE   GETBF02                                                          
         DC    H'0'                NO MATCH                                     
         DROP  RF                                                               
                                                                                
GENHITM  TM    PGMAQ,0                                                          
         EJECT                                                                  
***********************************************************************         
* LOOP THRU GEN TABLE TO BUILD RECORDS                                *         
***********************************************************************         
                                                                                
TLOOP    NTR1  LABEL=*                                                          
         USING GTD,R3                                                           
         ICM   R0,15,0(R1)         TEST FIRST TIME                              
         JNZ   TLOOP3              NO,                                          
         CLI   GTTYP,GTEFQ         TEST ENTRY FILTER                            
         JE    TLOOP5              YES,                                         
         DC    H'0'                FIRST MUST BE ENTRY FILTER                   
                                                                                
TLOOP3   ICM   R3,15,AGTFTL        TEST LAST ITEM                               
         CLI   0(R3),GTFSQ         WAS FILTER                                   
         JE    TLOOP4              YES,                                         
         CLI   0(R3),GTTXQ         TEXT                                         
         JE    TLOOP4              YES,                                         
         CLI   0(R3),GTLKQ         LINK                                         
         JE    TLOOP4              YES,                                         
         DC    H'0'                BAD TABLE ENTRY                              
                                                                                
TLOOP4   LA    R3,GTLNQ(R3)        BUMP TO NEXT ENTRY                           
         CLI   GTTYP,GTEIQ         TEST END 'IF'                                
         JE    TLOOP4                                                           
         CLI   GTTYP,GTEFQ         TEST ENTRY FILTER                            
         JE    TLOOP5              YES,                                         
         CLI   GTTYP,GTIFQ         TEST 'IF' ENTRY                              
         JE    TLOOP6                                                           
         CLI   GTTYP,EOT           TEST EOT                                     
         JNE   TLOOP9                                                           
         J     TLOOPX              YES, ALL DONE                                
                                                                                
TLOOP5   ST    R3,AGTEF            ** NEXT FILTER ENTRY **                      
         GOTOR ENTFLT              TEST ENTRY NEEDED                            
         JE    TLOOP7              YES, PROCESS THIS ENTRY                      
         GOTOR ENTNXT              GET NEXT ENTRY FILTER                        
         CLI   GTTYP,GTEFQ         TEST ENTRY FILTER                            
         JE    TLOOP5              YES,                                         
         CLI   GTTYP,GTLKQ         TEST LINK ENTRY                              
         JE    TLOOP9              YES,                                         
         J     TLOOPX              EOF                                          
                                                                                
TLOOP6   GOTOR ENTFLT              TEST ENTRY NEEDED                            
         JE    TLOOP4              YES, PROCESS NEXT STATEMENTS                 
         XR    RF,RF               TRACK NUMBER OF IF STATEMENTS                
                                                                                
TLOOP62  LA    R3,GTLNQ(R3)        BUMP TO NEXT ENTRY                           
         CLI   GTTYP,GTIFQ         TEST ANOTHER IF                              
         JNE   *+8                                                              
         LA    RF,1(RF)            COUNT EMBEDDED IF'S                          
         CLI   GTTYP,GTEIQ         TEST END IF                                  
         JNE   TLOOP62                                                          
         LTR   RF,RF               TEST END EMBEDDED IF'S                       
         JZ    TLOOP4                                                           
         BCTR  RF,0                REDUCE COUNT                                 
         J     TLOOP62             LOOK AT NEXT                                 
                                                                                
TLOOP7   LA    R3,GTLNQ(R3)        POINT TO NEXT ENTRY                          
         CLI   GTTYP,GTLOQ         TEST LOCATION                                
         JE    *+6                 MUST BE LOCATION ENTRY                       
         DC    H'0'                MUST HAVE LOCATION ENTRY                     
         ST    R3,AGTLO            SET TO NEXT ENTRY                            
         MVI   RECSEQ,0            INITIALIZE SET RECORD SEQUENCE               
         LA    R3,GTLNQ(R3)        NEXT MUST BE FS OR TX                        
                                                                                
TLOOP9   ST    R3,AGTFTL           SET ENTRY ADDRESS                            
         CLI   GTTYP,GTFSQ         TEST FILTER SECTION                          
         JE    TLOOP11             YES,                                         
         CLI   GTTYP,GTTXQ         TEST TEXT                                    
         JE    TLOOP11             YES                                          
                                                                                
         CLI   GTTYP,GTLKQ         TEST LINK POINTER                            
         JE    *+6                                                              
         DC    H'0'                INVALID TABLE ENTRY                          
         LLC   R1,LOOPD            INCREMENT LOOP DEPTH                         
         LA    R1,1(R1)                                                         
         STC   R1,LOOPD            TEST MAXIMUM DEPTH                           
         CHI   R1,3                                                             
         JNH   *+6                                                              
         DC    H'0'                MAXIMUM LINK POINTER                         
         LR    R5,R7               SAVE CURRENT R7                              
         LA    R7,AGTLNQ(R7)       R7 TO NEXT SAVE AREA                         
         XC    AGT(AGTLNQ),AGT     FIRST TIME - CLEAR ADDRESS LIST              
         LLH   RF,GTLINK           POINT TO FORMAT ENTRY                        
         LARL  RE,GENB                                                          
         AR    RF,RE                                                            
         LR    R0,R3               SAVE CURRENT R3                              
         LR    R3,RF               SET R3 TO NEW TABLE                          
         GOTOR TLOOP,DMCB,0        LOOP THRU TABLE                              
         LR    R7,R5               RESTORE REGISTERS                            
         LR    R3,R0                                                            
         J     TLOOP4              CONTINUE WITH NEXT                           
                                                                                
TLOOP11  XC    SEQBF,SEQBF         INITIALIZE SEQUENCE FOR BFMEL                
         GOTOR BLDR                BUILD A BILL FORMAT RECORD                   
         JNE   TLOOP3              NO RECORD REQUIRED                           
         XR    RF,RF                                                            
         ICM   RF,1,LOOPD          TEST LOOP DEPTH                              
         JZ    XIT                 END OF LOOP                                  
         L     RD,4(RD)                                                         
         JCT   RF,*+8                                                           
         J     *-8                                                              
         J     XIT                                                              
                                                                                
TLOOPX   XR    RF,RF                                                            
         ICM   RF,1,LOOPD          TEST LOOP DEPTH                              
         JZ    TLOOPX3             END OF LOOP                                  
         BCTR  RF,0                REDUCE DEPTH                                 
         STC   RF,LOOPD            REDUCE LOOP                                  
         J     XIT                 EXIT, WITHOUT EOT                            
                                                                                
TLOOPX3  MVI   DIR,EOT             SET EOT                                      
         J     XIT                                                              
*                                  TEST FILTER ENTRY                            
ENTFLT   LR    R0,RE                                                            
         LLC   RF,BILTTYP          PROGRAM TYPE BIT                             
         EXRL  RF,ENTFTMTB         TEST RECORD NEEDED BY THIS PROGRAM           
         JZ    ENTNO               IF NOT, GET NEXT TABLE ENTRY                 
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,PGLV           TEST A27 LEVEL CONTROL                       
         JZ    ENTFLT3             NO,                                          
         EXRL  RF,ENTFTMLV         TEST MATCH LEVEL OF TABLE                    
         JO    ENTFLT3             YES, PROCESS                                 
         TM    GTEFGLV,LVR         TEST TABLE ENTRY IS AT REQ LEVEL             
         JNO   ENTNO               NO, SKIP IT                                  
         MVC   BYTE,PGLV           CURRENT PROCESS LEVEL                        
         NC    BYTE,RQLV           REQUEST LEVEL                                
         CLI   BYTE,0              TEST AT REQUEST LEVEL                        
         JE    ENTNO               NO, SKIP IT                                  
                                                                                
ENTFLT3  XR    RF,RF                                                            
         ICM   RF,3,GTEFCND        TEST CONDITIONAL                             
         JZ    ENTYES              NO TEST - IT'S OK                            
         LA    R4,AMTS             FOR CONDITIONAL AMOUNTS                      
         USING AMTD,R4                                                          
         LARL  RE,GENB                                                          
         AR    RF,RE                                                            
         GOTOR (RF)                                                             
         JE    ENTYES                                                           
         J     ENTNO                                                            
                                                                                
ENTNXT   LR    R0,RE                                                            
ENTNXT1  LA    R3,GTLNQ(R3)        BUMP TO NEXT ENTRY                           
         CLI   GTTYP,EOT           TEST EOT                                     
         JE    ENTNO               YES, ALL DONE                                
         CLI   GTTYP,GTEFQ         TEST ENTRY FILTER                            
         JE    ENTYES                                                           
         CLI   GTTYP,GTLKQ         TEST LINK POINTER                            
         JE    ENTYES                                                           
         J     ENTNXT1             NO, GET NEXT                                 
                                                                                
ENTNO    LR    RE,R0                                                            
         LTR   R0,R0                                                            
         BR    RE                                                               
                                                                                
ENTYES   LR    RE,R0                                                            
         XR    R0,R0                                                            
         LTR   R0,R0                                                            
         BR    RE                                                               
                                                                                
ENTFTMTB TM    GTEFBTY,0                                                        
ENTFTMLV TM    GTEFGLV,0                                                        
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD A FORMAT RECORD FROM FORMAT TABLES                            *         
***********************************************************************         
                                                                                
BLDR     NTR1  LABEL=*                                                          
         MVI   TOP,0               TOP LINE OF PARAGRAPH                        
         MVI   BOT,0               BOTTOM OF PARAGRAPH                          
         MVI   NBFLDS,0                                                         
         XC    BFLDS,BFLDS         CLEAR LIST OF FIELD CODES                    
                                                                                
         XC    BFSWRK,BFSWRK       INITIALIZE SORT ELEMENT                      
         LA    RE,BFSWRK                                                        
         USING BFSELD,RE                                                        
         MVI   BFSEL,BFSELQ                                                     
         MVI   BFSLN,BFSLNQ                                                     
         MVC   BFSLVL,RECSEQ                                                    
         DROP  RE                                                               
                                                                                
         L     R3,AGTEF            R3 TO ENTRY FILTER                           
         USING GTD,R3                                                           
         L     R2,ABFMR                                                         
         USING BFMRECD,R2          BUILD RECORD KEY                             
         XC    BFMKEY(BFMRFST-BFMKEY+2),BFMKEY                                  
         MVI   BFMKTYP,BFMKTYPQ    RECORD TYPE                                  
         MVC   BFMKCPY,RCCOMPFL    COMPANY                                      
         MVI   BFMKSUB,BFMKSUBQ    SUB-RECORD                                   
         MVC   BFMKLANG,RCLANG     LANGUAGE                                     
         MVC   BFMKLVL,=AL2(BFMKLMNQ) LEVEL - MAIN                              
         CLI   GTEFILV,GTEFIDQ                                                  
         JE    BLDR5                                                            
         MVC   BFMKLVL,=AL2(BFMKLSMQ) LEVEL - SUMMARY                           
         CLI   GTEFILV,GTEFISQ                                                  
         JE    BLDR5                                                            
         DC    H'0'                INCORRECT LEVEL IN TABLE                     
                                                                                
BLDR5    L     R3,AGTLO            R3 TO LOCATION ENTRY                         
         MVC   BFMKWHER,GTLOWHRB WHERE                                          
         MVC   BFMKSWHR,GTLOWHRS WHERE - IN SECTION                             
         MVC   BFMKSEQ,RECSEQ                                                   
         LLC   R0,RECSEQ                                                        
         AHI   R0,1                                                             
         STC   R0,RECSEQ                                                        
                                                                                
         MVC   BFMKSECT,GTLOSECN SECTION                                        
                                                                                
         ICM   R3,15,AGTFTL        TEST FILTER SECTION                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),GTFSQ                                                      
         JNE   BLDR7                                                            
         GOTOR FLTR                                                             
         J     BLDR15                                                           
                                                                                
BLDR7    CLI   0(R3),GTTXQ         TEXT ENTRY                                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XR    RF,RF                                                            
         ICM   RF,3,GTTXCND        ANY CONDITIONS FOR TEXT                      
         JZ    BLDR8               NO, CREATE RECORD                            
         LARL  RE,GENB                                                          
         AR    RF,RE                                                            
         LA    R4,AMTS                                                          
         GOTOR (RF)                                                             
         JNE   XITN                NO RECORD REQUIRED                           
                                                                                
BLDR8    ICM   R3,15,AGTLO         GET LOCATION ENTRY                           
         TM    GTLOSTA,GTLOBO      TEST RECORD NEEDS BOFEL                      
         JNO   *+8                                                              
         GOTOR ADDBOF              ADD FORMAT PARAGRAPH ELEMENT                 
                                                                                
         TM    GTLOSTA,GTLOBF      TEST RECORD NEEDS BFPEL                      
         JNO   BLDR82                                                           
         GOTOR ADDBFP              ADD FORMAT PARAGRAPH ELEMENT                 
                                                                                
BLDR82   TM    GTLOSTA,GTLOBT      TEST RECORD NEEDS BTTEL                      
         JNO   BLDR84                                                           
         GOTOR ADDBTT              ADD BILLING TOTALS ELEMENT                   
                                                                                
BLDR84   ICM   R3,15,AGTFTL        GET TEXT ENTRY                               
         GOTOR FMTL                FORMAT TEXT ELEMENTS                         
                                                                                
BLDR9    TM    GTTXSTA,CONTQ       TEST FOR CONTINUATION                        
         JNO   BLDR11                                                           
         LA    R3,GTLNQ(R3)                                                     
                                                                                
         XR    RF,RF                                                            
         ICM   RF,3,GTTXCND        ANY CONDITIONS FOR TEXT                      
         JZ    BLDR10              NO, CREATE RECORD                            
         LARL  RE,GENB                                                          
         AR    RF,RE                                                            
         LA    R4,AMTS                                                          
         GOTOR (RF)                TEST CONDITIONAL                             
         JNE   BLDR9                                                            
                                                                                
BLDR10   ST    R3,AGTFTL           SET ENTRY ADDRESS                            
         J     BLDR8                                                            
                                                                                
BLDR11   L     R2,ABFMR                                                         
         MVI   ELCODE,BFMELQ       MUST HAVE SOME TEXT                          
         GOTOR GETELN                                                           
         JNE   XITN                SKIP RECORD                                  
                                                                                
         L     R2,ABFMR                                                         
BLDR15   OC    BFMKSWHR,BFMKSWHR   TEST SECTION MASTER RECORD                   
         JZ    BLDR16              YES, SKIP SORT ELEMENTS                      
         GOTOR SRTL                ADD SORT ELEMENT                             
BLDR16   LA    R4,BFMRFST                                                       
         CLI   0(R4),0             TEST ANY ELEMENTS                            
         JE    XITN                SKIP RECORD                                  
                                                                                
         CLI   0(R4),BFPELQ                                                     
         JNE   XITY                TEST FORMAT PARAGRAPH ELEMENT                
         USING BFPELD,R4                                                        
         LLC   RF,BOT              BOTTOM                                       
         LLC   RE,TOP                                                           
         SR    RF,RE               LESS TOP                                     
         LA    RF,1(RF)                                                         
         STC   RF,BFPHGT                                                        
         STC   RF,BFPHGTMN                                                      
                                                                                
         LLC   RF,BFPLN            LOOK AT NEXT ELEMENT                         
         AR    R4,RF                                                            
         CLI   0(R4),0             IF NO DATA                                   
         JE    XITN                SKIP RECORD                                  
         J     XITY                                                             
         DROP  R2,R3,R4,R9                                                      
         EJECT                                                                  
***********************************************************************         
* CREATE AND ADD BFMEL(FORMAT ELEMENT)  TO BFM RECORD                 *         
***********************************************************************         
                                                                                
FMTL     NTR1  LABEL=*                                                          
         L     R3,AGTFTL                                                        
         USING GTD,R3                                                           
         SR    R2,R2                                                            
         ICM   R2,3,GTTXPNL                                                     
         JZ    XITN                NO PANEL CODE IN TABLE                       
         LARL  RE,GENB                                                          
         AR    R2,RE                                                            
         USING DID,R2                                                           
                                                                                
FMTL3    XR    RF,RF                                                            
         ICM   RF,3,DICNDL         IS THERE A CONDITIONAL?                      
         JZ    FMTL5                                                            
         LA    R4,AMTS                                                          
         L     R5,ABFMR                                                         
         LARL  RE,GENB                                                          
         AR    RF,RE                                                            
         GOTOR (RF)                                                             
         JNE   FMTL19              SKIP IT                                      
                                                                                
FMTL5    L     R4,AELMNT                                                        
         XC    0(255,R4),0(R4)                                                  
                                                                                
         USING BFMELD,R4                                                        
         MVI   BFMEL,BFMELQ        ELEMENT CODE                                 
         MVI   BFMLN,BFMLNQ        LENGTH                                       
         XR    RF,RF                                                            
         ICM   RF,3,SEQBF          INCREMENT SEQUENCE NUMBER                    
         AHI   RF,1                                                             
         STCM  RF,3,SEQBF                                                       
         STCM  RF,3,BFMSEQ                                                      
         MVC   BFMLEFT,DICOL       COLUMN                                       
         MVC   BFMTOP,DIROW        ROW                                          
         MVC   BFMWTH,DIWTH        WIDTH                                        
         MVC   BFMHGT,DIHGT        HEIGHT                                       
         TM    DISTA,CNTR                                                       
         JNO   *+8                                                              
         MVI   BFMALN,BFMACTRQ     CENTER                                       
         TM    DISTA,RGHT                                                       
         JNO   *+8                                                              
         MVI   BFMALN,BFMARGTQ     RIGHT                                        
         TM    DISTA,UNDR                                                       
         JNO   *+8                                                              
         OI    BFMINDS1,BFMIUND    UNDERLINE                                    
         TM    DISTA,LFT                                                        
         JNO   *+8                                                              
         OI    BFMINDS1,BFMILFT    FIELD MOVES LEFT, 1 SPACE                    
         TM    DISTA,LFT0                                                       
         JNO   *+8                                                              
         OI    BFMINDS1,BFMILFT0   FIELD MOVES LEFT, NO SPACE                   
         TM    DISTA,CDO                                                        
         JNO   *+8                                                              
         OI    BFMINDS1,BFMICDO    ONLY APPLIES IF CD NE ZERO                   
         TM    DISTA,NUM                                                        
         JNO   *+8                                                              
         OI    BFMSTAT1,BFMSNUM    NUMBER                                       
         TM    DISTA2,REV                                                       
         JNO   *+8                                                              
         OI    BFMINDS1,BFMIREV    REVERSE NUMBER FOR DISPLAY                   
         TM    DISTA2,UNDR2                                                     
         JNO   *+8                                                              
         OI    BFMINDS1,BFMIUND+BFMIUND2                                        
                                                                                
         TM    PGMAQ,PGMALL27      TEST A27 GROUP BILL                          
         JNZ   *+16                YES, DON'T USE CR=YES                        
         TM    DISTA2,CRY                                                       
         JNO   *+8                                                              
         OI    BFMINDS1,BFMICRY    SET CR=YES                                   
                                                                                
         TM    DISTA2,FUP                                                       
         JNO   *+8                                                              
         OI    BFMINDS1,BFMIFUP    SET DATA CAN FLOAT UP                        
         TM    DISTA2,PZA                                                       
         JNO   *+8                                                              
         OI    BFMINDS2,BFMIPZA    SET PRINT ZERO AMOUNT                        
         TM    DISTA2,PDOL                                                      
         JNO   *+8                                                              
         OI    BFMINDS2,BFMIDOL                                                 
                                                                                
         TM    DISPC,DISPCQ        TEST SPECIAL CODE                            
         JO    FMTL11                                                           
         CLI   DIBLF,0                                                          
         JE    FMTL7                                                            
         MVC   BFMBODY,DIBLF       BLFFLD CODE                                  
         MVI   BFMTYPE,BFMTDATA                                                 
         CLI   DIBLFSQ,0           TEST SORT SEQUENCE NUMBER                    
         JE    FMTL7                                                            
                                                                                
         CLI   DIBLFSQ,NXTQ        TEST OK TO ADD TO END                        
         JNE   FMTL6                                                            
         LLC   RF,NBFLDS                                                        
         LA    RE,BFLDS(RF)                                                     
         MVC   0(1,RE),DIBLF       ADD TO LIST OF BLFFLD'S                      
         LA    RF,1(RF)                                                         
         STC   RF,NBFLDS           AND UPDATE NUMBER                            
         J     FMTL7                                                            
                                                                                
FMTL6    LA    RF,BFSWRK                                                        
         USING BFSELD,RF                                                        
         LLC   RE,DIBLFSQ          SET RE TO FIELD FOR CODE                     
         BCTR  RE,0                                                             
         LA    RE,BFSBODYS(RE)                                                  
         CLI   0(RE),0                                                          
         JE    *+6                                                              
         DC    H'0'                DUPLICATE SORT SEQUENCE NUMBER               
         MVC   0(L'DIBLF,RE),DIBLF ADD BFMFLD FIELD TO SORT                     
         DROP  RF                                                               
                                                                                
FMTL7    XR    RF,RF                                                            
         ICM   RF,1,DIPNL          PANEL CODE                                   
         JZ    FMTL9                                                            
         STC   RF,BFMPNLC                                                       
         MVI   BFMTYPE,BFMTDATA                                                 
         TM    DISTA,FTX           TREAT SPECIAL PANELS AS FREE TEXT            
         JNO   *+8                                                              
         MVI   BFMTYPE,BFMTFREE    FREE TEXT                                    
                                                                                
FMTL9    SR    R1,R1               DATA DICTIONARY ITEM?                        
         ICM   R1,1,DIDICL                                                      
         JZ    FMTL17                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DIDIC          DISPLACMENT TO DICTIONARY ITEM               
         A     RF,ADICO                                                         
         CLM   R1,1,DIWTH          OR WIDTH OF FIELD                            
         JNH   *+8                                                              
         IC    R1,DIWTH                                                         
         BCTR  R1,0                                                             
         EXRL  R1,FMTLMVTX         DICTIONARY DATA TO ELEMENT                   
         LA    RF,BFMTEXT(R1)      REMOVE TRAILING SPACES                       
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         JCT   R1,*-10                                                          
         LA    R1,1(R1)                                                         
         STCM  R1,3,BFMTLEN        SET DATA LENGTH                              
         LA    R1,BFMLNQ(R1)                                                    
         STC   R1,BFMLN            AND ELEMENT LENGTH                           
         J     FMTL17                                                           
                                                                                
FMTL11   LLC   R1,BFMWTH           SPECIAL CHARACTERS                           
         STCM  R1,3,BFMTLEN        DATA LENGTH                                  
         MVI   BFMTYPE,BFMTFREE    FREE TEXT                                    
         LA    RF,BFMLNQ(R1)                                                    
         STC   RF,BFMLN            ELEMENT LENGTH                               
         MVC   BFMTEXT(1),DISPCH   SPECIAL CHARACTER                            
         SHI   R1,2                                                             
         JM    FMTL17                                                           
         EXRL  R1,FMTLMVTT                                                      
         SR    RF,RF                                                            
         ICM   RF,1,DIPNL          PANEL CODE                                   
         JZ    FMTL17                                                           
         STC   RF,BFMPNLC                                                       
                                                                                
FMTL17   SR    RF,RF                                                            
         CLI   TOP,0                                                            
         JNE   *+10                                                             
         MVC   TOP,BFMTOP                                                       
         LLC   RF,BFMTOP                                                        
         CLM   RF,1,TOP                                                         
         JNL   *+8                                                              
         STC   RF,TOP              SAVE FIRST LINE IN PARAGRAPH                 
         LLC   RE,BFMHGT                                                        
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         CLM   RE,1,BOT                                                         
         JNH   *+8                                                              
         STC   RE,BOT              SAVE LAST LINE IN PARAGRAPH                  
                                                                                
         GOTOR HELLO,DMCB,(C'P',ACCMST),ABFMR,BFMELD,ADDEND                     
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
FMTL19   LA    R2,DILNQ(R2)                                                     
         CLI   0(R2),EOT                                                        
         JNE   FMTL3                                                            
         J     XITY                                                             
                                                                                
FMTLMVTX MVC   BFMTEXT(0),0(RF)                                                 
FMTLMVTT MVC   BFMTEXT+1(0),BFMTEXT                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE AND ADD BSDEL(FILTER ELEMENT)   TO BFM RECORD                *         
***********************************************************************         
                                                                                
FLTR     NTR1  LABEL=*                                                          
         L     R3,AGTFTL                                                        
         USING GTD,R3                                                           
         LLH   R2,GTFSCND                                                       
         LARL  RE,GENB                                                          
         AR    R2,RE                                                            
                                                                                
FLTR3    LLH   RF,0(R2)            DISPL. TO FILTER ITEM                        
         LARL  RE,GENB                                                          
         AR    RF,RE                                                            
                                                                                
         L     R4,AELMNT                                                        
         USING BSDELD,R4                                                        
         XC    BSDEL(BSDLN1Q),BSDEL                                             
         MVI   BSDEL,BSDELQ        ELEMENT CODE                                 
         MVI   BSDLN,BSDLN1Q       LENGTH                                       
         MVC   BSDSEC,GTFSNUM      SECTION NUMBER                               
         MVC   BSDSTYP,1(RF)       TYPE OF FILTER                               
                                                                                
         LLC   RE,0(RF)            LENGTH                                       
         SHI   RE,2                                                             
         LTR   RE,RE                                                            
         JNP   FLTR5               NO DATA                                      
         LA    R0,BSDLN1Q(RE)                                                   
         STC   R0,BSDLN            SET LENGTH WITH DATA                         
         BCTR  RE,0                                                             
         EXRL  RE,FLTRMVBF         MOVE DATA TO ELEMENT                         
                                                                                
FLTR5    GOTOR HELLO,DMCB,(C'P',ACCMST),ABFMR,BSDELD,ADDEND                     
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,2(R2)            BUMP TO NEXT FILTER ITEM                     
         CLI   0(R2),EOT                                                        
         JNE   FLTR3               PROCESS NEXT FILTER ITEM                     
         J     XITY                                                             
                                                                                
FLTRMVBF MVC   BSDDATA(0),2(RF)                                                 
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*  ADD BFSEL(SORT ELEMENT)   TO BFM RECORD                            *         
***********************************************************************         
                                                                                
SRTL     NTR1  LABEL=*                                                          
         LA    R4,BFSWRK                                                        
         USING BFSELD,R4                                                        
         LA    R5,BFSBODYS         SHIFT CODES LEFT                             
         SR    R3,R3               R3 COUNT NUMBER OF FIELDS ADDED              
         OC    BFSBODYS(MXSRT),BFSBODYS                                         
         JZ    SRTL9                                                            
                                                                                
         LA    R0,MXSRT                                                         
         CLI   0(R5),0             FIND FIRST OPEN BYTE                         
         JE    SRTL3                                                            
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)                                                         
         JCT   R0,*-16                                                          
         DC    H'0'                TOO MANY SORT FIELDS                         
                                                                                
SRTL3    LA    R6,1(R5)            FIND FIRST SORT ITEM AFTER NULLS             
         LA    R0,MXGAP                                                         
SRTL5    CLI   0(R6),0                                                          
         JE    SRTL7                                                            
         MVC   0(1,R5),0(R6)       MOVE FIELD NUMBER TO SORT ELEMENT            
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)            COUNT ITEMS ADDED                            
         MVI   0(R6),0                                                          
                                                                                
SRTL7    LA    R6,1(R6)                                                         
         JCT   R0,SRTL5                                                         
                                                                                
SRTL9    XR    R0,R0                                                            
         ICM   R0,1,NBFLDS         ADD OTHER SORT FIELDS TO END                 
         JZ    SRTL11                                                           
         AR    R3,R0                                                            
         LA    R6,BFLDS                                                         
         MVC   0(1,R5),0(R6)                                                    
         LA    R6,1(R6)                                                         
         LA    R5,1(R5)                                                         
         JCT   R0,*-14                                                          
                                                                                
SRTL11   SR    RF,RF                                                            
         ICM   RF,1,BFSLN          UPDATE ELEMENT LENGTH                        
         AR    RF,R3                                                            
         STCM  RF,1,BFSLN                                                       
                                                                                
         GOTOR HELLO,DMCB,(C'P',ACCMST),ABFMR,BFSWRK,ADDEND                     
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD BILLING OPTIONS FORMAT ELEMENT                                  *         
***********************************************************************         
                                                                                
ADDBOF   NTR1  LABEL=*                                                          
         L     R4,AELMNT                                                        
         USING BOFELD,R4                                                        
         XC    BOFEL(BOFLNQ),BOFEL                                              
         MVI   BOFEL,BOFELQ                                                     
         MVI   BOFLN,BOFLNQ                                                     
         MVC   BOFMAXLN,MAXLINES   LINES PER PAGE                               
         MVI   BOFMAXWD,132        PAGE WIDTH                                   
                                                                                
         GOTOR HELLO,DMCB,(C'P',ACCMST),ABFMR,BOFELD,ADDEND                     
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD FORMAT PARAGRAPH ELEMENT                                        *         
***********************************************************************         
                                                                                
ADDBFP   NTR1  LABEL=*                                                          
         L     R3,AGTFTL                                                        
         USING GTD,R3                                                           
         L     R4,AELMNT                                                        
         USING BFPELD,R4                                                        
         XC    BFPEL(BFPLNQ),BFPEL                                              
         MVI   BFPEL,BFPELQ                                                     
         MVI   BFPLN,BFPLNQ                                                     
         MVI   BFPHGT,1            SET HEIGHT                                   
         MVI   BFPHGTMN,1          SET MINIMUM HEIGHT                           
         TM    GTTXSPA,SP1A        TEST EXTRA SPACE AFTER                       
         JNO   *+8                                                              
         MVI   BFPSPLS,1           SPACE LINES                                  
                                                                                
         TM    GTTXSPA,SP1B        TEST EXTRA SPACE BEFORE                      
         JNO   *+8                                                              
         MVI   BFPSPBP,1           SPACE LINES                                  
                                                                                
         GOTOR HELLO,DMCB,(C'P',ACCMST),ABFMR,BFPELD,ADDEND                     
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ADD BILLINGS TOTAL ELEMENT                                          *         
***********************************************************************         
                                                                                
ADDBTT   NTR1  LABEL=*                                                          
         L     R4,AELMNT                                                        
         USING BTTELD,R4                                                        
         XC    BTTEL(BTTLNQ),BTTEL                                              
         MVI   BTTEL,BTTELQ                                                     
         MVI   BTTLN,BTTLNQ                                                     
         MVI   BTTMAINL,2          MAIN TOTALS LEFT COLUMN                      
         MVI   BTTMAINW,32         MAIN TOTALS TEXT WIDTH                       
         MVI   BTTVATL,2           VAT SUMMARY LEFT COLUMN                      
         MVI   BTTPREVL,2          PREVIOUS BILLS LEFT COLUMN                   
                                                                                
         OI    BTTINDS1,BTTIAVAT   APPEND VAT SUMMARY                           
         OI    BTTINDS1,BTTIAVAG   APPEND VAT SUMMARY IN AGENCY CURR.           
         OI    BTTINDS1,BTTIAPRV   APPEND PREVIOUS BILLS                        
         OI    BTTINDS1,BTTINET    SHOW NET TOTAL                               
         OI    BTTINDS1,BTTICOM    SHOW COMMISSION TOTAL                        
         OI    BTTINDS1,BTTIGRS    SHOW GROSS TOTAL                             
         OI    BTTINDS1,BTTISRCP   SHOW SURCHARGE PERCENTAGE                    
         OI    BTTINDS1,BTTIDSCP   SHOW DISCOUNT PERCENTAGE                     
                                                                                
         OI    BTTINDS2,BTTICURT   SHOW CURRENCY CODE FOR INVOICE TOTAL         
         OI    BTTINDS2,BTTIPPAY   PRINT "PLEASE PAY THIS AMOUNT"               
         OI    BTTINDS2,BTTICRAM   PRINT "CREDIT AMOUNT" ON CREDIT BILL         
         OI    BTTINDS2,BTTIVATA   SHOW AGENCY VAT AMOUNT                       
                                                                                
         GOTOR HELLO,DMCB,(C'P',ACCMST),ABFMR,BTTELD,ADDEND                     
         CLI   12(R1),0                                                         
         JE    XIT                                                              
         DC    H'0'                                                             
         DROP  R4,R7                                                            
         EJECT                                                                  
***********************************************************************         
* CONDITIONALS                                                        *         
***********************************************************************         
         USING JXBLKD,R9                                                        
         USING AMTD,R4                                                          
         USING BFMRECD,R5                                                       
IFORGN   TM    OPT3,OPT3PNH        ORIGIN NAME IN HEAD                          
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFORGA   TM    OPT3,OPT3PAH        ORIGIN ADDRESS IN HEAD                       
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFEST    TM    OPT1,OPT1ESTS       TEST SUPPRESS ESTIMATE                       
         JO    NOX                                                              
         J     YESX                WANT ESTIMATES                               
                                                                                
IFDD     TM    OPT3,OPT3SDD        TEST SUPPRESS DUE DATE                       
         JO    NOX                                                              
         J     YESX                WANT DUE DATE                                
                                                                                
IFSHPD   OC    SHPDT0,SHPDT0       TEST ANY SHIP DATE                           
         JZ    NOX                                                              
         J     YESX                WANT SHIP DATE                               
                                                                                
IFPLIN   TM    OPT1,OPT1PVIN       LONG INVOICE REQUIRES INVOICE                
         JZ    NOX                                                              
         TM    OPT6,OPT6PLIV                                                    
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFPVIN   TM    OPT1,OPT1PVIN       PRINT ANY REFERENCE                          
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFNPVIN  TM    OPT1,OPT1PVIN       NO PRINT VENDOR INVOICE                      
         JNZ   NOX                                                              
         TM    OPT6,OPT6PLIV       OR LONG INVOICE                              
         JNZ   NOX                                                              
         J     YESX                                                             
                                                                                
IFVN     TM    OPT1,OPT1SVN        TEST SUPPRESS VENDOR NAME                    
         JO    NOX                                                              
         J     YESX                WANT VENDOR NAME                             
                                                                                
IFNC     TM    OPT1,OPT1SNC        SUPPRESS NET & COMM.                         
         JO    NOX                                                              
         J     YESX                                                             
                                                                                
IFCDWC   TM    OPT2,OPT2CDWC       CD BY WORKCODE                               
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFWCLN   TM    OPT3,OPT3PWCL       WORKCODE LONG NAME                           
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFWCDS   TM    OPT3,OPT3PWCL       WORKCODE DESCRIPTION(NOT LONG)               
         JNO   YESX                                                             
         J     NOX                                                              
                                                                                
IFPROB   TM    OPT3,OPT3PROB       PRINT REGISTRATION #                         
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFRETL   TM    JXSTAT1,JXS1DIST    RETAIL BILL                                  
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFNRETL  TM    JXSTAT1,JXS1DIST    NOT RETAIL BILL                              
         JNO   YESX                                                             
         J     NOX                                                              
                                                                                
IFTOTB   TM    BILTTYP,TOTL        TOTAL BILL                                   
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFESTM   TM    BILTTYP,ESTM        %EST BILL                                    
         JNO   NOX                                                              
         TM    JXSTAT1,JXS1DIST    NOT RETAIL BILL                              
         JO    NOX                                                              
         J     YESX                                                             
                                                                                
IFNTOTB  TM    BILTTYP,TOTL        NOT TOTAL BILL                               
         JNO   YESX                                                             
         J     NOX                                                              
                                                                                
IFCSD    CP    CSD$,PZERO          NOT CASH DISCOUNT                            
         JNE   YESX                                                             
         J     NOX                                                              
                                                                                
IFNESWCN CLI   ESWCOPT,ESWCNONE    NOT - EST. DET=NO                            
         JNE   YESX                                                             
         J     NOX                                                              
                                                                                
IFPB     TM    OPT2,OPT2SPB        TEST SUPPRESS PREVIOUS BILLS                 
         JO    NOX                                                              
         OC    NPRVB,NPRVB         TEST ANY PREVIOUS BILLS                      
         JZ    NOX                                                              
         J     YESX                WANT PREVIOUS BILLS                          
                                                                                
IFGTAX   TM    CTAXOPT,CTAXOGST    GST APPLIES                                  
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFGST    TM    CTAXOPT,CTAXOBHG    ANY GST?                                     
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFPST    TM    CTAXOPT,CTAXOBHP    ANY PST?                                     
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFNGST   TM    CTAXOPT,CTAXOBHG    ANY TAX DUE                                  
         JZ    YESX                                                             
         J     NOX                                                              
                                                                                
IFESTGST TM    JXSTAT1,JXS1DIST    RETAIL BILL                                  
         JO    NOX                                                              
         TM    BILTTYP,ESTM        %EST BILL + GST                              
         JNO   NOX                                                              
         CP    DUEGST,PZERO        ANY TAX DUE                                  
         JE    NOX                                                              
         TM    CTAXOPT,CTAXOGST    GST APPLIES                                  
         JNO   NOX                                                              
         J     YESX                                                             
                                                                                
IFESTPST TM    JXSTAT1,JXS1DIST    RETAIL BILL                                  
         JO    NOX                                                              
         TM    BILTTYP,ESTM        %EST BILL + PST                              
         JNO   NOX                                                              
         CP    DUEPST,PZERO        ANY TAX DUE                                  
         JE    NOX                                                              
         TM    CTAXOPT,CTAXOGST    GST APPLIES                                  
         JNO   NOX                                                              
         J     YESX                                                             
                                                                                
IFRORT   TM    JXSTAT1,JXS1DIST    RETAIL BILL                                  
         JNO   IFRORT1             NO,                                          
         TM    BILTTYP,ONEL        ONE LINE                                     
         JNO   YESX                NO,                                          
         TM    OPT3,OPT3STFR       TEST OPTION TO SUPPRESS TOTAL                
         JO    NOX                                                              
         J     YESX                                                             
                                                                                
IFRORT1  TM    BILTTYP,TOTL        OR TOTAL BILL                                
         JO    YESX                                                             
         TM    BILTTYP,ONEL        OR ONE LINE                                  
         JO    YESX                                                             
         TM    JFLG,JFMED          OR MEDIA CHARGES                             
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFNRNTH  TM    JXSTAT1,JXS1DIST    NOT RETAIL BILL                              
         JO    NOX                                                              
         TM    BILTTYP,TOTL        NOT TOTAL BILL                               
         JO    NOX                                                              
         TM    BILTTYP,ONEL        NOT ONE LINE                                 
         JO    NOX                                                              
         TM    JFLG,JFMED          OR NOT MEDIA CHARGES                         
         JO    NOX                                                              
         CLI   HPP,1               AND IT IS DETAIL HEADING                     
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
IFESWCB  CLI   ESWCOPT,ESWCBOTH    WC=BOTH, OR                                  
         JE    YESX                                                             
         CLI   ESWCOPT,ESWCCODE    WC=CODE                                      
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
IFESWC1  CLI   ESWCOPT,ESWCBOTH    WC=BOTH, AND                                 
         JNE   NOX                                                              
         TM    OPT3,OPT3PWCL       DESCRIPTION (NOT LONG NAME)                  
         JNO   YESX                                                             
         J     NOX                                                              
                                                                                
IFESWC2  CLI   ESWCOPT,ESWCBOTH    WC=BOTH, AND                                 
         JNE   NOX                                                              
         TM    OPT3,OPT3PWCL       LONG NAME                                    
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFESWC3  CLI   ESWCOPT,ESWCNAME    DESCRIPTION                                  
         JNE   NOX                                                              
         TM    OPT3,OPT3PWCL       BUT NOT LONG NAME                            
         JNO   YESX                                                             
         J     NOX                                                              
                                                                                
IFESWC4  CLI   ESWCOPT,ESWCNAME     DESCRIPTION                                 
         JNE   NOX                                                              
         TM    OPT3,OPT3PWCL        AND LONG NAME ONLY                          
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFPROG   TM    BILTTYP,PROG        PROGRESS                                     
         JZ    NOX                                                              
         J     YESX                                                             
                                                                                
IFTOTPE  TM    BILTTYP,TOTL+ESTM+ONEL   TOTAL OR PERCENT OF EST.                
         JZ    NOX                                                              
         J     YESX                                                             
                                                                                
IFNCPRO  TM    OPT1,OPT1SNC        TEST PRINTING NET & COMMISSION               
         JO    NOX                                                              
         TM    BILTTYP,PROG        TEST PROGRESS                                
         JZ    NOX                                                              
         J     YESX                                                             
                                                                                
IFNCTOT  TM    OPT1,OPT1SNC        TEST PRINTING NET & COMMISSION               
         JO    NOX                                                              
         TM    BILTTYP,TOTL+ESTM+ONEL  TEST TOTAL OR PERCENT OF EST.            
         JZ    NOX                                                              
         J     YESX                                                             
                                                                                
IFHPP1   CLI   HPP,1               USE DETAIL HEAD PER PAGE                     
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
IFHPP2   CLI   HPP,2               USE NO DETAIL HEAD PER PAGE                  
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
IFPYNET  TM    JXSTAT3,JXS3PNET     PAY=NET                                     
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFPYNETZ TM    JXSTAT3,JXS3PNET     PAY=NET AND NET IS ZERO                     
         JNO   NOX                                                              
         CP    DUENET,PZERO                                                     
         JNE   NOX                                                              
         J     YESX                                                             
                                                                                
IFPYGRS  TM    JXSTAT3,JXS3PNET     PAY=GROSS (NOT PAY=NET)                     
         JNO   YESX                                                             
         J     NOX                                                              
                                                                                
IFPYGRSZ TM    JXSTAT3,JXS3PNET     PAY=GROSS AND GROSS IS ZERO                 
         JO    NOX                                                              
         CP    DUEGRS,PZERO                                                     
         JNE   NOX                                                              
         J     YESX                                                             
                                                                                
*                                  PRINT POB AFTER 'TOTAL CHARGES'              
IFPOB    TM    JXPBSTA,JXPBSPCT    TEST PERCENT OF BILL OPTION                  
         JZ    NOX                                                              
         TM    PGMSTA,PGMUNB       IS IT UNBILLING?                             
         JO    NOX                                                              
         OC    NPRVB,NPRVB         TEST ANY PREVIOUS BILLS                      
         JZ    YESX                NONE, PRINT IT NOW                           
         TM    BILTTYP,TOTL+ONEL   TOTAL BILL                                   
         JZ    YESX                NO,  OK TO PRINT                             
         J     NOX                                                              
                                                                                
*                                  PRINT POB AFTER 'PREVIOUS BILLS'             
IFPOBP   TM    JXPBSTA,JXPBSPCT    TEST PERCENT OF BILL OPTION                  
         JZ    NOX                                                              
         TM    PGMSTA,PGMUNB       IS IT UNBILLING?                             
         JO    NOX                                                              
         OC    NPRVB,NPRVB         TEST ANY PREVIOUS BILLS                      
         JZ    NOX                 NONE, ALREADY PRINTED                        
         TM    BILTTYP,TOTL+ONEL   TOTAL BILL                                   
         JZ    NOX                 NO,  ALREADY PRINTED                         
         J     YESX                                                             
         J     NOX                                                              
                                                                                
IFMEDC   TM    JFLG,JFMED          TEST MEDIA CHARGES                           
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFXFR    TM    OPT2,OPT2PTI        TEST TRANSFER INFO WANTED                    
         JNO   NOX                                                              
         J     YESX                                                             
                                                                                
IFNCTX$  TM    OPT1,OPT1SNC        SUPPRESS NET & COMM.                         
         JO    NOX                                                              
IFTAX$   CP    DUEGST,PZERO        TEST ANY TAX                                 
         JNE   YESX                                                             
         CP    DUEPST,PZERO                                                     
         JNE   YESX                                                             
         J     NOX                 SKIP IT - NO TAX                             
                                                                                
IFTAX    CP    LVGTAX,PZERO                                                     
         JE    NOX                                                              
         J     YESX                                                             
                                                                                
IFNCGS$  TM    OPT1,OPT1SNC        SUPPRESS NET & COMM.                         
         JO    NOX                                                              
IFGST$   CP    DUEGST,PZERO        TEST ANY TAX                                 
         JNE   YESX                                                             
         J     NOX                 SKIP IT - NO TAX                             
                                                                                
IFNCPS$  TM    OPT1,OPT1SNC        SUPPRESS NET & COMM.                         
         JO    NOX                                                              
IFPST$   CP    DUEPST,PZERO        TEST ANY PST                                 
         JNE   YESX                                                             
         J     NOX                 SKIP IT - NO TAX                             
                                                                                
IFNOGRP  TM    RQLV,LVA+LVB       TEST ANY GROUP                                
         JZ    YESX                                                             
         J     NOX                                                              
                                                                                
IFGRP    TM    RQLV,LVA+LVB       TEST A27 GROUP                                
         JZ    NOX                                                              
         J     YESX                                                             
                                                                                
IFGRPA   TM    RQLV,LVA           TEST CLIENT LEVEL                             
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFGRPB   TM    RQLV,LVB           TEST PRODUCT LEVEL                            
         JO    YESX                                                             
         TM    RQLV,LVA                                                         
         JO    NOX                                                              
         J     YESX                NO LEVEL                                     
                                                                                
IFGRPAB  TM    RQLV,LVA+LVB       TEST CLIENT OR PRODUCT LEVEL                  
         JNZ   YESX                                                             
         J     NOX                                                              
                                                                                
IFNINWC  CLI   FORMCDE,C'0'        FORMAT ZERO                                  
         JE    NOX                                                              
         CP    LVGCOM,PZERO        TEST AMOUNT IS ZERO                          
         JE    NOX                                                              
         TM    OPT6,OPT6COWC       TEST COMM IN WC                              
         JNO   YESX                NO,                                          
         J     NOX                                                              
                                                                                
IFWCG    CLI   WRKCGRP,C' '       TEST  WORKCODE GROUP                          
         JH    YESX                                                             
         J     NOX                                                              
                                                                                
IFLVC    TM    RQLV,LVC            TEST LEVEL  C                                
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFLVA    TM    RQLV,LVA            TEST LEVEL A                                 
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFF#3    CLI   FORMCDE,C'3'        TEST FORMAT 3                                
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
IFF#4    CLI   FORMCDE,C'4'        TEST FORMAT 4                                
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
                                                                                
IFF#5    CLI   FORMCDE,C'5'        TEST FORMAT 5                                
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
IFF#7    CLI   FORMCDE,C'7'        TEST FORMAT 7                                
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
IFF#8    CLI   FORMCDE,C'8'        TEST FORMAT 8                                
         JE    YESX                                                             
         J     NOX                                                              
                                                                                
IFNOPSF  CLI   FORMCDE,C'7'        ONLY FORMAT 7                                
         JNE   NOX                                                              
         TM    GTIND,GTIPFS        PFS LINE DEFAULTED                           
         JO    NOX                 YES,  SKIP THIS                              
         J     YESX                                                             
                                                                                
IFNAME   TM    OPT5,OPT5PVDE       TEST PRINT VENDOR DESCRIPTION(NAME)          
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFCODE   TM    OPT5,OPT5PVCO       TEST PRINT VENDOR CODE                       
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFNAC    TM    OPT5,OPT5PVDE+OPT5PVCO TEST PRINT VENDOR NAME & CODE             
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFNOC    TM    OPT5,OPT5PVDE+OPT5PVCO TEST PRINT VENDOR NAME OR CODE            
         JNZ   YESX                                                             
         J     NOX                                                              
                                                                                
IFNOTNC  TM    OPT5,OPT5PVDE+OPT5PVCO TEST NEITHER NAME NOR CODE                
         JZ    YESX                                                             
         J     NOX                                                              
                                                                                
IFNONLY  TM    OPT5,OPT5PVDE          TEST PRINT VENDOR DESC ONLY               
         JNO   NOX                                                              
         TM    OPT5,OPT5PVCO          TEST PRINT VENDOR CODE                    
         JO    NOX                                                              
         J     YESX                                                             
                                                                                
IFWCC    TM    OPT5,OPT5WCCO       PRINT WC CODE                                
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFNWCC   TM    OPT5,OPT5WCCO       PRINT NO WC CODE                             
         JNO   YESX                                                             
         J     NOX                                                              
                                                                                
IFWCD    TM    OPT5,OPT5WCDE       PRINT WC CODE DESCRIPTION                    
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFPSF    CP    LVGPSF,PZERO        TEST ANY PSF                                 
         JE    NOX                                                              
         J     YESX                                                             
                                                                                
IFOOP    CP    LVGOOP,PZERO        TEST ANY OOP                                 
         JE    NOX                                                              
         J     YESX                                                             
                                                                                
IFNSUMR  CLC   BFMKLVL,=AL2(BFMKLSMQ) TEST NOT A SUMMARY                        
         JNE   YESX                                                             
         J     NOX                                                              
                                                                                
IFTDTL   DS    0H                  ANY TIME DETAIL                              
         TM    OPT8,OPT8PTBW+OPT8PTBP                                           
         JNZ   YESX                                                             
         J     NOX                                                              
                                                                                
IFTOPT1  DS    0H                  TIME BY WC & PERSON                          
         TM    OPT8,OPT8PTBW+OPT8PTBP                                           
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFTOPT2  DS    0H                  TIME BY WC, NO PERSON DETAIL                 
         TM    OPT8,OPT8PTBW                                                    
         JNO   NOX                                                              
         TM    OPT8,OPT8PTBP                                                    
         JO    NOX                                                              
         J     YESX                                                             
                                                                                
IFTOPT3  DS    0H                  TIME BY PERSON,                              
         TM    OPT8,OPT8PTBW       TEST WC WANTED                               
         JO    NOX                 YES, DON'T USE THIS                          
         TM    OPT8,OPT8PTBP       TEST BY PERSON                               
         JO    YESX                YES                                          
         J     NOX                                                              
                                                                                
IFTOPT4  DS    0H                  TIME NO DETAIL                               
         TM    OPT8,OPT8PTBW       TEST WC WANTED                               
         JO    NOX                 YES, DON'T USE THIS                          
         TM    OPT8,OPT8PTBP       TEST BY PERSON                               
         JO    NOX                 YES, DON'T USE IT                            
         J     YESX                                                             
                                                                                
                                                                                
IFTTOT   DS    0H                    TOTAL FOR TIME (PSF)                       
         TM    OPT8,OPT8PTBW         TEST TIME BY WC                            
         JO    YESX                                                             
         TM    OPT8,OPT8PTBP         TEST TIME BY PERSON                        
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFTMD    TM    OPT8,OPT8PTMD           TEST TIME DETAIL                         
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFTBP    TM    OPT8,OPT8PTBP           TEST TIME BY PERSON                      
         JO    YESX                                                             
         J     NOX                                                              
                                                                                
IFA21    TM    PGMAQ,PGMALL21      TEST A21 BILLING                             
         JNZ   YESX                                                             
         J     NOX                                                              
                                                                                
IFA27    TM    PGMAQ,PGMALL27      TEST A27 BILLING                             
         JNZ   YESX                                                             
         J     NOX                                                              
                                                                                
IFNODTL  TM    OPT5,OPT5PVCO+OPT5PVDE+OPT5WCCO+OPT5WCDE                         
         JZ    YESX                                                             
         J     NOX                                                              
                                                                                
IFDTL    TM    OPT5,OPT5PVCO+OPT5PVDE+OPT5WCCO+OPT5WCDE                         
         JNZ   YESX                                                             
         J     NOX                                                              
                                                                                
IFPINV   TM    OPT6,OPT6PINV       TEST PRINT INVOICE                           
         JO    YESX                YES,                                         
         J     NOX                                                              
                                                                                
IFPBIL   TM    OPT6,OPT6PINV       TEST PRINT BILL                              
         JO    NOX                 NO, PRINT INVOICE                            
         J     YESX                                                             
*                                                                               
IFPUSD   TM    OPTC,OPTCUSD                                                     
         JNO   NOX                                                              
         J     YESX                                                             
*                                                                               
*FPUSDG  TM    OPTC,OPTCUSD                                                     
*        JNO   NOX                                                              
*        TM    JXSTAT3,JXS3PNET                                                 
*        JO    NOX                                                              
*        J     YESX                                                             
*                                                                               
         DROP  R9,R4,R5                                                         
                                                                                
YESX     CR    RE,RE                                                            
         BR    RE                                                               
NOX      LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* MISCELLANEOUS ROUTINES                                             *          
**********************************************************************          
                                                                                
ADDEL    NTR1  LABEL=*                                                          
         LM    R2,R3,0(R1)                                                      
         LA    R0,ADDEND                                                        
         CLI   0(R1),C'E'          ADD AT END                                   
         JE    *+6                                                              
         SR    R0,R0                                                            
         GOTOR HELLO,DMCB,(C'P',ACCMST),(R2),(R3),(R0)                          
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
         EJECT                                                                  
DELEL    NTR1  LABEL=*                                                          
         L     R2,0(R1)                                                         
         GOTOR HELLO,DMCB,(C'D',ACCMST),(X'FF',(R2)),0,0                        
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
                                                                                
         GETEL R2,DATADISP,ELCODE                                               
GETELN   AH    R2,NEWDISP                                                       
         J     FIRSTEL                                                          
                                                                                
XITN     DS    0H                                                               
XITL     LA    RF,0                                                             
         J     XIT                                                              
XITH     LA    RF,2                                                             
         J     XIT                                                              
XITY     DS    0H                                                               
XITE     LA    RF,1                                                             
XIT      CHI   RF,1                                                             
         XIT1  ,                                                                
         EJECT                                                                  
BOXTOP   EQU   C'T'                                                             
BOXMID   EQU   C'M'                                                             
BOXBOT   EQU   C'B'                                                             
BOXLEFT  EQU   C'L'                                                             
BOXCOLM  EQU   C'C'                                                             
BOXRIGHT EQU   C'R'                                                             
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
REPFLAG1 DC    XL(L'BFTFLAG1)'00'  REPORT INDICATORS                            
                                                                                
LASTX    DS    0C                  LAST TAX CODES                               
LASTPRVD DS    CL(L'CTXPRVD)                                                    
LASTREG  DS    CL(L'CTXREG)                                                     
LASTREGL DS    CL(L'BFMTLEN)                                                    
LASTXLQ  EQU   *-LASTX                                                          
                                                                                
ADDEND   DC    C'ADD=END'                                                       
                                                                                
ESWCOPT  DS    C                   DETAIL SWITCH FOR %EST                       
ESWCBOTH EQU   C'B'                PRINT BOTH - WC CODE & NAME                  
ESWCCODE EQU   C'C'                CODE ONLY                                    
ESWCNAME EQU   C'Y'                WC DESCRIPTION ONLY                          
ESWCNONE EQU   C'N'                NEITHER - ONLY 'TOTAL'                       
*                                                                               
USDOPT   DS    CL1                                                              
USDYES   EQU   C'Y'                                                             
USDNO    EQU   C'N'                                                             
*                                                                               
HPP      DS    X                   HEADING CONTROL                              
                                                                                
MXSRT    EQU   15                  MAXIMUM SORT FIELDS                          
MXGAP    EQU   4                   MAXIMUM NUMBER OF NULLS                      
                                                                                
NBFLDS   DS    X                   NUMBER OF BFMFLDS                            
BFLDS    DS    CL50                LIST OF BFMFLDS                              
BFSWRK   DS    XL255               BFSELD WORK AREA                             
                                                                                
RECSEQ   DS    X                                                                
SEQBF    DS    XL(L'BFMSEQ)                                                     
TOP      DS    XL(L'BFPHGT)                                                     
BOT      DS    XL(L'BFPHGT)                                                     
                                                                                
LOOPD    DS    X                   DEPTH OF LOOP                                
         DS    0F                                                               
GTTAB    DS    3XL(AGTLNQ)                                                      
                                                                                
SVSPA    DS    X                   SAVE SPACING                                 
                                                                                
TEMPCHOP DC    C'N'                                                             
                                                                                
TAXHTAB  DS    0XL4                                                             
         DC    AL1(PCQTXTYP,1),AL2(TXTYP-TAX)                                   
         DC    AL1(PCQTXREG,1),AL2(TXREG-TAX)                                   
         DC    AL1(PCQTXBAS,1),AL2(TXBAS-TAX)                                   
         DC    AL1(PCQTXRAT,1),AL2(TXRAT-TAX)                                   
         DC    AL1(PCQTXAMT,1),AL2(TXAMT-TAX)                                   
         DC    AL1(EOT)                                                         
                                                                                
PSTIND   DS    X                   PST INDICATOR                                
PSTIABT  EQU   X'80'               BEFORE TAX(TOTAL)                            
PSTIDUE  EQU   X'40'               DUE                                          
                                                                                
SAVDPST  DS    PL(L'DUEPST)                                                     
SAVTPST  DS    PL(L'JOTOTPST)                                                   
                                                                                
RTLPB    DS    C                                                                
                                                                                
STATUS   DC    X'00'                                                            
CATCHALL EQU   X'80'               BILL HAS CATCH ALL SECTION                   
                                                                                
         DS    0D                                                               
         DC    CL8'*SFPARM*'                                                    
SFPARM   DC    (TABLNQ/4)F'0'      SECTION FILTER TABLE PARAMETERS              
                                                                                
         DS    0D                                                               
         DC    CL8'*SSPARM*'                                                    
SSPARM   DC    (TABLNQ/4)F'0'      SECTION SORT TABLE PARAMETERS                
                                                                                
         DS    0D                                                               
         DC    CL8'*PDPARM*'                                                    
PDPARM   DC    (TABLNQ/4)F'0'      PARAGRAPH DETAIL PARAMETERS                  
                                                                                
         DS    0H                                                               
BPRTAB   DS    0XL3                ** RECORD TYPE/EDIT LOOKUP **                
         DC    AL1(BILKRCLI),AL2(CLIR-BPRT)    CLIENT RECORD                    
         DC    AL1(BILKRPRD),AL2(PRDR-BPRT)    PRODUCT RECORD                   
         DC    AL1(BILKRJOB),AL2(JOBR-BPRT)    JOB RECORD                       
         DC    AL1(BILKRPRV),AL2(PRVB-BPRT)    PREVIOUS BILLS                   
         DC    AL1(BILKRBFM),AL2(BFMR-BPRT)    FORMAT RECORD                    
         DC    AL1(BILKRBHD),AL2(BLHR-BPRT)    BILL HEADER                      
         DC    AL1(BILKRUPP),AL2(UPPR-BPRT)    UPPER                            
         DC    AL1(BILKRPGH),AL2(PGHR-BPRT)    PAGE HEADER                      
         DC    AL1(BILKRPH2),AL2(PGH2-BPRT)    PAGE HEADER 2                    
         DC    AL1(BILKRPGF),AL2(PGFR-BPRT)    PAGE FOOTER                      
         DC    AL1(BILKRBED),AL2(BEDR-BPRT)    BILL EDIT RECORD                 
BPRTABN  EQU   (*-BPRTAB)/L'BPRTAB                                              
                                                                                
         DS    0H                                                               
PGEHTAB  DS    0XL3                         SORT RECORD TYPE                    
         DC    AL2(BEDKWUPQ),AL1(BILKRUPP)   UPPER                              
         DC    AL2(BEDKWPHQ),AL1(BILKRPGH)   PAGE HEADER                        
         DC    AL2(BEDKWH2Q),AL1(BILKRPH2)   PAGE HEADER 2                      
         DC    AL2(BEDKWBDQ),AL1(BILKRBED)   BODY                               
         DC    AL2(BEDKWJCQ),AL1(BILKRBED)   JOB COMMENT                        
         DC    AL2(BEDKWPFQ),AL1(BILKRPGF)   PAGE FOOTER                        
         DC    AL1(EOT)                                                         
                                                                                
PCFRTABD DSECT ,                                                                
PCFRTWHR DS    XL(L'BFMKWHER)                                                   
PCFRTWIN DS    XL(L'BFMKSWHR)                                                   
PCFRTROU DS    AL2                                                              
PCFRTABL EQU   *-PCFRTABD                                                       
ACNB03   CSECT ,                                                                
                                                                                
         DS    0H                                                               
PCFRTAB  DS    0XL(PCFRTABL)                      RECORD ROUTINES               
         DC    AL2(BFMKWUPQ,0,PGEHDR-BFMT)         UPPER                        
         DC    AL2(BFMKWPHQ,0,PGEHDR-BFMT)         PAGE HEADER                  
         DC    AL2(BFMKWH2Q,0,PGEHDR-BFMT)         PAGE HEADER 2                
         DC    AL2(BFMKWBDQ,BFMKSHPQ,PGEHDR-BFMT)  BODY (PAGE HEAD)             
         DC    AL2(BFMKWBDQ,0,BDYBIL-BFMT)         BODY OF BILL                 
         DC    AL2(BFMKWBTQ,0,MBT-BFMT)            BILL TOTALS                  
         DC    AL2(BEDKWGSQ,0,TAX-BFMT)            GST SUMMARY                  
         DC    AL2(BEDKWG1Q,0,TAX-BFMT)            GST SUMMARY(PROG)            
         DC    AL2(BEDKWPBQ,0,PRV-BFMT)            PREV. BILLS                  
         DC    AL2(BEDKWJCQ,0,JCM-BFMT)            JOB COMMENT                  
         DC    AL2(BFMKWPFQ,0,PGEFOT-BFMT)         PAGE FOOTER                  
         DC    AL1(EOT)                                                         
         DS    0H                                                               
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(BEDKWBDQ),AL2(PBODY-BPRT)   PRINT BODY OF BILLS              
         DC    AL2(BEDKWJCQ),AL2(PBODY-BPRT)   JOB COMMENT                      
         DC    AL2(BEDKWBTQ),AL2(PBTTL-BPRT)   BILL TOTALS                      
         DC    AL2(BEDKWGSQ),AL2(PGSTS-BPRT)   GST SUMMARY                      
         DC    AL2(BEDKWG1Q),AL2(PGSTS-BPRT)   GST SUMMARY                      
         DC    AL2(BEDKWPBQ),AL2(PPRVB-BPRT)   PREVIOUS BILLS                   
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                                                               
MBTHTAB  DS    0XL4                                                             
         DC    AL1(PCQBTNET,0),AL2(MBTNET-MBT)                                  
         DC    AL1(PCQBTCOM,0),AL2(MBTCOM-MBT)                                  
         DC    AL1(PCQBTGRS,0),AL2(MBTGRS-MBT)                                  
         DC    AL1(PCQBTCD,0),AL2(MBTCD-MBT)                                    
         DC    AL1(PCQBTPAY,0),AL2(MBTPAY-MBT)                                  
         DC    AL1(PCQTXDES,0),AL2(MBTXDES-MBT)                                 
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                                                               
PRVHTAB  DS    0XL4                                                             
         DC    AL1(PCQPBDAT,1),AL2(PBDAT-PRV)   DATE                            
         DC    AL1(PCQPBREF,1),AL2(PBREF-PRV)   REFERENCE                       
         DC    AL1(PCQPBTYP,1),AL2(PBTYP-PRV)   TYPE                            
         DC    AL1(PCQPBNET,1),AL2(PBNET-PRV)   NET                             
         DC    AL1(PCQPBCOM,1),AL2(PBCOM-PRV)   COMMISSION                      
         DC    AL1(PCQPBGRS,1),AL2(PBGRS-PRV)   GROSS                           
         DC    AL1(PCQPBCD,1),AL2(PBCD-PRV)     CD                              
         DC    AL1(PCQDUGST,0),AL2(DUGST-PRV)   GST                             
         DC    AL1(PCQDUPST,0),AL2(DUPST-PRV)   PST                             
         DC    AL1(PCQDUNTX,0),AL2(DUNTX-PRV)   NET + TAX                       
         DC    AL1(PCQDUPAY,0),AL2(PBDUE-PRV)   PAY ABOVE                       
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                                                               
FLTTAB   DS    0XL(FLTTABL)        FILTER TABLE                                 
         DC    AL1(BSDSWCT,L'WRKCTYPE,0),AL2(WRKCTYPE-NBILD)  WC TYPE           
         DC    AL1(BSDSWCG,L'WRKCGRP,0),AL2(WRKCGRP-NBILD)  " GROUP             
         DC    AL1(BSDSWC,L'BTRNWC,1),AL2(BTRNWC-BTRND)     "                   
         DC    AL1(BSDSTRN,L'BTRNTYP,1),AL2(BTRNTYP-BTRND)  TRANS TYPE          
         DC    AL1(BSDSCAC,L'BTRNCUL,1),AL2(BTRNCUL-BTRND)  CONTRA UL           
         DC    AL1(BSDSCAT,0,0),AL2(0)                      CAT/SCHEME          
         DC    AL1(BSDSPOM,L'BTRNPORM,1),AL2(BTRNPORM-BTRND) PROD/MEDIA         
FLTTABX  DC    AL1(EOT)                                                         
                                                                                
         DS    0H                                                               
SOFTAB   DS    0XL3                                                             
         DC    AL1(07,02,0)         ROW 07, COL 2                               
*        DC    AL1(07,46,0)             07,     46                              
         DC    AL1(08,02,0)                                                     
*        DC    AL1(08,46,0)                                                     
         DC    AL1(09,02,0)                                                     
         DC    AL1(09,46,0)                                                     
         DC    AL1(10,02,0)                                                     
         DC    AL1(10,46,0)                                                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIELD TABLE - USES FLDTABD                                          *         
***********************************************************************         
                                                                                
         USING BTRND,R3                                                         
         USING AMTD,R6                                                          
         DS    0H                                                               
FLDTAB   DS    0XL(FLDTABL)        FIELD TABLE                                  
         DC    256XL(FLDTABL)'00'                                               
                                                                                
         ORG   FLDTAB+(PCQWC*FLDTABL)                                           
         DC    S(BTRNWC)           WORKCODE                                     
         DC    AL1(L'TRNKWORK,0,0)                                              
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQREF*FLDTABL)                                          
         DC    S(BTRNREF)          REFERENCE                                    
         DC    AL1(L'TRNKREF,0,0)                                               
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDATE*FLDTABL)                                         
         DC    S(BTRNDTE)          DATE                                         
         DC    AL1(L'TRNKDATE,8,FLDCPWOS)                                       
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQVNDN*FLDTABL)                                         
         DC    AL2(GETVNDN-GETDATA) VENDOR NAME                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQNARR*FLDTABL)                                         
         DC    AL2(GETNAR-GETDATA)  NARRATIVE                                   
         DC    AL1(0,0,0)                                                       
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQNET*FLDTABL)                                          
         DC    S(BTRNBK+(JOBNET-JOBD)) NET                                      
         DC    AL1(L'JOBNET,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCOM*FLDTABL)                                          
         DC    S(BTRNBK+(JOBCOM-JOBD)) COMMIS                                   
         DC    AL1(L'JOBCOM,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGRS*FLDTABL)                                          
         DC    S(BTRNBK+(JOBGRS-JOBD)) GROSS                                    
         DC    AL1(L'JOBGRS,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQHRS*FLDTABL)                                          
         DC    S(BTRNBK+(JOBHRS-JOBD)) HOURS                                    
         DC    AL1(L'JOBHRS,0,FLDCHRS)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQRAT*FLDTABL)                                          
         DC    S(BTRNBILR)              RATE                                    
         DC    AL1(L'BTRNBILR,0,FLDCRAT)                                        
         DC    AL1(FLDISORT+FLDIAMT)                                            
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQWCN*FLDTABL)                                          
         DC    S(WRKCNME)               WORKCODE NAME                           
         DC    AL1(L'WRKCNME,0,0)                                               
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQWCG*FLDTABL)                                          
         DC    S(WRKCGRP)               WORKCODE GROUP                          
         DC    AL1(L'WRKCGRP,0,0)                                               
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQWCGN*FLDTABL)                                         
         DC    AL2(GETWCGN-GETDATA) WORKCODE GROUP NAME                         
         DC    AL1(0,0,0)                                                       
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCD*FLDTABL)                                           
         DC    S(BTRNBK+(JOBCD-JOBD))    CASH DISCOUNT                          
         DC    AL1(L'JOBCD,0,FLDCAMT)                                           
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCMR*FLDTABL)                                          
         DC    S(BTRNCOMR)            COMMISSION RATE                           
         DC    AL1(L'BTRNCOMR,0,FLDCRAT)                                        
         DC    AL1(FLDISORT)                                                    
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQH@R*FLDTABL)                                          
         DC    AL2(GETH@R-GETDATA)    NN.NN HOURS @ $$.00                       
         DC    AL1(0,0,0)                                                       
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQWCD*FLDTABL)                                          
         DC    S(WRKCDSC)             WORKCODE DESCRIPTION                      
         DC    AL1(L'WRKCDSC,0,0)                                               
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQNIF*FLDTABL)                                          
         DC    AL2(GETNEW-GETDATA)     NEW ITEM TXFLG                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCNT*FLDTABL)                                          
         DC    S(BTRNSUP)             CONTRA ACCOUNT                            
         DC    AL1(L'BTRNSUP,0,0)                                               
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQSBR*FLDTABL)                                          
         DC    S(BTRNSBR)             SEQUENCE(SUBREF)                          
         DC    AL1(L'BTRNSBR,0,0)                                               
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQXFR*FLDTABL)                                          
         DC    AL2(GETXFR-GETDATA)    TRANSFER DATA                             
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQVNDC*FLDTABL)                                         
         DC    S(BTRNSUP+2)           VENDOR CODE                               
         DC    AL1(L'BTRNSUP-2,0,0)                                             
         DC    AL1(FLDISORT+FLDIRMTS)                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQVNDCN*FLDTABL)                                        
         DC    AL2(GETVNDCN-GETDATA)  VENDOR CODE & NAME                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQWRKCN*FLDTABL)                                        
         DC    AL2(GETWRKCN-GETDATA)  WORKCODE & NAME                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQAGYJ*FLDTABL)                                         
         DC    AL2(AGYJ-GETPD)       AGENCY JOB                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBA*FLDTABL)                                           
         DC    AL2(BA-GETPD)           BILLING ADDRESS                          
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCUR*FLDTABL)                                          
         DC    S(CURCOD)              BILL CURRENCY                             
         DC    AL1(L'CURCOD,0,0)                                                
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBD*FLDTABL)                                           
         DC    AL2(BD-GETPD)           BILL DATE                                
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBN*FLDTABL)                                           
         DC    S(LNGBILNO)             BILL NUMBER                              
         DC    AL1(L'LNGBILNO,0,0)                                              
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBSTA*FLDTABL)                                         
         DC    AL2(BST-GETPD)        BILL STATUS                                
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBTYP*FLDTABL)                                         
         DC    AL2(BTYP-GETPD)       BILL TYPE                                  
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCDF*FLDTABL)                                          
         DC    AL2(CD-GETPD)         CASH DISCOUNT TAG                          
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCDE*FLDTABL)                                          
         DC    AL2(CDE-GETPD)          CD=                                      
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCC*FLDTABL)                                           
         DC    S(CLI)                   CLIENT CODE                             
         DC    AL1(L'CLI,0,0)                                                   
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCN*FLDTABL)                                           
         DC    S(CLINAM)                CLIENT NAME                             
         DC    AL1(L'CLINAM,0,0)                                                
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCON*FLDTABL)                                          
         DC    AL2(CON-GETPD)          COMPANY NAME                             
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCOA*FLDTABL)                                          
         DC    AL2(COA-GETPD)          COMPANY ADDRESS                          
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDC*FLDTABL)                                           
         DC    AL2(DC-GETPD)            DEBTORS CODE                            
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDD*FLDTABL)                                           
         DC    AL2(DD-GETPD)            DUE DATE                                
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQEST*FLDTABL)                                          
         DC    AL2(EST-GETPD)          ESTIMATE FIELD                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCL*FLDTABL)                                           
         DC    AL2(CL-GETPD)            JOB CLOSE DATE                          
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJC*FLDTABL)                                           
         DC    S(JOB)                   JOB CODE                                
         DC    AL1(L'JOB,0,0)                                                   
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCPJ*FLDTABL)                                          
         DC    S(CPJ)              JOB CODE(CLIPROJOB)                          
         DC    AL1(L'CPJ,0,0)                                                   
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCPJS*FLDTABL)                                         
         DC    AL2(CPJSEP-GETPD)   JOB CODE(CLI/PRO/JOB)                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJCO*FLDTABL)                                          
         DC    AL2(JCO-GETPD)      JOB COMMENTS                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJC2*FLDTABL)                                          
         DC    AL2(JC2-GETPD)      EXTRA JOB COMMENTS                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJN*FLDTABL)                                           
         DC    S(JOBNAM)           JOB NAME                                     
         DC    AL1(L'JOBNAM,0,0)                                                
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQOP*FLDTABL)                                           
         DC    AL2(OP-GETPD)       JOB OPEN DATE                                
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQMC*FLDTABL)                                           
         DC    S(MEDIA)            MEDIA CODE                                   
         DC    AL1(L'MEDIA,0,0)                                                 
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQMN*FLDTABL)                                           
         DC    S(MEDNS)            MEDIA NAME                                   
         DC    AL1(L'MEDNS,0,0)                                                 
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQAN*FLDTABL)                                           
         DC    AL2(AN-GETPD)       ORIGIN NAME                                  
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQAA*FLDTABL)                                           
         DC    AL2(AA-GETPD)       ORIGIN ADDRESS                               
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPG*FLDTABL)                                           
         DC    AL2(PG-GETPD)       PAGE NUMBER                                  
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPONB*FLDTABL)                                         
         DC    AL2(PONB-GETPD)     PRINT ON BILLS                               
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPC*FLDTABL)                                           
         DC    S(PRD)              PRODUCT CODE                                 
         DC    AL1(L'PRD,0,0)                                                   
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPN*FLDTABL)                                           
         DC    S(PRDNAM)           PRODUCT NAME                                 
         DC    AL1(L'PRDNAM,0,0)                                                
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQTD*FLDTABL)                                           
         DC    AL2(TD-GETPD)       TODAY'S DATE                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF1*FLDTABL)                                          
         DC    AL2(UF1-GETPD)      USER FIELD 1                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF2*FLDTABL)                                          
         DC    AL2(UF2-GETPD)      USER FIELD 2                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF3*FLDTABL)                                          
         DC    AL2(UF3-GETPD)      USER FIELD 3                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF4*FLDTABL)                                          
         DC    AL2(UF4-GETPD)      USER FIELD 4                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF5*FLDTABL)                                          
         DC    AL2(UF5-GETPD)      USER FIELD 5                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF6*FLDTABL)                                          
         DC    AL2(UF6-GETPD)      USER FIELD 6                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF7*FLDTABL)                                          
         DC    AL2(UF7-GETPD)      USER FIELD 7                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF8*FLDTABL)                                          
         DC    AL2(UF8-GETPD)      USER FIELD 8                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF9*FLDTABL)                                          
         DC    AL2(UF9-GETPD)          USER FIELD 9                             
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQUF10*FLDTABL)                                         
         DC    AL2(UF10-GETPD)        USER FIELD 10                             
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQAUTH*FLDTABL)                                         
         DC    AL2(AUTH-GETPD)         AUTHORIZATION                            
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQSHIP*FLDTABL)                                         
         DC    AL2(SHIP-GETPD)          SHIP DATE                               
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBMN*FLDTABL)                                          
         DC    AL2(BMN-GETPD)            BILL MEDIA NAME                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQAMDU*FLDTABL)                                         
         DC    AL2(AMTDU-GETPD)        AMOUNT DUE                               
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQRTLN*FLDTABL)                                         
         DC    AL2(RTLRN-GETPD)        RETAILER NAME                            
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQRTLA*FLDTABL)                                         
         DC    AL2(RTLRA-GETPD)        RETAILER ADDRESS                         
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPBA*FLDTABL)                                          
         DC    S(POBCUR)              % OF BILL AMOUNT                          
         DC    AL1(L'POBCUR,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPBW*FLDTABL)                                          
         DC    S(POBWRK)             % OF BILL WORKCODE                         
         DC    AL1(L'POBWRK,0,0)                                                
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPBN*FLDTABL)                                          
         DC    S(POBNAM)              % OF BILL W/C NAME                        
         DC    AL1(L'POBNAM,0,0)                                                
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQLIN*FLDTABL)                                          
         DC    AL2(GETLIN-GETDATA)     LONG INVOICE#                            
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPREBN*FLDTABL)                                        
         DC    AL2(PREBN-GETPD)       PREBILL NAME                              
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQTTLDA*FLDTABL)                                        
         DC    AL2(TTLDA-GETPD)       TITLE DATE                                
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPROFS*FLDTABL)                                        
         DC    AL2(PROFS-GETPD)       PROFESSIONAL SERV.                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBD8*FLDTABL)                                          
         DC    AL2(BD8-GETPD)         BILL DATE - MMMDD/YY                      
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBNS*FLDTABL)                                          
         DC    S(SHTBILNO)             BILL NUMBER(SHORT)                       
         DC    AL1(L'SHTBILNO,0,0)                                              
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQHANDF*FLDTABL)                                        
         DC    AL2(HANDF-GETPD)       "HANDLING FEE"                            
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPNBJ*FLDTABL)                                         
         DC    AL2(PONBJ-GETPD)        PRINT ON BILLS(JOB)                      
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCTNET*FLDTABL)                                        
         DC    S(JOCURNET)           CURRENT - NET                              
         DC    AL1(L'JOCURNET,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCTCOM*FLDTABL)                                        
         DC    S(JOCURCOM)           CURRENT - COMM                             
         DC    AL1(L'JOCURCOM,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCTGRS*FLDTABL)                                        
         DC    S(JOCURGRS)           CURRENT - GROSS                            
         DC    AL1(L'JOCURGRS,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCTCD*FLDTABL)                                         
         DC    S(JOCURCSD)            CURRENT - CD                              
         DC    AL1(L'JOCURCSD,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
*                                               BEFORE TAX 'CURRENT'            
         ORG   FLDTAB+(PCQBCNET*FLDTABL)                                        
         DC    S(BTXCNET)                    - NET                              
         DC    AL1(L'BTXCNET,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBCCOM*FLDTABL)                                        
         DC    S(BTXCCOM)                    - COMM                             
         DC    AL1(L'BTXCCOM,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBCGRS*FLDTABL)                                        
         DC    S(BTXCGRS)                    - GROSS                            
         DC    AL1(L'BTXCGRS,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
*                                               BEFORE TAX 'TOTAL'              
         ORG   FLDTAB+(PCQBXNET*FLDTABL)                                        
         DC    S(BTXTNET)                                                       
         DC    AL1(L'BTXTNET,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBXCOM*FLDTABL)                                        
         DC    S(BTXTCOM)                    - COMM                             
         DC    AL1(L'BTXTCOM,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBXGRS*FLDTABL)                                        
         DC    S(BTXTGRS)                    - GROSS                            
         DC    AL1(L'BTXTGRS,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJTNET*FLDTABL)                                        
         DC    S(JOTOTNET)           JOB TOTAL - NET                            
         DC    AL1(L'JOTOTNET,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJTCOM*FLDTABL)                                        
         DC    S(JOTOTCOM)           JOB TOTAL - COMM                           
         DC    AL1(L'JOTOTCOM,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJTGRS*FLDTABL)                                        
         DC    S(JOTOTGRS)           JOB TOTAL - GROSS                          
         DC    AL1(L'JOTOTGRS,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJTCD*FLDTABL)                                         
         DC    S(JOTOTCSD)             JOB TOTAL - CD                           
         DC    AL1(L'JOTOTCSD,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJTGST*FLDTABL)                                        
         DC    S(JOTOTGST)            JOB TOTAL - GST                           
         DC    AL1(L'JOTOTGST,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQJTPST*FLDTABL)                                        
         DC    S(JOTOTPST)            JOB TOTAL - PST                           
         DC    AL1(L'JOTOTPST,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQSHPCT*FLDTABL)                                        
         DC    S(SHARPCT)             SHARE - PCT                               
         DC    AL1(L'SHARPCT,0,FLDCCMR)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQSHUNT*FLDTABL)                                        
         DC    AL2(RTLUNTS-GETPD)    SHARE - UNITS                              
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQSHNET*FLDTABL)                                        
         DC    S(SHARNET)             SHARE - NET                               
         DC    AL1(L'SHARNET,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQSHCOM*FLDTABL)                                        
         DC    S(SHARCOM)             SHARE - COMM                              
         DC    AL1(L'SHARCOM,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQSHGRS*FLDTABL)                                        
         DC    S(SHARGRS)             SHARE - GROSS                             
         DC    AL1(L'SHARGRS,0,FLDCAMT)                                         
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQTPNET*FLDTABL)                                        
         DC    S(PBTOTNET)           TOTAL P.B. - NET                           
         DC    AL1(L'PBTOTNET,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQTPCOM*FLDTABL)                                        
         DC    S(PBTOTCOM)           TOTAL P.B. - COM                           
         DC    AL1(L'PBTOTCOM,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQTPGRS*FLDTABL)                                        
         DC    S(PBTOTGRS)           TOTAL P.B. - GROSS                         
         DC    AL1(L'PBTOTGRS,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQTPCD*FLDTABL)                                         
         DC    S(PBTOTCSD)            TOTAL P.B. - CD                           
         DC    AL1(L'PBTOTCOM,0,FLDCAMT)                                        
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDUNET*FLDTABL)                                        
         DC    S(DUENET)             AMOUNT DUE - NET                           
         DC    AL1(L'DUENET,0,FLDCAMT)                                          
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDUCOM*FLDTABL)                                        
         DC    S(DUECOM)             AMOUNT DUE - COM                           
         DC    AL1(L'DUECOM,0,FLDCAMT)                                          
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDUGRS*FLDTABL)                                        
         DC    S(DUEGRS)             AMOUNT DUE - GROSS                         
         DC    AL1(L'DUEGRS,0,FLDCAMT)                                          
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDUABT*FLDTABL)                                        
         DC    S(DUEGRS)             AMOUNT DUE - BEFORE                        
         DC    AL1(L'DUEGRS,0,FLDCAMT)                                          
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDURCV*FLDTABL)                                        
         DC    S(DUERCV)             AMOUNT + TAX                               
         DC    AL1(L'DUERCV,0,FLDCAMT)                                          
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQDUPAY*FLDTABL)                                        
         DC    S(0)                  PAY ABOVE AMOUNT                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQDUGST*FLDTABL)                                        
         DC    S(DUEGST)             GST                                        
         DC    AL1(L'DUEGST,0,FLDCAMT)                                          
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQDUPST*FLDTABL)                                        
         DC    S(DUEPST)             PST                                        
         DC    AL1(L'DUEPST,0,FLDCAMT)                                          
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQDUNTX*FLDTABL)                                        
         DC    S(DUENTX)             NET + TAX                                  
         DC    AL1(L'DUENTX,0,FLDCAMT)                                          
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQPBNET*FLDTABL)                                        
         DC    AL2(0)                PREVIOUS BILL - NET                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQPBCOM*FLDTABL)                                        
         DC    AL2(0)                PREVIOUS BILL - COM                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQPBGRS*FLDTABL)                                        
         DC    AL2(0)                PREVIOUS BILL - GRS                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQPBCD*FLDTABL)                                         
         DC    AL2(0)                 PREVIOUS BILL - CD                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQPBDAT*FLDTABL)                                        
         DC    AL2(0)                PREVIOUS BILL - DATE                       
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQPBREF*FLDTABL)                                        
         DC    AL2(0)                PREVIOUS BILL - REF                        
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQPBTYP*FLDTABL)                                        
         DC    AL2(0)                PREVIOUS BILL - TYPE                       
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(PRV)                                                         
                                                                                
         ORG   FLDTAB+(PCQBTNET*FLDTABL)                                        
         DC    S(NET$)               BILL TOTAL - NET                           
         DC    AL1(L'NET$,0,FLDCAMT)                                            
         DC    AL1(0)                                                           
         DC    AL4(MBT)                                                         
                                                                                
         ORG   FLDTAB+(PCQBTCOM*FLDTABL)                                        
         DC    S(COM$)               BILL TOTAL - COMM                          
         DC    AL1(L'COM$,0,FLDCAMT)                                            
         DC    AL1(0)                                                           
         DC    AL4(MBT)                                                         
                                                                                
         ORG   FLDTAB+(PCQBTGRS*FLDTABL)                                        
         DC    S(GRS$)               BILL TOTAL - GROSS                         
         DC    AL1(L'GRS$,0,FLDCAMT)                                            
         DC    AL1(0)                                                           
         DC    AL4(MBT)                                                         
                                                                                
         ORG   FLDTAB+(PCQBTCD*FLDTABL)                                         
         DC    S(CSD$)                BILL TOTAL - CD                           
         DC    AL1(L'CSD$,0,FLDCAMT)                                            
         DC    AL1(0)                                                           
         DC    AL4(MBT)                                                         
                                                                                
         ORG   FLDTAB+(PCQBTPAY*FLDTABL)                                        
         DC    S(0)                  PAY ABOVE AMOUNT                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(MBT)                                                         
                                                                                
         ORG   FLDTAB+(PCQTXTYP*FLDTABL)                                        
         DC    AL2(0)                TAX - TYPE                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(TAX)                                                         
                                                                                
         ORG   FLDTAB+(PCQTXREG*FLDTABL)                                        
         DC    AL2(0)                TAX - REGISTRATION                         
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(TAX)                                                         
                                                                                
         ORG   FLDTAB+(PCQTXBAS*FLDTABL)                                        
         DC    AL2(0)                TAX - BASIS                                
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(TAX)                                                         
                                                                                
         ORG   FLDTAB+(PCQTXRAT*FLDTABL)                                        
         DC    AL2(0)                TAX - RATE                                 
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(TAX)                                                         
                                                                                
         ORG   FLDTAB+(PCQTXAMT*FLDTABL)                                        
         DC    AL2(0)                TAX - AMOUNT                               
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(TAX)                                                         
                                                                                
         ORG   FLDTAB+(PCQTXDES*FLDTABL)                                        
         DC    AL2(0)              TAX - DESCRIPTION                            
         DC    AL1(0,0,0)                                                       
         DC    AL1(0)                                                           
         DC    AL4(MBT)                                                         
                                                                                
         ORG   FLDTAB+(PCQAOOP*FLDTABL)                                         
         DC    S(LVAOOP)           LEVEL A OOP                                  
         DC    AL1(L'LVAOOP,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQAPSF*FLDTABL)                                         
         DC    S(LVAPSF)           LEVEL A PSF                                  
         DC    AL1(L'LVAPSF,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQANET*FLDTABL)                                         
         DC    S(LVANET)            LEVEL A NET                                 
         DC    AL1(L'LVANET,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQACOM*FLDTABL)                                         
         DC    S(LVACOM)            LEVEL A COMM                                
         DC    AL1(L'LVACOM,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQACSD*FLDTABL)                                         
         DC    S(LVACSD)            LEVEL A CSD                                 
         DC    AL1(L'LVACSD,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQATOT*FLDTABL)                                         
         DC    S(LVATOT)            LEVEL A TOTAL                               
         DC    AL1(L'LVATOT,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBOOP*FLDTABL)                                         
         DC    S(LVBOOP)            LEVEL B OOP                                 
         DC    AL1(L'LVBOOP,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBPSF*FLDTABL)                                         
         DC    S(LVBPSF)            LEVEL B PSF                                 
         DC    AL1(L'LVBPSF,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBNET*FLDTABL)                                         
         DC    S(LVBNET)            LEVEL B NET                                 
         DC    AL1(L'LVBNET,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBCOM*FLDTABL)                                         
         DC    S(LVBCOM)            LEVEL B COMM                                
         DC    AL1(L'LVBCOM,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBCSD*FLDTABL)                                         
         DC    S(LVBCSD)            LEVEL B CSD                                 
         DC    AL1(L'LVBCSD,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQBTOT*FLDTABL)                                         
         DC    S(LVBTOT)            LEVEL B TOTAL                               
         DC    AL1(L'LVBCOM,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCOOP*FLDTABL)                                         
         DC    S(LVCOOP)            LEVEL C OOP                                 
         DC    AL1(L'LVCOOP,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCPSF*FLDTABL)                                         
         DC    S(LVCPSF)            LEVEL C PSF                                 
         DC    AL1(L'LVCPSF,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCNET*FLDTABL)                                         
         DC    S(LVCNET)            LEVEL C NET                                 
         DC    AL1(L'LVCNET,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCCOM*FLDTABL)                                         
         DC    S(LVCCOM)            LEVEL C COM                                 
         DC    AL1(L'LVCCOM,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCCSD*FLDTABL)                                         
         DC    S(LVCCSD)            LEVEL C CSD                                 
         DC    AL1(L'LVCCSD,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQCTOT*FLDTABL)                                         
         DC    S(LVCTOT)           LEVEL C TOTAL                                
         DC    AL1(L'LVCTOT,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGOOP*FLDTABL)                                         
         DC    S(LVGOOP)           CURRENT GROUP LEVEL                          
         DC    AL1(L'LVGOOP,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGPSF*FLDTABL)                                         
         DC    S(LVGPSF)           PSF                                          
         DC    AL1(L'LVGPSF,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGNET*FLDTABL)                                         
         DC    S(LVGNET)            NET                                         
         DC    AL1(L'LVGNET,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGCOM*FLDTABL)                                         
         DC    S(LVGCOM)            COM                                         
         DC    AL1(L'LVGCOM,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGCSD*FLDTABL)                                         
         DC    S(LVGCSD)            CSD                                         
         DC    AL1(L'LVGCSD,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGTOT*FLDTABL)                                         
         DC    S(LVGTOT)            TOTAL                                       
         DC    AL1(L'LVGTOT,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGGST*FLDTABL)                                         
         DC    S(LVGGST)             GST                                        
         DC    AL1(L'LVGGST,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGPST*FLDTABL)                                         
         DC    S(LVGPST)             PST                                        
         DC    AL1(L'LVGPST,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGTAX*FLDTABL)                                         
         DC    S(LVGTAX)             TOTAL TAX                                  
         DC    AL1(L'LVGTAX,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG   FLDTAB+(PCQGRCV*FLDTABL)                                         
         DC    S(LVGRCV)             RECEIVABLE                                 
         DC    AL1(L'LVGRCV,0,FLDCAMT)                                          
         DC    AL1(FLDIAMT+FLDITOT)                                             
         DC    AL4(0)                                                           
                                                                                
         ORG                                                                    
         DROP  R3,R6                                                            
         EJECT                                                                  
**********************************************************************          
* TABLE OF BILL FORMAT TYPES                                         *          
**********************************************************************          
                                                                                
         DS    0H                                                               
BFTAB    DS    0XL5                                                             
         DC    AL1(PGMALL21),C'0',AL2(A21F0-*),AL1(0)                           
         DC    AL1(0)                                                           
         DC    AL1(PGMALL21),C'1',AL2(A21F1-*),AL1(0)                           
         DC    AL1(BFTFXHDS)                                                    
         DC    AL1(PGMALL27),C'0',AL2(A27F0-*),AL1(0)                           
         DC    AL1(0)                                                           
         DC    AL1(PGMALL27),C'1',AL2(A27F1-*),AL1(0)                           
         DC    AL1(0)                                                           
         DC    AL1(PGMALL27),C'2',AL2(A27F2-*),AL1(0)                           
         DC    AL1(0)                                                           
         DC    AL1(PGMALL27),C'3',AL2(A27F3-*),AL1(0)                           
         DC    AL1(0)                                                           
         DC    AL1(PGMALL27),C'4',AL2(A27F4-*),AL1(0)                           
         DC    AL1(0)                                                           
         DC    AL1(PGMALL27),C'5',AL2(A27F5-*),AL1(0)                           
         DC    AL1(0)                                                           
         DC    AL1(PGMALL27),C'6',AL2(A27F6-*),AL1(REPTISSM)                    
         DC    AL1(0)                                                           
         DC    AL1(PGMALL27),C'7',AL2(A27F7-*),AL1(REPTISSM)                    
         DC    AL1(0)                                                           
         DC    AL1(PGMALL27),C'8',AL2(A27F8-*),AL1(0)                           
         DC    AL1(0)                                                           
         DC    X'FF'                                                            
                                                                                
BFTABD   DSECT ,                                                                
BFTPGMAQ DS    XL(L'PGMAQ)         Bill flavor                                  
BFTFCODE DS    C                   Format code                                  
BFTTDISP DS    HL2                 Displacement to table                        
BFTRINDS DS    XL(L'REPTINDS)      REPINDS value                                
BFTFLAG1 DS    X                   ** Indicator byte 1 **                       
BFTFXHDS EQU   X'80'               Uses XHEADS exclusively                      
BFTLNQ   EQU   *-BFTABD                                                         
ACNB03   CSECT ,                                                                
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 21 FORMATS * USED WITH DSECT GTD TO BUILD BFMRECS * *          
**********************************************************************          
                                                                                
A21F0    DS    0X                  RECORD TABLE                                 
* SECTION 1 FILTER                                                              
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC1-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* SECTION 2 FILTER                                                              
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC2,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC2-GENB),AL1(SEC2,0),AL2(0)                   
                                                                                
* SECTION 3 FILTER                                                              
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC3,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC3-GENB),AL1(SEC3,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR1-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(CONTQ,0),AL2(0,PDPGH1-GENB)                       
         DC    AL1(GTTXQ),AL1(CONTQ,0),AL2(IFHPP1-GENB,PDHPP1-GENB)             
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFHPP2-GENB,PDHPP2-GENB)                 
                                                                                
*  ** SECTION 1 **                                                              
*  *PRODUCTION CHARGES*                                                         
* HEADING PER SECTION                                                           
         DC    AL1(GTEFQ),AL1(PROG+TOTL,0,0,0),AL2(IFHPP1-GENB)                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCH1-GENB)                           
                                                                                
* SECTION PARAGRAPH                                                             
         DC    AL1(GTEFQ),AL1(PROG+TOTL,0,0,0),AL2(0)                           
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFHPP1-GENB,PDWCP1-GENB)              
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFHPP2-GENB,PDWCP2-GENB)              
                                                                                
* SECTION PARAGRAPH                                                             
         DC    AL1(GTEFQ),AL1(ESTM,0,0,0),AL2(IFNESWCN-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDWCP3-GENB)                        
                                                                                
* SUB-TOTAL                                                                     
         DC    AL1(GTEFQ),AL1(PROG+TOTL,0,0,0),AL2(IFHPP1-GENB)                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSTQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDWCS1-GENB)                        
                                                                                
* SECTION TOTAL                                                                 
         DC    AL1(GTEFQ),AL1(ALL-ONEL,0,0,0),AL2(0)                            
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFMEDC-GENB,PDTOTPC-GENB)             
                                                                                
* ** SECTION 2 **                                                               
* *MEDIA CHARGES*                                                               
* HEADING PER SECTION                                                           
         DC    AL1(GTEFQ),AL1(ALL-ONEL,0,0,0),AL2(0)                            
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFMEDC-GENB,PDWCH2-GENB)                 
* SECTION PARAGRAPH                                                             
         DC    AL1(GTEFQ),AL1(ALL-ONEL,0,0,0),AL2(0)                            
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFMEDC-GENB,PDWCP4-GENB)              
                                                                                
* SUB-TOTAL                                                                     
         DC    AL1(GTEFQ),AL1(PROG+TOTL,0,0,0),AL2(IFHPP1-GENB)                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSTQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFMEDC-GENB,PDWCS2-GENB)              
                                                                                
* SECTION TOTAL                                                                 
         DC    AL1(GTEFQ),AL1((ALL-(SPCL+ONEL)),0,0,0),AL2(0)                   
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFMEDC-GENB,PDTOTMC-GENB)             
         EJECT                                                                  
                                                                                
* BILL TOTALS                                                                   
         DC    AL1(GTEFQ),AL1((ALL-SPCL),0,0,0),AL2(0)                          
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFRORT-GENB,PDCSDT2-GENB)             
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFRORT-GENB,PDTOTS2-GENB)             
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFRETL-GENB,PDSHR1-GENB)                 
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFNRNTH-GENB,PDCSDT1-GENB)            
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFESTGST-GENB,PDTOTS4-GENB)              
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFESTPST-GENB,PDTOTS5-GENB)              
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFESTM-GENB,PDTOTS3-GENB)                
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFPOB-GENB,PDPCT1-GENB)               
                                                                                
* BILL TOTALS                                                                   
         DC    AL1(GTEFQ),AL1(SPCL,0,0,0),AL2(0)                                
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFRETL-GENB,PDTOTS2-GENB)             
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFRETL-GENB,PDSHR1-GENB)                 
                                                                                
* BILL TOTALS                                                                   
         DC    AL1(GTEFQ),AL1(PROG+SPCL,0,0,0),AL2(0)                           
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFNGST-GENB,PDDUE1-GENB)              
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFGST-GENB,PDDUE2-GENB)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFPST-GENB,PDDUE3-GENB)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDDUE4-GENB)                  
                                                                                
* TAX SUMMARY(PROGRESSIVE)                                                      
         DC    AL1(GTEFQ),AL1(PROG,0,0,0),AL2(IFGTAX-GENB)                      
         DC    AL1(GTLOQ),AL2(BEDKWG1Q,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTAXA1-GENB)                          
                                                                                
* PREVIOUS BILLS - PROGRESSIVE                                                  
         DC    AL1(GTEFQ),AL1(PROG,0,0,0),AL2(IFPB-GENB)                        
         DC    AL1(GTLOQ),AL2(BEDKWPBQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPRVB1-GENB)                          
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDJOBT3-GENB)                          
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFNGST-GENB,PDTAD1-GENB)                 
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDTAD2-GENB)                  
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFPST-GENB,PDTAD3-GENB)                  
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDTAD4-GENB)                  
                                                                                
* PREVIOUS BILLS  - TOTAL BILL,ETC                                              
         DC    AL1(GTEFQ),AL1(TOTL+ESTM+ONEL,0,0,0),AL2(0)                      
         DC    AL1(GTLOQ),AL2(BEDKWPBQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPRVB1-GENB)                          
                                                                                
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFPOBP-GENB,PDPCT1-GENB)              
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFNGST-GENB,PDDUE1-GENB)                 
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDDUE2-GENB)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFPST-GENB,PDDUE3-GENB)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDDUE4-GENB)                  
                                                                                
* TAX SUMMARY                                                                   
         DC    AL1(GTEFQ),AL1(ALL-PROG,0,0,0),AL2(IFGTAX-GENB)                  
         DC    AL1(GTLOQ),AL2(BEDKWGSQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTAXA1-GENB)                          
                                                                                
* JOB COMMENTS                                                                  
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BEDKWJCQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDCMNT1-GENB)                          
                                                                                
* PAGE FOOTER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWPFQ),AL2(0),AL1(0,GTLOBF)                    
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,0)                                     
         DC    AL1(EOT)            END OF TABLE                                 
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 21 FORMAT 1                                         *          
**********************************************************************          
                                                                                
A21F1    DS    0X                  RECORD TABLE                                 
* SECTION 1 FILTER                                                              
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC1-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* SECTION 2 FILTER                                                              
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC2,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC2-GENB),AL1(SEC2,0),AL2(0)                   
                                                                                
* SECTION 3 FILTER                                                              
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC3,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC3-GENB),AL1(SEC3,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR1-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(CONTQ,0),AL2(0,PDPGH3-GENB)                       
         DC    AL1(GTTXQ),AL1(CONTQ,0),AL2(IFHPP1-GENB,PDHPP1-GENB)             
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFHPP2-GENB,PDHPP2-GENB)                 
                                                                                
* EXTRA JOB COMMENTS (HEADER 2)                                                 
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWH2Q,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDCMNT2-GENB)                          
                                                                                
*  ** SECTION 1 **                                                              
*  *PRODUCTION CHARGES*                                                         
* HEADING PER SECTION                                                           
         DC    AL1(GTEFQ),AL1(PROG+TOTL,0,0,0),AL2(IFHPP1-GENB)                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCH1-GENB)                           
                                                                                
* SECTION PARAGRAPH                                                             
         DC    AL1(GTEFQ),AL1(PROG+TOTL,0,0,0),AL2(0)                           
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,1),AL2(IFHPP1-GENB,PDWCP1-GENB)                 
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFHPP2-GENB,PDWCP2-GENB)              
                                                                                
* SECTION PARAGRAPH                                                             
         DC    AL1(GTEFQ),AL1(ESTM,0,0,0),AL2(IFNESWCN-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDWCP3-GENB)                        
                                                                                
* SUB-TOTAL                                                                     
         DC    AL1(GTEFQ),AL1(PROG+TOTL,0,0,0),AL2(IFHPP1-GENB)                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSTQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDWCS1-GENB)                        
                                                                                
* SECTION TOTAL                                                                 
         DC    AL1(GTEFQ),AL1(ALL-ONEL,0,0,0),AL2(0)                            
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFMEDC-GENB,PDTOTPC-GENB)             
         EJECT                                                                  
* ** SECTION 2 **                                                               
* *MEDIA CHARGES*                                                               
* HEADING PER SECTION                                                           
         DC    AL1(GTEFQ),AL1(ALL-ONEL,0,0,0),AL2(0)                            
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFMEDC-GENB,PDWCH2-GENB)                 
* SECTION PARAGRAPH                                                             
         DC    AL1(GTEFQ),AL1(ALL-ONEL,0,0,0),AL2(0)                            
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFMEDC-GENB,PDWCP4-GENB)              
                                                                                
* SUB-TOTAL                                                                     
         DC    AL1(GTEFQ),AL1(PROG+TOTL,0,0,0),AL2(IFHPP1-GENB)                 
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSTQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFMEDC-GENB,PDWCS2-GENB)              
                                                                                
* SECTION TOTAL                                                                 
         DC    AL1(GTEFQ),AL1((ALL-(SPCL+ONEL)),0,0,0),AL2(0)                   
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFMEDC-GENB,PDTOTMC-GENB)             
         EJECT                                                                  
                                                                                
* BILL TOTALS                                                                   
         DC    AL1(GTEFQ),AL1((ALL-SPCL),0,0,0),AL2(0)                          
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFRORT-GENB,PDCSDT2-GENB)             
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFRORT-GENB,PDTOTS2-GENB)             
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFRETL-GENB,PDSHR1-GENB)                 
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFNRNTH-GENB,PDCSDT1-GENB)            
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFESTGST-GENB,PDTOTS4-GENB)              
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFESTPST-GENB,PDTOTS5-GENB)              
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFESTM-GENB,PDTOTS3-GENB)                
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFPOB-GENB,PDPCT1-GENB)               
                                                                                
* BILL TOTALS                                                                   
         DC    AL1(GTEFQ),AL1(SPCL,0,0,0),AL2(0)                                
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFRETL-GENB,PDTOTS2-GENB)             
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFRETL-GENB,PDSHR1-GENB)                 
                                                                                
* BILL TOTALS                                                                   
         DC    AL1(GTEFQ),AL1(PROG+SPCL,0,0,0),AL2(0)                           
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFNGST-GENB,PDDUE1-GENB)              
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFGST-GENB,PDDUE2-GENB)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFPST-GENB,PDDUE3-GENB)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDDUE4-GENB)                  
                                                                                
* TAX SUMMARY(PROGRESSIVE)                                                      
         DC    AL1(GTEFQ),AL1(PROG,0,0,0),AL2(IFGTAX-GENB)                      
         DC    AL1(GTLOQ),AL2(BEDKWG1Q,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTAXA1-GENB)                          
                                                                                
* PREVIOUS BILLS - PROGRESSIVE                                                  
         DC    AL1(GTEFQ),AL1(PROG,0,0,0),AL2(IFPB-GENB)                        
         DC    AL1(GTLOQ),AL2(BEDKWPBQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPRVB1-GENB)                          
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDJOBT3-GENB)                          
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFNGST-GENB,PDTAD1-GENB)                 
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDTAD2-GENB)                  
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFPST-GENB,PDTAD3-GENB)                  
                                                                                
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDTAD4-GENB)                  
                                                                                
* PREVIOUS BILLS  - TOTAL BILL,ETC                                              
         DC    AL1(GTEFQ),AL1(TOTL+ESTM+ONEL,0,0,0),AL2(0)                      
         DC    AL1(GTLOQ),AL2(BEDKWPBQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPRVB1-GENB)                          
                                                                                
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFPOBP-GENB,PDPCT1-GENB)              
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFNGST-GENB,PDDUE1-GENB)                 
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDDUE2-GENB)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFPST-GENB,PDDUE3-GENB)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFGST-GENB,PDDUE4-GENB)                  
                                                                                
* TAX SUMMARY                                                                   
         DC    AL1(GTEFQ),AL1(ALL-PROG,0,0,0),AL2(IFGTAX-GENB)                  
         DC    AL1(GTLOQ),AL2(BEDKWGSQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTAXA1-GENB)                          
                                                                                
* JOB COMMENTS                                                                  
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BEDKWJCQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDCMNT1-GENB)                          
                                                                                
* PAGE FOOTER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,0,0),AL2(0)                                 
         DC    AL1(GTLOQ),AL2(BFMKWPFQ),AL2(0),AL1(0,GTLOBF)                    
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,0)                                     
         DC    AL1(EOT)            END OF TABLE                                 
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 27 FORMAT #0                                        *          
*  ALL WORKCODE TYPES                                                *          
**********************************************************************          
                                                                                
A27F0    DS    0X                                                               
* SECTION 1 FILTER(NO FILTER)                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC3-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR2-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(CONTQ,0),AL2(0,PDPGH2-GENB)                       
         DC    AL1(GTTXQ),AL1(CONTQ,0),AL2(IFHPP1-GENB,PDHPP1-GENB)             
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFHPP2-GENB,PDHPP2-GENB)                 
                                                                                
* HEADING PER SECTION                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFHPP1-GENB)                     
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCH3-GENB)                           
                                                                                
* SECTION PARAGRAPH                                                             
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFHPP1-GENB,PDWCP1-GENB)              
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(IFHPP2-GENB,PDWCP2-GENB)              
                                                                                
* W/C TOTAL                                                                     
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSTQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDWCS1-GENB)                        
                                                                                
* INVOICE DETAIL - DUE SECTION                                                  
         DC    AL1(GTLKQ),AL2(IDDUE-GENB),AL2(0,0)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 27 FORMAT #1                                        *          
*  PREBILL WORKCODE TYPES ONLY                                       *          
**********************************************************************          
                                                                                
A27F1    DS    0X                                                               
* SECTION FILTER - PREBILL                                                      
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC3-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR2-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPGH2-GENB)                           
                                                                                
* SECTION PARAGRAPH                                                             
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDPREH1-GENB)                       
                                                                                
* INVOICE DETAIL - DUE SECTION                                                  
         DC    AL1(GTLKQ),AL2(IDDUE-GENB),AL2(0,0)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 27 FORMAT #2                                        *          
*  OOP/TIME WORKCODE TYPES                                           *          
*  PREBILL - TYPE 48 ONLY                                            *          
**********************************************************************          
                                                                                
A27F2    DS    0X                                                               
* SECTION FILTER - PROFESSIONAL                                                 
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC4-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* SECTION FILTER - OOP                                                          
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC2,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC5-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR2-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPGH2-GENB)                           
                                                                                
* SECTION PARAGRAPH - PROF. SERV.                                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDPSF3-GENB)                        
                                                                                
* HEADING PER SECTION - OOP                                                     
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOP1-GENB)                           
                                                                                
* SUB-HEADING VENDOR                                                            
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFNOC-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSHQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDVCN1-GENB)                           
                                                                                
* SECTION PARAGRAPH - OOP (IF VENDOR NAME OR CODE)                              
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFNOC-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDVNP1-GENB)                           
                                                                                
* SECTION PARAGRAPH - OOP (IF NO VENDOR NAME OR CODE)                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFNOTNC-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCP5-GENB)                           
                                                                                
* SECTION TOTAL - OOP                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC2,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDVNS1-GENB)                           
                                                                                
* SUBTOTAL                                                                      
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDSUB2-GENB)                           
                                                                                
* INVOICE DETAIL - DUE SECTION                                                  
         DC    AL1(GTLKQ),AL2(IDDUE-GENB),AL2(0,0)                              
                                                                                
* FORMAT 2 SUMMARY **                                                           
         DC    AL1(GTLKQ),AL2(IVSUM-GENB),AL2(0,0)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 27 FORMAT #3 & #7                                   *          
*  OOP/TIME WORKCODE TYPES                                           *          
*  PREBILL - TYPE 48 ONLY                                            *          
**********************************************************************          
                                                                                
A27F3    DS    0X                                                               
A27F7    DS    0X                                                               
* SECTION FILTER - PROFESSIONAL                                                 
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC4-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* SECTION FILTER - OOP                                                          
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC2,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC5-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR2-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPGH2-GENB)                           
                                                                                
* SECTION PARAGRAPH - PROF. SERV.                                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFF#3-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPSF3-GENB)                           
                                                                                
* SECTION PARAGRAPH - PROF. SERV.(HEADING ONLY)                                 
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFNOPSF-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPSF2-GENB)                           
                                                                                
* SECTION PARAGRAPH - PROF. SERV.                                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFF#7-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCG1-GENB)                           
* SECTION TOTAL - PSF                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFF#7-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC1,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPSFT2-GENB)                          
                                                                                
* HEADING PER SECTION - OOP                                                     
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOP1-GENB)                           
                                                                                
* SUB-HEADING WORKCODE                                                          
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSHQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCH4-GENB)                           
                                                                                
* SECTION PARAGRAPH - OOP -VENDOR                                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCP6-GENB)                           
                                                                                
* SECTION TOTAL - OOP                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC2,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCS3-GENB)                           
                                                                                
* SUBTOTAL                                                                      
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOPS2-GENB)                          
                                                                                
* INVOICE DETAIL - DUE SECTION                                                  
         DC    AL1(GTLKQ),AL2(IDDUE-GENB),AL2(0,0)                              
                                                                                
* FORMAT 3 & 7 SUMMARY **                                                       
         DC    AL1(GTLKQ),AL2(IVSUM-GENB),AL2(0,0)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 27 FORMAT #4 & #5  GROUP LEVEL 'C' & 'P'            *          
*  OOP/TIME WORKCODE TYPES                                           *          
*  PREBILL - TYPE 48 ONLY                                            *          
**********************************************************************          
                                                                                
A27F4    DS    0X                                                               
A27F5    DS    0X                                                               
* SECTION FILTER - PROFESSIONAL                                                 
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC4-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* SECTION FILTER - OOP                                                          
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC2,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC5-GENB),AL1(SEC2,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR2-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPGH2-GENB)                           
                                                                                
* PRODUCT HEADER                                                                
         DC    AL1(GTEFQ),AL1(ALL,0,LVB,0),AL2(IFLVA-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHPQ),AL1(0,GTLOBF)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPROH1-GENB)                          
                                                                                
* JOB HEADER                                                                    
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHPQ),AL1(0,GTLOBF)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDJOBH1-GENB)                          
                                                                                
* PSF                                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPSFH1-GENB)                          
                                                                                
**********************************************************************          
* FORMAT #4 - VENDOR / WC  / DESCRIPTION                                        
         DC    AL1(GTIFQ),AL1(ALL,0,LVC,0),AL2(IFF#4-GENB)                      
                                                                                
* IF NO VENDOR OR WC DETAIL                                                     
         DC    AL1(GTIFQ),AL1(ALL,0,LVC,0),AL2(IFNODTL-GENB)                    
                                                                                
* OOP                                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOPT1-GENB)                          
                                                                                
* END NO VENDOR OR WC DETAIL                                                    
         DC    AL1(GTEIQ),XL6'00'                                               
                                                                                
* IF ANY WC OR VENDOR DETAIL                                                    
         DC    AL1(GTIFQ),AL1(ALL,0,LVC,0),AL2(IFDTL-GENB)                      
                                                                                
* OOP - HEADER (OUT-OF-POCKET EXPENSES)                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOP2-GENB)                           
                                                                                
* SUB-HEADING VENDOR                                                            
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSHQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDVCN2-GENB)                           
                                                                                
* SECTION PARAGRAPH - OOP(IF VENDOR NAME OR CODE)                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFNOC-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDVNP1-GENB)                           
                                                                                
* SECTION PARAGRAPH - OOP(IF NO VENDOR NAME OR CODE)                            
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFNOTNC-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCP5-GENB)                           
                                                                                
* SECTION TOTAL                                                                 
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC2,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDVNS2-GENB)                           
                                                                                
* END WC OR VENDOR DETAIL                                                       
         DC    AL1(GTEIQ),XL6'00'                                               
                                                                                
* END FORMAT #4                                                                 
         DC    AL1(GTEIQ),XL6'00'                                               
**********************************************************************          
                                                                                
                                                                                
**********************************************************************          
* FORMAT #5  -  WC / DESCRIPTION / VENDOR                                       
         DC    AL1(GTIFQ),AL1(ALL,0,LVC,0),AL2(IFF#5-GENB)                      
                                                                                
* IF NO VENDOR OR WC DETAIL                                                     
         DC    AL1(GTIFQ),AL1(ALL,0,LVC,0),AL2(IFNODTL-GENB)                    
                                                                                
* OOP                                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOPT1-GENB)                          
                                                                                
* END NO VENDOR OR WC DETAIL                                                    
         DC    AL1(GTEIQ),XL6'00'                                               
                                                                                
* IF ANY WC OR VENDOR DETAIL                                                    
         DC    AL1(GTIFQ),AL1(ALL,0,LVC,0),AL2(IFDTL-GENB)                      
                                                                                
                                                                                
* OOP - HEADER (OUT-OF-POCKET EXPENSES)                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOP2-GENB)                           
                                                                                
* SUB-HEADING WC                                                                
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSHQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCH5-GENB)                           
                                                                                
* SECTION PARAGRAPH - OOP                                                       
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCP6-GENB)                           
                                                                                
* SECTION TOTAL - OOP                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC2,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCS4-GENB)                           
                                                                                
* END WC OR VENDOR DETAIL                                                       
         DC    AL1(GTEIQ),XL6'00'                                               
                                                                                
* END FORMAT #5                                                                 
         DC    AL1(GTEIQ),XL6'00'                                               
                                                                                
**********************************************************************          
                                                                                
* TOTAL - JOB                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDJOBT2-GENB)                       
                                                                                
* TOTAL - PRODUCT                                                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVB,0),AL2(IFLVA-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDPROT1-GENB)                       
                                                                                
* SUBTOTAL                                                                      
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDSUB3-GENB)                           
                                                                                
* INVOICE DETAIL - DUE SECTION                                                  
         DC    AL1(GTLKQ),AL2(IDDUE-GENB),AL2(0,0)                              
                                                                                
* FORMAT 4 & 5 SUMMARY **                                                       
         DC    AL1(GTLKQ),AL2(IVSUM-GENB),AL2(0,0)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 27 FORMAT #6                                        *          
*  OOP/TIME/RETAINER WORKCODE TYPES                                  *          
*  PREBILL - TYPE 48 ONLY                                            *          
**********************************************************************          
                                                                                
A27F6    DS    0X                                                               
* SECTION FILTER - PROFESSIONAL                                                 
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC8-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* SECTION FILTER - OOP                                                          
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC2,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC5-GENB),AL1(SEC2,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR2-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPGH2-GENB)                           
                                                                                
* PRODUCT HEADER                                                                
         DC    AL1(GTEFQ),AL1(ALL,0,LVB,0),AL2(IFLVA-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHPQ),AL1(0,GTLOBF)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPROH1-GENB)                          
                                                                                
* JOB HEADER                                                                    
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHPQ),AL1(0,GTLOBF)                  
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDJOBH1-GENB)                          
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPOBH1-GENB)                          
                                                                                
* SECTION PARAGRAPH - PROF. SERV.(IF ANY TIME DETAIL)                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFTDTL-GENB)                     
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPSF1-GENB)                           
                                                                                
* HEADING PER SECTION - TIME(PSF) WC HEADING                                    
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFTOPT1-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSSHQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCH6-GENB)                           
                                                                                
* SECTION PARAGRAPH - TIME(PSF) SHOW WC/PER                                     
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFTOPT1-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTPW1-GENB)                           
                                                                                
* SECTION PARAGRAPH - TIME(PSF) SHOW WC, NO PERSON                              
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFTOPT2-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTPW2-GENB)                           
                                                                                
* SECTION PARAGRAPH - TIME(PSF) NO WC, BY PERSON                                
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFTOPT3-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTPW3-GENB)                           
                                                                                
* SECTION PARAGRAPH - TIME(PSF) NO WC, NO PERSON                                
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFTOPT4-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTPW4-GENB)                           
                                                                                
* SECTION TOTAL - PFS - IF ANY WC/PER/DETAIL                                    
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(IFTTOT-GENB)                     
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC1,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPSFT1-GENB)                          
                                                                                
* HEADING PER SECTION - OOP                                                     
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHSQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOP3-GENB)                           
                                                                                
* SECTION PARAGRAPH/OOP/WC/VENDOR                                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC2,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCP7-GENB)                           
                                                                                
* SECTION TOTAL - OOP                                                           
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSTOQ),AL1(SEC2,GTLOFT)               
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDOOPT2-GENB)                       
                                                                                
* SUBTOTAL                                                                      
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDJOBS1-GENB)                       
                                                                                
* TOTAL PRODUCT                                                                 
         DC    AL1(GTEFQ),AL1(ALL,0,LVB,0),AL2(IFGRPAB-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPROT1-GENB)                          
                                                                                
* SUBTOTAL - LEVEL A                                                            
         DC    AL1(GTEFQ),AL1(ALL,0,LVA,0),AL2(IFGRPA-GENB)                     
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDSUB3-GENB)                           
                                                                                
* INVOICE DETAIL - DUE SECTION                                                  
         DC    AL1(GTLKQ),AL2(IDDUE-GENB),AL2(0,0)                              
                                                                                
* FORMAT 6 SUMMARY **                                                           
         DC    AL1(GTLKQ),AL2(IVSUM-GENB),AL2(0,0)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* RECORD TABLE - 27 FORMAT #8                                        *          
*  ALL WORKCODE TYPES                                                *          
**********************************************************************          
                                                                                
A27F8    DS    0X                                                               
* SECTION FILTER - ALL                                                          
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,0),AL1(SEC1,0)                           
         DC    AL1(GTFSQ),AL2(FLSEC3-GENB),AL1(SEC1,0),AL2(0)                   
                                                                                
* UPPER                                                                         
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR2-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPGH2-GENB)                           
                                                                                
* JOB NAME                                                                      
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSHPQ),AL1(0,GTLOBF)                  
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDJOBH2-GENB)                       
                                                                                
* SECTION PARAGRAPH - WORKCODE NAME                                             
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBDQ,BFMKSPRQ),AL1(SEC1,GTLOBF)               
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDWCP8-GENB)                           
                                                                                
* SUBTOTAL JOB                                                                  
         DC    AL1(GTEFQ),AL1(ALL,0,LVC,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDJOBT1-GENB)                       
                                                                                
* SUBTOTAL PRODUCT                                                              
         DC    AL1(GTEFQ),AL1(ALL,0,LVB,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDPROT2-GENB)                       
                                                                                
* SUBTOTAL                                                                      
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDSUB2-GENB)                        
                                                                                
* INVOICE DETAIL - DUE SECTION                                                  
         DC    AL1(GTLKQ),AL2(IDDUE-GENB),AL2(0,0)                              
                                                                                
* FORMAT 8 SUMMARY **                                                           
         DC    AL1(GTLKQ),AL2(IVSUM-GENB),AL2(0,0)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* ** INVOICE DETAIL - TOTAL DUE SECTION                                         
                                                                                
IDDUE    DS    0H                                                               
* CASH DISCOUNT                                                                 
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFCSD-GENB)                      
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDCSDT3-GENB)                       
                                                                                
* HANDLING CHARGE                                                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFNINWC-GENB)                    
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1B),AL2(0,PDHC1-GENB)                         
                                                                                
* SUBTOTAL                                                                      
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1B),AL2(0,PDSUB1-GENB)                        
                                                                                
* GST/PST                                                                       
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,SP1B),AL2(IFGST-GENB,PDGSTT1-GENB)              
         DC    AL1(GTTXQ),AL1(0,0),AL2(IFPST-GENB,PDPSTT1-GENB)                 
                                                                                
* PLEASE PAY...."                                                               
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(0)                               
         DC    AL1(GTLOQ),AL2(BEDKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1B),AL2(0,PDDUE5-GENB)                        
                                                                                
* TAX SUMMARY                                                                   
         DC    AL1(GTEFQ),AL1(ALL,0,LVR,0),AL2(IFGTAX-GENB)                     
         DC    AL1(GTLOQ),AL2(BEDKWGSQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTAXA1-GENB)                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* ** INVOICE SUMMARY **                                                         
* UPPER                                                                         
IVSUM    DS    0H                                                               
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(0)                         
         DC    AL1(GTLOQ),AL2(BFMKWUPQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDUPR2-GENB)                           
                                                                                
* PAGE HEADER                                                                   
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(0)                         
         DC    AL1(GTLOQ),AL2(BFMKWPHQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDPGH2-GENB)                           
                                                                                
* PERIOD                                                                        
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(0)                         
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDBPS1-GENB)                        
                                                                                
* PROF. SERV. FEE                                                               
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(IFPSF-GENB)                
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,SP1A),AL2(0,PDPSFS1-GENB)                       
                                                                                
* OOP                                                                           
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(IFOOP-GENB)                
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDOOPS1-GENB)                          
                                                                                
* TOTAL                                                                         
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(0)                         
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1B),AL2(0,PDTOTS1-GENB)                       
                                                                                
* CASH DISCOUNT                                                                 
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(IFCSD-GENB)                
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1B),AL2(0,PDCSDT3-GENB)                       
                                                                                
* HANDLING CHARGE                                                               
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(0)                         
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1B),AL2(0,PDHCS1-GENB)                        
                                                                                
* SUBTOTAL                                                                      
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(0)                         
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDSUB1-GENB)                           
                                                                                
* TOTAL TAX                                                                     
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(IFTAX-GENB)                
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDTAXT1-GENB)                          
                                                                                
* SUB-TOTAL-FINAL                                                               
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(IFTAX-GENB)                
         DC    AL1(GTLOQ),AL2(BFMKWBTQ,0),AL1(0,GTLOBF)                         
         DC    AL1(GTTXQ),AL1(0,0),AL2(0,PDSUB4-GENB)                           
                                                                                
* "PLEASE PAY...."                                                              
         DC    AL1(GTEFQ),AL1(ALL,GTEFISQ,LVR,0),AL2(0)                         
         DC    AL1(GTLOQ),AL2(BEDKWBTQ,0),AL1(0,GTLOFT)                         
         DC    AL1(GTTXQ),AL1(0,SP1B),AL2(0,PDDUE5-GENB)                        
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT DEFINITION TABLES  USE DID DSECT                              *         
***********************************************************************         
                                                                                
PDBPS1   DS    0X                                  JOB COMMENTS                 
         DC    AL1(1,2,15,1,0,0)                   "BILL PERIOD"                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@BPER),AL2(DD@BPER-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,18,20,1,LFT,0)                 DATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTTLDA)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,19,1,UNDR,0)               'INVOICE SUMMARY'             
         DC    AL1(0,0)                            --------------               
         DC    AL1(0)                                                           
         DC    AL1(L'DD@INVSU),AL2(DD@INVSU-DICD)                               
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDCMNT1  DS    0X                                  JOB COMMENTS                 
         DC    AL1(2,2,72,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJCO)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
                                                                                
PDCMNT2  DS    0X                                  EXTRA JOB COMMENTS           
         DC    AL1(2,2,L'XHEADS-1,1,0,0)                                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJC2)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDCSDT1  DS    0X                                   SECTION TOTAL               
         DC    AL1(2,2,15,1,CDO,0)                  'CASH DISCOUNT'             
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@CSHDS),AL2(DD@CSHDS-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,38,15,1,RGTNUM+CDO,REV)        CD IN NET COL.              
         DC    AL1(PCQCD,0)                                                     
         DC    AL1(PCQJTCD)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,70,15,1,RGTNUM+CDO,REV)        CD IN GROSS COL.            
         DC    AL1(PCQCD,0)                                                     
         DC    AL1(PCQJTCD)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDCSDT2  DS    0X                                   SECTION TOTAL               
         DC    AL1(2,2,15,1,CDO,0)                  'CASH DISCOUNT'             
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@CSHDS),AL2(DD@CSHDS-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,38,15,1,RGTNUM+CDO,REV)        CD IN NET COL.              
         DC    AL1(PCQCD,0)                                                     
         DC    AL1(PCQCTCD)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,70,15,1,RGTNUM+CDO,REV)         CD IN GROSS COL.           
         DC    AL1(PCQCD,0)                                                     
         DC    AL1(PCQCTCD)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDCSDT3  DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,15,1,CDO,0)                  'CASH DISCOUNT'             
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@CSHDS),AL2(DD@CSHDS-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM+CDO,REV)        CD IN GROSS COL.            
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGCSD)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDDUE1   DS    0X                                   AMOUNT DUE                  
         DC    AL1(1,2,83,1,0,0)                    -------------               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,40,1,0,0)                    'TOTAL AMOUNT DUE'          
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAMDU)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,38,15,1,RTNUFT,PDOL)           NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,51,5,1,0,0)                    NIL                         
         DC    AL1(0,0)                             NET COLUMN                  
         DC    AL1(0)                                                           
         DC    AL1(L'DD@NIL),AL2(DD@NIL-DICD)                                   
         DC    AL2(IFPYNETZ-GENB),AL2(0)                                        
                                                                                
         DC    AL1(2,54,15,1,RTNUFT,PDOL)           COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,CRY+PDOL)       GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,82,5,1,0,0)                    NIL                         
         DC    AL1(0,0)                             GROSS COLUMN                
         DC    AL1(0)                                                           
         DC    AL1(L'DD@NIL),AL2(DD@NIL-DICD)                                   
         DC    AL2(IFPYGRSZ-GENB),AL2(0)                                        
                                                                                
*        DC    AL1(2,87,3,1,0,0)                    USD                         
*        DC    AL1(0,0)                             GROSS COLUMN                
*        DC    AL1(0)                                                           
*        DC    AL1(L'DD@USD),AL2(DD@USD-DICD)                                   
*        DC    AL2(IFPUSDG-GENB),AL2(0)                                         
                                                                                
         DC    AL1(3,21,33,1,RGHT+FTX,0)            'PAY ABOVE'                 
         DC    AL1(0,0)                             NET COLUMN                  
         DC    AL1(PCQBTPAY)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPYNET-GENB),AL2(0)                                         
                                                                                
         DC    AL1(3,52,33,1,RGHT+FTX,0)            'PAY ABOVE'                 
         DC    AL1(0,0)                             GROSS COLUMN                
         DC    AL1(PCQBTPAY)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPYGRS-GENB),AL2(0)                                         
                                                                                
         DC    AL1(4,2,83,1,0,0)                    --------------              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDDUE2   DS    0X                                   AMOUNT DUE WITH TAX         
         DC    AL1(1,2,83,1,0,0)                    -------------               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,40,1,0,0)                    'AMOUNT BEFORE TAX'         
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@ABTAX),AL2(DD@ABTAX-DICD)                               
         DC    AL2(IFTAX$-GENB),AL2(0)                                          
                                                                                
         DC    AL1(2,38,15,1,RTNUFT,PDOL)           NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBCNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCTX$-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,54,15,1,RTNUFT,PDOL)           COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBCCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCTX$-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,CRY+PDOL)       GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBCGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFTAX$-GENB),AL2(0)                                          
                                                                                
         DC    AL1(3,2,17,1,0,0)                    'GST'                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@VAT1),AL2(DD@VAT1-DICD)                                 
         DC    AL2(IFGST$-GENB),AL2(0)                                          
                                                                                
         DC    AL1(3,38,15,1,RTNUFT,0)              GST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUGST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCGS$-GENB),AL2(0)                                         
                                                                                
         DC    AL1(3,70,15,1,RTNUFT,CRY)                                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUGST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFGST$-GENB),AL2(0)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDDUE3   DC    AL1(1,2,17,1,0,0)                    'PST'                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXTYP)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPST$-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,0)              PST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUPST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCPS$-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)                                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUPST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPST$-GENB),AL2(0)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDDUE4   DC    AL1(1,2,40,1,0,0)                    'TOTAL AMOUNT DUE'          
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAMDU)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,PDOL)           GST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUNTX)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RTNUFT,PDOL)           COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY+PDOL)       GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDURCV)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,21,33,1,RGHT+FTX,PDOL)         'PAY ABOVE'                 
         DC    AL1(0,0)                             NET COLUMN                  
         DC    AL1(PCQBTPAY)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPYNET-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,52,33,1,RGHT+FTX,PDOL)         'PAY ABOVE'                 
         DC    AL1(0,0)                             GROSS COLUMN                
         DC    AL1(PCQBTPAY)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPYGRS-GENB),AL2(0)                                         
                                                                                
         DC    AL1(3,2,83,1,0,0)                    --------------              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                   'INVOICE TOTAL'             
PDDUE5   DC    AL1(1,59,26,1,0,0)                   --------------              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,59,15,1,0,0)                   INVOICE TOTAL               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@INVTO),AL2(DD@INVTO-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,CRY+PZA+PDOL)   GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDURCV)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNOGRP-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,CRY+PZA+PDOL)   GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGRCV)                         GROUP LEVEL                 
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFGRP-GENB),AL2(0)                                           
                                                                                
         DC    AL1(3,59,27,1,0,0)                   'PLEASE PAY...              
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBTPAY)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,59,26,1,0,0)                   --------------              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDGSTT1  DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,17,1,0,0)                    'AMOUNT BEFORE TAX'         
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@ABTAX),AL2(DD@ABTAX-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY+PDOL)       GROSS BEFORE TAX            
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGTOT)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,17,1,0,0)                    'GST'                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@VAT1),AL2(DD@VAT1-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,CRY+PDOL)       GST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJTGST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFA21-GENB),AL2(0)                                           
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,CRY+PDOL)       GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGGST)                         GROUP LEVEL                 
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFA27-GENB),AL2(0)                                           
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDHC1    DS    0X                                   HANDLING CHARGE             
         DC    AL1(1,2,16,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQHANDF)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM+UNDR,0)         COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGCOM)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDHCS1   DS    0X                                   HANDLING CHARGE             
         DC    AL1(1,2,16,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQHANDF)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGCOM)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDHPP1   DS    0X                                    HEADING PER PAGE           
         DC    AL1(11,2,15,1,0,0)                    'DESCRIPTION'              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@DESC),AL2(DD@DESC-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(11,24,15,1,0,0)                   'VENDOR'                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@VNN),AL2(DD@VNN-DICD)                                   
         DC    AL2(IFVN-GENB),AL2(0)                                            
                                                                                
         DC    AL1(11,42,15,1,0,0)                   'NET AMOUNT'               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@NETAM),AL2(DD@NETAM-DICD)                               
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(11,58,15,1,0,0)                   'COMMISSION'               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@CMN),AL2(DD@CMN-DICD)                                   
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(11,79,15,1,0,0)                   'TOTAL'                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TOTAL),AL2(DD@TOTAL-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(11,75,15,1,0,0)                  OR TOTAL USD                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TOTAU),AL2(DD@TOTAU-DICD)                               
         DC    AL2(IFPUSD-GENB),AL2(0)                                          
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDHPP2   DS    0X                                    HEADING PER PAGE           
         DC    AL1(11,2,9,1,0,0)                     'WORK CODE'                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@WC9),AL2(DD@WC9-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(11,42,15,1,0,0)                   'NET AMOUNT'               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@NETAM),AL2(DD@NETAM-DICD)                               
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(11,58,15,1,0,0)                   'COMMISSION'               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@CMN),AL2(DD@CMN-DICD)                                   
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(11,79,15,1,0,0)                   'TOTAL'                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TOTAL),AL2(DD@TOTAL-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(11,75,15,1,0,0)                  OR TOTAL USD                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TOTAU),AL2(DD@TOTAU-DICD)                               
         DC    AL2(IFPUSD-GENB),AL2(0)                                          
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDJOBH1  DC    AL1(1,2,6,1,UNDR,0)                  JOB CODE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,11,36,1,0,UNDR2)               JOB NAME                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDJOBH2  DC    AL1(1,16,36,1,0,0)                   JOB NAME                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDJOBS1  DS    0X                                   JOB                         
         DC    AL1(1,2,14,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TJOB),AL2(DD@TJOB-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,16,8,1,0,0)                    JOB CODE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,23,36,1,0,0)                   JOB NAME                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY)            NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCNET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDJOBT1  DS    0X                                  TOTAL FOR                    
         DC    AL1(1,16,12,1,0,0)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TFOR),AL2(DD@TFOR-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,28,36,1,LFT,0)                JOB NAME                     
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY)            NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDJOBT2  DS    0X                                                               
         DC    AL1(1,2,25,1,0,0)                    TOTAL FOR JOB               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TJOB),AL2(DD@TJOB-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCNET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDJOBT3  DS    0X                                                               
         DC    AL1(1,2,15,1,0,0)                    'TOTAL FOR JOB'             
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TJOB),AL2(DD@TJOB-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBXNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RTNUFT,0)              COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBXCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,0)              GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBXGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDOOPS1  DS    0X                                                               
         DC    AL1(1,6,36,1,0,0)                    'OUT-OF-POCKET'             
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@OOP),AL2(DD@OOP-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY)            OOP NET                     
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGOOP)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDOOPS2  DS    0X                                   OOP                         
         DC    AL1(1,70,15,1,RGTNUM+UNDR,CRY)       OOP NET                     
         DC    AL1(0,0)                             ------                      
         DC    AL1(PCQCOOP)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,2,12,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@SUBT),AL2(DD@SUBT-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,70,15,1,RGTNUM,CRY)            NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGNET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDOOPT1  DS    0X                                   TOTAL OOP                   
         DC    AL1(1,6,36,1,0,0)                    'OUT-OF- POCKET'            
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@OOP),AL2(DD@OOP-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,50,15,1,RGTNUM+UNDR,CRY)       NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCOOP)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDOOPT2  DS    0X                                   TOTAL OOP                   
         DC    AL1(1,78,6,1,0,0)                    --------                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,10,10,1,0,0)                   TOTAL FOR                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TFOR),AL2(DD@TFOR-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,21,30,1,LFT,0)                  OOP                        
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@OOP),AL2(DD@OOP-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,70,15,1,RGTNUM,CRY)             NET                        
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDOOP1   DS    0X                                   OOP HEADER                  
         DC    AL1(1,2,36,1,UNDR,0)                 'OUT-OF- POCKET'            
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@OOP),AL2(DD@OOP-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDOOP2   DS    0X                                  OOP                          
         DC    AL1(1,6,36,1,UNDR,0)                'OUT-OF- POCKET'             
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@OOP),AL2(DD@OOP-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDOOP3   DS    0X                                 OOP                           
         DC    AL1(1,6,36,1,UNDR,0)               'OUT-OF- POCKET'              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@OOP),AL2(DD@OOP-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPCT1   DS    0X                                    % OF BILLING               
         DC    AL1(1,2,2,1,LFT,0)                    WC                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBW)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,5,20,1,0,0)                     WC DESC.                   
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBN)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)               POB AMOUNT                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBA)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)               POB AMOUNT                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBA)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPGH1   DS    0X                                   PAGE HEADER                 
         DC    AL1(1,2,9,1,0,0)                     'BILL DATE'                 
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@BILDT),AL2(DD@BILDT-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,12,10,1,0,0)                   BILL DATE                   
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBD)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,32,17,2,UNDR+CNTR,0)           'PRODUCTION BILL'           
         DC    AL1(0,0)                             OR MEDIA NAME               
         DC    AL1(PCQBMN)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,56,13,1,0,0)                   'BILL NUMBER'               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@BLN11),AL2(DD@BLN11-DICD)                               
         DC    AL2(IFPBIL-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,56,07,1,0,0)                   'INVOICE                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@INV),AL2(DD@INV-DICD)                                   
         DC    AL2(IFPINV-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,68,9,1,0,0)                    BILL NUMBER                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,79,10,1,0,0)                   PAGE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPG)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,9,1,0,0)                    'SHIP DATE'                  
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@SHIP),AL2(DD@SHIP-DICD)                                 
         DC    AL2(IFSHPD-GENB),AL2(0)                                          
                                                                                
         DC    AL1(2,12,9,1,0,0)                    SHIP DATE                   
         DC    AL1(0,0)                                                         
         DC    AL1(PCQSHIP)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFSHPD-GENB),AL2(0)                                          
                                                                                
         DC    AL1(2,56,9,1,0,0)                    'DUE DATE'                  
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@DUEDT),AL2(DD@DUEDT-DICD)                               
         DC    AL2(IFDD-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,65,9,1,0,0)                    DUE DATE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDD)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFDD-GENB),AL2(0)                                            
                                                                                
         DC    AL1(3,2,8,1,0,0)                     'CLIENT'                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@CLINT),AL2(DD@CLINT-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,10,8,1,0,0)                    CLIENT CODE                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,17,36,1,0,0)                   CLIENT NAME                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,56,26,5,0,0)                   BILLING ADDRESS             
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBA)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNRETL-GENB),AL2(0)                                         
                                                                                
         DC    AL1(3,56,36,1,0,0)                   RETAILER NAME               
         DC    AL1(0,0)                                                         
         DC    AL1(PCQRTLN)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFRETL-GENB),AL2(0)                                          
                                                                                
         DC    AL1(4,56,26,5,0,0)                   RETAILER ADDRESS            
         DC    AL1(0,0)                                                         
         DC    AL1(PCQRTLA)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFRETL-GENB),AL2(0)                                          
                                                                                
         DC    AL1(4,2,8,1,0,0)                     'PRODUCT'                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@PRO),AL2(DD@PRO-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,10,8,1,0,0)                    PRODUCT CODE                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,17,36,1,0,0)                   PRODUCT NAME                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,2,8,1,0,0)                     'MEDIA'                     
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@MED),AL2(DD@MED-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,10,8,1,0,0)                    MEDIA CODE                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQMC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,17,12,1,0,0)                   MEDIA NAME                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQMN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,30,28,1,0,0)                   ESTIMATE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQEST)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFEST-GENB),AL2(0)                                           
                                                                                
         DC    AL1(6,2,8,1,0,0)                     'JOB'                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@JOB),AL2(DD@JOB-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(6,10,8,1,0,0)                    JOB CODE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(6,17,36,1,0,0)                   JOB NAME                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(7,2,44,1,0,0)                    AUTHORIZATION               
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAUTH)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                    AGENCY JOB                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAGYJ)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,56,50,1,0,0)                   PRINT ON BILLS              
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPONB)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                    USER FIELD 1-8              
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF1)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF2)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF3)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF4)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF5)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF6)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF7)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF8)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPGH2   DS    0H                                  PAGE HEADER                  
         DC    AL1(1,39,17,2,UNDR+CNTR,0)         'PRODUCTION BILL'             
         DC    AL1(0,0)                              OR MEDIA NAME              
         DC    AL1(PCQBMN)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,79,10,1,0,0)                   PAGE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPG)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNSUMR-GENB),AL2(0)                                         
                                                                                
         DC    AL1(3,2,5,1,0,0)                     CLIENT CODE                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,10,36,1,0,0)                   CLIENT NAME                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,61,9,1,0,0)                    'DUE DATE'                  
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@DUEDT),AL2(DD@DUEDT-DICD)                               
         DC    AL2(IFDD-GENB),AL2(0)                                            
                                                                                
         DC    AL1(3,71,9,1,0,0)                    DUE DATE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDD)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFDD-GENB),AL2(0)                                            
                                                                                
         DC    AL1(4,2,8,1,0,0)                    PRODUCT CODE                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFGRPB-GENB),AL2(0)                                          
                                                                                
         DC    AL1(4,10,36,1,0,0)                   PRODUCT NAME                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFGRPB-GENB),AL2(0)                                          
                                                                                
         DC    AL1(4,61,9,1,0,0)                     'BILL DATE'                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@BILDT),AL2(DD@BILDT-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,76,10,1,0,0)                   BILL DATE                   
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBD8)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,2,8,1,0,0)                    JOB CODE                     
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNOGRP-GENB),AL2(0)                                         
                                                                                
         DC    AL1(5,10,36,1,0,0)                   JOB NAME                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNOGRP-GENB),AL2(0)                                         
                                                                                
         DC    AL1(5,61,15,1,0,0)                   'INVOICE NUMBER'            
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@INVC),AL2(DD@INVC-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,77,7,1,0,0)                    BILL NUMBER(SHORT)          
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBNS)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(6,10,50,1,0,FUP)                 PRINT ON BILLS              
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPONB)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(6,61,15,1,0,0)                   MEDIA NAME                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQMN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFF#8-GENB),AL2(0)                                           
                                                                                
         DC    AL1(7,10,26,5,0,FUP)                 BILLING ADDRESS             
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBA)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNRETL-GENB),AL2(0)                                         
                                                                                
         DC    AL1(11,2,85,1,0,0)                    -------------              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPGH3   DS    0X                                   PAGE HEADER                 
         DC    AL1(1,2,9,1,0,0)                     'BILL DATE'                 
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@BILDT),AL2(DD@BILDT-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,12,10,1,0,0)                   BILL DATE                   
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBD)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,32,17,2,UNDR+CNTR,0)           'PRODUCTION BILL'           
         DC    AL1(0,0)                             OR MEDIA NAME               
         DC    AL1(PCQBMN)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,56,13,1,0,0)                   'BILL NUMBER'               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@BLN11),AL2(DD@BLN11-DICD)                               
         DC    AL2(IFPBIL-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,56,07,1,0,0)                   'INVOICE                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@INV),AL2(DD@INV-DICD)                                   
         DC    AL2(IFPINV-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,68,9,1,0,0)                    BILL NUMBER                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,79,10,1,0,0)                   PAGE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPG)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,9,1,0,0)                    'SHIP DATE'                  
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@SHIP),AL2(DD@SHIP-DICD)                                 
         DC    AL2(IFSHPD-GENB),AL2(0)                                          
                                                                                
         DC    AL1(2,12,9,1,0,0)                    SHIP DATE                   
         DC    AL1(0,0)                                                         
         DC    AL1(PCQSHIP)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFSHPD-GENB),AL2(0)                                          
                                                                                
         DC    AL1(2,56,9,1,0,0)                    'DUE DATE'                  
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@DUEDT),AL2(DD@DUEDT-DICD)                               
         DC    AL2(IFDD-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,65,9,1,0,0)                    DUE DATE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDD)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFDD-GENB),AL2(0)                                            
                                                                                
         DC    AL1(3,2,8,1,0,0)                     'CLIENT'                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@CLINT),AL2(DD@CLINT-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,10,8,1,0,0)                    CLIENT CODE                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,17,36,1,0,0)                   CLIENT NAME                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(3,56,26,5,0,0)                   BILLING ADDRESS             
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBA)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNRETL-GENB),AL2(0)                                         
                                                                                
         DC    AL1(3,56,36,1,0,0)                   RETAILER NAME               
         DC    AL1(0,0)                                                         
         DC    AL1(PCQRTLN)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFRETL-GENB),AL2(0)                                          
                                                                                
         DC    AL1(4,56,26,5,0,0)                   RETAILER ADDRESS            
         DC    AL1(0,0)                                                         
         DC    AL1(PCQRTLA)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFRETL-GENB),AL2(0)                                          
                                                                                
         DC    AL1(4,2,8,1,0,0)                     'PRODUCT'                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@PRO),AL2(DD@PRO-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,10,8,1,0,0)                    PRODUCT CODE                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,17,36,1,0,0)                   PRODUCT NAME                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,2,8,1,0,0)                     'MEDIA'                     
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@MED),AL2(DD@MED-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,10,8,1,0,0)                    MEDIA CODE                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQMC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,17,12,1,0,0)                   MEDIA NAME                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQMN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(5,30,28,1,0,0)                   ESTIMATE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQEST)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFEST-GENB),AL2(0)                                           
                                                                                
         DC    AL1(6,2,8,1,0,0)                     'JOB'                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@JOB),AL2(DD@JOB-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(6,10,8,1,0,0)                    JOB CODE                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(6,17,36,1,0,0)                   JOB NAME                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(7,2,50,1,0,0)                    PRINT ON BILLS              
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPONB)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(7,2,44,1,0,0)                    AUTHORIZATION               
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAUTH)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                    AGENCY JOB                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAGYJ)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                    USER FIELD 1-8              
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF1)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF2)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF3)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF4)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF5)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF6)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF7)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(8,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF8)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(9,2,44,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF9)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(10,2,44,1,0,0)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(PCQUF10)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPOBH1  DC    AL1(1,6,50,1,0,0)                    PRINT ON BILLS(JOB)         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPNBJ)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPREH1  DS    0X                                   PARAGRAPH                   
         DC    AL1(1,2,42,1,0,0)                    PREBILL NAME                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPREBN)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,44,20,1,LFT,0)                 DATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTTLDA)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              NET                         
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPROH1  DC    AL1(1,2,5,1,UNDR,0)                  PRODUCT CODE                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,7,36,1,0,UNDR2)                PRODUCT NAME                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPROT1  DS    0X                                                               
         DC    AL1(1,2,12,1,0,0)                    TOTAL FOR                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TFOR),AL2(DD@TFOR-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,14,5,1,LFT,0)                  PRODUCT CODE                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPC)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,20,36,1,LFT,0)                 PRODUCT NAME                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBNET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPROT2  DS    0X                                                               
         DC    AL1(1,16,12,1,0,0)                   TOTAL FOR                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TFOR),AL2(DD@TFOR-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,28,36,1,LFT,0)                 PRODUCT NAME                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBNET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                   PREVIOUS BILLS              
*                                                   NOT A TOTAL BILL            
PDPRVB1  DS    0X                                                               
         DC    AL1(2,2,20,1,UNDR,0)                 'PREVIOUS BILLS'            
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@PRVBL),AL2(DD@PRVBL-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,2,15,1,0,0)                    BILL TYPE                   
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBTYP)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCPRO-GENB),AL2(0)             PROG OR ONE-LINE            
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,REV)            NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCTOT-GENB),AL2(0)             TOTAL/%                     
                                                                                
         DC    AL1(1,54,15,1,RTNUFT,0)              COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCPRO-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,54,15,1,RTNUFT,REV)            COMM REVERSED               
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCTOT-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,0)              GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPROG-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,REV)            GROSS REVERSED              
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFTOTPE-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,2,8,1,0,0)                     DATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBDAT)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,13,6,1,0,0)                    NUMBER                      
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPBREF)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,25,1,0,0)                    'TOTAL P.B.'                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TPRVB),AL2(DD@TPRVB-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,38,15,1,RTNUFT,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTPNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCPRO-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,38,15,1,RTNUFT,0)              NET REVERSED                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTPNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCTOT-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,54,15,1,RTNUFT,0)              COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTPCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCPRO-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,54,15,1,RTNUFT,0)              COMM REVERSE                
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTPCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNCTOT-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,0)              GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTPGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPROG-GENB),AL2(0)                                          
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,0)              GROSS REVERSED              
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTPGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFTOTPE-GENB),AL2(0)                                         
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPSFH1  DC    AL1(1,2,42,1,0,0)                    PROFESSIONAL SERV           
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPROFS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,44,20,1,LFT,0)                 DATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTTLDA)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,50,15,1,RGTNUM,0)              PSF                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCPSF)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPSFS1  DS    0X                                                               
         DC    AL1(1,6,42,1,0,0)                    PSF                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPROFS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY)            PSF NET                     
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGPSF)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPSFT1  DS    0X                                   TOTAL PSF                   
         DC    AL1(1,78,6,1,0,0)                    --------                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,10,10,1,0,0)                   TOTAL FOR                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TFOR),AL2(DD@TFOR-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,21,13,1,LFT,0)                 PSF                         
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@PROFS),AL2(DD@PROFS-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,34,10,1,LFT,0)                 SERVICES                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@SERVE),AL2(DD@SERVE-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,70,15,1,RGTNUM,CRY)            NET                         
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPSFT2  DS    0X                                   TOTAL PSF                   
         DC    AL1(1,78,6,1,0,0)                    --------                    
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,70,15,1,RGTNUM,CRY)            NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGPSF)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPSF1   DC    AL1(1,6,42,1,UNDR,0)                 PROFESSIONAL SERV           
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPROFS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,49,20,1,LFT,0)                 DATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTTLDA)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPSF2   DC    AL1(1,2,42,1,0,0)                    PROFESSIONAL SERV           
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPROFS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,46,20,1,LFT,0)                 DATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTTLDA)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPSF3   DS    0X                                   PARAGRAPH                   
         DC    AL1(1,2,42,1,0,0)                    PROFESSIONAL SERV           
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPROFS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,44,20,1,LFT,0)                 DATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTTLDA)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGPSF)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDPSTT1  DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,17,1,0,0)                    'PST'                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXTYP)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)                                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJTPST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFA21-GENB),AL2(0)                                           
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)            PST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGPST)                         GROUP LEVEL                 
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFA27-GENB),AL2(0)                                           
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDSHR1   DS    0X                                   SHARE                       
         DC    AL1(2,2,13,1,0,0)                    'YOUR SHARE'                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@YSHRE),AL2(DD@YSHRE-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,17,9,1,LFT,0)                  NN.0000%                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQSHPCT)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,27,15,1,LFT,0)                 (NN.NN UNITS)               
         DC    AL1(0,0)                                                         
         DC    AL1(PCQSHUNT)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,38,15,1,RTNUFT,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQSHNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,54,15,1,RTNUFT,0)              COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQSHCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,0)              GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQSHGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDSUB1   DS    0X                                   SUB-TOTAL                   
         DC    AL1(1,2,12,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@SUBT),AL2(DD@SUBT-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY+PZA+PDOL)   GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGTOT)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDSUB2   DS    0X                                   SUB-TOTAL - OOP             
         DC    AL1(1,2,12,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@SUBT),AL2(DD@SUBT-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY)            OOP NET                     
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGNET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDSUB3   DS    0X                                   SUBTOTAL                    
         DC    AL1(1,2,12,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@SUBT),AL2(DD@SUBT-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY)            NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGNET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDSUB4   DS    0X                                   FINAL SUB-TOTAL             
         DC    AL1(1,2,12,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@SUBT),AL2(DD@SUBT-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)            GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDURCV)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNOGRP-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)            GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGRCV)                         GROUP LEVEL                 
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFGRP-GENB),AL2(0)                                           
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTAD1   DS    0X                                   NO   TAX                    
         DC    AL1(1,2,40,1,0,0)                    'TOTAL AMOUNT DUE'          
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAMDU)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,PDOL)           NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,51,5,1,0,0)                    NIL                         
         DC    AL1(0,0)                             NET COLUMN                  
         DC    AL1(0)                                                           
         DC    AL1(L'DD@NIL),AL2(DD@NIL-DICD)                                   
         DC    AL2(IFPYNETZ-GENB),AL2(0)                                        
                                                                                
         DC    AL1(1,54,15,1,RTNUFT,PDOL)           COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY+PDOL)       GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDURCV)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,82,5,1,0,0)                    NIL                         
         DC    AL1(0,0)                             GROSS COLUMN                
         DC    AL1(0)                                                           
         DC    AL1(L'DD@NIL),AL2(DD@NIL-DICD)                                   
         DC    AL2(IFPYGRSZ-GENB),AL2(0)                                        
                                                                                
*        DC    AL1(1,87,3,1,0,0)                    USD                         
*        DC    AL1(0,0)                             GROSS COLUMN                
*        DC    AL1(0)                                                           
*        DC    AL1(L'DD@USD),AL2(DD@USD-DICD)                                   
*        DC    AL2(IFPUSDG-GENB),AL2(0)                                         
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTAD2   DS    0X                                   WITH TAX                    
         DC    AL1(1,2,17,1,0,0)                    'AMOUNT BEFORE TAX'         
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@ABTAX),AL2(DD@ABTAX-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,PDOL)            NET                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RTNUFT,PDOL)           COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY+PDOL)       GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,17,1,0,0)                    'GST'                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@VAT1),AL2(DD@VAT1-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,38,15,1,RTNUFT,CRY)            GST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUGST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,CRY)            GST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUGST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTAD3   DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,17,1,0,0)                    'PST'                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXTYP)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,0)              PST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUPST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)                                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUPST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTAD4   DC    AL1(1,2,20,1,0,0)                    'TOTAL AMT DUE'             
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TAMTD),AL2(DD@TAMTD-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,PDOL)           NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUNTX)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RTNUFT,PDOL)           COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDUCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,PDOL)           GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQDURCV)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTAXA1  DS    0X                                   TAX SUMMARIES               
         DC    AL1(1,51,17,2,UNDR+FTX,0)            TAX ANALYSIS                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TAXAN),AL2(DD@TAXAN-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,25,5,2,UNDR+FTX,0)             TAX                         
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TAX),AL2(DD@TAX-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,31,13,2,UNDR+FTX+RGHT,0)       ACCOUNT #                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@ACC#),AL2(DD@ACC#-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,46,12,2,UNDR+FTX+RGHT,0)       BASIS                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@BASIS),AL2(DD@BASIS-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,61,7,2,UNDR+FTX+RGHT,0)        RATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@RATE),AL2(DD@RATE-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,74,10,1,UNDR+FTX+RGHT,0)       AMOUNT                      
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@AMT),AL2(DD@AMT-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,25,3,1,FTX,0)                  TYPE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXTYP)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,34,12,1,FTX,0)                 ACCOUNT                     
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXREG)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPROB-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,49,10,1,RTNUFT,0)              BASIS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXBAS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,63,8,1,RTNUFT,0)               RATE                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXRAT)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,72,13,1,RTNUFT,0)              AMOUNT OF TAX               
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXAMT)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,25,61,1,0,0)                   --------------              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTAXT1  DS    0X                                   TOTAL TAX                   
         DC    AL1(1,2,17,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TTAX),AL2(DD@TTAX-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)                                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGTAX)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTOTMC  DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,25,1,0,0)                    TOTAL MEDIA CHARGES         
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TMEDC),AL2(DD@TMEDC-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)              NET                         
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RGTNUM,0)              COMMISSION                  
         DC    AL1(PCQCOM,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              GROSS                       
         DC    AL1(PCQGRS,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTOTPC  DS    0X                          TOTAL PRODUCTION CHARGES             
         DC    AL1(1,2,25,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TPRCG),AL2(DD@TPRCG-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)              NET                         
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RGTNUM,0)              COMMISSION                  
         DC    AL1(PCQCOM,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              GROSS                       
         DC    AL1(PCQGRS,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTOTS1  DS    0X                                   TOTAL                       
         DC    AL1(1,2,16,1,0,0)                                                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TOTAL),AL2(DD@TOTAL-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,2,16,1,0,0)                    OR TOTAL USD                
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TOTAU),AL2(DD@TOTAU-DICD)                               
         DC    AL2(IFPUSD-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGNET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTOTS2  DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,15,1,0,0)                    'TOTAL'                     
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TOTAL),AL2(DD@TOTAL-DICD)                               
         DC    AL2(IFRETL-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,2,15,1,0,0)                    'TOTAL CHARGES'             
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TCRGS),AL2(DD@TCRGS-DICD)                               
         DC    AL2(IFNRETL-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCTNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RGTNUM,0)              COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCTCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCTGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTOTS3  DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,15,1,0,0)                    'TOTAL'                     
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TOTAL),AL2(DD@TOTAL-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)              NET                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCTNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RGTNUM,0)              COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCTCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)              GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCTGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,83,1,0,0)                    -------------               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTOTS4  DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,17,1,0,0)                    'AMOUNT BEFORE TAX'         
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@ABTAX),AL2(DD@ABTAX-DICD)                               
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,0)               NET                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBXNET)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RTNUFT,0)              COMMISSION                  
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBXCOM)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)            GROSS                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQBXGRS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,2,17,1,0,0)                    'GST'                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@VAT1),AL2(DD@VAT1-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,38,15,1,RTNUFT,0)               GST                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJTGST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(2,70,15,1,RTNUFT,CRY)            GST                         
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJTGST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTOTS5  DS    0X                                   SECTION TOTAL               
         DC    AL1(1,2,17,1,0,0)                    'PST'                       
         DC    AL1(0,0)                                                         
         DC    AL1(PCQTXTYP)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RTNUFT,0)               PST                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJTPST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RTNUFT,CRY)                                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQJTPST)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTPW1   DS    0X                                 TIME WC/PERSON                
         DC    AL1(0,0,0,0,0,0)                   WORKCODE DESCRIPTION          
         DC    AL1(PCQWCD,1)                     (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,8,36,1,0,0)                  PERSON                        
         DC    AL1(PCQVNDN,NXTQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,47,21,1,0,0)                 HOURS @                       
         DC    AL1(PCQH@R,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFTMD-GENB),AL2(0)                                           
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)            NET                           
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTPW2   DS    0X                                TIME BY WC, NO PERSON          
         DC    AL1(1,6,15,1,0,0)                 WORKCODE DESCRIPTION           
         DC    AL1(PCQWCD,1)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCDS-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,6,36,1,0,0)                 WORKCODE LONGNAME              
         DC    AL1(PCQWCN,1)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCLN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,47,21,1,0,0)                HOURS @                        
         DC    AL1(PCQH@R,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFTMD-GENB),AL2(0)                                           
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTPW3   DS    0X                                TIME BY PERSON                 
         DC    AL1(1,8,36,1,0,0)                 PERSON                         
         DC    AL1(PCQVNDN,1)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,47,21,1,0,0)                HOURS @                        
         DC    AL1(PCQH@R,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFTMD-GENB),AL2(0)                                           
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDTPW4   DS    0X                                TIME - NO DETAIL               
         DC    AL1(1,6,42,1,0,0)                 PSF                            
         DC    AL1(0,0)                                                         
         DC    AL1(PCQPROFS)                                                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,CRY)         PSF NET                        
         DC    AL1(0,0)                                                         
         DC    AL1(PCQGPSF)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDUPR1   DS    0X                                UPPER                          
         DC    AL1(1,34,33,1,0,0)                ORIGIN NAME                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFORGN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(2,34,33,1,0,0)                ORIGIN ADDRESS                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAA)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFORGA-GENB),AL2(0)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDUPR2   DS    0X                                UPPER                          
         DC    AL1(1,25,33,1,0,0)                ORIGIN NAME                    
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAN)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFORGN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(2,25,33,1,0,0)                ORIGIN ADDRESS                 
         DC    AL1(0,0)                                                         
         DC    AL1(PCQAA)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFORGA-GENB),AL2(0)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDVCN1   DS    0X                                                               
         DC    AL1(1,2,50,1,0,0)                 VENDOR CODE & NAME             
         DC    AL1(PCQVNDCN,1)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDVCN2   DS    0X                                                               
         DC    AL1(1,6,12,1,0,0)                 SUPPLIER CODE                  
         DC    AL1(PCQVNDC,1)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFCODE-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,19,36,1,0,0)                SUPPLIER NAME&CODE             
         DC    AL1(PCQVNDN,NXTQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNAC-GENB),AL2(0)                                           
                                                                                
         DC    AL1(1,6,36,1,0,0)                 SUPPLIER NAME ONLY             
         DC    AL1(PCQVNDN,NXTQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNONLY-GENB),AL2(0)                                         
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDVNP1   DS    0X                                                               
         DC    AL1(0,0,0,0,0,0)                  SUPPLIER CODE                  
         DC    AL1(PCQVNDC,1)                    (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  SUPPLIER NAME                  
         DC    AL1(PCQVNDN,2)                    (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  WORKCODE                       
         DC    AL1(PCQWC,3)                      (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,9,40,1,0,0)                 WORKCODE & NAME                
         DC    AL1(PCQWRKCN,NXTQ)                                               
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,50,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDVNS1   DS    0X                                SUB-TOTAL                      
         DC    AL1(0,0,0,0,0,0)                  SUPPLIER CODE                  
         DC    AL1(PCQVNDC,1)                     (SORT)                        
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  SUPPLIER NAME                  
         DC    AL1(PCQVNDN,2)                    (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,58,6,1,0,0)                 --------                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,50,15,1,RGTNUM,CRY)         NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDVNS2   DS    0X                                SUB-TOTAL                      
         DC    AL1(0,0,0,0,0,0)                  SUPPLIER CODE                  
         DC    AL1(PCQVNDC,1)                    (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  SUPPLIER NAME                  
         DC    AL1(PCQVNDN,2)                    (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,58,6,1,0,0)                 --------                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCG1   DS    0X                                                               
         DC    AL1(0,0,0,0,0,0)                  WORKCODE GROUP                 
         DC    AL1(PCQWCG,1)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,2,50,1,0,0)                 WORKCODE GROUP NAME            
         DC    AL1(PCQWCGN,2)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCH1   DS    0X                                WORKCODE HEADER                
         DC    AL1(1,2,2,1,UNDR,0)               WORKCODE                       
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,5,15,1,0,UNDR2)             WC DESCRIPTION                 
         DC    AL1(PCQWCD,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCDS-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,5,36,1,0,UNDR2)             WC LONG NAME                   
         DC    AL1(PCQWCN,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCLN-GENB),AL2(0)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCH2   DS    0X                                SECTION PARAGRAPH              
         DC    AL1(1,2,2,1,0,0)                  WORKCODE                       
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,5,15,1,0,0)                 WC DESCRIPTION                 
         DC    AL1(PCQWCD,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCDS-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,5,36,1,0,0)                 WC LONG NAME                   
         DC    AL1(PCQWCN,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCLN-GENB),AL2(0)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                        **A27 - NO UNDERLINE **                
PDWCH3   DS    0X                                WORKCODE HEADER                
         DC    AL1(1,2,2,1,0,0)                  WORKCODE                       
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,5,15,1,0,0)                 WC DESCRIPTION                 
         DC    AL1(PCQWCD,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCDS-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,5,36,1,0,0)                 WC LONG NAME                   
         DC    AL1(PCQWCN,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCLN-GENB),AL2(0)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCH4   DS    0X                                                               
         DC    AL1(1,2,12,1,0,0)                 WORKCODE                       
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,5,36,1,0,0)                 W/C NAME NAME                  
         DC    AL1(PCQWCN,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCH5   DS    0X                                                               
         DC    AL1(0,0,0,0,0,0)                  WORKCODE(SORT)                 
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,6,40,1,0,0)                 WORKCODE & NAME                
         DC    AL1(PCQWRKCN,NXTQ)                                               
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCH6   DS    0X                                WORKCODE HEADER                
         DC    AL1(1,6,15,1,0,0)                 WORKCODE DESCRIPTION           
         DC    AL1(PCQWCD,1)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCDS-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,6,36,1,0,0)                 WORKCODE LONG NAME             
         DC    AL1(PCQWCN,1)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCLN-GENB),AL2(0)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCP1   DS    0X                                PARAGRAPH                      
         DC    AL1(0,0,0,0,0,0)                  WORKCODE(SORT)                 
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  WC DESC. (SORT)                
         DC    AL1(PCQWCD,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCDS-GENB),AL2(0)                                          
                                                                                
         DC    AL1(0,0,0,0,0,0)                  WC LONG NAME(SORT)             
         DC    AL1(PCQWCN,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCLN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(0,0,0,0,0,0)                  CONTRA(SORT)                   
         DC    AL1(PCQCNT,3)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  DATE(SORT)                     
         DC    AL1(PCQDATE,4)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,2,15,1,0,0)                 REFERENCE                      
         DC    AL1(PCQREF,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPVIN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(0,0,0,0,0,0)                  REFERERENCE(SORT)              
         DC    AL1(PCQREF,5)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  SUB-REF(SORT)                  
         DC    AL1(PCQSBR,6)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,2,20,1,0,0)                 LONG INVOICE                   
         DC    AL1(PCQLIN,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPLIN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,24,15,3,0,0)                SUPPLIER NAME                  
         DC    AL1(PCQVNDN,NXTQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFVN-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,38,4,1,0,0)                 NEW ITEM TXFLG '*'             
         DC    AL1(PCQNIF,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFTOTB-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,39,4,1,CDO,0)               *CD*                           
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCDF)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,CDO,0)                CD$                            
         DC    AL1(PCQCD,0)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RGTNUM,0)           COMMISSION                     
         DC    AL1(PCQCOM,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           GROSS                          
         DC    AL1(PCQGRS,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,2,21,1,0,0)                 HOURS @                        
         DC    AL1(PCQH@R,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNPVIN-GENB),AL2(0)                                         
                                                                                
         DC    AL1(2,2,21,1,0,0)                 HOURS @                        
         DC    AL1(PCQH@R,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFPVIN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(3,2,21,8,0,FUP)               NARRATIVE                      
         DC    AL1(PCQNARR,NXTQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(4,2,40,8,0,0)                 TRANSFER TO                    
         DC    AL1(PCQXFR,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFXFR-GENB),AL2(0)                                           
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCP2   DS    0X                                PARAGRAPH                      
         DC    AL1(1,2,2,1,0,0)                  WORKCODE(SORT)                 
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,5,15,1,0,0)                 WC DESC. (SORT)                
         DC    AL1(PCQWCD,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCDS-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,5,36,1,0,0)                 WC LONG NAME(SORT)             
         DC    AL1(PCQWCN,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCLN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RGTNUM,0)           COMMISSION                     
         DC    AL1(PCQCOM,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           GROSS                          
         DC    AL1(PCQGRS,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCP3   DS    0X                                PARAGRAPH                      
         DC    AL1(0,0,0,0,0,0)                  SEQUENCE BY EST                
         DC    AL1(PCQSBR,1)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,2,2,1,0,0)                  WORKCODE(SORT)                 
         DC    AL1(PCQWC,NXTQ)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFESWCB-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,5,15,1,0,0)                 WC DESC.                       
         DC    AL1(PCQWCD,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFESWC1-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,5,36,1,0,0)                 WC LONG NAME                   
         DC    AL1(PCQWCN,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFESWC2-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,2,15,1,0,0)                 WC DESC. - ONLY                
         DC    AL1(PCQWCD,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFESWC3-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,2,36,1,0,0)                 WC LONG - ONLY                 
         DC    AL1(PCQWCN,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFESWC4-GENB),AL2(0)                                         
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RGTNUM,0)           COMMISSION                     
         DC    AL1(PCQCOM,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           GROSS                          
         DC    AL1(PCQGRS,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCP4   DS    0X                                PARAGRAPH                      
         DC    AL1(1,3,2,1,0,0)                  WORKCODE(SORT)                 
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,3,19,1,0,0)                 'DOCUMENT NUMBER'              
         DC    AL1(0,2)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@DOCC),AL2(DD@DOCC-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,23,1,1,LFT0,0)              =                              
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISEQUQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,23,6,1,LFT0,0)              REFERERENCE                    
         DC    AL1(PCQREF,3)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           GROSS                          
         DC    AL1(PCQGRS,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCP5   DS    0X                                PARAGRAPH                      
         DC    AL1(0,0,0,0,0,0)                  WORKCODE(SORT)                 
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,9,40,1,0,0)                 WC & NAME                      
         DC    AL1(PCQWRKCN,NXTQ)                                               
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,50,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCP6   DS    0X                                PARAGRAPH                      
         DC    AL1(0,0,0,0,0,0)                  WORKCODE CODE                  
         DC    AL1(PCQWC,1)                      (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  WORKCODE NAME                  
         DC    AL1(PCQWCN,2)                     (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  VENDOR CODE                    
         DC    AL1(PCQVNDC,3)                    (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,9,50,1,0,0)                 VENDOR CODE & NAME             
         DC    AL1(PCQVNDCN,NXTQ)                                               
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,50,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCP7   DS    0X                                PARAGRAPH                      
         DC    AL1(0,0,0,0,0,0)                  WORKCODE CODE                  
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,6,15,1,0,0)                 WORKCODE NAME                  
         DC    AL1(PCQWCN,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,22,36,1,0,0)                SUPPLIER NAME                  
         DC    AL1(PCQVNDN,3)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNAME-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCP8   DS    0X                                PARAGRAPH                      
         DC    AL1(0,0,0,0,0,0)                  WORKCODE                       
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,16,36,1,0,0)                WORKCODE NAME                  
         DC    AL1(PCQWCN,2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,50,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCS1   DS    0X                                SUB-TOTAL                      
         DC    AL1(1,2,19,1,0,0)                 'TOTAL FOR WC'                 
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TWC),AL2(DD@TWC-DICD)                                   
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,21,2,1,LFT,0)               WC                             
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,26,4,1,CDO,0)               CD=                            
         DC    AL1(0,0)                                                         
         DC    AL1(PCQCDE)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFCDWC-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,29,7,1,CDO+LFT0,0)          CD$                            
         DC    AL1(PCQCD,0)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFCDWC-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,54,15,1,RGTNUM,0)           COMMISSION                     
         DC    AL1(PCQCOM,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           GROSS                          
         DC    AL1(PCQGRS,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCS2   DS    0X                                SUB-TOTAL                      
         DC    AL1(1,2,12,1,0,0)                 'TOTAL FOR '                   
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(L'DD@TFOR),AL2(DD@TFOR-DICD)                                 
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,15,2,1,LFT,0)               WC                             
         DC    AL1(PCQWC,1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,19,20,1,LFT,0)              WC DESC.                       
         DC    AL1(PCQWCD,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCDS-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,19,36,1,LFT,0)              WC LONG NAME                   
         DC    AL1(PCQWCN,NXTQ)                                                 
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFWCLN-GENB),AL2(0)                                          
                                                                                
         DC    AL1(1,38,15,1,RGTNUM,0)           NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(IFNC-GENB),AL2(0)                                            
                                                                                
         DC    AL1(1,70,15,1,RGTNUM,0)           GROSS                          
         DC    AL1(PCQGRS,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCS3   DS    0X                                SUB-TOTAL                      
         DC    AL1(0,0,0,0,0,0)                  WORKCODE                       
         DC    AL1(PCQWC,1)                      (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  WORKCODE NAME                  
         DC    AL1(PCQWCN,2)                     (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,58,6,1,0,0)                 --------                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(2,50,15,1,RGTNUM,CRY)         NET                            
         DC    AL1(PCQNET,0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
PDWCS4   DS    0X                                SUB-TOTAL                      
         DC    AL1(0,0,0,0,0,0)                  WORKCODE                       
         DC    AL1(PCQWC,1)                      (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(0,0,0,0,0,0)                  WORKCODE NAME                  
         DC    AL1(PCQWCN,2)                     (SORT)                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
                                                                                
         DC    AL1(1,58,6,1,0,0)                 --------                       
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0),AL1(DISPCQ,DISDSHQ)                                       
         DC    AL2(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* TRANSACTION FILTER TABLES                                          *          
**********************************************************************          
*                                         FILTER DATA TABLE                     
FLSEC1   DS    0X                         FILTER SECTION                        
         DC    AL2(FLTPROD-GENB)          ALL PRODUCTION                        
         DC    AL1(EOT)                                                         
                                                                                
FLSEC2   DS    0X                         FILTER SECTION                        
         DC    AL2(FLTMEDA-GENB)          ALL MEDIA TRANSFER                    
         DC    AL1(EOT)                                                         
                                                                                
FLSEC3   DS    0X                                                               
         DC    AL2(FLTNO-GENB)            ALL OTHERS(CATCHALL)                  
         DC    AL1(EOT)                                                         
                                                                                
FLSEC4   DS    0X                                                               
         DC    AL2(FLTPBIL-GENB)          PREBILL                               
         DC    AL2(FLTTIME-GENB)          TIME                                  
         DC    AL1(EOT)                                                         
                                                                                
FLSEC5   DS    0X                                                               
         DC    AL2(FLTOOP-GENB)           OUT-OF-POCKET                         
         DC    AL1(EOT)                                                         
                                                                                
FLSEC7   DS    0X                                                               
         DC    AL2(FLTWAA-GENB)           FILTER WORKCODE AA                    
         DC    AL1(EOT)                                                         
                                                                                
FLSEC8   DS    0X                                                               
         DC    AL2(FLTPBIL-GENB)          PREBILL                               
         DC    AL2(FLTTIME-GENB)          TIME                                  
         DC    AL2(FLTRET-GENB)           RETAINER                              
         DC    AL1(EOT)                                                         
                                                                                
FLTPROD  DC    AL1(FLTPRODX-*),AL1(BSDSPOM)     PRODUCTION CHARGES              
         DC    AL1(BTRNPROD)                                                    
FLTPRODX EQU   *                                                                
                                                                                
FLTMEDA  DC    AL1(FLTMEDAX-*),AL1(BSDSPOM)     MEDIA CHARGES                   
         DC    AL1(BTRNMEDA)                                                    
FLTMEDAX EQU   *                                                                
                                                                                
FLTNO    DC    AL1(FLTNOX-*),AL1(BSDSALL)        NO FILTER                      
FLTNOX   EQU   *                                                                
                                                                                
FLTPBIL  DC    AL1(FLTPBILX-*),AL1(BSDSWCT)       PREBILL                       
         DC    AL1(WRKCTPRE)                                                    
FLTPBILX EQU   *                                                                
                                                                                
FLTTIME  DC    AL1(FLTTIMEX-*),AL1(BSDSWCT)      TIME                           
         DC    AL1(WRKCTTIM)                                                    
FLTTIMEX EQU   *                                                                
                                                                                
FLTMEDI  DC    AL1(FLTMEDIX-*),AL1(BSDSWCT)      MEDIA                          
         DC    AL1(WRKCTMED)                                                    
FLTMEDIX EQU   *                                                                
                                                                                
FLTOOP   DC    AL1(FLTOOPX-*),AL1(BSDSWCT)       OUT-OF-POCKET                  
         DC    AL1(WRKCTOOP)                                                    
FLTOOPX  EQU   *                                                                
                                                                                
FLTRET   DC    AL1(FLTRETX-*),AL1(BSDSWCT)       RETAINER                       
         DC    AL1(WRKCTRET)                                                    
FLTRETX  EQU   *                                                                
                                                                                
FLTWAA   DC    AL1(FLTWAAX-*),AL1(BSDSWC)         WORK CODE AA                  
         DC    CL2'AA'                                                          
FLTWAAX  EQU   *                                                                
         EJECT                                                                  
**********************************************************************          
* FORMAT STORAGE                                                     *          
**********************************************************************          
                                                                                
FWSD     DSECT ,                                                                
                                                                                
ANXTBFM  DS    A                   A(NEXT AREA IN BFM BUFFER)                   
                                                                                
SRTSSEL  DS    XL100               SORT DATA ELEMENT                            
TOTSSEL  DS    XL100               TOTAL DATA ELEMENT                           
                                                                                
SECSEQ   DS    X                   SECTION SEQUENCE                             
PARSEQ   DS    XL2                 PATAGRAPH SEQUENCE                           
                                                                                
                                                                                
AMTFLG   DS    X                                                                
AMTRTE   EQU   X'80'               HAVE A RATE                                  
AMTHRS   EQU   X'40'               HAVE HOURS                                   
                                                                                
DETLFLG  DS    X                                                                
DETLDET  EQU   X'80'               DETAIL BFMELD'S                              
DETLTRL  EQU   X'40'               TRAILER BFMELD'S                             
                                                                                
FRSTIND  DS    X                                                                
FRSTTAX  EQU   X'80'               ALREADY PROCESSED FIRST TAX RECORD           
FRSTPRV  EQU   X'40'               ALREADY PROCESSED FIRST PREV. BILL           
                                                                                
NET      DS    PL8                                                              
HRS      DS    PL8                                                              
RTE      DS    PL8                                                              
CSD      DS    PL8                                                              
                                                                                
PK8      DS    0PL8                                                             
PK16     DS    PL16                                                             
                                                                                
TEXTLEN  DS    X                                                                
TEXT     DS    CL255                                                            
                                                                                
LINNUM   DS    X                                                                
RECNO    DS    X                                                                
                                                                                
         DS    0F                                                               
FAPARMS  DS    0XL8                                                             
FAPINDS  DS    X                   INDICATOR BYTE                               
FAPIPZA  EQU   X'80'               PRINT ZERO AMOUNT                            
FAPINOC  EQU   X'40'               SUPPRESS COMMAS(IGNORE OPTION)               
FAPIDOL  EQU   X'20'               PRINT DOLLAR SIGN                            
FAPABFM  DS    AL3                 A(BFMELD)                                    
FAPINPL  DS    X                   INPUT LENGTH                                 
FAPAAMT  DS    AL3                 A(AMOUNT)                                    
FAPL8    DS    PL8                                                              
                                                                                
SVREG    DS    0F        SAVE RE THRU R7                                        
SVRE     DS    F                                                                
SVRF     DS    F                                                                
SVR0     DS    F                                                                
SVR1     DS    F                                                                
SVR2     DS    F                                                                
SVR3     DS    F                                                                
SVR4     DS    F                                                                
SVR5     DS    F                                                                
SVR6     DS    F                                                                
SVR7     DS    F                                                                
                                                                                
SPAFTR   DS    X                                                                
TXFLG    DS    C                                                                
APRVB    DS    F                                                                
                                                                                
PRVDES   DS    CL(L'CPRODES)                                                    
                                                                                
BTTWRK   DS    XL(BTTLNQ)                                                       
                                                                                
SFWK     DS    XL(SFRLNQ)          SECTION FILTER WORK AREA                     
         ORG   *-1                                                              
SFWKX    DS    X                                                                
                                                                                
SSWK     DS    XL(SSRLNQ)          SECTION SORT WORK AREA                       
         ORG   *-1                                                              
SSWKX    DS    X                                                                
                                                                                
PDWK     DS    XL(PDRLNQ)          PARAGRAPH DETAIL WORK AREA                   
         ORG   *-1                                                              
PDWKX    DS    X                                                                
PDOLD    DS    XL(PDRLNQ)          OLD PARAGRAPH DETAIL WORK AREA               
FWSLNQ   EQU   *-FWSD                                                           
         EJECT                                                                  
***********************************************************************         
* FMTTXT WORKING STORAGE                                              *         
***********************************************************************         
                                                                                
FTWORKD  DSECT ,                                                                
FTAPARMS DS    A                   A(INPUT PARAMETER BLOCK)                     
FTATEXT  DS    A                   A(TEXT)                                      
                                                                                
FTACT    DS    C                   ACTION                                       
FTACTPRT EQU   C'P'                                                             
FTACTSIZ EQU   C'S'                                                             
FTAREC   DS    A                   A(RECORD)                                    
FTAPBLK  DS    A                   A(PRINT BLOCK)                               
                                                                                
FTABFM   DS    A                   A(BFMEL)                                     
FTINDS   DS    X                   INDICATOR BYTE                               
FTIEXSPC EQU   X'80'               EXTRA SPACE ON RHS OF FIELD                  
FTIEXLFT EQU   X'40'               EXTRA SPACE ON LEFT FOR CR=YES               
                                                                                
FTWTHMIN DS    X                   TEXT WIDTH MINIMUM                           
FTWTHMAX DS    X                   TEXT WIDTH MAXIMUM                           
FTHGTMIN DS    X                   TEXT HEIGHT MINIMUM                          
FTHGTMAX DS    X                   TEXT HEIGHT MAXIMUM                          
FTROWS   DS    50XL(ROWL)          TEXT ROWS                                    
FTADATAX DS    A                   A(END OF TEXT OUTUT)                         
FTDATA   DS    2048X               TEXT OUPUT DATA                              
                                                                                
FTWORKL  EQU   *-FTWORKD                                                        
         EJECT                                                                  
***********************************************************************         
* ROW DSECT                                                           *         
***********************************************************************         
                                                                                
ROWD     DSECT ,                                                                
ROWLEN   DS    X                                                                
ROWEOTQ  EQU   FF                                                               
ROWADDR  DS    AL4                                                              
ROWL     EQU   *-ROWD                                                           
                                                                                
***********************************************************************         
* DSECT TO COVER FILTER TABLE                                         *         
***********************************************************************         
                                                                                
FLTTABD  DSECT ,                                                                
FLTTYPE  DS    X                   BSDSTYP VALUE                                
FLTLEN   DS    X                   LENGTH OF DATA ENTRY                         
FLTBAS   DS    X                   BASE REGISTER 0=GLOBAL, 1=TRANS              
FLTDSP   DS    AL2                 DISP. TO DATA                                
FLTTABL  EQU   *-FLTTABD                                                        
                                                                                
***********************************************************************         
* FIELD TABLE                                                         *         
***********************************************************************         
                                                                                
FLDTABD  DSECT ,                                                                
FLDRADR  DS    AL2                 ADDRESS OF ROUTINE                           
         ORG   FLDRADR                                                          
FLDBDSP  DS    S                   BASE/DISPLACEMENT TO DATA                    
                                                                                
FLDGLEN  DS    X                   LENGTH OF DATA                               
FLDCLEN  DS    X                   LENGTH OF CONVERTED DATA                     
FLDCNV   DS    X                   CONVERSION ROUTINE NUMBER                    
FLDCPWOS EQU   1                   CONVERT DATE (PWOS)                          
FLDCAMT  EQU   2                           AMOUNT                               
FLDCHRS  EQU   3                           HOURS                                
FLDCRAT  EQU   4                           RATE                                 
FLDCCMR  EQU   5                           COMMISSION RATE                      
                                                                                
FLDINDS  DS    X                   INDICATOR BYTE                               
FLDISORT EQU   X'80'               RECORD CAN BE SORTED BY THIS TYPE            
FLDIAMT  EQU   X'40'               FIELD IS AN AMOUNT (HELD ON FWTELD)          
FLDITOT  EQU   X'20'               FIELD IS TOTALLED AMOUNT                     
FLDIRMTS EQU   X'10'               REMOVE TRAILING SPACES                       
                                                                                
FLDHOOK  DS    AL4                 A(OUTPUT HOOK ROUTINE)                       
FLDTABL  EQU   *-FLDTABD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER BILL FORMAT RECORD TABLE                             *         
***********************************************************************         
                                                                                
GTD      DSECT ,                   GENERATE TABLE DSECT                         
GTTYP    DS    X                   TYPE                                         
GTEFQ    EQU   1                   ENTRY FILTER                                 
GTLOQ    EQU   2                   DATA LOCATION                                
GTFSQ    EQU   3                   FILTER SECTION                               
GTTXQ    EQU   4                   TEXT PANELS                                  
GTLKQ    EQU   5                   LINK POINTER                                 
GTIFQ    EQU   6                   IF                                           
GTEIQ    EQU   7                   END IF                                       
GTDATA   DS    XL6                                                              
GTLNQ    EQU   *-GTD                                                            
                                                                                
         ORG   GTD                                                              
GTEF     DS    X                   ENTRY FILTERS                                
         ORG   GTEF                                                             
GTIF     DS    X                   IF STATEMENT                                 
GTEFBTY  DS    X                   BILL TYPES                                   
GTEFILV  DS    X                   INVOICE LEVEL                                
GTEFIDQ  EQU   0                           DETAIL                               
GTEFISQ  EQU   1                           SUMMARY                              
GTEFGLV  DS    X                   GROUP LEVEL                                  
         DS    X                   N/D                                          
GTEFCND  DS    AL2                 CONDITIONAL                                  
                                                                                
         ORG   GTD                                                              
GTLO     DS    X                   LOCATION                                     
GTLOWHRB DS    AL2                 WHERE - PAGE HEAD/BODY                       
GTLOWHRS DS    AL2                 WHERE - IN SECTION (BODY ONLY)               
GTLOSECN DS    X                   SECTION NUMBER (BODY ONLY)                   
GTLOSTA  DS    X                                                                
GTLOBF   EQU   X'80'               NEEDS BFPEL                                  
GTLOBT   EQU   X'40'                 "   BTTEL                                  
GTLOBO   EQU   X'20'                 "   BOFFEL                                 
GTLOFT   EQU   GTLOBF+GTLOBT                                                    
                                                                                
         ORG   GTD                                                              
GTFS     DS    X                   FILTER SECTION                               
GTFSCND  DS    AL2                 FILTER CONDITION                             
GTFSNUM  DS    X                   SECTION NUMBER                               
SEC1     EQU   1                                                                
SEC2     EQU   2                                                                
SEC3     EQU   3                                                                
         DS    XL3                 N/D                                          
                                                                                
         ORG   GTD                                                              
GTTX     DS    X                   TEXT                                         
GTTXSTA  DS    X                   STATUS                                       
CONTQ    EQU   X'80'               NEXT ENTRY IS A CONTINUATION                 
GTTXSPA  DS    X                   SPACING                                      
SP1A     EQU   X'80'                SPACE 1 AFTER                               
SP1B     EQU   X'40'                SPACE 1 BEFORE                              
GTTXCND  DS    AL2                 CONDITIONAL                                  
GTTXPNL  DS    AL2                 TEXT PANEL                                   
                                                                                
         ORG   GTD                                                              
GTLK     DS    X                   LINK                                         
GTLINK   DS    AL2                 DISP TO NEXT FILTER ENTRY                    
         DS    XL4                 N/D                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER DATA ITEM TABLE                                      *         
***********************************************************************         
                                                                                
DID      DSECT ,                                                                
DIROW    DS    X                   ROW                                          
DICOL    DS    X                   COLUMN                                       
DIWTH    DS    X                   WIDTH                                        
DIHGT    DS    X                   HEIGHT                                       
DISTA    DS    X                   STATUS                                       
CNTR     EQU   X'80'               ALIGN CENTER                                 
UNDR     EQU   X'40'               UNDERLINE                                    
RGHT     EQU   X'20'               ALIGN RIGHT                                  
NUM      EQU   X'10'               NUMBER                                       
FTX      EQU   X'08'               FREE TEXT                                    
LFT      EQU   X'04'               MOVE FIELD LEFT, 1 SPACE FROM PREV.          
LFT0     EQU   X'02'               MOVE FIELD LEFT TO ATTACH TO PREV.           
CDO      EQU   X'01'               LINKED TO NON-ZERO CD AMOUNTS                
RTNUFT   EQU   RGHT+NUM+FTX                                                     
RGTNUM   EQU   RGHT+NUM                                                         
                                                                                
DISTA2   DS    X                   STATUS - 2                                   
REV      EQU   X'80'               REVERSE SIGN FOR PRINT                       
UNDR2    EQU   X'40'               UNDERLINE GAP BETWEEN UNDERLINES             
CRY      EQU   X'20'               CR=YES                                       
FUP      EQU   X'10'               FLOAT DATA UP TO LAST AVAILABLE ROW          
PZA      EQU   X'08'               PRINT ZERO AMOUNT                            
PDOL     EQU   X'04'               PRINT DOLLAR SIGN                            
PUSD     EQU   X'02'               PRINT USD IF GOUSD=Y                         
                                                                                
DIBLF    DS    X                   BFMFLD CODE                                  
DIBLFSQ  DS    X                   SORT SEQUENCE FOR BFMFLD                     
NXTQ     EQU   50                  NEXT SEQUENCE (ADD TO END)                   
                                                                                
DIPNL    DS    X                   PANEL CODE                                   
                                                                                
DIDICL   DS    X                   LENGTH OF DICTIONARY ITEM                    
DIDIC    DS    XL2                 DISPLACEMENT TO DICTIONARY ITEM              
         ORG   DIDIC                                                            
DISPC    DS    X                   SPECIAL CHARACTER TO FOLLOW                  
DISPCQ   EQU   X'80'                                                            
DISPCH   DS    C                                                                
DISDSHQ  EQU   C'-'                DASHES                                       
DISSPAQ  EQU   C' '                SPACES                                       
DISEQUQ  EQU   C'='                EQUAL                                        
                                                                                
DICNDL   DS    AL2                 CONDITIONAL                                  
         DS    AL2                 N/D                                          
DILNQ    EQU   *-DID                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GEN TABLE ADDRESSES                                  *         
***********************************************************************         
                                                                                
AGT      DSECT ,                                                                
AGTEF    DS    A                   A(ENTRY FILTER)                              
AGTLO    DS    A                   A(LOCATION ENTRY)                            
AGTFTL   DS    A                   A(FILTER/TEXT/LINK)                          
AGTLNQ   EQU   *-AGT                                                            
         EJECT                                                                  
NBILD    DSECT                                                                  
* ACREPNBC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPNBC                                                       
         PRINT ON                                                               
* ACREPNBD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPNBD                                                       
         PRINT ON                                                               
JXBLKD   DSECT OFF                                                              
* ACJAXD                                                                        
       ++INCLUDE ACJAXD                                                         
BLHELD   DSECT ,                   ** Extra bill header fields **               
         ORG   BLHTSEQ+L'BLHTSEQ   ORG to spare                                 
BLHRINDS DS    XL(L'BFTRINDS)      ACREPORT indicators                          
BLHFLAG1 DS    XL(L'BFTFLAG1)      Format flags                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACREPNB03B10/09/14'                                      
         END                                                                    
