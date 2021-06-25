*          DATA SET ACCLB44    AT LEVEL 063 AS OF 08/16/00                      
*PHASE T62144A                                                                  
*&&      SET   NOP=N                                                            
CLB44    TITLE '- BILL PROGRAM UPDATE ROUTINES'                                 
CLB44    CSECT                                                                  
         PRINT NOGEN                                                            
UPDATE   NMOD1 0,**CB44**,RR=R8                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING MIXLST,LSMIXLST                                                  
         USING PBLKD,R7                                                         
         USING POSTVALS,PBPOSTV                                                 
*                                                                               
         SRL   RF,24               RF = A(ROUTINE, SIZE OF LOCAL W/S)           
         SLL   RF,3                                                             
         LA    RF,AROUT(RF)                                                     
         A     R8,0(RF)            R8 = A(ROUTINE)                              
*                                                                               
         ICM   R3,15,4(RF)         TEST FOR W/S REQUIRED                        
         BZ    ROUT                                                             
         BCTR  R3,0                                                             
         SRL   R3,3                ENSURE DOUBLE WORD LENGTH                    
         LA    R3,1(R3)                                                         
         SLL   R3,3                                                             
         L     RF,4(RD)            EXTEND W/S                                   
         AR    RD,R3                                                            
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         LR    R2,RC               CLEAR W/S                                    
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
ROUT     DS    0H                  BRANCH TO ROUTINE                            
         BR    R8                                                               
*                                                                               
         SPACE 1                                                                
AROUT    DS    0A                  ROUTINES / LOCAL W/S                         
         DC    A(POST,PBLKL)                                                    
         DC    A(INIPBLK,0)                                                     
         DC    A(VALBREF,0)                                                     
         DC    A(VALBMOA,VBWORKL)                                               
         DC    A(BLDDAL,0)                                                      
         DC    A(OPNJRN,0)                                                      
         DC    A(CLOJRN,0)                                                      
         DC    A(SETACTN,SAWORKL)                                               
         DC    A(ADDXTRA,0)                                                     
         DC    A(XTRAELS,XEWORKL)                                               
         DC    A(ADDWMDT,0)                                                     
         DC    A(ADDBMDT,0)                                                     
         DC    A(ADDRFL,0)                                                      
         SPACE 1                                                                
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
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   CLB44+X'800'                                                     
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* MAIN POST ROUTINE                                                   *         
*                                                                     *         
* NTRY: P1 = A(PBPARMS SET BY CALLER)                                 *         
***********************************************************************         
         SPACE 1                                                                
POST     DS    0H                                                               
         USING *,R8                                                             
         LR    R7,RC               R7=A(PBLKD)                                  
*                                                                               
         GOTO1 AINIPBLK,(R1)       INITIALIZE PBLKD                             
         CLI   PBMODE,PBTOTQ       TEST ONLY REFRESHING BILL TOTALS             
         BNE   *+8                                                              
         OI    POSTSTA2,POSTNO     TELL BLDTRN NOT TO MAKE POSTINGS             
*                                                                               
         L     R2,ATYPLST          VALIDATE BATCH HEADERS                       
         XC    BCHALF,BCHALF       BCHALF = UPDATED ITEMS COUNT                 
         USING TYPLSTD,R2                                                       
POST02   MVC   BCBYTE1,TYPTYPES                                                 
         NC    BCBYTE1,PBTYPES                                                  
         BZ    POST18                                                           
         LH    R3,TYPPBLK                                                       
         LA    R3,PBLKD(R3)                                                     
         USING PBBATCHD,R3         R3=A(BATCH DETAILS)                          
*                                                                               
         CLI   TYPTYPES,PBBILQ     TEST BILLING                                 
         BNE   POST04              YES - VALIDATE WITHIN BILL OVERLAY           
         L     RE,AOVERWRK         CLEAR OVERLAY W/S                            
         LHI   RF,OVERWRKL                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    RF,O#VALBIL         ACCLB1E                                      
         TM    BCINDS1,BCPCBILL                                                 
         BZ    *+8                                                              
         LA    RF,OC#BEUPD         ACCLB64                                      
         GOTO1 VCOLY,BODMCB,((RF),0),0,0                                        
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
         BNE   EXIT                                                             
         CLI   PBMODE,PBDISQ       DISPLAY MODE - FINISHED                      
         BE    EXITY                                                            
         CLI   PBMODE,PBTOTQ       TEST UPDATING BILL TOTALS ONLY               
         BNE   *+12                                                             
         TM    PBINDS1,PBITUPD     TEST THEY DO NEED UPDATING                   
         BZ    EXITN                                                            
         TM    PBTYPES,PBBILQ      TEST BILLING TURNED OFF                      
         BZ    POST18                                                           
         MVC   PBACURS,PBADRAH                                                  
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   POST14                                                           
         MVC   PBACURS,PBALIVH                                                  
         B     POST14                                                           
*                                                                               
POST04   DS    0H                                                               
         ICM   RF,15,PBAREFH       TEST ANY INPUT TO REF/MOA FIELDS             
         CLI   FHILD(RF),0                                                      
         BNE   POST08                                                           
         ICM   RF,15,PBAMOAH                                                    
         CLI   FHILD(RF),0                                                      
         BNE   POST08                                                           
POST06   XC    PBTYPES,TYPTYPES    NO - TURN OFF BIT                            
         B     POST18                                                           
*                                                                               
POST08   OC    PBACURS,PBACURS                                                  
         BNZ   *+10                                                             
         MVC   PBACURS,PBAREFH                                                  
         GOTO1 AVALBREF,TYPLSTD                                                 
         BNE   POSTN                                                            
         GOTO1 AVALBMOA,(R1)                                                    
         BNE   POSTN                                                            
         ICM   RF,15,PBAREFH                                                    
         BNZ   *+8                                                              
         LA    RF,BASACTH          ONLY PASSED FOR CURSOR POSITION              
         GOTO1 AADDOBH,BCPARM,(TYPBTYP,PBREF),(RF)                              
         BNE   POSTN                                                            
         CLI   TYPBTYP2,0                                                       
         BE    POST14                                                           
         ICM   RF,15,PBAREFH                                                    
         BNZ   *+8                                                              
         LA    RF,BASACTH                                                       
         GOTO1 AADDOBH,BCPARM,(TYPBTYP2,PBREF),(RF)                             
         BNE   POSTN                                                            
*                                                                               
POST14   CLI   PBMODE,PBVALQ       TEST ONLY VALIDATING                         
         BE    POST18                                                           
         XR    RF,RF               TEST ANYTHING PENDING                        
         ICM   RF,3,TYPJCB                                                      
         LA    RF,PBJCBEL(RF)                                                   
         OC    0(L'JCBALL,RF),0(RF)                                             
         BNZ   POST16                                                           
         CLI   TYPTYPES,PBBILQ     TEST BILLING                                 
         BNE   *+12                                                             
         TM    BCINDS1,BCPCBILL    ALLOW TO CONTINUE IF PC BILLING              
         BO    POST16              (MAY BE ADVANCE ONLY BILL)                   
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         MVC   FVADDR,PBAREFH                                                   
         B     POSTN                                                            
*                                                                               
POST16   CLI   TYPTYPES,PBBILQ                                                  
         BE    POST18                                                           
         LH    RE,BCHALF           UPDATE UPDATED ITEM COUNT                    
         AH    RE,0(RF)                                                         
         ST    RE,BCHALF                                                        
         CH    RE,=H'100'          TEST WITHIN MAXIMUM                          
         BNH   POST18                                                           
         MVC   FVMSGNO,=AL2(AE$TMTRN)                                           
         MVC   FVADDR,PBAREFH                                                   
         B     POSTN                                                            
*                                                                               
POST18   LA    R2,TYPLSTL(R2)                                                   
         CLI   TYPLSTD,EOT                                                      
         BNE   POST02                                                           
         DROP  R2,R3                                                            
*                                                                               
         CLI   PBMODE,PBVALQ       FINISHED IF ONLY VALIDATING                  
         BE    POSTY                                                            
*                                                                               
         CLI   PBTYPES,0           RETURN IF NOT DOING ANYTHING                 
         BE    POSTY                                                            
*                                                                               
         GOTO1 ABLDDAL             BUILD DISK ADDRESS LIST                      
         BNE   POSTN                                                            
*                                                                               
         TM    PBTYPES,PBBILQ      TEST BILLING                                 
         BO    POST20              YES - JOURNAL OPENED IN ACCLB1F              
         GOTO1 AOPNJRN             OPEN JOURNAL                                 
         BNE   POSTN                                                            
*                                                                               
POST20   L     R2,ATYPLST          CALL UPDATE ROUTINE(S)                       
         USING TYPLSTD,R2                                                       
POST22   MVC   BCBYTE1,TYPTYPES                                                 
         NC    BCBYTE1,PBTYPES                                                  
         BZ    POST38                                                           
         XR    R3,R3                                                            
         LH    R3,TYPPBLK                                                       
         LA    R3,PBLKD(R3)                                                     
         USING PBBATCHD,R3         R3=A(BATCH DETAILS)                          
         MVC   CSBSECL,PBSECL      SET BATCH SECURITY                           
*                                                                               
         LA    RE,PBXTRA                                                        
         ST    RE,POSTXTRA         A(EXTRA POSTING ELEMENTS)                    
         MVI   0(RE),0                                                          
         LA    RE,PBPTRS                                                        
         ST    RE,POSTPTRS         A(ANALYSIS POINTERS)                         
         MVI   0(RE),FF            INITIALIZE TO BE EMPTY                       
*                                                                               
         L     RE,AOVERWRK         CLEAR OVERLAY W/S                            
         LHI   RF,OVERWRKL                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         XR    RF,RF               CALL OVERLAY TO DRAFT/UPDATE                 
         ICM   RF,8,TYPOLY                                                      
         GOTO1 VCOLY,BODMCB,(RF),0,0                                            
         L     RF,0(R1)                                                         
         GOTO1 (RF),TYPLSTD                                                     
         BE    *+14                                                             
         MVC   FVADDR,PBAREFH                                                   
         B     POSTN                                                            
*                                                                               
         CLI   PBMODE,PBLIVEQ      TEST LIVE                                    
         BNE   POST38                                                           
*                                                                               
         CLI   TYPBTYP2,0          TEST ALTERNATIVE BATCH TYPES                 
         BNE   POST30                                                           
         GOTO1 AADDOBH,BCPARM,(TYPBTYP,PBREF),0                                 
*                                                                               
         CLI   TYPTYPES,PBBILQ     TEST BILLING                                 
         BNE   POST38                                                           
         XR    R0,R0                                                            
         ICM   R0,1,PBMNTHSN       YES - ADD FOR EACH ACCRUAL MONTH             
         BZ    POST38                                                           
         LA    R4,PBMNTHS                                                       
         USING PBMNTHSD,R4                                                      
POST26   CLC   PBMREF(L'PBMREF+L'PBMMP),PBREF                                   
         BE    POST28                                                           
         GOTO1 AADDOBH,BCPARM,(TYPBTYP,PBMREF),0                                
POST28   LA    R4,PBMNTHSL(R4)                                                  
         BCT   R0,POST26                                                        
         B     POST38                                                           
         DROP  R4                                                               
*                                  SPECIAL CODE FOR US W/OS AND RECS.           
POST30   LA    RE,PBIWOFC          ADD BATCH HEADER FOR COST                    
         CLI   TYPTYPES,PBRECQ                                                  
         BNE   *+8                                                              
         LA    RE,PBIRECC                                                       
         EX    RE,*+8                                                           
         BZ    POST34                                                           
         TM    PBINDS1,0                                                        
         GOTO1 AADDOBH,BCPARM,(TYPBTYP,PBREF),0                                 
*                                                                               
POST34   LA    RE,PBIWOFT          ADD BATCH HEADER FOR TIME                    
         CLI   TYPTYPES,PBRECQ                                                  
         BNE   *+8                                                              
         LA    RE,PBIRECT                                                       
         EX    RE,*+8                                                           
         BZ    POST38                                                           
         TM    PBINDS1,0                                                        
         GOTO1 AADDOBH,BCPARM,(TYPBTYP2,PBREF),0                                
*                                                                               
POST38   LA    R2,TYPLSTL(R2)                                                   
         CLI   TYPLSTD,EOT                                                      
         BNE   POST22                                                           
         DROP  R2,R3                                                            
*                                                                               
POST40   GOTO1 ACLOJRN             CLOSE JOURNAL                                
         BNE   POSTN                                                            
*                                                                               
         TM    PBTYPES,PBBILQ      TEST BILLING                                 
         BZ    POST50                                                           
         TM    BCINDS1,BCPCBILL    AND PC BILLING                               
         BZ    POST50                                                           
         BAS   RE,UPDBILL          UPDATE THE BILL                              
         L     RF,ALINK                                                         
         USING LINKD,RF                                                         
         TM    BEWINDS,BEWIUREP    TEST UPDATED REPLICATE BILL                  
         BZ    POST50                                                           
         MVC   BEWHDRDA,BEWREPDA   UPDATE DETAILS TO BE FOR NEW BILL            
         MVC   BEWHDRKY,BEWREPKY                                                
         USING BLHELD,BEWBLH                                                    
         MVC   BLHBILD,BCTODAYC                                                 
         MVC   BLHBLNO,PBLIVE#                                                  
         DROP  RF                                                               
         BAS   RE,UPDBILL          UPDATE NEW REPLICATE BILL                    
*                                                                               
POST50   DS    0H                                                               
         CLI   PBMODE,PBLIVEQ      UPDATE JOB RECORD IF LIVE                    
         BNE   POST60                                                           
         MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,ATYPLST                                                       
         TM    PBTYPES,PBBILQ      TEST UPDATING BILLING                        
         BNO   POST54                                                           
*                                                                               
         LA    R0,GDAELQ           GET LAST BILLED DATE ELEMENT                 
         GOTO1 VHELLO,BCPARM,(C'G',ACCMST),((R0),AIO1),                *        
               (1,=AL1(GDATDLB))                                                
         L     RE,BCPARM+12                                                     
         CLI   BCPARM+12,0                                                      
         BE    POST52                                                           
         LA    RE,BOWORK1          ADD NEW ELEMENT                              
         USING GDAELD,RE                                                        
         XC    GDAEL(GDALNQ),GDAEL                                              
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATDLB                                                  
         GOTO1 VHELLO,BCPARM,(C'P',ACCMST),AIO1,BOWORK1                         
         CLI   BCPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,BCPARM+16                                                     
POST52   MVC   GDADATE,BCTODAYP    SET NEW LAST BILLED DATE                     
         DROP  RE                                                               
*                                                                               
         LA    R0,FFTELQ           GET AUTO BILL NUMBER ELEMENT                 
         GOTO1 VHELLO,BCPARM,(C'G',ACCMST),((R0),AIO1),                *        
               (1,=AL1(FFTTAUTR))                                               
         CLI   BCPARM+12,0         TEST AUTO REVERSE NUMBER FOUND               
         BNE   POST54                                                           
         L     RF,BCPARM+12                                                     
         MVI   0(RF),FF                                                         
         GOTO1 VHELLO,BCPARM,(C'D',ACCMST),('FF',AIO1),0,0                      
         CLI   BCPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TYPLSTD,R2                                                       
POST54   MVC   BCBYTE1,TYPTYPES                                                 
         NC    BCBYTE1,PBTYPES                                                  
         BZ    POST56                                                           
         XR    RF,RF                                                            
         IC    RF,TYPSCIT                                                       
         GOTO1 AUPDJOB,BCPARM,(C'C',(RF)),AIO1                                  
POST56   LA    R2,TYPLSTL(R2)                                                   
         CLI   TYPLSTD,EOT                                                      
         BNE   POST54                                                           
         DROP  R2                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
POST60   DS    0H                                                               
         CLI   PBMODE,PBTOTQ                                                    
         BE    POSTY                                                            
         L     RE,AGOPBLK          PRINTING CORRUPTS GO BLOCK                   
         XC    8(GOADM+8-GOBLOCK,RE),8(RE)                                      
*                                                                               
         TM    PBTYPES,PBBILQ      IF BILLING - PRINT BILL                      
         BZ    POSTY                                                            
         L     RE,AGOPBLK                                                       
         LA    RF,L'GOBLOCK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,AGOPBLK          FOR BILLING EXTENSION                        
         MVC   GOABEXT-GOBLOCKD(L'GOABEXT,RE),AJOBBLK                           
         L     R2,AIO1                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   ACTKACT,BCJOBCOD                                                 
         L     RF,ATIA                                                          
         XC    0(256,RF),0(RF)                                                  
         GOTO1 AGETOPT,BODMCB,AIO1                                              
*                                                                               
         TM    BCINDS1,BCPCBILL    TEST BEDRECD FOUND                           
         BO    POST62                                                           
         L     RE,AOVERWRK         CLEAR OVERLAY W/S                            
         LHI   RF,OVERWRKL                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0               CALL PRINT OVERLAY (ACCLB0B)                 
         GOTO1 VCOLY,BODMCB,('O#BILPRT',0),0,0                                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),BODMCB,PBLKD                                                
         B     POSTY                                                            
*                                                                               
POST62   DS    0H                  PC BILLING PRINTING                          
         ICM   RF,15,ALINK         PASS REPORT ID IF POSSIBLE                   
         BZ    *+10                                                             
         MVC   CSREPID,BEWBLLPQ-LINKD(RF)                                       
         L     RE,AOVERWRK         CLEAR OVERLAY W/S                            
         LHI   RF,OVERWRKL                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0               CALL PRINT OVERLAY (ACCLB66)                 
         GOTO1 VCOLY,BODMCB,('OC#BEPRT',0),0,0                                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),BODMCB,PBLKD                                                
*                                                                               
POSTY    DS    0H                  EXIT WITH CC=EQUAL                           
         MVC   FVADDR,PBACURS                                                   
         ICM   RF,15,PBAPARMS      COPY INTO CALLERS PBPARMS                    
         MVC   0(PBPARMSL,RF),PBPARMS                                           
         B     EXITY                                                            
*                                                                               
POSTN    DS    0H                  EXT WITH CC=NOT EQUAL                        
         ICM   RF,15,PBAPARMS      COPY INTO CALLERS PBPARMS                    
         MVC   0(PBPARMSL,RF),PBPARMS                                           
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   EXITN                                                            
         OI    CSINDSG1,CSINDUNW   UNWIND ANY UPDATES                           
         B     EXITN                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO UPDATE TOTALS/PANELS OF BILL (PC BILLING)                *         
***********************************************************************         
         SPACE 1                                                                
UPDBILL  NTR1  ,                                                                
         TM    PBINDS1,PBITUPD     TEST TOTALS REQUIRE UPDATING                 
         BZ    UBILL02                                                          
         L     RE,AOVERWRK         CLEAR OVERLAY W/S                            
         LHI   RF,OVERWRKL                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0               UPDATE TOTALS (ACCLB65)                      
         GOTO1 VCOLY,BCPARM,('OC#BETOT',0),0,0                                  
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
*                                                                               
UBILL02  DS    0H                                                               
         CLI   PBMODE,PBTOTQ       FINISHED IF JUST UPDATING TOTALS             
         BE    UPDBILLX                                                         
         L     RE,AOVERWRK         CLEAR OVERLAY W/S                            
         LHI   RF,OVERWRKL                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0               UPDATE PANELS (ACCLB62)                      
         GOTO1 VCOLY,BCPARM,('OC#BEBUP',0),0,0                                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),=C'UPDATE'                                             
*                                                                               
UPDBILLX B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PBLKD                                                    *         
*                                                                     *         
* NTRY: P1 = A(PBPARMS SET BY CALLER)                                 *         
***********************************************************************         
         SPACE 1                                                                
INIPBLK  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         L     RF,0(R1)            COPY INPUT PARAMETERS                        
         MVC   PBPARMS,0(RF)                                                    
         STCM  RF,15,PBAPARMS                                                   
         LH    RE,=Y(PBDATAB-PBLKD)                                             
         LA    RE,PBLKD(RE)                                                     
         ST    RE,PBADATAB                                                      
         LH    RE,=Y(PBACTAB-PBLKD)                                             
         LA    RE,PBLKD(RE)                                                     
         ST    RE,PBAACTAB                                                      
*                                                                               
         LA    R0,PBAMTSN          ZEROISE BILL AMOUNTS                         
         LA    RF,PBAMTS                                                        
         ZAP   0(L'PBAMTS,RF),BCPZERO                                           
         LA    RF,L'PBAMTS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         L     R1,AIOA             PROCESS JOB RECORD                           
         LA    R1,ACCORFST(R1)                                                  
         XR    R0,R0                                                            
*                                                                               
         USING ABLELD,R1                                                        
IPBLK02  CLI   ABLEL,ABLELQ        PROCESS BALANCE ELEMENT                      
         BNE   IPBLK04                                                          
         ZAP   PBBLBL,ABLDR                                                     
         SP    PBBLBL,ABLCR                                                     
         B     IPBLK18                                                          
*                                                                               
         USING SCIELD,R1                                                        
IPBLK04  CLI   SCIEL,SCIELQ        PROCESS SUBSIDIARY CASH ELEMENTS             
         BNE   IPBLK16                                                          
         CLI   SCITYPE,SCITCBAP    ALLOCATION                                   
         BNE   IPBLK08                                                          
         AP    PBCBAPN,SCIAMNT                                                  
         AP    PBCBAPC,SCIADMN                                                  
         B     IPBLK18                                                          
IPBLK08  CLI   SCITYPE,SCITCBWP    WRITE-OFFS                                   
         BNE   IPBLK10                                                          
         AP    PBCBWP,SCIAMNT                                                   
         AP    PBCBWPT,SCIAMNT                                                  
         AP    PBCBWP,SCIADMN                                                   
         AP    PBCBWPC,SCIADMN                                                  
         B     IPBLK18                                                          
IPBLK10  CLI   SCITYPE,SCITCBTP    TRANSFERS                                    
         BNE   IPBLK12                                                          
         AP    PBCBTP,SCIAMNT                                                   
         B     IPBLK18                                                          
IPBLK12  CLI   SCITYPE,SCITCBRP    RECOVERIES                                   
         BNE   IPBLK14                                                          
         AP    PBCBRP,SCIAMNT                                                   
         AP    PBCBRPT,SCIAMNT                                                  
         AP    PBCBRP,SCIADMN                                                   
         AP    PBCBRPC,SCIADMN                                                  
         B     IPBLK18                                                          
*                                                                               
IPBLK14  CLI   SCITYPE,SCITCBIP    FEE ADJUSTMENT                               
         BNE   IPBLK18                                                          
         AP    PBCBFP,SCIAMNT                                                   
         B     IPBLK18                                                          
*                                                                               
         USING JCBELD,R1                                                        
IPBLK16  CLI   JCBEL,JCBELQ                                                     
         BNE   IPBLK18                                                          
         MVC   PBJCBEL,JCBELD                                                   
         DROP  R1                                                               
*                                                                               
IPBLK18  IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   IPBLK02                                                          
*                                                                               
INIPBLKX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH REFERENCE                                            *         
*                                                                     *         
* NTRY: R1 = A(TYPLSTD ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
VALBREF  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,TYPPBLK-TYPLSTD(R1)                                         
         LA    R3,PBLKD(R3)                                                     
         USING PBBATCHD,R3         R3=A(BATCH DETAILS)                          
         L     R4,PBAREFH                                                       
         USING REFFLDD,R4          R4=A(FIELD)                                  
*                                                                               
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,REFFLDH                                                    
         BNE   EXITN                                                            
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
VBREF02  CLI   0(R1),C'A'                                                       
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VBREF02                                                       
*                                                                               
         MVC   PBREF,FVIFLD                                                     
         OI    REFFLDH+FHOID,FHOITR                                             
         B     EXITY                                                            
         DROP  R3,R4                                                            
         SPACE 1                                                                
REFFLDD  DSECT                     ** REFERENCE FIELD **                        
REFFLDH  DS    XL8                                                              
REFFLD   DS    CL4                                                              
CLB44    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH MOA                                                  *         
*                                                                     *         
* NTRY: R1 = A(TYPLSTD ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
VALBMOA  DS    0H                                                               
         USING *,R8                                                             
         USING VBWORKD,RC                                                       
         USING BMONVALD,VBMON                                                   
         LR    R2,R1                                                            
         USING TYPLSTD,R2                                                       
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,TYPPBLK                                                     
         LA    R3,PBLKD(R3)                                                     
         USING PBBATCHD,R3         R3=A(BATCH DETAILS)                          
         L     R4,PBAMOAH                                                       
         USING MOAFLDD,R4          R4=A(FIELD)                                  
*                                                                               
         CLI   MOAFLDH+FHILD,0                                                  
         BNE   VBMOA02                                                          
         GOTO1 VDATCON,BCPARM,(2,BCTODAYC),(9,MOAFLD)                           
VBMOA02  GOTO1 AFVAL,MOAFLDH                                                    
         BNE   EXITN                                                            
         GOTO1 VBMONVAL,BCPARM,(FVILEN,FVIFLD),(TYPBTYP,ACOM),         *        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   PBSECL,0(R1)        BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                                                             
         MVC   FVMSGNO,BMOMSG                                                   
         B     EXITN                                                            
*                                                                               
         MVC   VBPWOS(L'BMOMOSP),BMOMOSP                                        
         MVI   VBPWOS+L'VBPWOS-1,X'01'                                          
         MVC   MOAFLD,BCSPACES                                                  
         GOTO1 VDATCON,BCPARM,(1,VBPWOS),(9,MOAFLD)                             
         OI    MOAFLDH+FHOID,FHOITR                                             
         MVC   PBMP,BMOMOSP                                                     
         MVC   PBMC,BMOMOSC                                                     
         MVC   CSBSECL,PBSECL      SET BATCH SECURITY                           
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         SPACE 1                                                                
MOAFLDD  DSECT                     ** MOA FIELD **                              
MOAFLDH  DS    XL8                                                              
MOAFLD   DS    CL6                                                              
         SPACE 1                                                                
VBWORKD  DSECT                     ** VALBMOA LOCAL W/S **                      
*                                                                               
VBMON    DS    XL(BMONVALQ)        BMONVALD BLOCK                               
VBSECL   DS    XL(L'CSBSECL)       SECURITY LEVEL                               
VBPWOS   DS    PL3                 PWOS DATE                                    
*                                                                               
VBWORKL  EQU   *-VBWORKD                                                        
CLB44    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST OF DISK ADDRESSES                             *         
*                                                                     *         
* NTRY: PBTYPES = TYPES TO BE UPDATED                                 *         
* EXIT: UPDATAB = TRNKSTA2/DISK ADDRESS LIST                          *         
***********************************************************************         
         SPACE 1                                                                
BLDDAL   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVI   BCBYTE1,0           BCBYTE1=PENDING MASK                         
         L     RF,ATYPLST                                                       
         USING TYPLSTD,RF                                                       
BDAL02   MVC   BCBYTE2,TYPTYPES                                                 
         NC    BCBYTE2,PBTYPES     TEST TYPE REQUIRED                           
         BZ    *+10                                                             
         OC    BCBYTE1,TYPSTA2                                                  
         LA    RF,TYPLSTL(RF)                                                   
         CLI   TYPLSTD,EOT                                                      
         BNE   BDAL02                                                           
         DROP  RF                                                               
*                                                                               
         LA    R0,PBMAXDA          SET MAXIMUM LOOP COUNT                       
         L     R4,PBADATAB         R4=A(D/A TABLE)                              
*                                                                               
         PUSH  USING                                                            
         USING TRNRECD,IOKEY                                                    
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         LA    R1,IOACCDIR+IOHI+IO1                                             
         B     *+8                                                              
BDAL04   LA    R1,IOACCDIR+IOSQ+IO1                                             
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   BDAL10              CHANGE OF JOB - FINISHED                     
         CLC   TRNKWORK,=C'99'                                                  
         BNL   BDAL10              BILL WORKCODE - FINISHED                     
         CLC   TRNKDATE,BCSPACES                                                
         BE    BDAL04              NOT A TRANSACTION                            
*                                                                               
         MVC   BCBYTE2,TRNKSTA2                                                 
         NI    BCBYTE2,TRNSXFRP+TRNSWOFP+TRNSBILP                               
         TM    TRNKSTAT,TRNSDRFT   TEST IF DRAFT TRANSACTION                    
         BNO   BDAL06                                                           
         CLI   TRNKSTYP,169        TEST IF CBIL ORIGIN BT8                      
         BNE   BDAL06                                                           
         MVI   BCBYTE2,TYPSFEE                                                  
*                                                                               
BDAL06   NC    BCBYTE2,BCBYTE1     TEST INCLUDE IN LIST                         
         BZ    BDAL04                                                           
         MVC   0(L'TRNKDA,R4),TRNKDA                                            
         MVC   L'TRNKDA(L'TRNKSTA2,R4),BCBYTE2                                  
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         BCT   R0,BDAL04                                                        
         MVC   FVMSGNO,=AL2(AE$TMTRN)  TOO MANY TRANSACTIONS TO PROCESS         
         B     EXITN                                                            
*                                                                               
BDAL10   DS    0H                                                               
         TM    BCINDS1,BCPCBILL    TEST PC BILLING                              
         BZ    BDAL20                                                           
         TM    PBTYPES,PBBILQ      TEST BILLING                                 
         BZ    BDAL20                                                           
         USING BEDRECD,IOKEY       CHECK FOR ADVANCES                           
         XC    BEDPAS,BEDPAS       READ PASSIVE TO GET NORMAL KEY               
         MVI   BEDPTYP,BEDPTYPQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVC   BEDPBLNO,PBDRAFT#                                                
         GOTO1 AIO,IOACCDIR+IOHIGH+IO1                                          
         BNE   BDAL20                                                           
         CLC   BEDPAS(BEDPIND-BEDPAS),IOKEYSAV                                  
         BNE   BDAL20                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BNE   BDAL20                                                           
         L     RF,AIO1                                                          
         MVC   BEDKEY,0(RF)                                                     
         GOTO1 AIO,IOACCDIR+IOREAD+IO1                                          
         BNE   BDAL20                                                           
BDAL12   GOTO1 AIO,IOACCDIR+IOSEQ+IO1                                           
         CLC   BEDKEY(BEDKLVL-BEDKEY),IOKEYSAV                                  
         BNE   BDAL20                                                           
         CLC   BEDKLVL,=AL2(BEDKLADV)                                           
         BL    BDAL12                                                           
         BH    BDAL20                                                           
         MVC   0(L'TRNKDA,R4),BEDKDA                                            
         MVI   L'TRNKDA(R4),TRNSBILP+TYPSADV                                    
         LA    R4,L'BEDKDA+L'TRNKSTA2(R4)                                       
         BCT   R0,BDAL12                                                        
         MVC   FVMSGNO,=AL2(AE$TMTRN)  TOO MANY TRANSACTIONS TO PROCESS         
         B     EXITN                                                            
*                                                                               
BDAL20   DS    0H                                                               
         MVI   0(R4),FF            SET E-O-L MARKER                             
         POP   USING                                                            
*                                                                               
         CLI   PBMODE,PBTOTQ                                                    
         BE    BLDDALX                                                          
         L     RF,PBADATAB                                                      
         CLI   0(RF),FF            TEST ANYTHING THERE                          
         BNE   *+6                                                              
         DC    H'0'                JCBEL COUNTER IS WRONG                       
*                                                                               
BLDDALX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO OPEN REPORT AND INITIALIZE ADDRRN FOR JOURNAL            *         
**********************************************************************          
         SPACE 1                                                                
OPNJRN   DS    0H                                                               
         USING *,R8                                                             
         TM    POSTSTA2,POSTNO     TEST POSTINGS REQUIRED                       
         BO    EXITY                                                            
*                                                                               
         GOTO1 AINIADT             INITIALISE ADDTRN BLOCK                      
         L     RF,AADTBLK                                                       
         XC    TRNOFA-ADDTRND(L'TRNOFA,RF),TRNOFA-ADDTRND(RF)                   
         L     R4,AREP             INITIALISE REPORT                            
         USING REPD,R4                                                          
         MVI   REPSUBPG,0          SET FOR DRAFT JOURNAL HEADS                  
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   *+8                                                              
         MVI   REPSUBPG,1          SET FOR LIVE JOURNAL HEADS                   
         MVC   REPRLH,=AL2(48)                                                  
         MVC   REPRDH,=AL2(12)                                                  
*                                                                               
         LH    RE,=Y(LC@LVLST-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         MVC   REPDESC,0(RE)                                                    
         MVC   REPSUBID,=C'BWL'                                                 
         CLI   PBMODE,PBDRAFTQ                                                  
         BNE   OJRN02                                                           
         SH    RE,=Y(LC@LVLST-LC@DFLST)                                         
         MVC   REPSUBID,=C'DWL'                                                 
         MVC   REPDESC,0(RE)                                                    
*                                                                               
OJRN02   DS    0H                                                               
         CLI   CSACT,ACTDWN        TEST FLEXIBILL                               
         BNE   OJRN03                                                           
         OC    CSREPID,CSREPID     TEST PASSED PQ ID                            
         BZ    OJRN03                                                           
         MVC   REPSUBID,CSREPID                                                 
         XC    CSREPID,CSREPID     CLEAR (SO IS NOT KEPT PERMANENTLY)           
*                                                                               
OJRN03   MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
*                                                                               
         CLI   CSACT,ACTDWN        DON'T COPY SCREEN IF PC BILLING              
         BE    OJRN20                                                           
         L     R0,AIO1             CLEAR SPACE FOR HEADER SCREEN FORMAT         
         LH    R1,=Y(TWACOLS*TWAROWS)                                           
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,BASMSGH          FORMAT HEADER SCREEN INTO IO/IO2             
         USING FHD,R1                                                           
         XR    RE,RE                                                            
         ICM   RF,15,PBATWAX                                                    
         BNZ   *+8                                                              
         LA    RF,4095(R1)                                                      
OJRN04   ICM   RE,1,FHLN                                                        
         BZ    OJRN10                                                           
         SR    R2,R2                                                            
         ICM   R2,3,FHAD                                                        
         A     R2,AIO1                                                          
         SH    RE,=Y(L'FVIHDR)                                                  
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         SH    RE,=Y(FHDAD)                                                     
         BCTR  RE,0                                                             
         TM    FHAT,FHATLO                                                      
         BO    *+14                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),FHDA                                                     
         ICM   RE,1,FHLN                                                        
         BXLE  R1,RE,OJRN04                                                     
         DROP  R1                                                               
*                                                                               
OJRN10   L     RF,VREPORT          INITIALISE HEADER PRINT LOOP                 
         LA    R1,REPD                                                          
         L     R2,AIO1                                                          
         MVC   0(L'BASMSG,R2),BCSPACES                                          
         LA    R0,TWAROWS                                                       
*                                                                               
         MVI   REPP1,C'*'          PRINT HEADER SCREEN ON FIRST PAGE            
         MVC   REPP1+1(TWACOLS+1),REPP1                                         
         BASR  RE,RF                                                            
OJRN12   MVI   REPP1,C'*'                                                       
         MVC   REPP1+1(TWACOLS),0(R2)                                           
         MVI   REPP1+1+TWACOLS,C'*'                                             
         BASR  RE,RF                                                            
         LA    R2,TWACOLS(R2)                                                   
         BCT   R0,OJRN12                                                        
         MVI   REPP1,C'*'                                                       
         MVC   REPP1+1(TWACOLS+1),REPP1                                         
         BASR  RE,RF                                                            
*                                                                               
OJRN20   DS    0H                                                               
         LA    RE,JRNSPEC                                                       
         ST    RE,REPAPHS                                                       
         OI    REPHEADI,REPHFRCE                                                
         MVC   REPPAGE,=Y(1)                                                    
*                                                                               
         MVI   POSTMODE,POSTLVQ    SET BLDTRN MODE LIVE                         
         CLI   PBMODE,PBLIVEQ                                                   
         BE    *+8                                                              
         MVI   POSTMODE,POSTDFQ    SET BLDTRN MODE DRAFT                        
*                                                                               
OPNJRNX  B     EXITY                                                            
         SPACE 1                                                                
JRNSPEC  DS    0X                  ** SPECS FOR JOURNAL **                      
         SPROG 0,1                                                              
         SPEC  H1,1,RUN                                                         
         SPEC  H1,73,PAGE                                                       
         SPROG 0                                                                
         SPEC  H1,20,AC#DPOS,40,C                                               
         SPEC  H2,20,AC#DPOS,40,CU                                              
         SPROG 1                                                                
         SPEC  H1,20,AC#APOS,40,C                                               
         SPEC  H2,20,AC#APOS,40,CU                                              
JRNSPECX DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO CLOSE THE JOURNAL                                       *          
**********************************************************************          
         SPACE 1                                                                
CLOJRN   DS    0H                                                               
         USING *,R8                                                             
         TM    POSTSTA2,POSTNO     TEST POSTINGS REQUIRED                       
         BO    EXITY                                                            
*                                                                               
         GOTO1 ABLDTRN,BCPARM,('FF',POSTVALS)                                   
         BNE   EXITN                                                            
         L     R4,AREP                                                          
         USING REPD,R4                                                          
         LH    R6,=Y(BSDICT-TWAD)                                               
         LA    R6,TWAD(R6)                                                      
         USING BSDICT,R6                                                        
         OI    REPHEADI,REPHFRCE   FORCE NEW PAGE FOR SUMMARY                   
         LA    R2,REPP1                                                         
         USING JRNLINED,R2                                                      
         MVC   JRNSUM,PS@PSTGU                                                  
         GOTO1 VREPORT,REPD                                                     
         GOTO1 VREPORT,REPD                                                     
         LA    R2,REPP1                                                         
         MVC   JRNDESC,PS@INCCR                                                 
         LA    R3,PBMNTHS                                                       
         USING PBMNTHSD,R3                                                      
         XR    R0,R0                                                            
         ICM   R0,1,PBMNTHSN                                                    
         BZ    CJRN04                                                           
*                                                                               
CJRN02   MVC   BCHALF(2),PBMMP                                                  
         MVI   BCHALF+2,X'01'                                                   
         GOTO1 VDATCON,BCPARM,(1,BCHALF),(9,JRNLEDG)                            
         CURED PBMAMT,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BCDMCB               
         LA    R3,PBMNTHSL(R3)                                                  
         LA    R2,L'REPP1(R2)                                                   
         BCT   R0,CJRN02                                                        
         DROP  R3                                                               
*                                                                               
CJRN04   GOTO1 VREPORT,REPD                                                     
         LA    R2,REPP1                                                         
         MVC   JRNDESC,PS@BLG                                                   
         MVC   JRNLEDG,=C'SR'                                                   
         ZAP   BODUB1,PBTOTAC                                                   
         AP    BODUB1,PBTOTAN                                                   
         AP    BODUB1,PBTOTAV                                                   
         AP    BODUB1,PBTOTAS                                                   
         CURED BODUB1,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,PS@INCM                                                  
         MVC   JRNLEDG,=C'SI'                                                   
         CURED PBTOTAC,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB              
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,PS@XFRS                                                  
         MVC   JRNLEDG,=C'SJ'                                                   
         ZAP   BODUB1,BCPZERO                                                   
         TM    PBTYPES,PBXFRQ                                                   
         BZ    *+10                                                             
         ZAP   BODUB1,PBCBTP                                                    
         CURED BODUB1,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,PS@WRTFS                                                 
         MVC   JRNLEDG,=C'SE'                                                   
         ZAP   BODUB1,BCPZERO                                                   
         TM    PBTYPES,PBWOFQ                                                   
         BZ    *+10                                                             
         ZAP   BODUB1,PBCBWP                                                    
         CURED BODUB1,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,PS@SRCRG                                                 
         MVC   JRNLEDG(5),=C'SQ/SI'                                             
         CURED PBTOTAS,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB              
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,PS@DISS                                                  
         CURED PBTOTAD,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BCDMCB              
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    CJRN10                                                           
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,PS@VAT1                                                  
         MVC   JRNLEDG,=C'SG'                                                   
         CURED PBTOTAV,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BCDMCB              
         GOTO1 VREPORT,REPD                                                     
*                                                                               
CJRN10   MVI   REPACTN,REPACLO     CLOSE REPORT                                 
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         MVC   CSJRNRID,REPSUBID   SAVE REPORT ID/NUMBER                        
         MVC   CSJRNRNO,REPREPNO                                                
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         LA    RE,FVXTRA                                                        
         CLC   FVMSGNO,=Y(AI$LOKXF)                                             
         BE    CJRN12                                                           
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         LA    RE,FVPARMS+1                                                     
CJRN12   MVC   0(L'REPSUBID,RE),REPSUBID                                        
         MVC   L'REPSUBID(1,RE),BCCOMMA                                         
         LA    RF,L'REPSUBID+1(RE)                                              
         EDIT  (2,REPREPNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1          
*                                                                               
         CLI   CSACT,ACTDWN                                                     
         BE    CLOJRNX                                                          
         CLI   P#DQU,C'Y'          TEST ALWAYS OUTPUT DQU                       
         BE    CJRN14                                                           
         CLI   PBMODE,PBDRAFTQ                                                  
         BE    *+16                                                             
         CLI   P#DQU,C'L'          TEST ONLY FOR LIVE                           
         BE    CJRN14                                                           
         B     CLOJRNX                                                          
         CLI   P#DQU,C'D'          TEST ONLY FOR DRAFT                          
         BNE   CLOJRNX                                                          
CJRN14   MVC   BASSRV,DQU                                                       
         OI    BASSRVH+FHOID,FHOITR                                             
*                                                                               
CLOJRNX  B     EXITY                                                            
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET ACCOUNT NAME                                         *         
*                                                                     *         
* NTRY: P1 = A(U/L/ACCOUNT CODE)                                      *         
*       P2 = A(AREA FOR ACCOUNT NAME)                                 *         
* EXIT: P1 = A(PBCATAB ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SAWORKD,RC                                                       
SETACTN  DS    0H                                                               
         USING *,R8                                                             
         ST    R1,SAAR1                                                         
         LM    R2,R3,0(R1)                                                      
         MVC   SAKEY,0(R2)                                                      
         ST    R3,SAANAM                                                        
*                                                                               
         L     RF,PBAACTAB                                                      
         LA    R0,PBMAXAC                                                       
         USING PBACTABD,RF                                                      
SETA10   CLI   PBACKEY,0           TEST E-O-T                                   
         BE    SETA20                                                           
         CLC   PBACKEY,SAKEY                                                    
         BE    SETA90              FOUND IT - WE'VE SAVED AN IO!                
         LA    RF,PBACTABL(RF)                                                  
         BCT   R0,SETA10                                                        
         B     SETA20                                                           
*                                                                               
SETA20   LA    R2,IOKEY            READ ACCOUNT RECORD                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,SAKEY                                                    
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    SETA30                                                           
*&&US                                                                           
         CLC   =C'13',POSTCAC+(ACTKUNT-ACTKEY)                                  
         BNE   SETA30                                                           
         L     R1,SAANAM                                                        
         MVC   0(L'NAMEREC,R1),BCSPACES                                         
         L     R1,SAAR1                                                         
         XC    0(4,R1),0(R1)                                                    
         B     EXIT                                                             
*&&                                                                             
         DC    H'0'                                                             
SETA30   L     R2,AIO2                                                          
         LA    R3,ACTRFST                                                       
         SR    RE,RE                                                            
         LTR   RF,R0               TEST ANY SPARE SLOTS IN TABLE                
         BZ    *+10                RE-START AT THE TOP                          
         LA    RF,PBMAXAC                                                       
         SR    RF,R0                                                            
         MH    RF,=Y(PBACTABL)                                                  
         A     RF,PBAACTAB         RF=SPARE SLOT IN PBACTAB                     
         XC    PBACTABD(PBACTABL),PBACTABD                                      
         MVI   PBACTABL(RF),0      FREE-UP THE NEXT SLOT                        
         MVC   PBACKEY,ACTKUNT-ACTKEY(R2)                                       
         MVC   PBACNAM,BCSPACES                                                 
*                                                                               
         USING NAMELD,R3                                                        
SETA40   CLI   NAMEL,0                                                          
         BE    SETA90                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    SETA50                                                           
         CLI   NAMEL,SPAELQ                                                     
         BE    SETA60                                                           
         CLI   NAMEL,RSTELQ                                                     
         BE    SETA70                                                           
SETA45   IC    RE,NAMLN                                                         
         AR    R3,RE                                                            
         B     SETA40                                                           
*                                                                               
SETA50   IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMEREC+1-NAMEL)                                           
         EX    RE,*+4                                                           
         MVC   PBACNAM(0),NAMEREC                                               
         B     SETA45                                                           
*                                                                               
         USING SPAELD,R3                                                        
SETA60   MVC   PBACANC(2),=C'12'                                                
         MVC   PBACANC+2(L'ACTKACT),SPAAULA                                     
         B     SETA45                                                           
*                                                                               
         USING RSTELD,R3                                                        
SETA70   OC    PBACANC,PBACANC     TEST ALREADY SET FROM SPAEL                  
         BNZ   SETA45              YES - SPAEL OVERRIDES                        
         MVC   PBACANC,BCSPACES                                                 
         MVC   PBACANC(2),=C'12'                                                
         MVC   PBACANC+2(L'RSTCOSTG),RSTCOSTG                                   
         B     SETA45                                                           
         DROP  R3                                                               
*                                                                               
SETA90   L     R1,SAANAM                                                        
         MVC   0(L'PBACNAM,R1),PBACNAM                                          
         L     R1,SAAR1                                                         
         ST    RF,0(R1)                                                         
         B     EXIT                                                             
         DROP  RF                                                               
         SPACE 1                                                                
SAWORKD  DSECT                     ** SETACTN LOCAL W/S **                      
*                                                                               
SAAR1    DS    A                   A(CALLERS R1)                                
SAANAM   DS    A                   A(CALLERS NAME AREA)                         
SAKEY    DS    CL(L'ACTKULA)       CALLERS KEY                                  
*                                                                               
SAWORKL  EQU   *-SAWORKD                                                        
CLB44    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT IN BOELEM INTO THE EXTRA POSTING ELEMENT AREA           *         
***********************************************************************         
         SPACE 1                                                                
ADDXTRA  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         L     RE,POSTXTRA         START IF EXTRA AREA                          
         SR    RF,RF                                                            
ADDX10   CLI   0(RE),0             END MARKER                                   
         BE    ADDX20                                                           
*                                  IF VAT SCIEL FIND MATCH                      
         USING SCIELD,RE                                                        
         CLI   BOELEM+(SCIEL-SCIELD),SCIELQ                                     
         BNE   ADDX15                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   ADDX15                                                           
         TM    BOELEM+(SCITYPE-SCIELD),X'40'                                    
         BO    ADDX15                                                           
         CLC   SCITYPE,BOELEM+(SCITYPE-SCIELD)                                  
         BNE   ADDX15                                                           
         AP    SCIAMNT,BOELEM+(SCIAMNT-SCIELD)(L'SCIAMNT)                       
         B     ADDXX                                                            
         DROP  RE                                                               
*                                                                               
ADDX15   IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     ADDX10                                                           
ADDX20   IC    RF,BOELEM+1         RF=L'NEW ELEMENT                             
         LA    R0,PBXTRA+L'PBXTRA  R0=END OF AVAILABLE SPACES                   
         SR    R0,RF                                                            
         CR    RE,R0               CHECK THE NEW ONE WILL FIT                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BOELEM                                                   
         LA    RE,1(RE,RF)                                                      
         MVI   0(RE),0                                                          
ADDXX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD EXTRA ELEMENTS FOR WRITE-OFFS/TRANSFERS              *         
*                                                                     *         
* NTRY: P1 = A(TRNELD)                                                *         
*       P2 = A(PTAELD)                                                *         
***********************************************************************         
         SPACE 1                                                                
XTRAELS  DS    0H                                                               
         USING *,R8                                                             
         USING XEWORKD,RC                                                       
*                                                                               
         LM    R2,R3,0(R1)                                                      
         USING TRNELD,R2                                                        
         USING PTAELD,R3                                                        
*                                  GET TRANSFER PERCENTAGE                      
         ZAP   XEXPCNT,BCPZERO                                                  
         CP    PTANET,BCPZERO                                                   
         BE    XTRA001                                                          
         CP    TRNAMNT,BCPZERO                                                  
         BE    XTRA001                                                          
         ZAP   BODUB1(2*L'BODUB1),PTANET                                        
         ZAP   BODUB3,TRNAMNT                                                   
         SRP   BODUB1(2*L'BODUB1),4,5                                           
         DP    BODUB1(2*L'BODUB1),BODUB3                                        
         ZAP   XEXPCNT,BODUB1                                                   
*                                                                               
XTRA001  DS    0H                                                               
         USING TRXELD,R4                                                        
         LA    R4,BOELEM                                                        
         XC    TRXEL(TRXLN1Q),TRXEL                                             
         MVI   TRXEL,TRXELQ        ADD EXTRA STATUS TO JOB POSTING              
         MVI   TRXLN,TRXLN1Q                                                    
         OI    TRXSTA1,TRXSXALC    EXCLUDE THIS FROM ALLOCATION                 
         LA    R1,TRNEL            FIND TRXEL ON ORIGINAL                       
         XR    R0,R0                                                            
XTRA002  CLI   0(R1),0                                                          
         BE    XTRA003                                                          
         CLI   0(R1),TRXELQ                                                     
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     XTRA002                                                          
         TM    TRXSTA2-TRXELD(R1),TRXSNTMS                                      
         BZ    XTRA003                                                          
         OI    TRXSTA2,TRXSNTMS                                                 
XTRA003  GOTO1 AADDXTRA                                                         
*                                                                               
         USING SCIELD,R4                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCDSC                                                 
         ZAP   SCIAMNT,PTACDSC                                                  
         MP    SCIAMNT,=P'-1'                                                   
         BZ    XTRA004                                                          
         GOTO1 AADDXTRA                                                         
XTRA004  DS    0H                                                               
*                                                                               
*&&US                                                                           
         LA    R1,TRNEL            DON'T ADD HOURS SCIEL                        
         USING PRTELD,R1             IF PRTEL ON RECORD                         
         XR    R0,R0                                                            
XTRA005  CLI   PRTEL,0                                                          
         BE    XTRA006                                                          
         CLI   PRTEL,PRTELQ                                                     
         BE    XTRA008                                                          
         IC    R0,PRTLN                                                         
         AR    R1,R0                                                            
         B     XTRA005                                                          
         DROP  R1                                                               
*&&                                                                             
XTRA006  MVI   SCITYPE,SCITSJHR                                                 
         MVC   BCHALF,PTAHOURS                                                  
         LH    RE,BCHALF                                                        
         CVD   RE,BODUB1                                                        
         ZAP   SCIAMNT,BODUB1                                                   
         MP    SCIAMNT,=P'-1'                                                   
         BZ    XTRA008                                                          
         GOTO1 AADDXTRA                                                         
*                                                                               
XTRA008  LA    R4,TRNEL                                                         
         SR    R0,R0                                                            
         USING SCIELD,R4                                                        
NEWEL    USING SCIELD,BOELEM                                                    
XTRA010  CLI   SCIEL,0                                                          
         BE    XTRAX                                                            
         CLI   SCIEL,SCIELQ                                                     
         BNE   XTRA020                                                          
         CLI   SCITYPE,SCITCOMM                                                 
         BNE   XTRA290                                                          
         SR    RE,RE                                                            
         IC    RE,SCILN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.SCIEL(0),SCIEL                                             
         ZAP   BODUB1(2*L'BODUB1),NEWEL.SCIAMNT                                 
         MP    BODUB1(2*L'BODUB1),XEXPCNT                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   NEWEL.SCIAMNT,BODUB2                                             
         MP    NEWEL.SCIAMNT,=P'-1'                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                                                               
         USING OTHELD,R4                                                        
NEWEL    USING OTHELD,BOELEM                                                    
XTRA020  CLI   OTHEL,OTHELQ                                                     
         BNE   XTRA030                                                          
         SR    RE,RE                                                            
         IC    RE,OTHLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.OTHEL(0),OTHEL                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                                                               
         USING FFNELD,R4                                                        
NEWEL    USING FFNELD,BOELEM                                                    
XTRA030  CLI   FFNEL,FFNELQ                                                     
         BNE   XTRA040                                                          
         SR    RE,RE                                                            
         IC    RE,FFNLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.FFNEL(0),FFNEL                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                                                               
         USING APEELD,R4                                                        
NEWEL    USING APEELD,BOELEM                                                    
XTRA040  CLI   APEEL,APEELQ                                                     
         BNE   XTRA050                                                          
         SR    RE,RE                                                            
         IC    RE,APELN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.APEEL(0),APEEL                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                                                               
         USING RATELD,R4                                                        
NEWEL    USING RATELD,BOELEM                                                    
XTRA050  CLI   RATEL,RATETAXQ                                                   
         BNE   XTRA060                                                          
         SR    RE,RE                                                            
         IC    RE,RATLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.RATEL(0),RATEL                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                                                               
         USING SPDELD,R4                                                        
NEWEL    USING SPDELD,BOELEM                                                    
XTRA060  CLI   SPDEL,SPDELQ                                                     
         BNE   XTRA070                                                          
         SR    RE,RE                                                            
         IC    RE,SPDLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.SPDEL(0),SPDEL                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                                                               
         USING PRTELD,R4                                                        
NEWEL    USING PRTELD,BOELEM                                                    
XTRA070  CLI   PRTEL,PRTELQ                                                     
         BNE   XTRA080                                                          
         IC    RE,PRTLN                                                         
         EX    RE,*+4                                                           
         MVC   NEWEL.PRTELD(0),PRTELD                                           
         ZAP   BODUB1(2*L'BODUB1),NEWEL.PRTHOUR                                 
         MP    BODUB1(2*L'BODUB1),XEXPCNT                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         MP    BODUB2,=P'-1'                                                    
         ZAP   NEWEL.PRTHOUR,BODUB2                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                  UNIT PRICE ELEMENT                           
         USING UNPELD,R4                                                        
NEWEL    USING UNPELD,BOELEM                                                    
XTRA080  CLI   UNPEL,UNPELQ        COPY ELEMENT AND REVERSE UNITS               
         BNE   XTRA090                                                          
         IC    RE,UNPLN                                                         
         EX    RE,*+4                                                           
         MVC   NEWEL.UNPELD(0),UNPELD                                           
         MP    NEWEL.UNPUNIT,=P'-1'                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                                                               
         USING AFCELD,RF                                                        
NEWEL    USING AFCELD,BOELEM                                                    
XTRA090  CLI   AFCEL,AFCELQ                                                     
         BNE   XTRA100                                                          
         IC    RE,AFCLN                                                         
         EX    RE,*+4                                                           
         MVC   NEWEL.AFCELD(0),AFCELD                                           
         ZAP   BODUB1(2*L'BODUB1),NEWEL.AFCAMNT                                 
         MP    BODUB1(2*L'BODUB1),XEXPCNT                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         MP    BODUB2,=P'-1'                                                    
         ZAP   NEWEL.AFCAMNT,BODUB2                                             
         GOTO1 AADDXTRA                                                         
         B     XTRA290                                                          
*                                                                               
XTRA100  DS    0H                                                               
*                                                                               
XTRA290  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     XTRA010                                                          
*                                                                               
XTRAX    B     EXIT                                                             
         DROP  R2,R3,R4,NEWEL                                                   
         SPACE 1                                                                
XEWORKD  DSECT                     ** XTRAELS LOCAL W/S **                      
XEXPCNT  DS    PL4                 TRANSFER PERCENTAGE                          
XEWORKL  EQU   *-XEWORKD                                                        
CLB44    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD MEDIA TRANSFER ELEMENT - NON-BILLING VARIETY                    *         
* NB: HARD LEDGER LENGTHS ASSUMES US ONLY                             *         
*                                                                     *         
* NTRY: P1 = A(PTAELD)                                                *         
*       P2 = A(JOB CODE)                                              *         
***********************************************************************         
         SPACE 1                                                                
ADDWMDT  DS    0H                                                               
         USING *,R8                                                             
*&&UK*&& DC    H'0'                                                             
*&&US                                                                           
         LM    R3,R4,0(R1)                                                      
         USING PTAELD,R3                                                        
         LA    RF,BOELEM                                                        
         USING MDTEL,RF                                                         
         XC    MDTEL(MDTLNQ),MDTEL                                              
         MVI   MDTEL,MDTELQ                                                     
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,MDTSPROD                                                  
         MVC   MDTMED,6(R4)                                                     
         MVC   MDTCLI(L'BCJOBCOD),0(R4)                                         
         MVC   MDTMOS,PTAMOA                                                    
         ZAP   BCDUB,POSTAMNT                                                   
         CVB   R0,BCDUB                                                         
         STCM  R0,15,MDTCOM                                                     
         STCM  R0,15,MDTINTL                                                    
         GOTO1 AADDXTRA                                                         
         B     EXIT                                                             
         DROP  R3,RF                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ADD MEDIA TRANSFER ELEMENT - BILLING VARIETY                        *         
* NB: HARD LEDGER LENGTHS ASSUMES US ONLY                             *         
*                                                                     *         
* NTRY: P1 BYTE 0 = 1 TO ADD ELEMENT NOW                              *         
*                   0 TO INITIALIZE ELEMENT ONLY                      *         
*             1-3 = A(PTAELD)                                         *         
*       P2        = A(JOB CODE)                                       *         
***********************************************************************         
         SPACE 1                                                                
ADDBMDT  DS    0H                                                               
         USING *,R8                                                             
*&&UK*&& DC    H'0'                                                             
*&&US                                                                           
         LM    R3,R4,0(R1)                                                      
         USING PTAELD,R3                                                        
         LA    RF,BOELEM                                                        
         USING MDTEL,RF                                                         
         XC    MDTEL(MDTLNQ),MDTEL                                              
         MVI   MDTEL,MDTELQ                                                     
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,MDTSPROD                                                  
         MVC   MDTMED,6(R4)                                                     
         MVC   MDTCLI(L'BCJOBCOD),0(R4)                                         
         MVC   MDTDSCP,BCJOBNAM                                                 
         MVC   MDTMOS,PBBILMP                                                   
*                                                                               
         CLI   0(R1),1                                                          
         BNE   ADDBMDTX            DO NOT ADD ELEMENT YET                       
*                                                                               
         ZAP   BCDUB,PBTOTSK       TOTAL POSTING TO SK                          
         CVB   R0,BCDUB                                                         
         STCM  R0,15,MDTINTL                                                    
*                                                                               
         ZAP   BCDUB,PBTOTAN       TOTAL POSTING TO SJ                          
         CVB   R0,BCDUB                                                         
         STCM  R0,15,MDTNET                                                     
         LR    RE,R0               SAVE NET IN RE                               
*                                                                               
         ZAP   BCDUB,PBTOTAC       TOTAL INCOME                                 
         CVB   R0,BCDUB                                                         
         STCM  R0,15,MDTCOM                                                     
*                                                                               
         AR    RE,R0               ADD COMMISSION TO SAVED NET                  
         STCM  RE,15,MDTGRS        RE=GROSS                                     
*                                                                               
         ZAP   BCDUB,PBTOTAD                                                    
         CVB   R0,BCDUB                                                         
         STCM  R0,15,MDTCD                                                      
         SR    RE,R0               NOW RE=GROSS-CD                              
*                                                                               
         ZAP   BCDUB,PBTOTAV                                                    
         CVB   R0,BCDUB                                                         
         STCM  R0,15,MDTVAT                                                     
*                                                                               
         AR    RE,R0                                                            
         STCM  RE,15,MDTRECV                                                    
*                                                                               
         GOTO1 AADDXTRA                                                         
*                                                                               
ADDBMDTX B     EXIT                                                             
         DROP  R3,RF                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*              ADD R. L. FILTER ELEMENT FOR WORKCODE                  *         
*                                                                     *         
* NTRY: R1 = A(WORK-CODE)                                             *         
***********************************************************************         
         SPACE 1                                                                
ADDRFL   DS    0H                                                               
         USING *,R8                                                             
         LA    RF,BOELEM                                                        
         USING RFLEL,RF                                                         
         XC    RFLEL(RFLLNQ+L'TRNKWORK),RFLEL                                   
         MVI   RFLEL,RFLELQ                                                     
         MVI   RFLLN,(RFLLNQ+L'TRNKWORK)                                        
         MVI   RFLTYPE,RFLWC                                                    
         MVC   RFLDATA(L'TRNKWORK),0(R1)                                        
         GOTO1 AADDXTRA                                                         
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
DQU      DC    CL(L'BASSRV)'=DQU'                                               
ACCMST   DC    C'ACCMST'                                                        
         SPACE 1                                                                
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
         SPACE 1                                                                
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
TWAROWS  EQU   24                                                               
TWACOLS  EQU   80                                                               
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
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063ACCLB44   08/16/00'                                      
         END                                                                    
