*          DATA SET SPNWS17A   AT LEVEL 080 AS OF 05/01/02                      
*PHASE T20717A,*                                                                
         TITLE 'T20717 - BUYER''S WORKSHEET DMB&&B TRANSMIT OVERLAY'            
T20717   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20717**,RA,RR=RE,CLEAR=YES                                    
         USING TWAD,R5             R5=A(TWA)                                    
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         ST    RE,APRELO                                                        
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         MVC   REPDESC,=C'NWS DOWNLD'                                           
         CLC   QAGY,=C'DB'                                                      
         BNE   *+10                                                             
         MVC   REPDESC,RPTDESC                                                  
         CLC   QAGY,=C'CE'                                                      
         BNE   *+10                                                             
         MVC   REPDESC,RPTDESC_CE                                               
         OI    REPIND2,REPISKP     SKIP SPEC PROCESSINMG                        
         NI    REPHEADI,255-REPHFRCE  DON'T FORCE HEADLINE                      
*                                                                               
         CLI   APACTN,ACTTRB       UNLESS TEST RUN,                             
         BE    MAIN10                                                           
         CLC   QAGY,=C'DB'         DMB DOING THE XMIT?                          
         BNE   MAIN10                                                           
         MVI   REPCLASS,C'G'       YES, TO CLASS G                              
MAIN10   MVC   REPRLH,=H'72'       *** KEEP IT AROUND FOR A WHILE               
         MVC   REPRDH,=H'24'       ***                                          
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
VALREQ   MVI   INWHEN,MIXIOKN      ALWAYS NOW REPORT                            
         MVI   INWIDTH,C'W'        WIDE PRINT LINES (165)                       
         LA    R2,IOKEY            R2=A(CAMPAIGN MARKET HEADER KEY)             
         USING BWHRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BWSKEYH                                                    
         BNE   VALRX                                                            
         GOTO1 VSCANNER,APPARM,AKEYHDR,AIOAREA1,C',=,='                         
         MVI   LFLAG,0                                                          
         LA    R8,1                                                             
*                                                                               
VALR1    SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    VALR98                                                           
         L     R4,AIOAREA1                                                      
*                                                                               
VALR2    STC   R8,FVINDX                                                        
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)                                                       
         BZ    VALR98                                                           
         CLI   1(R4),0                                                          
         BNE   VALR96                                                           
         BAS   RE,VALRSETF                                                      
*                                                                               
         CLI   FVINDX,1                                                         
         BNE   VALR4                                                            
         GOTO1 AVALMED,APWORK      VALIDATE MEDIA                               
         BNE   VALR99                                                           
         MVC   BWHKAGMD,BAGYMD                                                  
         B     VALR12                                                           
*                                                                               
VALR4    CLI   FVINDX,2                                                         
         BNE   VALR6                                                            
         GOTO1 AVALBYR,APWORK      VALIDATE BUYER                               
         BNE   VALR99                                                           
         MVC   BWHKBYR,BBYR                                                     
         MVC   INUSER,QBYR         SET REPORT ID TO THE BUYER CODE              
         OC    BYRPW,BYRPW         CHECK BUYER'S PASSWORD                       
         BZ    VALR12                                                           
         GOTO1 AVALPWD                                                          
         BNE   VALRX                                                            
         B     VALR12                                                           
*                                                                               
VALR6    CLI   FVINDX,3                                                         
         BNE   VALR7                                                            
         GOTO1 AVALCAM,APWORK      VALIDATE CAMPAIGN NUMBER                     
         BNE   VALR99                                                           
         MVC   BWHKCAM,BCAM                                                     
         BAS   RE,GETCAM           GET CAMPAIGN DETAILS                         
         BNE   VALRX                                                            
         B     VALR12                                                           
*                                                                               
VALR7    CLI   FVINDX,4                                                         
         BNE   VALR10                                                           
         TM    2(R4),X'80'         TEST NUMERIC                                 
         BZ    VALR8                                                            
         GOTO1 AVALMKT,APWORK      YES-VALIDATE MARKET                          
         BNE   VALR99                                                           
         B     VALR9                                                            
*                                                                               
VALR8    GOTO1 AVALSTA,APWORK      VALIDATE STATION                             
         BNE   VALR99                                                           
*                                                                               
VALR9    MVC   BWHKMKT,BMKT                                                     
         B     VALR12                                                           
*                                                                               
VALR10   B     VALR96                                                           
*                                                                               
VALR12   LA    R4,32(R4)           NEXT KEY VALUE                               
         LA    R8,1(R8)                                                         
         BCT   R0,VALR2                                                         
*                                                                               
         LA    R8,1                CHECK FOR ALL KEY FIELDS PRESENT             
         OC    BWHKAGMD,BWHKAGMD                                                
         BZ    VALR98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKBYR,BWHKBYR                                                  
         BZ    VALR98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKCAM,BWHKCAM                                                  
         BZ    VALR98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKMKT,BWHKMKT                                                  
         BNZ   VALR13                                                           
         TM    LFLAG,LKEY2         TEST VALIDATING 2ND KEY FIELD                
         BO    VALR98              YES                                          
         OI    LFLAG,LKEY2         YES-VALIDATE 2ND KEY FIELD FOR MKT           
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BWSKY2H                                                    
         BNE   VALRX                                                            
         GOTO1 VSCANNER,APPARM,(C'C',FVIFLD),(2,AIOAREA1)                       
         B     VALR1                                                            
*                                                                               
VALR13   MVI   FVINDX,0                                                         
         MVC   HDRKEY,BWHKEY       SAVE HEADER KEY                              
         GOTO1 AIO,DIRHI+IO1       READ HEADER POINTER                          
         BNE   VALRX                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV  TEST RECORD FOUND                
         BNE   VALR97                                                           
         MVC   HDRDA,IODA          SAVE HEADER D/A                              
         GOTO1 AIO,FILGETU1        GET HEADER RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         MVC   BCMSEQ,BWHKSEQ      SAVE CAMPAIGN/MARKET SEQ NO                  
*                                                                               
         MVI   BSTACD,0                                                         
         OC    BSTA,BSTA           TEST STATION REQUESTED                       
         BZ    VALR16                                                           
         LA    R4,BWHFSTEL         YES-LOOK FOR IN HEADER                       
         SR    R0,R0                                                            
*                                                                               
VALR14   CLI   0(R4),0                                                          
         BE    VALR97              STATION RECORDS NOT FOUND                    
         CLI   0(R4),BWHELCDQ                                                   
         BNE   VALR15                                                           
         USING BWHEL,R4                                                         
         CLC   QSTA,BWHSTA                                                      
         BNE   VALR15                                                           
         MVC   BSTACD,BWHSEQ       SET STATION SEQ NO                           
         B     VALR16                                                           
*                                                                               
VALR15   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALR14                                                           
*                                                                               
VALR16   XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING BWDRECD,R3                                                       
         MVI   BWDKTYP,BWDKTYPQ    SET UP FIRST PART OF DETAIL KEY              
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BCMSEQ      CAMPAIGN/MKT SEQ NO FROM HEADER KEY          
         MVI   BWDKELCD,BWDELCDQ                                                
         LA    R4,BWDKELST-BWDKEY-1   R4=LEN FOR EXECUTED KEY COMPARE           
         CLI   BSTACD,0                                                         
         BE    *+14                                                             
         MVC   BWDKELST,BSTACD                                                  
         LA    R4,L'BWDKELST(R4)                                                
         STC   R4,LKEYCOMP                                                      
         GOTO1 AMIN,MINHI2         READ DETAILS                                 
         BNE   VALR97                                                           
         EX    R4,KEYCOMP          TEST RECORD FOUND                            
         BNE   VALR97                                                           
         B     VALR18                                                           
*                                                                               
VALR17   GOTO1 AMIN,MINSEQ2                                                     
         BNE   VALR95                                                           
         EX    R4,KEYCOMP          TEST RECORD FOUND                            
         BNE   VALR95                                                           
*                                                                               
VALR18   L     R3,AIOAREA2         LOOK FOR SPOTS PER WEEK ELEMENT              
         LA    R1,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
VALR20   CLI   0(R1),0                                                          
         BE    VALR17              NOT FOUND - TRY NEXT RECORD                  
         CLI   0(R1),SPWELCDQ                                                   
         BNE   *+14                                                             
         MVC   DTLKEY,BWDKEY       FOUND - SAVE RECORD'S KEY                    
         B     VALRX                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALR20                                                           
*                                                                               
VALR94   MVC   FVMSGNO,=AL2(FVXDONE)   DATA ALREADY TRANSMITTED                 
         B     VALRX                                                            
*                                                                               
VALR95   MVC   FVMSGNO,=AL2(FVNOXMIT)  NO DATA TO TRANSMIT                      
         B     VALR99                                                           
*                                                                               
VALR96   MVC   FVMSGNO,=AL2(FVFNOTV)   INVALID INPUT FIELD                      
         B     VALR99                                                           
*                                                                               
VALR97   MVC   FVMSGNO,=AL2(FVFERNF)   RECORD NOT FOUND                         
         B     VALR99                                                           
*                                                                               
VALR98   MVC   FVMSGNO,=AL2(FVFNONE)   KEY FIELD MISSING                        
*                                                                               
VALR99   LA    R1,BWSKEYH          KEY ERROR EXIT                               
         TM    LFLAG,LKEY2         TEST VALIDATING SECOND KEY FIELD             
         BZ    *+12                                                             
         LA    R1,BWSKY2H                                                       
         SH    R8,=H'3'                                                         
         ST    R1,FVADDR                                                        
         STC   R8,FVINDX                                                        
*                                                                               
VALRX    B     EXIT                                                             
         SPACE 2                                                                
VALRSETF XC    APWORK,APWORK                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APWORK+L'FVIHDR(0),12(R4)                                        
         LA    RF,1(RF)                                                         
         STC   RF,FVILEN-FVIHDR+APWORK                                          
         LA    RF,L'FVIHDR(RF)                                                  
         STC   RF,APWORK                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET THE CAMPAIGN DETAILS                                            *         
***********************************************************************         
         SPACE 1                                                                
GETCAM   NTR1  ,                                                                
         GOTO1 VDATCON,APPARM,(3,CMPST),CAMPST   CAMPAIGN START/END             
         GOTO1 (RF),(R1),(3,CMPND),CAMPND                                       
         GOTO1 AGETCLT,CMPCLTC     GET CLIENT                                   
         BNE   GETCX                                                            
         MVC   PRDNM1,SPACES                                                    
         MVC   PRDNM2,SPACES                                                    
         MVC   LEN2,SPACES                                                      
         GOTO1 AGETPRD,CMPPRDN     GET PRODUCT                                  
         BNE   GETCX                                                            
         MVC   PRDNM1,PRDNM                                                     
         CLI   CMPPRD1,0           TEST FOR PIGGYBACKS                          
         BE    GETC4                                                            
         CLI   CMPPRD2,0                                                        
         BE    GETC2                                                            
         GOTO1 AGETPRD,CMPPRD2     YES-GET PIGGYBACK PRD 2                      
         MVC   PRDNM2,PRDNM                                                     
         MVC   SVQPRD2,QPRD                                                     
         MVC   SVBPRD2,BPRD                                                     
         ZIC   RE,CMPLEN2          FORMAT 2ND SPOT LENGTH                       
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LEN2,APDUB                                                       
*                                                                               
GETC2    GOTO1 AGETPRD,CMPPRD1     GET PIGGYBACK PRD 1                          
         MVC   PRDNM1,PRDNM                                                     
*                                                                               
GETC4    MVC   SVQPRD1,QPRD                                                     
         MVC   SVBPRD1,BPRD                                                     
*                                                                               
         GOTO1 AGETEST,CMPESTN     GET ESTIMATE                                 
         BNE   GETCX                                                            
         XC    DBLOCK,DBLOCK       GET DEMO NAMES                               
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    RE,ESTUSRNM                                                      
         ST    RE,APPARM+12                                                     
         XC    DEMNAMES,DEMNAMES                                                
         GOTO1 VDEMOCON,APPARM,(4,ESTDEMS),(11,DEMNAMES),(C'S',DBLOCK)          
*                                                                               
         TM    CMPOPTS,CAMODLY     TEST DAILY SCHEDULING                        
         BZ    GETCX                                                            
         GOTO1 VDATCON,APPARM,(3,CMPST),APDUB  YES-GET DAY OF WEEK OF           
         GOTO1 VGETDAY,(R1),APDUB,APFULL           CAMPAIGN START               
         CLC   APFULL(3),SPACES                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,0(R1)                                                         
         STC   RE,CMPSTDAY         CAMPAIGN START DAY                           
         LA    RF,1                FIGURE OUT WHICH SCHEDULE DAY NUMBER         
         CLI   ESTOWSDY,1          THE 1ST WEEK ENDS IN                         
         BNH   *+8                                                              
         IC    RF,ESTOWSDY                                                      
         CR    RE,RF                                                            
         BNL   *+12                                                             
         SR    RF,RE                                                            
         LR    RE,RF                                                            
         B     *+12                                                             
         SR    RE,RF                                                            
         LNR   RE,RE                                                            
         LA    RE,7(RE)                                                         
         STC   RE,WK1END                                                        
         LA    RE,7(RE)                                                         
         STC   RE,WK2END           2ND WEEK ENDS ON THIS DAY                    
*                                                                               
GETCX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT THE REPORT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PRINTREP DS    0H                                                               
         CLI   XFRCALL,C'Y'        CALLED WITH GLOBBER                          
         BE    *+8                                                              
         B     XFRMIS                                                           
*                                                                               
PRIN1    BAS   RE,PRTSHDR          PRINT SCRIPT HEADER                          
         L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         LA    R3,IOKEY                                                         
         USING BWDRECD,R3                                                       
         MVI   LFLAG,0                                                          
         MVI   BSTACD,0                                                         
         XC    STA,STA                                                          
         SR    R8,R8               R8=LINE NUMBER                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'DTLKEY),DTLKEY                                           
         LA    R1,MINHI2           GET FIRST RECORD                             
         B     PRIN4                                                            
*                                                                               
PRIN2    LA    R1,MINSEQ2          GET NEXT RECORD                              
*                                                                               
PRIN4    GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     PRIN14                                                           
         ZIC   RE,LKEYCOMP                                                      
         EX    RE,KEYCOMP          TEST EOF                                     
         BNE   PRIN14                                                           
         L     R3,AIOAREA2         LOOK FOR ELEMENTS                            
         LA    R4,BWDEL                                                         
         SR    R0,R0                                                            
         XC    ASPWEL,ASPWEL                                                    
         XC    ADEMEL,ADEMEL                                                    
         XC    ACOMEL,ACOMEL                                                    
*                                                                               
PRIN6    CLI   0(R4),0                                                          
         BE    PRIN10                                                           
         CLI   0(R4),SPWELCDQ                                                   
         BNE   *+12                                                             
         ST    R4,ASPWEL                                                        
         B     PRIN8                                                            
         CLI   0(R4),DMOELCDQ                                                   
         BNE   *+12                                                             
         ST    R4,ADEMEL                                                        
         B     PRIN8                                                            
         CLI   0(R4),COMELCDQ                                                   
         BNE   PRIN8                                                            
         OC    ACOMEL,ACOMEL                                                    
         BNZ   PRIN8                                                            
         ST    R4,ACOMEL                                                        
*                                                                               
PRIN8    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PRIN6                                                            
*                                                                               
PRIN10   OC    ASPWEL,ASPWEL       TEST FOR SPOTS PER WEEK ELEMENT              
         BZ    PRIN2               NO-READ NEXT RECORD                          
         CLC   BWDKELST,BSTACD     TEST NEW STATION CODE                        
         BNE   *+16                                                             
         CLC   BWDSTA,STA          NO-CHECK STATION'S NOT CHANGED               
         BE    PRIN12                                                           
         DC    H'0'                                                             
         NI    LFLAG,255-LPRTSTA   YES-SHOULD PRINT NEW STATION HEADER          
         MVC   BSTACD,BWDKELST                                                  
         CLC   BWDSTA,STA          CHECK STATION'S CHANGED                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   STA,BWDSTA                                                       
         SR    R8,R8               INITIALIZE LINE NUMBER                       
         ICM   R8,3,INFLINE        TEST LINE= OPTION                            
         BZ    PRIN12                                                           
         SH    R8,=H'10'           YES                                          
*                                                                               
PRIN12   LA    R8,10(R8)           INCREMENT LINE NUMBER                        
         C     R8,=F'10000'        TEST REACHED MAXIMUM                         
         BNL   PRIN99                                                           
         TM    LFLAG,LPRTHDR       TEST PRINTED HEADERS YET                     
         BO    PRIN13                                                           
         BAS   RE,PRTHDR           NO-PRINT EDICT HEADER INFO                   
         BAS   RE,PRTBHD              PRINT BHD HEADER                          
         BAS   RE,PRTMHD              AND MHD HEADER                            
         OI    LFLAG,LPRTHDR                                                    
*                                                                               
PRIN13   TM    LFLAG,LPRTSTA       TEST PRINTED STATION HEADER                  
         BO    *+12                                                             
         BAS   RE,PRTSHD           NO-PRINT SHD HEADER                          
         OI    LFLAG,LPRTSTA                                                    
*                                                                               
         BAS   RE,PRTSSL           PRINT STATION SCHEDULE LINE RECORD           
         CLC   FVMSGNO,=AL2(FVMAXLN)                                            
         BE    PRIN99              HIT MAX LINE SEQ NUMBER                      
         L     R8,SAVER8           R8 = LAST LINE NUMBER USED                   
*                                                                               
         OC    ACOMEL,ACOMEL       TEST COMMENT ELEMENT                         
         BZ    PRIN2                                                            
         BAS   RE,PRTLCM           YES-PRINT SCHEDULE LINE COMMENT REC          
         B     PRIN2               READ NEXT RECORD                             
*                                                                               
PRIN14   B     EXIT                                                             
*                                  CHRIS SAID WE NO LONGER NEED                 
*                                      THE TRANSMIT ELEMENTS                    
*&&DO                                                                           
PRIN14   MVC   IODA,HDRDA                                                       
         GOTO1 AIO,FILGETU1        GET HEADER RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         LA    R4,APELEM           BUILD TRANSMIT ELEMENT                       
         USING XMTELD,R4                                                        
         MVI   XMTELCD,XMTELCDQ                                                 
         MVI   XMTELLN,XMTELLNQ                                                 
         MVC   XMTID,INUSER                                                     
         MVC   XMTREP,REPREPNO                                                  
         GOTO1 VDATCON,APPARM,(5,0),(3,XMTDATE)                                 
         MVC   XMTSTA,QSTA         STATION (IF ANY)                             
         GOTO1 AADDELS,BWHRECD     ADD TO RECORD                                
         GOTO1 AIO,FILPUT1         AND PUT                                      
         BE    PRINX                                                            
         DC    H'0'                                                             
*&&                                                                             
PRIN99   MVC   FVMSGNO,=AL2(FVMAXLN)   MAX LINE NUMBER REACHED                  
*                                                                               
PRINX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT SCRIPT HEADER RECORD                                          *         
***********************************************************************         
PRTSHDR  NTR1  ,                                                                
         L     R4,REPAPRNT                                                      
         CLC   QAGY,=C'SJ'                                                      
         BNE   PRTSHDRX                                                         
         MVC   0(21,R4),=C'HDR*SPNWSCME002E     '                               
         GOTO1 VREPORT,REPD                                                     
PRTSHDRX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT EDICT HEADER INFO                                             *         
***********************************************************************         
         SPACE 1                                                                
PRTHDR   NTR1  ,                                                                
         CLC   QAGY,=C'DB'         DMB DOING A XMIT?                            
         BNE   PRTHDRX             NO, DON'T NEED EDICT INFO                    
***************                                                                 
* DMBDE XMIT                                                                    
***************                                                                 
         L     R4,REPAPRNT                                                      
         MVC   0(165,R4),SPACES                                                 
         MVC   4(5,R4),=C'*HDR*'                                                
         MVC   9(6,R4),=C'EDICT='                                               
         MVC   15(5,R4),=C'DMBDE'  COL 16 - DESTINATION ID                      
         MVI   34(R4),C'W'         COL 35 - REPORT IS 132 CHARS WIDE            
         MVC   38(5,R4),=C'DMBDE'  COL 39 - FORMATTED DEST ID                   
         MVC   54(1,R4),QMED       COL 55 - BILLING INFO                        
         MVC   55(3,R4),QCLT                                                    
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         MVC   0(165,R4),SPACES                                                 
         MVC   0(14,R4),=C'++DDS SPDATTRN'                                      
         LA    R4,15(R4)           COL 16 - APPLICATION AREA                    
         USING SPEDICTD,R4                                                      
         MVI   SPDATYPE,SPDADATQ   TYPE = DMB&B AS/400 TRANSMISSION             
         MVC   SPDAMED,QMED        MEDIA                                        
         MVC   SPDACLT,QCLT        CLIENT                                       
         MVC   SPDAPRD,QPRD        PRODUCT                                      
         MVC   SPDAEST,QEST        ESTIMATE                                     
         MVC   SPDAMKT,QMKT        MARKET                                       
         OC    BSTA,BSTA                                                        
         BZ    *+10                                                             
         MVC   SPDASTA,QSTA        STATION                                      
         MVC   SPDABYR,QBYR        BUYER                                        
         MVC   SPDACAM,QCAM        CAMPAIGN                                     
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PRTHDRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT BROADCAST HEADER RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
PRTBHD   NTR1  ,                                                                
         L     R4,REPAPRNT                                                      
         USING BHDRECD,R4                                                       
*                                                                               
         CLC   QAGY,=C'SJ'                                                      
         BE    PRTBHDX                                                          
***************                                                                 
* DMBDE XMIT                                                                    
***************                                                                 
         MVC   BHDREC,SPACES                                                    
         MVC   BHDRECTY,=C'BHD'                                                 
         MVC   BHDCLT,PRDNM1                                                    
         MVC   BHDREF,ESTNM                                                     
         MVC   BHDCMPST,CAMPST                                                  
         MVC   BHDCMPND,CAMPND                                                  
         MVC   BHDDEM1,DEMNAMES                                                 
         MVC   BHDDEM2,DEMNAMES+6                                               
         MVC   BHDDEM3,DEMNAMES+12                                              
         MVC   BHDDEM4,DEMNAMES+18                                              
         MVC   BHDCLT2,PRDNM2                                                   
         MVC   BHDQMED,QMED                                                     
         MVC   BHDQCLT,QCLT                                                     
         MVC   BHDQPRD1,QPRD                                                    
         MVC   BHDQPRD2,SVQPRD2                                                 
         MVC   BHDQEST,QEST        ESTIMATE                                     
         MVC   BHDQCAMP,QCAM       CAMPAIGN                                     
         MVC   BHDQBYR,QBYR                                                     
         GOTO1 VREPORT,REPD                                                     
         DROP  R4                                                               
*                                                                               
PRTBHDX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT MARKET HEADER RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
PRTMHD   NTR1  ,                                                                
         L     R4,REPAPRNT                                                      
         USING MHDRECD,R4                                                       
         MVC   MHDREC,SPACES                                                    
*                                                                               
         CLC   QAGY,=C'SJ'         DMB DOING AN XMIT?                           
         BE    PRTMHDX             NO                                           
***************                                                                 
* DMBDE XMIT                                                                    
***************                                                                 
         MVC   MHDRECTY,=C'MHD'                                                 
         MVC   MHDCLT,PRDNM1                                                    
         MVC   MHDREF,ESTNM                                                     
         MVC   MHDMKT,QMKT                                                      
         MVC   MHDBYR,QBYR                                                      
         GOTO1 VREPORT,REPD                                                     
         DROP  R4                                                               
*                                                                               
PRTMHDX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT STATION HEADER RECORD                                         *         
***********************************************************************         
PRTSHD   NTR1  ,                                                                
         L     R4,REPAPRNT                                                      
         USING SHDRECD,R4                                                       
         MVC   SHDREC,SPACES                                                    
*                                                                               
         CLC   QAGY,=C'SJ'                                                      
         BE    PRTSHD10                                                         
***************                                                                 
* DMBDE XMIT                                                                    
***************                                                                 
         MVC   SHDRECTY,=C'SHD'                                                 
         MVC   SHDREF,ESTNM                                                     
         MVC   SHDMKT,QMKT                                                      
         MVC   SHDSTA(4),STA                                                    
         MVI   SHDSTA+4,C'-'                                                    
         MVC   SHDSTA+5(1),STA+4                                                
         B     PRTSHDX                                                          
         DROP  R4                                                               
*                                                                               
***************                                                                 
* CME XMIT                                                                      
***************                                                                 
PRTSHD10 DS    0H                                                               
         USING HDRRECD,R4                                                       
         MVC   HDRREC(HDRRECLQ),SPACES                                          
*                                                                               
         MVC   HDRRECTY,=C'HDR*'                                                
         MVC   HDRQMED,QMED                                                     
         MVC   HDRQBYR,QBYR                                                     
         MVC   HDRQCAMP,QCAM       CAMPAIGN                                     
         MVC   HDRQCLT,QCLT                                                     
         MVC   HDRQPRD1,QPRD                                                    
         MVC   HDRQPRD2,SVQPRD2                                                 
         MVC   HDRQEST,QEST        ESTIMATE                                     
         MVC   HDRMKT,QMKT                                                      
         CLI   STA,X'F0'                                                        
         BL    PRTSHD20                                                         
         MVC   HDRSTA,STA          CABLE STATION                                
         MVI   HDRSTA+4,C'/'                                                    
         MVI   HDRBAND,C'T'                                                     
         B     PRTSHD30                                                         
PRTSHD20 MVC   HDRSTA(4),STA                                                    
         MVC   HDRBAND,STA+4                                                    
PRTSHD30 GOTO1 VREPORT,REPD                                                     
*                                                                               
         USING HD1RECD,R4                                                       
         MVC   HD1REC(HD1RECLQ),SPACES                                          
         MVC   HD1RECTY,=C'HD1*'                                                
         MVC   HD1CLT,CLTNM                                                     
         MVC   HD1PRD1,PRDNM1                                                   
         MVC   HD1PRD2,PRDNM2                                                   
         MVC   HD1REF,ESTNM                                                     
         MVC   HD1ESTST,ESTST                                                   
         MVC   HD1ESTND,ESTND                                                   
         MVC   HD1CMPST,CAMPST                                                  
         MVC   HD1CMPND,CAMPND                                                  
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         USING HD2RECD,R4                                                       
         MVC   HD2REC(HD2RECLQ),SPACES                                          
         MVC   HD2RECTY,=C'HD2*'                                                
         MVC   HD2MKTNM,MKTNM                                                   
         MVC   HD2DEM1(6),DEMNAMES                                              
         MVC   HD2DEM2(6),DEMNAMES+6                                            
         MVC   HD2DEM3(6),DEMNAMES+12                                           
         MVC   HD2DEM4(6),DEMNAMES+18                                           
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,=C'GETD',WORK,60,GLVSGLS                             
         CLI   APPARM+8,0                                                       
         BNE   PRTSHDX                                                          
         EDIT  (4,WORK),HD2GRP,1,ALIGN=LEFT,WRK=APWORK,DUB=APDUB,      +        
               ZERO=NOBLANK                                                     
         EDIT  (4,WORK+4),HD2GLD,ALIGN=LEFT,WRK=APWORK,DUB=APDUB,      +        
               ZERO=NOBLANK                                                     
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         DROP  R4                                                               
PRTSHDX  GOTO1 VREPORT,REPD                                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT STATION SCHEDULE LINE RECORD                                            
* ON ENTRY:    (R8)                LINE SEQUENCE NUMBER                         
*                                                                               
* ON EXIT:     SAVER8              LAST LINE SEQUENCE NUMBER USED               
***********************************************************************         
         SPACE 1                                                                
PRTSSL   NTR1  ,                                                                
         ST    R8,SAVER8                                                        
         ST    R9,SAVER9                                                        
         XC    SAVERE,SAVERE       1ST TIME IN                                  
         XC    SAVERF,SAVERF                                                    
*                                                                               
         L     R4,REPAPRNT                                                      
         USING SSLRECD,R4                                                       
         BAS   RE,SSLOVRHD         SET UP THE SCHEDULE LINE OVERHEAD            
*                                                                               
         L     R5,ASPWEL           SCHEDULED SPOTS                              
         USING SPWEL,R5                                                         
*                                                                               
         CLC   QAGY,=C'SJ'         DMB DOING AN XMIT?                           
         BE    *+8                 NO, WE ALREADY HAVE R2                       
         LA    R2,SSLWKS           R2 = A(1ST WEEK IN REPORT LINE)              
*                                                                               
         LA    R8,SPWPERWK         R8 = A(1ST WEEK/DAY IN ELEMENT)              
         LA    RE,1                RE = WEEK/DAY COUNTER (FOR BXLE)             
         ZIC   RF,SPWELLN          RF = A(LAST WEEK/DAY IN ELEMENT)             
         AR    RF,R5                                                            
         BCTR  RF,0                                                             
*                                                                               
         LA    R0,13               R0 = # WEEKS/DAYS TO CHECKS THROUGH          
         CLC   QAGY,=C'SJ'                                                      
         BE    *+8                                                              
         LA    R0,14                                                            
*                                                                               
         LA    R1,1                R1 = WEEK/DAY COUNTER                        
         SR    R9,R9               R9 = TOTAL SPOTS FOR THE WEEK                
         MVI   APBYTE,0            WEEK STILL PENDING, 1=YES, 0=NO              
*                                                                               
SSL10    LTR   R8,R8               TEST BEYOND END OF SCHEDULE                  
         BZ    SSL90               YES, SPOTS=0                                 
         ZIC   R6,0(R8)            NO,  R6=N'SPOTS                              
*                                                                               
         TM    CMPOPTS,CAMODLY     TEST DAILY SCHEDULE                          
         BZ    SSL70               NO                                           
*                                                                               
         TM    CLTIND2,CLTISDLY    SEPARATE LINES FOR DAILY SCHEDULE?           
         BZ    SSL60               NO                                           
         MVC   APHALF(1),WK1END    ASSUME FIRST WEEK FIRST                      
         CLM   R1,1,WK1END         TEST INTO 2ND WEEK NOW                       
         BNH   *+10                                                             
         MVC   APHALF(1),WK2END    YES                                          
         CLM   R1,1,APHALF         TEST REACHED END OF WEEK                     
         BE    *+12                                                             
         MVI   APBYTE,1            NO-INDICATE WEEK PENDING                     
         B     SSL70                                                            
         MVI   APBYTE,0                                                         
         B     SSL70                                                            
*                                                                               
SSL60    AR    R9,R6               YES-ACCUMULTE SPOTS FOR WEEK                 
         MVC   APHALF(1),WK1END    IN FIRST WEEK                                
         CLM   R1,1,WK1END         TEST INTO 2ND WEEK NOW                       
         BNH   *+10                                                             
         MVC   APHALF(1),WK2END    YES                                          
         CLM   R1,1,APHALF         TEST REACHED END OF WEEK                     
         BE    *+12                                                             
         MVI   APBYTE,1            NO-INDICATE WEEK PENDING                     
         B     SSL80                                                            
         LR    R6,R9               YES-FORMAT TOTAL FOR WEEK                    
         SR    R9,R9               CLEAR THE SPOT COUNTER                       
         MVI   APBYTE,0            NOTHING PENDING                              
*                                                                               
SSL70    CVD   R6,APDUB            FORMAT N'SPOTS                               
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(2,R2),APDUB                                                    
*                                                                               
         TM    CMPOPTS,CAMODLY     TEST DAILY SCHEDULE                          
         BZ    SSL80               NO                                           
         TM    CLTIND2,CLTISDLY    SEPARATE LINES FOR DAILY SCHEDULE?           
         BZ    SSL80               NO                                           
*                                                                               
         LTR   R6,R6               IF NO SPOTS THIS DAY                         
         BZ    SSL80               THEN NOTHING TO PRINT                        
*                                                                               
         ST    R1,SAVER1           SAVE THESE REGISTERS FOR LATER               
         ST    RE,SAVERE                                                        
         OC    SAVERF,SAVERF                                                    
         BZ    SSL75                                                            
         L     RE,SAVER8                                                        
         LA    RE,10(RE)                                                        
         C     RE,=F'10000'           HIT MAX LINE SEQ NUMBER?                  
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(FVMAXLN)  YES                                       
         B     SSLX                                                             
         ST    RE,SAVER8                                                        
         CLC   QAGY,=C'SJ'                                            X         
         BE    SSL75                                                  X         
         CVD   RE,APDUB            LINE NUMBER                                  
         OI    APDUB+7,X'0F'                                                    
         UNPK  SSLLINE(4),APDUB                                                 
*                                                                               
SSL75    ST    RF,SAVERF                                                        
*                                                                               
         BCTR  R1,0                R1 = DAY NUMBER FOR THIS SPOT                
         ZIC   RE,CMPSTDAY                                                      
         AR    R1,RE                                                            
         CH    R1,=H'8'                                                         
         BL    *+12                                                             
         SH    R1,=H'7'                                                         
         B     *-12                                                             
*                                                                               
         CLC   QAGY,=C'SJ'                                            X         
         BE    SSL77                                                  X         
         MVC   SSLDAYS,SPACES      CLEAR THE DAYS                     X         
         BCTR  R1,0                'X' THE DAY FOR THIS SPOT                    
         LA    RE,SSLDAYS                                             X         
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-4                                                           
         MVI   0(RE),C'X'                                                       
         B     SSL79                                                  X         
*                                                                               
SSL77    DS    0H                                                     X         
         LR    R6,R4               REPAPRNT                           X         
         USING LINRECD,R6                                             X         
         MVC   LINDAYS,SPACES      CLEAR THE DAYS                     X         
         BCTR  R1,0                'X' THE DAY FOR THIS SPOT          X         
         LA    RE,LINDAYS                                             X         
         LA    RF,DAYCHARS                                            X         
         LTR   R1,R1                                                            
         BZ    *+16                                                             
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,*-8                                                           
         MVC   0(1,RE),0(RF)                                                    
         DROP  R6                                                               
*                                                                               
SSL79    L     R1,AREP                                                          
         GOTO1 VREPORT             PRINT THE RECORD                             
         BAS   RE,SSLOVRHD         SETUP THE SSL OVERHEAD AGAIN                 
         L     R1,SAVER1           RESTORE THESE REGISTERS                      
         L     RE,SAVERE                                                        
         L     RF,SAVERF                                                        
*                                                                               
SSL80    LA    R1,1(R1)            ADVANCE TO NEXT SCHEDULE POSITION            
         BXLE  R8,RE,SSL90                                                      
         SR    R8,R8               SCHEDULE DONE-                               
         CLI   APBYTE,1            TEST WEEK PENDING                            
         BNE   SSL90                                                            
         CVD   R9,APDUB            YES-FORMAT NOW                               
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(2,R2),APDUB                                                    
         MVI   APBYTE,0                                                         
*                                                                               
SSL90    CLI   APBYTE,1            IS THIS WEEK STILL PENDING?                  
         BE    SSL10                                                            
*                                                                               
         LA    R2,2(R2)            NO, ADVANCE TO NEXT WEEK ON PRINT LN         
         BCT   R0,SSL10            GO THROUGH ALL WEEKS/DAYS                    
*                                                                               
SSL100   TM    CMPOPTS,CAMODLY     DAILY CAMPAIGN?                              
         BZ    SSL110              NO                                           
         TM    CLTIND2,CLTISDLY    SEPARATE LINES FOR DAILY SCHEDULES?          
         BNZ   SSLX                YES, PRINTED THIS LINE ALREADY               
*                                                                               
SSL110   L     R1,AREP                                                          
         GOTO1 VREPORT             PRINT THE RECORD                             
*                                                                               
SSLX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SETS UP THE SCHEDULE LINE OVERHEAD                                            
***********************************************************************         
         SPACE 1                                                                
SSLOVRHD NTR1  ,                                                                
         L     R8,SAVER8                                                        
         L     R9,SAVER9                                                        
*                                                                               
         L     R4,REPAPRNT                                                      
         USING SSLRECD,R4                                                       
         LR    R6,R4                                                            
         USING LINRECD,R6                                                       
*                                                                               
         CLC   QAGY,=C'SJ'                                                      
         BNE   SSLOVR03                                                         
         MVC   LINREC(LINRECLQ),SPACES                                          
         MVC   LINRECTY,=C'LIN*'                                                
         LA    R1,LINDAYS          DAYS                                         
         B     SSLOVR05                                                         
*                                                                               
***************                                                                 
* DMB XMIT                                                                      
***************                                                                 
SSLOVR03 MVC   SSLREC,SPACES                                                    
         MVC   SSLRECTY,=C'SSL'                                                 
         MVC   SSLREF,ESTNM        ESTIMATE                                     
         MVC   SSLMKT,QMKT         MARKET                                       
         MVC   SSLSTA(4),STA       STATION                                      
         MVI   SSLSTA+4,C'-'                                                    
         MVC   SSLSTA+5(1),STA+4                                                
         CVD   R8,APDUB            LINE NUMBER                                  
         OI    APDUB+7,X'0F'                                                    
         UNPK  SSLLINE(4),APDUB                                                 
         MVC   LINE,SSLLINE                                                     
         LA    R1,SSLDAYS          DAYS                                         
*                                                                               
***************                                                                 
* DAYS                                                                          
***************                                                                 
SSLOVR05 LA    R0,7                                                             
         LA    RE,DAYCHARS                                                      
         ICM   RF,8,BWDDAYS                                                     
SSLOVR10 SLL   RF,1                                                             
*                                                                               
         LTR   RF,RF               IS THIS DAY BIT ON?                          
         BM    SSLOVR12            YES                                          
*                                                                               
         CLC   QAGY,=C'SJ'                                                      
         BNE   SSLOVR14                                                         
         MVI   0(R1),C'.'                                                       
         B     SSLOVR14                                                         
*                                                                               
DAYCHARS DC    CL7'MTWTFSS'                                                     
*                                                                               
SSLOVR12 CLC   QAGY,=C'SJ'                                                      
         BE    *+12                                                             
         MVI   0(R1),C'X'          MARK WITH AN 'X'                             
         B     *+10                                                             
         MVC   0(1,R1),0(RE)       CME GETS THE DAY CHARACTER                   
*                                                                               
SSLOVR14 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,SSLOVR10                                                      
*                                                                               
         CLC   QAGY,=C'SJ'                                                      
         BNE   *+12                                                             
         LA    R1,LINTIMES         TIMES                                        
         B     SSLOVR35                                                         
*                                                                               
***************                                                                 
* SPECIAL INDICATOR                                                             
***************                                                                 
         LA    R0,L'BWDPROG-2      SPECIAL                                      
         LA    R1,BWDPROG+L'BWDPROG-2                                           
*                                                                               
SSLOVR20 CLC   0(2,R1),=C'-S'                                                   
         BNE   SSLOVR25                                                         
         MVC   SSLSPCL,=C'SPC'                                                  
         B     SSLOVR30                                                         
*                                                                               
SSLOVR25 CLI   1(R1),C' '                                                       
         BH    SSLOVR30                                                         
         BCTR  R1,0                                                             
         BCT   R0,SSLOVR20                                                      
*                                                                               
*********                                                                       
* TIMES                                                                         
*********                                                                       
SSLOVR30 LA    R1,LINTIMES         TIMES                                        
SSLOVR35 LA    RF,BWDTIMES         RF = A(START TIME)                           
         LA    R0,2                                                             
*                                                                               
SSLOVR40 SR    RE,RE               GET THE TIME                                 
         ICM   RE,3,0(RF)                                                       
*                                                                               
         LTR   RE,RE                                                            
         BZ    *+12                                                             
         CH    RE,=H'2400'                                                      
         BNE   *+12                                                             
         MVI   4(R1),C'M'                                                       
         B     SSLOVR50                                                         
*                                                                               
         MVI   4(R1),C'A'                                                       
         CH    RE,=H'1200'                                                      
         BL    SSLOVR50                                                         
         BH    *+12                                                             
         MVI   4(R1),C'N'                                                       
         B     SSLOVR50                                                         
         MVI   4(R1),C'P'                                                       
*                                                                               
SSLOVR50 CH    RE,=H'100'                                                       
         BNL   *+8                                                              
         AH    RE,=H'1200'                                                      
         CH    RE,=H'1300'                                                      
         BL    *+8                                                              
         SH    RE,=H'1200'                                                      
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(4,R1),APDUB                                                    
*                                                                               
         CLC   QAGY,=C'SJ'                                                      
         BNE   SSLOVR52            YES                                          
         MVI   LINTIMES+5,C'-'                                                  
         LA    R1,LINTIMES+6                                                    
         B     *+8                                                              
*                                                                               
SSLOVR52 LA    R1,SSLTIM2                                                       
         LA    RF,BWDTIMES+2       POINT TO THE END TIME NOW                    
         BCT   R0,SSLOVR40                                                      
*                                                                               
         CLC   QAGY,=C'SJ'                                                      
         BE    SSLOVR54                                                         
***************                                                                 
* DMB PORTION FOR DPT, SLN, PRG, COST                                           
***************                                                                 
SSLOVR53 MVC   SSLDPT,BWDDPT       DAYPART                                      
         ZIC   RE,BWDSLN           SPOT LENGTH                                  
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  SSLLEN,APDUB                                                     
         MVC   SSLPRG,BWDPROG      PROGRAM                                      
         ICM   RE,15,BWDCOST1      COST                                         
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  SSLCOST,APDUB                                                    
         B     SSLOVR56                                                         
***************                                                                 
* CME PORTION FOR DPT, SLN, SLN2, PRG, COST                                     
***************                                                                 
SSLOVR54 DS    0H                                                               
****     GOTO1 VUNTIME,APPARM,DEMNAMES,LINTIMES                                 
         MVC   LINDPT,BWDDPT       DAYPART                                      
         MVC   LINSDPT,BWDSUBDP    SUBDAYPART                                   
         ZIC   RE,BWDSLN           SPOT LENGTH                                  
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LINLEN,APDUB                                                     
         MVC   LINLEN2,LEN2        PIGGYBACK LENGTH 2                           
         MVC   LINPRG,BWDPROG      PROGRAM                                      
         EDIT  BWDCOST1,LINCOST,2,ALIGN=LEFT,WRK=APWORK,DUB=APDUB               
****     ICM   RE,15,BWDCOST1      COST                                         
****     CVD   RE,APDUB                                                         
****     OI    APDUB+7,X'0F'                                                    
****     UNPK  LINCOST,APDUB                                                    
*                                                                               
SSLOVR56 ICM   R5,15,ADEMEL        DEMOS                                        
         BZ    SSLOVR80                                                         
         USING DMOEL,R5                                                         
*                                                                               
         LA    R2,SSLDEM1                                                       
         CLC   QAGY,=C'SJ'                                                      
         BNE   *+8                                                              
         LA    R2,LINDEM1                                                       
         DROP  R4,R6                                                            
*                                                                               
         LA    R8,ESTDEMS                                                       
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,DMOELLN                                                       
         AR    RF,R5                                                            
         BCTR  RF,0                                                             
         LA    R4,4                                                             
*                                                                               
SSLOVR60 SR    R6,R6                                                            
         OC    0(3,R8),0(R8)                                                    
         BZ    SSLOVR70                                                         
         LA    R1,DMODEMO                                                       
         CLC   1(2,R1),1(R8)                                                    
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     SSLOVR70                                                         
         ICM   R6,15,4(R1)                                                      
         SLL   R6,1                                                             
         SRL   R6,1                                                             
*                                                                               
SSLOVR70 DS    0H                                                               
         CLC   QAGY,=C'SJ'                                                      
         BE    SSLOVR75                                                         
*                                                                               
         CVD   R6,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(L'SSLDEM1,R2),APDUB                                            
         LA    R2,L'SSLDEM1(R2)                                                 
         B     SSLOVR77                                                         
*                                                                               
SSLOVR75 EDIT  (R6),(7,(R2)),1,ALIGN=LEFT,WRK=APWORK,DUB=APDUB                  
         LA    R2,L'LINDEM1(R2)                                                 
*                                                                               
SSLOVR77 LA    R8,3(R8)                                                         
         BCT   R4,SSLOVR60                                                      
*                                                                               
SSLOVR80 CLC   QAGY,=C'SJ'                                                      
         BE    SSLOVR90                                                         
***************                                                                 
* DMB PORTION FOR WKS, CMPST, CMPND, AND LEN2                                   
***************                                                                 
         L     R4,REPAPRNT                                                      
         USING SSLRECD,R4                                                       
         MVI   SSLWKS,C'0'         C'0' OUT SPOTS/WEEK OR SPOTS/DAY             
         MVC   SSLWKS+1(L'SSLWKS-1),SSLWKS                                      
         MVC   SSLCMPST,CAMPST     CAMPAIGN START/END                           
         MVC   SSLCMPND,CAMPND                                                  
         MVC   SSLLEN2,LEN2        PIGGYBACK LENGTH 2                           
         LA    R2,SSLWKS                                                        
         B     SSLOVRX                                                          
***************                                                                 
* CME PORTION FOR WEEKS                                                         
***************                                                                 
SSLOVR90 DS    0H                                                               
         L     R6,REPAPRNT                                                      
         USING LINRECD,R6                                                       
*                                                                               
         MVI   LINWKS,C'0'         C'0' OUT SPOTS/WEEK OR SPOTS/DAY             
         MVC   LINWKS+1(L'LINWKS-1),LINWKS                                      
         LA    R2,LINWKS                                                        
         DROP  R6                                                               
*                                                                               
SSLOVRX  XIT1  REGS=(R2)           RETURN R2 WITH A(SSLWKS) FOR NON-DMB         
         EJECT                                                                  
***********************************************************************         
* PRINT SCHEDULE LINE COMMENT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTLCM   NTR1  ,                                                                
         L     R5,ACOMEL                                                        
         USING COMEL,R5                                                         
         L     R4,REPAPRNT                                                      
         CLC   QAGY,=C'SJ'                                                      
         BE    LCM30                                                            
         USING LCMRECD,R4                                                       
         LA    R8,1                                                             
         SR    R0,R0                                                            
*                                                                               
LCM10    MVC   LCMREC,SPACES                                                    
         MVC   LCMRECTY,=C'LCM'                                                 
         MVC   LCMREF,ESTNM        ESTIMATE                                     
         MVC   LCMMKT,QMKT         MARKET                                       
         MVC   LCMSTA(4),STA       STATION                                      
         MVI   LCMSTA+4,C'-'                                                    
         MVC   LCMSTA+5(1),STA+4                                                
         MVC   LCMLINE,LINE        SCHEDULE LINE NUMBER                         
         CVD   R8,APDUB            COMMENT SEQUENCE NUMBER                      
         OI    APDUB+7,X'0F'                                                    
         UNPK  LCMSEQ,APDUB                                                     
         ZIC   RE,COMELLN          COMMENT TEXT                                 
         SH    RE,=Y(COMCOM-COMEL)                                              
         BNP   LCM20                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   LCMTEXT(0),COMCOM                                                
         GOTO1 VREPORT,REPD        PRINT THE RECORD                             
         LA    R8,1(R8)                                                         
*                                                                               
LCM20    IC    R0,1(R5)            LOOK FOR OTHER COMMENTS                      
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    LCMX                                                             
         CLI   0(R5),COMELCDQ                                                   
         BE    LCM10                                                            
         B     LCM20                                                            
         DROP  R4                                                               
*                                                                               
LCM30    DS    0H                                                               
         USING LC1RECD,R4                                                       
         MVC   LC1REC(LINRECLQ),SPACES                                          
         MVC   LC1RECTY,=C'LC1*'                                                
*                                                                               
LCM40    DS    0H                                                               
         CLI   COMNUM,4            ONLY ACCEPTION COMMENTS 1-4                  
         BH    LCM50                                                            
         LA    R2,LC1TEXT1                                                      
         CLI   COMNUM,1                                                         
         BE    LCM45                                                            
         CLI   COMNUM,3                                                         
         BE    LCM43                                                            
         LA    R2,L'LC1TEXT1(R2)   COMMENTS 2 & 4 ONLY                          
*                                                                               
         CLI   COMNUM,3                                                         
         BL    LCM45                                                            
         CLC   LC1RECTY,=C'LC2*'   COMMENT # 4, SO CHECK IF #3 IS THERE         
         BE    LCM45               YES                                          
LCM43    DS    0H                                                               
         TM    LFLAG,LCOMNTS                                                    
         BNO   LCM44                                                            
         GOTO1 VREPORT,REPD        PRINT THE RECORD                             
         NI    LFLAG,X'FF'-LCOMNTS                                              
         MVC   LC1REC(LC1RECLQ),SPACES                                          
LCM44    MVC   LC1RECTY,=C'LC2*'                                                
*                                                                               
LCM45    ZIC   RE,COMELLN          COMMENT TEXT                                 
         SH    RE,=Y(COMCOM-COMEL)                                              
         BNP   LCM50                                                            
         CH    RE,=H'60'                                                        
         BNH   *+8                                                              
         LA    RE,60                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),COMCOM                                                   
         OI    LFLAG,LCOMNTS                                                    
*                                                                               
LCM50    IC    R0,1(R5)            LOOK FOR OTHER COMMENTS                      
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    LCM60                                                            
         CLI   0(R5),COMELCDQ                                                   
         BNE   LCM50                                                            
         B     LCM40                                                            
*                                                                               
LCM60    DS    0H                                                               
         TM    LFLAG,LCOMNTS                                                    
         BNO   LCMX                                                             
         GOTO1 VREPORT,REPD        PRINT THE RECORD                             
         NI    LFLAG,X'FF'-LCOMNTS                                              
         DROP  R4                                                               
LCMX     B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TRANSFER TO MIS                                                     *         
***********************************************************************         
XFRMIS   DS    0H                                                               
         USING TWAD,R5                                                          
         L     RF,ACOM                                                          
         L     RF,CGLOBBER-COMFACSD(RF)                                         
*                                                                               
         GOTO1 (RF),APPARM,=C'PUTD',QMED,L'QMED,GLVSPMD                         
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),APPARM,=C'PUTD',QCLT,L'QCLT,GLVSPCLT                        
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),APPARM,=C'PUTD',QPRD,L'QPRD,GLVSPPRD                        
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),APPARM,=C'PUTD',QEST,L'QEST,GLVSPEST                        
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),APPARM,=C'PUTD',QMKT,L'QMKT,GLVSPMKT                        
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(6),CAMPST                                                   
         MVI   WORK+6,C'-'                                                      
         MVC   WORK+7(6),CAMPND                                                 
         GOTO1 (RF),APPARM,=C'PUTD',WORK,13,GLVSPPER                            
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING GLVXFRSY,R2                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'NWS'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'MIS'                                                 
         DROP  R2,R5                                                            
         GOTO1 (RF),APPARM,=C'PUTD',(R2),22,GLVXCTL                             
*                                                                               
XFRMISX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS, ETC                                                      *         
***********************************************************************         
         SPACE 1                                                                
RPTDESC    DC    CL11'DMB&&B SCHED'                                             
RPTDESC_CE DC    CL11'CEMN DNLD'                                                
SPACES   DC    165C' '                                                          
*                                                                               
KEYCOMP  CLC   IOKEY(0),IOKEYSAV   EXECUTED INSTRUCTION                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                     ** LOCAL WORKING STORAGE **                  
*                                                                               
ASPWEL   DS    A                                                                
ADEMEL   DS    A                                                                
ACOMEL   DS    A                                                                
*                                                                               
SAVER1   DS    F                   SAVED REGISTERS                              
SAVER8   DS    F                                                                
SAVER9   DS    F                                                                
SAVERE   DS    F                                                                
SAVERF   DS    F                                                                
*                                                                               
CAMPST   DS    CL6                                                              
CAMPND   DS    CL6                                                              
CMPSTDAY DS    XL1                 CAMPAIGN START DAY                           
*                                                                               
SVQPRD1  DS    CL3                                                              
SVBPRD1  DS    XL1                                                              
SVQPRD2  DS    CL3                                                              
SVBPRD2  DS    XL1                                                              
*                                                                               
DEMNAMES DS    XL24                4 DEMO NAMES                                 
STA      DS    CL8                                                              
WK1END   DS    XL1                                                              
WK2END   DS    XL1                                                              
LEN2     DS    CL3                                                              
LINE     DS    CL4                                                              
LKEYCOMP DS    XL1                                                              
LFLAG    DS    XL1                                                              
LPRTHDR  EQU   X'80'                                                            
LPRTSTA  EQU   X'40'                                                            
LCOMNTS  EQU   X'20'                                                            
LKEY2    EQU   X'01'                                                            
WORK     DS    CL64                                                             
*                                                                               
         DS    0D                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRKA                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* CME SCHEDULE RECORD DSECTS                                          *         
***********************************************************************         
HDRRECD  DSECT                     STATION HEADER RECORD                        
HDRREC   DS    0C                                                               
HDRRECTY DS    CL4                 HDR*                                         
HDRQMED  DS    CL1                                                              
HDRQBYR  DS    CL3                                                              
HDRQCAMP DS    CL5                                                              
HDRQCLT  DS    CL3                                                              
HDRQPRD1 DS    CL3                                                              
HDRQPRD2 DS    CL3                                                              
HDRQEST  DS    CL3                                                              
HDRMKT   DS    CL4                                                              
HDRSTA   DS    CL8                                                              
HDRBAND  DS    CL2                                                              
         DS    CL12                SPARE                                        
HDRRECLQ EQU   *-HDRREC                                                         
*                                                                               
HD1RECD  DSECT                     STATION HEADER CONTINUED                     
HD1REC   DS    0C                                                               
HD1RECTY DS    CL4                 HD1*                                         
HD1CLT   DS    CL20                                                             
HD1PRD1  DS    CL20                                                             
HD1PRD2  DS    CL20                                                             
HD1REF   DS    CL20                                                             
HD1ESTDT DS    0CL12                                                            
HD1ESTST DS    CL6                                                              
HD1ESTND DS    CL6                                                              
HD1CMPDT DS    0CL12                                                            
HD1CMPST DS    CL6                                                              
HD1CMPND DS    CL6                                                              
HD1RECLQ EQU   *-HD1REC                                                         
*                                                                               
HD2RECD  DSECT                     STATION HEADER CONTINUED                     
HD2REC   DS    0C                                                               
HD2RECTY DS    CL4                 HD2*                                         
HD2MKTNM DS    CL20                                                             
HD2DEM1  DS    CL7                                                              
HD2DEM2  DS    CL7                                                              
HD2DEM3  DS    CL7                                                              
HD2DEM4  DS    CL7                                                              
HD2GLD   DS    CL9                                                              
HD2GRP   DS    CL8                                                              
HD2RECLQ EQU   *-HD2REC                                                         
         SPACE 1                                                                
*                                                                               
LINRECD  DSECT                     DETAIL LINE                                  
LINREC   DS    0C                                                               
LINRECTY DS    CL4                 LIN*                                         
LINDAYS  DS    CL7                                                              
LINTIMES DS    CL11                                                             
LINDPT   DS    CL1                                                              
LINSDPT  DS    CL1                                                              
         DS    CL12                SPARE                                        
LINLEN   DS    CL3                                                              
LINLEN2  DS    CL3                                                              
LINPRG   DS    CL17                                                             
LINCOST  DS    CL9                                                              
LINDEM1  DS    CL7                                                              
LINDEM2  DS    CL7                                                              
LINDEM3  DS    CL7                                                              
LINDEM4  DS    CL7                                                              
LINWKS   DS    CL(14*2)                                                         
         DS    CL12                SPARE                                        
LINRECLQ EQU   *-LINREC                                                         
*                                                                               
LC1RECD  DSECT                     COMMENT LINE                                 
LC1REC   DS    0C                                                               
LC1RECTY DS    CL4                 LC1*                                         
LC1TEXT1 DS    CL60                                                             
LC1TEXT2 DS    CL60                                                             
LC1RECLQ EQU   *-LC1REC                                                         
*                                                                               
LC2RECD  DSECT                     COMMENT LINE CONTINUED                       
LC2REC   DS    0C                                                               
LC2RECTY DS    CL4                 LC2*                                         
LC2TEXT3 DS    CL60                                                             
LC2TEXT4 DS    CL60                                                             
LC2RECLQ EQU   *-LC2REC                                                         
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
* SCHEDULE RECORD DSECTS                                              *         
***********************************************************************         
         SPACE 1                                                                
BHDRECD  DSECT                                                                  
*                                                                               
BHDREC   DS    0CL165              BROADCAST HEADER RECORD                      
BHDRECTY DS    CL3                                                              
BHDCLT   DS    CL8                                                              
BHDREF   DS    CL8                                                              
BHDCMPST DS    CL6                                                              
BHDCMPND DS    CL6                                                              
BHDDEM1  DS    CL6                                                              
BHDDEM2  DS    CL6                                                              
BHDDEM3  DS    CL6                                                              
BHDDEM4  DS    CL6                                                              
BHDCLT2  DS    CL8                                                              
BHDQMED  DS    CL1                                                              
BHDQCLT  DS    CL3                                                              
BHDQPRD1 DS    CL3                                                              
BHDQPRD2 DS    CL3                                                              
BHDQEST  DS    CL3                                                              
BHDQCAMP DS    CL5                                                              
BHDQBYR  DS    CL3                                                              
         SPACE 1                                                                
MHDRECD  DSECT                                                                  
*                                                                               
MHDREC   DS    0CL165              MARKET HEADER RECORD                         
MHDRECTY DS    CL3                                                              
MHDCLT   DS    CL8                                                              
MHDREF   DS    CL8                                                              
MHDMKT   DS    CL4                                                              
MHDBYR   DS    CL3                                                              
         SPACE 1                                                                
SHDRECD  DSECT                                                                  
*                                                                               
SHDREC   DS    0CL165              STATION HEADER RECORD                        
SHDRECTY DS    CL3                                                              
SHDREF   DS    CL8                                                              
SHDMKT   DS    CL4                                                              
SHDSTA   DS    CL6                                                              
SHDREMK  DS    CL90                                                             
         SPACE 1                                                                
SSLRECD  DSECT                                                                  
*                                                                               
SSLREC   DS    0CL165              STATION SCHEDULE LINE RECORD                 
SSLRECTY DS    CL3                                                              
SSLREF   DS    CL8                                                              
SSLMKT   DS    CL4                                                              
SSLSTA   DS    CL6                                                              
SSLLINE  DS    CL4                                                              
SSLDAYS  DS    CL7                                                              
SSLSPCL  DS    CL3                                                              
SSLTIM1  DS    CL5                                                              
SSLTIM2  DS    CL5                                                              
SSLDPT   DS    CL1                                                              
SSLLEN   DS    CL3                                                              
SSLPRG   DS    CL17                                                             
SSLCOST  DS    CL9                                                              
SSLDEM1  DS    CL9                                                              
SSLDEM2  DS    CL9                                                              
SSLDEM3  DS    CL9                                                              
SSLDEM4  DS    CL9                                                              
SSLWKS   DS    CL(13*2)                                                         
SSLCMPST DS    CL6                                                              
SSLCMPND DS    CL6                                                              
SSLLEN2  DS    CL3                                                              
         SPACE 1                                                                
LCMRECD  DSECT                                                                  
*                                                                               
LCMREC   DS    0CL165              SCHEDULE LINE COMMENT RECORD                 
LCMRECTY DS    CL3                                                              
LCMREF   DS    CL8                                                              
LCMMKT   DS    CL4                                                              
LCMSTA   DS    CL6                                                              
LCMLINE  DS    CL4                                                              
LCMSEQ   DS    CL2                                                              
LCMTEXT  DS    CL60                                                             
         EJECT                                                                  
* SPNWSHDR                                                                      
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* SPEDICT                                                                       
         PRINT OFF                                                              
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080SPNWS17A  05/01/02'                                      
         END                                                                    
