*          DATA SET ACCLB09B   AT LEVEL 119 AS OF 12/22/99                      
*PHASE T62109B                                                                  
CLB09    TITLE '- BILL PROGRAM - INSERT EXTRA BILLING ITEM'                     
CLB09    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB9**,R8,R7,R6,CLEAR=YES,RR=RE                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
INIT     L     RC,AOVERWRK                                                      
         USING IWORKD,RC                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
*                                                                               
         SRL   RF,24                                                            
         LTR   RF,RF               TEST RETURN FROM NTRSES                      
         BZ    INIT02                                                           
         LA    RE,BASACTH                                                       
         ST    RE,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$PSRES)                                           
         B     EXIT                                                             
*                                                                               
INIT02   LA    R2,IWORKD                                                        
         LA    R3,IWORKL                                                        
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         SR    RE,RE               SET PREVIOUS SESSION RECORD/ACTION           
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   IRECACT,0(RE)                                                    
*                                                                               
         CLC   TWASCRN,CSSCRN      LOAD SCREEN IF FIRST TIME                    
         BE    VAL10                                                            
         GOTO1 AOVRSCR,BOPARM,(CSSCRN,BASOLAYH)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   BCPFKEY,PFKNEXTQ    TEST NEXT OF MULTI                           
         BNE   INIT04                                                           
         MVC   INSNAR1,OSSAVE                                                   
         CLC   INSNAR1,BCSPACES                                                 
         BNH   *+8                                                              
         MVI   INSNAR1H+(FVILEN-FVIHDR),L'INSNAR1                               
         MVC   INSNAR2,OSSAVE+(1*L'INSNAR1)                                     
         CLC   INSNAR2,BCSPACES                                                 
         BNH   *+8                                                              
         MVI   INSNAR2H+(FVILEN-FVIHDR),L'INSNAR1                               
         MVC   INSNAR3,OSSAVE+(2*L'INSNAR1)                                     
         CLC   INSNAR3,BCSPACES                                                 
         BNH   *+8                                                              
         MVI   INSNAR3H+(FVILEN-FVIHDR),L'INSNAR1                               
         MVC   INSNAR4,OSSAVE+(3*L'INSNAR1)                                     
         CLC   INSNAR4,BCSPACES                                                 
         BNH   *+8                                                              
         MVI   INSNAR4H+(FVILEN-FVIHDR),L'INSNAR1                               
*                                                                               
INIT04   CLI   CUCTRY2,CTRYGER     TEST GERMANY                                 
         BE    INIT06                                                           
         XC    INSFHRN,INSFHRN     REMOVE GERMAN FREEFORM HOURS FIELDS          
         OI    INSFHRSH+FHATD,FHATPR                                            
         XC    INSRA2N,INSRA2N                                                  
         OI    INSRAT2H+FHATD,FHATPR                                            
*                                                                               
INIT06   CLI   CUCTRY2,CTRYSCA     TEST SCANDINAVIA                             
         BE    INIT08                                                           
         XC    INSHRSN,INSHRSN     REMOVE SCANDINAVIAN WRITE-UP FIELDS          
         XC    INSWUCN,INSWUCN                                                  
         XC    INSWUHN,INSWUHN                                                  
         XC    INSRATN,INSRATN                                                  
         OI    INSALHRH+FHATD,FHATPR                                            
         OI    INSWUCMH+FHATD,FHATPR                                            
         OI    INSWUHRH+FHATD,FHATPR                                            
         OI    INSRATEH+FHATD,FHATPR                                            
*                                                                               
*NIT08   TM    BCCPYST7,CPYSSCNV   TEST EURO ZONE AGENCY                        
*        BO    INIT10              STARRED OUT - NEVER VALIDATED                
INIT08   DS    0H                                                               
*        XC    INSCURN,INSCURN     REMOVE CURRENCY CODE                         
         OI    INSCURH+FHATD,FHATPR                                             
         EJECT                                                                  
***********************************************************************         
*              DISPLAY CLIENT PRODUCT AND JOB                         *         
***********************************************************************         
         SPACE 1                                                                
INIT10   LA    R0,SINSAVE          CLEAR SAVED DATA                             
         LA    R1,SINSAVL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
BOW      USING ACTRECD,BOWORK1                                                  
         MVC   BOW.ACTKEY,BCSPACES                                              
         MVC   BOW.ACTKCPY,CUABIN                                               
         MVC   BOW.ACTKUNT(L'BCCPYPRD),BCCPYPRD                                 
         MVC   BOW.ACTKACT,BCJOBCOD                                             
         GOTO1 VACSRCHC,BOPARM,INSCLIH,ATWA,(BCCLILEN,0),              *        
               (X'C0',19),BOW.ACTKEY,(L'BCCLINAM,BCCLINAM)                      
         GOTO1 VACSRCHC,BOPARM,INSPROH,ATWA,(BCPROLEN,BCCLILEN),       *        
               (X'C0',19),BOW.ACTKEY,(L'BCPRONAM,BCPRONAM)                      
         GOTO1 VACSRCHC,BOPARM,INSJOBH,ATWA,(BCJOBLEN,BCPROLEN),       *        
               (X'C0',19),BOW.ACTKEY,(L'BCJOBNAM,BCJOBNAM)                      
*        TM    INSCURH+FHATD,FHATPR                                             
*        BO    *+10                                                             
         MVC   INSCUR,CSBILCUR                                                  
         DROP  BOW                                                              
*                                                                               
         LA    R2,INSSUPCH         SET EXIT MESSAGE                             
         ST    R2,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
*                                                                               
         CLI   IACT,ACTSET         TEST ENTERED DIRECT FROM SETUP               
         BE    EXIT                YES - LEAVE CLIENT/PRODUCT/JOB OPEN          
         OI    INSCLIH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    INSPROH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    INSJOBH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         SR    RF,RF               TEST WHETHER SUBACTION WAS RECALL            
         ICM   RF,3,CSSELCUR                                                    
         A     RF,AOVERSEL                                                      
         CLC   =Y(UC@RECAL-TWAD),0(RF)                                          
         BNE   EXIT                                                             
*                                  PROTECT KEY FLDS AND SET AS VALID            
         OI    INSSUPCH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    INSSUPCH+(FVIIND-FVIHDR),FVIVAL                                  
         OI    INSWCH+(FVATRB-FVIHDR),FVAPROT                                   
         OI    INSWCH+(FVIIND-FVIHDR),FVIVAL                                    
         OI    INSREFH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    INSREFH+(FVIIND-FVIHDR),FVIVAL                                   
         OI    INSDATEH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    INSDATEH+(FVIIND-FVIHDR),FVIVAL                                  
         OI    INSDRFTH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    INSFRMTH+(FVATRB-FVIHDR),FVAPROT                                 
*                                  DISPLAY THE ADVANCE IN IO1                   
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,TRNKCULC                                                
         DROP  RF                                                               
*                                  SUPPLIER CODE                                
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         GOTO1 VACSRCHC,BOPARM,INSSUPCH,ATWA,(C'*',SV),19,AIO2                  
         MVC   SINSUPP,TRNKULA                                                  
*                                  WORKCODE                                     
         GOTO1 AGETWRK,TRNKWORK                                                 
         BNE   EXITN                                                            
         MVI   SINWCSTA,0          CLEAR SAVED WORKCODE STATUS                  
         L     R1,AIO2             GETWRK LEAVES WORKCODE RECORD IN IO2         
         CLI   WCOEL-WCOELD(R1),WCOELQ                                          
         BNE   *+10                                                             
         MVC   SINWCSTA,(WCOSTAT-WCOELD)+(WCORFST-WCORECD)(R1)                  
         GOTO1 VACSRCHC,BOPARM,INSWCH,ATWA,WC,19,AIO2                           
         MVC   SINWC,TRNKWORK                                                   
*                                  REFERENCE                                    
         MVC   INSREF,TRNKREF                                                   
*                                  DATE                                         
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,INSDATE)                         
         MVC   SINDATE,TRNKDATE                                                 
*                                                                               
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         SH    RE,=Y(TRNLN1Q)                                                   
         BCT   RE,*+8                                                           
         B     INIT12              NO NARRATIVE                                 
         EX    RE,*+4                                                           
         MVC   BOELEM(0),TRNNARR                                                
         MVC   INSNAR1,BOELEM+(0*L'INSNAR1)                                     
         MVC   INSNAR2,BOELEM+(1*L'INSNAR1)                                     
         MVC   INSNAR3,BOELEM+(2*L'INSNAR1)                                     
         MVC   INSNAR4,BOELEM+(3*L'INSNAR1)                                     
*                                                                               
INIT12   GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('PTAELQ',TRNRECD),0                 
         CLI   BOPARM+12,0                                                      
         BNE   EXIT                                                             
         L     R3,BOPARM+12                                                     
         USING PTAELD,R3                                                        
         CLI   PTATYPE,PTATRAL                                                  
         BE    *+14                                                             
         CLI   PTATYPE,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   SINNET,PTANET                                                    
         CLC   CSCPYCUR,PTACUR                                                  
         BE    INIT14                                                           
         CLC   BCCPYSEC,PTACUR                                                  
         BE    INIT14                                                           
         ZAP   SINNET,PTANETF                                                   
INIT14   CURED SINNET,(14,INSALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         ZAP   SINCOM,PTARCOM                                                   
         CLC   CSCPYCUR,PTACUR                                                  
         BE    INIT16                                                           
         CLC   BCCPYSEC,PTACUR                                                  
         BE    INIT16                                                           
         CLC   PTACUR,BCSPACES                                                  
         BE    INIT16                                                           
         ZAP   SINCOM,PTARFCOM                                                  
INIT16   CURED SINCOM,(14,INSALCM),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
*                                                                               
         XC    SINHOURS,SINHOURS                                                
         XC    SINFRHRS,SINFRHRS                                                
         CLI   CUCTRY2,CTRYGER     TEST GERMANY                                 
         BNE   *+12                                                             
         CLI   CUCTRY2,CTRYSCA     TEST SCANDINAVIA                             
         BNE   *+4                                                              
         XC    SINWUCOM,SINWUCOM                                                
         XC    SINWUHRS,SINWUHRS                                                
         XC    SINRATE,SINRATE                                                  
         XC    SINFHRAT,SINFHRAT                                                
         MVC   SINDA,TLDA          SAVE THIS RECORD DISK ADDRESS                
         LA    R1,INSALNTH                                                      
         ST    R1,FVADDR                                                        
*                                                                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE ITEM DETAILS                                  *         
***********************************************************************         
         SPACE 1                                                                
VAL10    LA    R1,INSCLIH          TEST ANY SCREEN INPUT                        
         XR    RF,RF                                                            
VAL12    ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    VAL14                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BO    *+12                                                             
         TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BZ    VAL16                                                            
         BXH   R1,RF,VAL12                                                      
VAL14    L     RF,AINP             KEEP CURSOR & MESSAGE AS THEY WERE           
         USING TIOBD,RF                                                         
         MVC   TIOBCURS,CSCURDSP                                                
         XC    TIOBCURD,TIOBCURD                                                
         OI    TIOBINDS,TIOBSETC                                                
         DROP  RF                                                               
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     EXIT                                                             
*                                                                               
VAL16    MVI   IKEYINP,C'N'        SET NO KEY CHANGE                            
         TM    INSCLIH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VAL26                                                            
         NI    CSINDSG1,FF-(CSINDSET)                                           
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
*                                                                               
         GOTO1 VACSRCHC,BOPARM,(4,INSCLIH),ATWA,BCCPYPRD,ACOM,         *        
               (X'11',0)                                                        
         MVI   FVMINL,1            CLIENT REQUIRED                              
         MVC   FVMAXL,BCCLILEN                                                  
         GOTO1 AFVAL,INSCLIH                                                    
         BNE   EXITNC                                                           
         OC    FVIFLD,BCSPACES                                                  
         IC    R1,BCCLILEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ACTKACT(0),FVIFLD   SET CLIENT PORTION OF KEY                    
         TM    FVIIND,FVIVAL       TEST PREVIOUSLY VALIDATED                    
         BO    VAL18                                                            
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         OI    INSCLIH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    INSPROH+(FVIIND-FVIHDR),FF-(FVIVAL)                              
*                                                                               
VAL18    GOTO1 VACSRCHC,BOPARM,(4,INSPROH),ATWA,BCCPYPRD,              *        
               (BCCLILEN,ACOM),(X'22',ACTKACT)                                  
         MVI   FVMINL,1            PRODUCT REQUIRED                             
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         SR    RE,RF                                                            
         STC   RE,FVMAXL                                                        
         GOTO1 AFVAL,INSPROH                                                    
         BNE   EXITNC                                                           
         OC    FVIFLD,BCSPACES                                                  
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         SR    RE,RF                                                            
         LA    RF,ACTKACT(RF)                                                   
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      SET PRODUCT PORTION OF KEY                   
         TM    FVIIND,FVIVAL       TEST PREVIOUSLY VALIDATED                    
         BO    VAL20                                                            
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         OI    INSPROH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    INSJOBH+(FVIIND-FVIHDR),FF-(FVIVAL)                              
*                                                                               
VAL20    GOTO1 VACSRCHC,BOPARM,(4,INSJOBH),ATWA,BCCPYPRD,              *        
               (BCPROLEN,ACOM),(X'33',ACTKACT)                                  
         MVI   FVMINL,1            JOB REQUIRED                                 
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         SR    RF,RF                                                            
         IC    RF,BCJOBLEN                                                      
         SR    RE,RF                                                            
         STC   RE,FVMAXL                                                        
         GOTO1 AFVAL,INSJOBH                                                    
         BNE   EXITNC                                                           
         OC    FVIFLD,BCSPACES                                                  
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         SR    RE,RE                                                            
         IC    RE,BCJOBLEN                                                      
         SR    RE,RF                                                            
         LA    RF,ACTKACT(RF)                                                   
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      SET JOB PORTION OF KEY                       
         TM    FVIIND,FVIVAL       TEST PREVIOUSLY VALIDATED                    
         BO    VAL22                                                            
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         OI    INSJOBH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VAL22    MVC   IEXCVAL,CSEXCVAL    PRESERVE EXCHANGE RATE                       
         MVC   ITYPEXC,CSTYPEXC                                                 
         MVC   IDATEXC,CSDATEXC                                                 
         GOTO1 ASETUP,BOPARM,(X'F8',ACTKACT),0,IEXCVAL                          
         BNE   EXITNC                                                           
         MVC   CSTYPEXC,ITYPEXC                                                 
         MVC   CSDATEXC,IDATEXC                                                 
         GOTO1 VACSRCHC,BOPARM,INSCLIH,ATWA,(BCCLILEN,0),              *        
               (X'C0',19),ACCODE,(L'BCCLINAM,BCCLINAM)                          
         GOTO1 VACSRCHC,BOPARM,INSPROH,ATWA,(BCPROLEN,BCCLILEN),       *        
               (X'C0',19),ACCODE,(L'BCPRONAM,BCPRONAM)                          
         GOTO1 VACSRCHC,BOPARM,INSJOBH,ATWA,(BCJOBLEN,BCPROLEN),       *        
               (X'C0',19),ACCODE,(L'BCJOBNAM,BCJOBNAM)                          
         OI    CSINDSG1,CSINDSET                                                
         MVC   INSCUR,CSBILCUR                                                  
         OI    INSCURH+FHOID,FHOITR                                             
         DROP  R2                                                               
*                                                                               
         CLC   ILASTJOB,BCJOBCOD   TEST JOB HAS CHANGED                         
         BE    VAL26                                                            
         LA    R1,INSSUPCH         YES - SET ALL FIELDS UNVALIDATED             
         SR    RF,RF                                                            
         ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+12                                                             
         NI    FVIIND-FVIHDR(R1),FF-FVIVAL                                      
         BXH   R1,RF,*-12                                                       
*                                                                               
         GOTO1 AFVAL,INSSUPCH      TEST SUPPLIER                                
         CLI   FVIFLD,C'*'         = *SJ + LAST JOB                             
         BNE   VAL26                                                            
         CLC   FVIFLD+1(2),BCCPYPRD                                             
         BNE   VAL26                                                            
         CLC   FVIFLD+3(L'ILASTJOB),ILASTJOB                                    
         BNE   VAL26                                                            
         MVC   INSSUPC+3(L'BCJOBCOD),BCJOBCOD CHANGE TO                         
         OI    INSSUPCH+FHOID,FHOITR           *SJ + CURRENT JOB                
*                                                                               
VAL26    MVC   ILASTJOB,BCJOBCOD                                                
         GOTO1 VACSRCHC,BOPARM,(4,INSSUPCH),ATWA,(C'*',SV),ACOM,0               
         GOTO1 AFVAL,INSSUPCH                                                   
         BE    VAL28                                                            
         CLI   INSFHRSH+(FVILEN-FVIHDR),0                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IVPER)                                           
         B     EXITN               FREEFORM HOURS MUST HAVE 1R C/A              
         MVC   SINSUPP(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   SINSUPP+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                           
         MVI   INSSUPC,C'*'                                                     
         MVC   INSSUPC+1(L'SINSUPP),SINSUPP                                     
         OI    INSSUPCH+FVOIND-FVIHDR,FVOXMT                                    
         GOTO1 AFVAL,INSSUPCH                                                   
*                                                                               
VAL28    CLI   INSFHRSH+(FVILEN-FVIHDR),0                                       
         BE    VAL30                                                            
         CLC   =C'*1R',FVIFLD      TEST C/A OVERRIDE TO 1R                      
         BE    VAL30                                                            
         MVC   FVMSGNO,=AL2(AE$IVPER)                                           
         B     EXITN               FREEFORM HOURS MUST HAVE 1R C/A              
VAL30    TM    FVIIND,FVIVAL                                                    
         BO    *+8                                                              
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         OI    INSSUPCH+FVOIND-FVIHDR,FVOXMT                                    
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,C'S'        DEFAULT UNIT/LEDGER IS SV                    
         MVI   ACTKLDG,C'V'                                                     
         MVC   ACTKACT,FVIFLD                                                   
         CLI   FVIFLD,C'*'         INDICATES UNIT/LEDGER OVERRIDDEN             
         BNE   VAL32                                                            
         MVC   ACTKUNT,FVIFLD+1                                                 
         MVC   ACTKLDG,FVIFLD+2                                                 
         MVC   ACTKACT,FVIFLD+3                                                 
         DROP  RF                                                               
VAL32    GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         TM    ACBSTAT,ACBSABAL                                                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     EXITN                                                            
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     EXITN                                                            
         MVC   SINSUPN,ACNAME                                                   
         MVC   SINSUPP,ACCODE+1                                                 
         GOTO1 VACSRCHC,BOPARM,INSSUPCH,ATWA,(C'*',SV),(X'40',19),AIO1,*        
               (L'SINSUPN,SINSUPN)                                              
*                                                                               
VAL36    GOTO1 VACSRCHC,BOPARM,(4,INSWCH),ATWA,WC,ACOM,BCCPYPRD                 
         MVI   FVMINL,1            WORKCODE - REQUIRED INPUT                    
         GOTO1 AFVAL,INSWCH                                                     
         BNE   EXIT                                                             
         TM    FVIIND,FVIVAL                                                    
         BO    *+8                                                              
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         CLC   FVIFLD(2),=C'99'                                                 
         BE    *+14                                                             
         CLC   FVIFLD(2),=C'**'                                                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITN                                                            
         OI    INSWCH+FVOIND-FVIHDR,FVOXMT                                      
         GOTO1 AGETWRK,FVIFLD                                                   
         BNE   EXITN                                                            
         MVI   SINWCSTA,0          CLEAR SAVED WORKCODE STATUS                  
         L     R1,AIO2             GETWRK LEAVES WORKCODE RECORD IN IO2         
         LA    R1,WCORFST-WCORECD(R1)                                           
         USING WCOELD,R1                                                        
         CLI   WCOEL,WCOELQ                                                     
         BNE   *+10                                                             
         MVC   SINWCSTA,WCOSTAT                                                 
         TM    SINWCSTA,WCOSHCOE           TEST TIME WORKCODE                   
         BO    VAL37                                                            
         CLI   INSFHRSH+(FVILEN-FVIHDR),0  NO - THEN CAN'T HAVE HOURS           
         BE    VAL37                                                            
         MVC   FVMSGNO,=AL2(AE$MUTWC)      MUST BE TIME WORKCODE                
         B     EXITN                                                            
VAL37    MVC   SINWC,FVIFLD                                                     
         GOTO1 VACSRCHC,BOPARM,INSWCH,ATWA,WC,19,AIO2                           
*                                                                               
VAL38    MVI   FVMINL,1            REFERENCE - REQUIRED INPUT                   
         GOTO1 AFVAL,INSREFH                                                    
         BNE   EXIT                                                             
         TM    FVIIND,FVIVAL                                                    
         BO    VAL40                                                            
         OI    INSREFH+FVOIND-FVIHDR,FVOXMT                                     
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         MVC   SINREF,FVIFLD                                                    
*                                                                               
VAL40    MVI   FVMINL,1            DATE - REQUIRED INPUT                        
         GOTO1 AFVAL,INSDATEH                                                   
         TM    FVIIND,FVIVAL                                                    
         BO    VAL42                                                            
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         GOTO1 AVALDAT                                                          
         BNE   EXITN                                                            
         MVC   SINDATE,BCWORK+2                                                 
*                                                                               
VAL42    ZAP   SINRATE,BCPZERO     RATE (IF HOURS IN USE)                       
         NI    SININD1,FF-SIN1RAT  CLEAR NON-ZERO RATE                          
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSRATEH                                                   
         BH    EXITN                                                            
         BL    VAL44                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         TM    SINWCSTA,WCOSHCOE   TEST HOURS WORKCODE                          
         BZ    EXITN                                                            
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   SINRATE,BODMCB+4(8)                                              
         BZ    *+8                                                              
         OI    SININD1,SIN1RAT     NON-ZERO RATE PRESENT                        
         CURED SINRATE,(8,INSRATE),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSRATEH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL44    MVI   FVMINL,0            VALIDATE DRAFT BILL REQUIREMENT              
         GOTO1 AFVAL,INSDRFTH                                                   
         BH    EXITN                                                            
         MVC   BOBYTE1,SINDRF      SAVE DRAFT BILL REQUIREMENT                  
         BL    VAL48                                                            
         OC    FVIFLD,BCSPACES                                                  
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BNE   VAL46                                                            
         CLC   FVIFLD(0),0(RE)                                                  
         NI    SINDRF,FF-SINDRFY   DRAFT BILL NOT REQUIRED                      
         B     VAL48                                                            
VAL46    LH    RE,=Y(UC@YES-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BNE   EXITN                                                            
         CLC   FVIFLD(0),0(RE)                                                  
         OI    SINDRF,SINDRFY      ADD DRAFT BILL AUTOMATICALLY                 
VAL48    LH    RE,=Y(UC@YES-TWAD)                                               
         TM    SINDRF,SINDRFY                                                   
         BO    *+8                                                              
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         MVC   INSDRFT(L'UC@YES),0(RE)                                          
         OI    INSDRFTH+(FVOIND-FVIHDR),FVOXMT                                  
         CLC   SINDRF,BOBYTE1      IF DRAFT REQUIREMENT JUST CHANGED            
         BE    *+8                                                              
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
*                                                                               
         MVC   SINFORMT,CSFORMAT   PRE-SET OVERRIDE WITH DEFAULT                
         GOTO1 AFVAL,INSFRMTH                                                   
         BL    VAL49                                                            
         TM    FVIIND,FVINUM       TEST INPUT IS NUMERIC                        
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NONIF)                                           
         B     EXITN                                                            
         MVC   SINFORMT,BCFULL+3   SAVE INPUT FORMAT CODE                       
*                                                                               
         MVC   INSFRMD,BCSPACES                                                 
         OI    INSFRMDH+FHOID,FHOITR                                            
         USING PBCRECD,IOKEY       READ FORMAT CONTROL RECORD                   
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,SINFORMT                                                 
         MVC   PBCKLANG,CSFMLANG                                                
         GOTO1 AIO,IO3+IOACCDIR+IORD                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITN                                                            
         MVC   IODAOVER,PBCKDA     OUTPUT FORMAT NAME                           
         GOTO1 AIO,IO3+IOACCMST+IOGET                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         LA    R1,PBCRFST-PBCRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R1,RF,*-12                                                       
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   INSFRMD(0),NAMEREC                                               
*                                                                               
VAL49    XC    SINFRHRS,SINFRHRS   VALIDATE FREEFORM HOURS (OPTIONAL)           
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSFHRSH                                                   
         BNE   VAL50                                                            
         TM    SINWCSTA,WCOSHCOE                                                
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(2,FVIFLD),(X'80',(RF))                          
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         L     RF,4(R1)                                                         
         C     RF,MAXHOUR          CHECK HOURS FIT IN A HALFWORD                
         BH    *+12                                                             
         C     RF,MINHOUR                                                       
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         STH   RF,SINFRHRS         EXTRACT BINARY VALUE FOR HOURS               
         CURED SINFRHRS,(L'INSFHRS,INSFHRS),2,DMCB=BODMCB,ALIGN=LEFT,  X        
               MINUS=YES                                                        
         OI    INSFHRSH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL50    ZAP   SINFHRAT,BCPZERO    RATE (IF HOURS IN USE)                       
         NI    SININD1,FF-SIN1NET  CLEAR NON-ZERO NET                           
         ZAP   SINNET,BCPZERO                                                   
         ZAP   SINCOM,BCPZERO                                                   
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSALNTH                                                   
         BH    EXITN                                                            
         BL    VAL52                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   SINNET,BODMCB+4(8)                                               
         BZ    *+8                                                              
         OI    SININD1,SIN1NET     NON-ZERO NET PRESENT                         
         CURED SINNET,(14,INSALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALNTH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
         ZAP   SINFHRAT,BCPZERO    RATE (IF HOURS IN USE)                       
         OC    SINFRHRS,SINFRHRS   TEST FREEFORM HOURS (GERMANY)                
         BZ    VAL52                                                            
         LH    R1,SINFRHRS         CALCULATE RATE=NET/HOURS                     
         CVD   R1,BODUB1                                                        
         ZAP   BOPL81(16),SINNET                                                
         SRP   BOPL81(16),4,0                                                   
         DP    BOPL81(16),BODUB1                                                
         SRP   BOPL81,64-2,5                                                    
         ZAP   SINFHRAT,BOPL81                                                  
         CURED SINFHRAT,(8,INSRAT2),CSCURBIL,ALIGN=LEFT,MINUS=YES,     X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSRAT2H+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL52    NI    SININD1,FF-SIN1COM  CLEAR NON-ZERO COMMISSION                    
         GOTO1 AFVAL,INSALCMH                                                   
         BNE   VAL54                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN                                                            
         ZAP   SINCOM,BODMCB+4(8)                                               
         BZ    *+8                                                              
         OI    SININD1,SIN1COM     NON-ZERO COMMISSION PRESENT                  
         CURED SINCOM,(14,INSALCM),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALCMH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL54    XC    SINHOURS,SINHOURS   VALIDATE HOURS (OPTIONAL)                    
         NI    SININD1,FF-SIN1HRS  CLEAR NON-ZERO HOURS                         
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSALHRH                                                   
         BNE   VAL56                                                            
         TM    SINWCSTA,WCOSHCOE                                                
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(2,FVIFLD),(X'80',(RF))                          
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         L     RF,4(R1)                                                         
         C     RF,MAXHOUR          CHECK HOURS FIT IN A HALFWORD                
         BH    *+12                                                             
         C     RF,MINHOUR                                                       
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         STH   RF,SINHOURS         EXTRACT BINARY VALUE FOR HOURS               
         OC    SINHOURS,SINHOURS                                                
         BZ    *+8                                                              
         OI    SININD1,SIN1HRS     NON-ZERO HOURS PRESENT                       
         CURED SINHOURS,(L'INSALHR,INSALHR),2,DMCB=BODMCB,ALIGN=LEFT,  X        
               MINUS=YES                                                        
         OI    INSALHRH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL56    TM    SININD1,SIN1NET+SIN1HRS+SIN1RAT                                  
         BZ    VAL66               RATE/NET/HOURS ALL ABSENT                    
         BO    VAL66               RATE/NET/HOURS ALL PRESENT                   
         TM    SININD1,SIN1NET+SIN1HRS                                          
         BNO   VAL58                                                            
         LH    R1,SINHOURS         CALCULATE RATE=NET/HOURS                     
         CVD   R1,BODUB1                                                        
         ZAP   BOPL81(16),SINNET                                                
         SRP   BOPL81(16),4,0                                                   
         DP    BOPL81(16),BODUB1                                                
         SRP   BOPL81,64-2,5                                                    
         ZAP   SINRATE,BOPL81                                                   
         BZ    *+8                                                              
         OI    SININD1,SIN1RAT     NON-ZERO RATE DERIVED                        
         CURED SINRATE,(8,INSRATE),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSRATEH+FVOIND-FVIHDR,FVOXMT                                    
         B     VAL66                                                            
*                                                                               
VAL58    TM    SININD1,SIN1NET+SIN1RAT                                          
         BNO   VAL60                                                            
         ZAP   BOPL81(16),SINNET   CALCULATE HOURS=NET/RATE                     
         SRP   BOPL81(16),4,0                                                   
         DP    BOPL81(16),SINRATE                                               
         SRP   BOPL81,64-2,5                                                    
         ZAP   BODUB1,BOPL81                                                    
         CVB   R1,BODUB1                                                        
         STH   R1,SINHOURS                                                      
         OC    SINHOURS,SINHOURS                                                
         BZ    *+8                                                              
         OI    SININD1,SIN1HRS     NON-ZERO HOURS DERIVED                       
         CURED SINHOURS,(L'INSALHR,INSALHR),2,DMCB=BODMCB,ALIGN=LEFT,  X        
               MINUS=YES                                                        
         OI    INSALHRH+FVOIND-FVIHDR,FVOXMT                                    
         B     VAL66                                                            
*                                                                               
VAL60    TM    SININD1,SIN1HRS+SIN1RAT                                          
         BNO   VAL62                                                            
         LH    R1,SINHOURS         CALCULATE NET=HOURS*RATE                     
         CVD   R1,BODUB1                                                        
         ZAP   BOPL81(16),BODUB1                                                
         MP    BOPL81(16),SINRATE                                               
         SRP   BOPL81(16),64-2,5                                                
         ZAP   SINNET,BOPL81(16)                                                
         BZ    *+8                                                              
         OI    SININD1,SIN1NET     NON-ZERO NET DERIVED                         
         CURED SINNET,(8,INSALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,       X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALNTH+FVOIND-FVIHDR,FVOXMT                                    
         B     VAL66                                                            
*                                                                               
VAL62    TM    SININD1,SIN1NET     NET ALONE IS OK                              
         BO    VAL68                                                            
         LA    RF,INSRATEH         CANNOT HAVE RATE ALONE                       
         TM    SININD1,SIN1RAT                                                  
         BO    VAL64                                                            
         LA    RF,INSALHRH         CANNOT HAVE HOURS ALONE                      
         TM    SININD1,SIN1HRS                                                  
         BO    VAL64                                                            
         DC    H'0'                TEST SININD1 IN DUMP                         
VAL64    STCM  RF,15,FVADDR                                                     
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
*                                                                               
VAL66    TM    SININD1,SIN1NET+SIN1COM                                          
         BNZ   VAL68                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         LA    RF,INSALNTH         NET OR COMM MUST BE NON-ZERO                 
         STCM  RF,15,FVADDR                                                     
         B     EXITN                                                            
*                                                                               
VAL68    ZAP   SINWUCOM,BCPZERO    VALIDATE WRITE-UP COMMISSION/HOURS           
         NI    SININD1,FF-SIN1WUC  CLEAR NON-ZERO WRITE-UP COMMISSION           
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSWUCMH                                                   
         BH    EXITN                                                            
         BL    VAL70                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   SINWUCOM,BODMCB+4(8)                                             
         BZ    *+8                                                              
         OI    SININD1,SIN1WUC     NON-ZERO WRITE-UP COMMISSION                 
         CURED SINWUCOM,(14,INSWUCM),CSCURBIL,ALIGN=LEFT,MINUS=YES,    X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSWUCMH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL70    XC    SINWUHRS,SINWUHRS   VALIDATE WRITE-UP HOURS (OPTIONAL)           
         NI    SININD1,FF-SIN1WUH  CLEAR NON-ZERO WRITE-UP HOURS                
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSWUHRH                                                   
         BNE   VAL72                                                            
         TM    SINWCSTA,WCOSHCOE                                                
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(2,FVIFLD),(X'80',(RF))                          
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         L     RF,4(R1)                                                         
         C     RF,MAXHOUR          CHECK HOURS FIT IN A HALFWORD                
         BH    *+12                                                             
         C     RF,MINHOUR                                                       
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         STH   RF,SINWUHRS         EXTRACT BINARY VALUE FOR HOURS               
         OC    SINWUHRS,SINWUHRS                                                
         BZ    *+8                                                              
         OI    SININD1,SIN1WUH     NON-ZERO WRITE-UP HOURS PRESENT              
         CURED SINWUHRS,(L'INSWUHR,INSWUHR),2,DMCB=BODMCB,ALIGN=LEFT,  X        
               MINUS=YES                                                        
         OI    INSWUHRH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL72    TM    SININD1,SIN1WUC+SIN1WUH                                          
         BZ    VAL80               WRITE-UP COMM/HOURS BOTH ABSENT              
         BO    VAL80               WRITE-UP COMM/HOURS BOTH PRESENT             
         TM    SININD1,SIN1RAT     TEST RATE KNOWN                              
         BO    VAL74                                                            
         TM    SININD1,SIN1WUC     TEST WRITE-UP COMMISSION ONLY                
         BO    VAL80               OK                                           
         TM    SININD1,SIN1WUH     TEST WRITE-UP HOURS ONLY                     
         BO    *+6                 ERROR                                        
         DC    H'0'                TEST SININD1 IN DUMP                         
         LA    RF,INSWUHRH                                                      
         STCM  RF,15,FVADDR                                                     
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
*                                                                               
VAL74    TM    SININD1,SIN1WUC                                                  
         BZ    VAL76                                                            
         ZAP   BOPL81(16),SINWUCOM WRITE-UP HOURS=WRITE-UP COMM/RATE            
         SRP   BOPL81(16),4,0                                                   
         DP    BOPL81(16),SINRATE                                               
         SRP   BOPL81,64-2,5                                                    
         ZAP   BODUB1,BOPL81                                                    
         CVB   R1,BODUB1                                                        
         STH   R1,SINWUHRS                                                      
         OC    SINWUHRS,SINWUHRS                                                
         BZ    *+8                                                              
         OI    SININD1,SIN1WUH     NON-ZERO WRITE-UP HOURS DERIVED              
         CURED SINWUHRS,(L'INSWUHR,INSWUHR),2,DMCB=BODMCB,ALIGN=LEFT,  X        
               MINUS=YES                                                        
         OI    INSWUHRH+FVOIND-FVIHDR,FVOXMT                                    
         B     VAL80                                                            
*                                                                               
VAL76    TM    SININD1,SIN1WUH                                                  
         BO    *+6                                                              
         DC    H'0'                CHECK SININD1 IN DUMP                        
         LH    R1,SINWUHRS         WRITE-UP COMM=WRITE-UP HOURS*RATE            
         CVD   R1,BODUB1                                                        
         ZAP   BOPL81(16),BODUB1                                                
         MP    BOPL81(16),SINRATE                                               
         SRP   BOPL81(16),64-2,5                                                
         ZAP   SINWUCOM,BOPL81(16)                                              
         BZ    *+8                                                              
         OI    SININD1,SIN1WUC     NON-ZERO WRITE-UP COMMISSION DERIVED         
         CURED SINWUCOM,(8,INSWUCM),CSCURBIL,ALIGN=LEFT,MINUS=YES,     X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSWUCMH+FVOIND-FVIHDR,FVOXMT                                    
         B     VAL80                                                            
*                                                                               
VAL80    MVC   SINNAR1,BCSPACES                                                 
         MVC   SINNAR2,BCSPACES                                                 
         MVC   SINNAR3,BCSPACES                                                 
         MVC   SINNAR4,BCSPACES                                                 
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSNAR1H                                                   
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    VAL82                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SINNAR1(0),FVIFLD                                                
         LA    RF,1(RF)                                                         
         STC   RF,SINNARL                                                       
VAL82    MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSNAR2H                                                   
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    VAL84                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SINNAR2(0),FVIFLD                                                
         LA    RF,L'INSNAR1+1(RF)                                               
         STC   RF,SINNARL                                                       
VAL84    MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSNAR3H                                                   
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    VAL86                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SINNAR3(0),FVIFLD                                                
         LA    RF,L'INSNAR1+L'INSNAR2+1(RF)                                     
         STC   RF,SINNARL                                                       
VAL86    MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSNAR4H                                                   
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    VAL90                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SINNAR4(0),FVIFLD                                                
         LA    RF,L'INSNAR1+L'INSNAR2+L'INSNAR3+1(RF)                           
         STC   RF,SINNARL                                                       
VAL90    MVC   OSSAVE(4*L'INSNAR1),SINNAR1                                      
         B     ADD10                                                            
         EJECT                                                                  
***********************************************************************         
*              ADD DRAFT TRANSACTION RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
ADD10    CLI   IKEYINP,C'Y'        TEST IF RECORD ALREADY THERE                 
         BNE   CHA10               YES - READ AND UPDATE IT                     
         L     R4,AIO3             BUILD RECORD IN IO3                          
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,SINWC                                                   
         MVC   TRNKCCPY,CUABIN                                                  
         MVC   TRNKULC,SINSUPP                                                  
         MVC   TRNKDATE,SINDATE                                                 
         MVC   TRNKREF,SINREF                                                   
*                                                                               
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVC   TRNDATE,SINDATE                                                  
         MVC   TRNREF,SINREF                                                    
         MVI   TRNTYPE,99          ***** TEMP *****                             
         MVC   TRNMOS,BCTMON       SET TODAYS MOA                               
         MVC   TRNBREF,BCSPACES    SET NULL BATCH REFERENCE                     
         MVI   TRNSTAT,TRNSDR+TRNSAUTH                                          
         MVC   TRNANAL,SINWC                                                    
         ZAP   TRNAMNT,BCPZERO                                                  
         SR    RE,RE                                                            
         ICM   RE,1,SINNARL                                                     
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNNARR(0),SINNAR1                                               
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         LA    RF,TRNEL(RE)                                                     
         MVI   0(RF),0             SET E-O-R                                    
         SR    RF,R4                                                            
         LA    RF,1(RF)                                                         
         STCM  RF,3,TRNRLEN        SET RECORD LENGTH                            
*&&UK                                                                           
         TM    BCCPYST7,CPYSSCNV   TEST CONVERTED FOR 2ND CURRENCY              
         BZ    ADD20                                                            
         PUSH  USING                                                            
         USING OCAELD,BOELEM       ADD EMPTY OCAELD                             
         MVI   OCAEL,OCAELQ                                                     
         MVI   OCALN,OCALN1Q                                                    
         MVI   OCAINDS,0                                                        
         CLC   CSBILCUR,BCCPYSEC   TEST IN SECONDARY CURRENCY                   
         BNE   *+12                                                             
         OI    OCAINDS,OCAIDSEC                                                 
         OI    LSINDS2,LSTOBACC                                                 
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),TRNRECD,OCAELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*&&                                                                             
ADD20    BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
*                                                                               
         L     R4,AADTBLK                                                       
         USING ADDTRND,R4                                                       
         GOTO1 AINIADT                                                          
         MVI   TRNINDS,TRNIDRFT+TRNICONV                                        
         MVC   TRNCACNM,SINSUPN    CONTRA-ACCOUNT NAME                          
         GOTO1 VADDTRN,ADDTRND                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNINDS,TRNILAST                                                 
         GOTO1 VADDTRN,ADDTRND                                                  
         L     RE,AIO3                                                          
         AH    RE,=Y(2000-4)                                                    
         MVC   SINDA,0(RE)         SAVE DISK ADDRESS OF TRANSACTION             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              INSERT NEW ITEM IN THE LIST                            *         
***********************************************************************         
         SPACE 1                                                                
INS10    CLI   IACT,ACTALC         ONLY IF FROM ALLOCATE                        
         BNE   INS30                                                            
*NS10    CLI   IACT,ACTSET         NOT IF FROM SETUP                            
*        BE    INS30                                                            
*        CLI   IACT,ACTJLI         NOT IF FROM JOB LIST                         
*        BE    INS30                                                            
*                                                                               
         XC    IIOKEY,IIOKEY                                                    
         L     RE,AIO3                                                          
I        USING TRNRECD,IIOKEY                                                   
         MVC   I.TRNKEY,0(RE)                                                   
         MVC   I.TRNKSTA,TRNRSTA-TRNKEY(RE)                                     
         MVC   I.TRNKDA,SINDA                                                   
         DROP  I                                                                
         GOTO1 ASETTRN,BOPARM,(C'S',IIOKEY),AIO3,LSPRATA                        
         IC    RE,TWASESNL                                                      
         BCTR  RE,0                                                             
         STC   RE,TLKSES                                                        
I        USING TLSTD,ILST                                                       
         MVC   I.TLKEY,TLKEY                                                    
         MVC   I.TLKSEQ,BCEFFS     GET RECORD AFTER INSERTION                   
         GOTO1 ATSARIO,BOPARM,('TSARDH',I.TLSTD)                                
         BL    *+10                END-OF-FILE                                  
         BH    INS12                                                            
         DC    H'0'                THIS KEY SHOULD NOT EXIST                    
         MVC   I.TLNUM,CSHIRECN                                                 
         B     INS14                                                            
INS12    SR    RE,RE                                                            
         ICM   RE,3,I.TLNUM                                                     
         BCT   RE,*+4                                                           
         B     INS16                                                            
         STCM  RE,3,I.TLNUM                                                     
*                                                                               
INS14    GOTO1 ATSARIO,BOPARM,('TSAGET',I.TLSTD)  GET 'BEFORE' RECORD           
         CLC   I.TLKEY(TLKSEQ-TLKEY),TLKEY                                      
         BNE   *+10                                                             
         MVC   TLKSEQ,I.TLKSEQ                                                  
         ICM   RE,3,TLKSEQ         INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLKSEQ                                                      
INS16    MVC   TLRLEN,LSMIXLST+(MIXLRECL-MIXLST)                                
         MVC   TLDA,SINDA                                                       
         L     RF,AIO3                                                          
         MVC   TLSTA,TRNRSTA-TRNRECD(RF)                                        
         MVC   TLRECACT,IRECACT                                                 
         GOTO1 ATSARIO,TSAADD                                                   
*                                                                               
INS30    MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO1,0,LSPRATA                        
         L     RF,0(R1)                                                         
         USING JCBELD,RF                                                        
         ICM   RE,3,JCBADV         INCREMENT COUNT OF ADVANCE BILLS             
         LA    RE,1(RE)                                                         
         STCM  RE,3,JCBADV                                                      
         DROP  RF                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(AI$RECAD)                                           
         TM    SINDRF,SINDRFY      TEST ADDING DRAFT BILL, TOO                  
         BZ    INS40                                                            
         L     R1,AIO5             PASS D/A LIST TO AUTOFORM                    
         LA    RF,L'SINDA(R1)      RF=A(FIRST D/A)                              
         ST    RF,0(R1)            SET A(CURRENT D/A)                           
         MVC   0(L'SINDA,RF),SINDA SET D/A AND CLEAR NEXT                       
         XC    L'SINDA(L'SINDA,RF),L'SINDA(RF)                                  
         OC    SINDA,SINDA                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    CSFORMAT,SINFORMT   SWAP OVERRIDE FORMAT CODE                    
         XC    SINFORMT,CSFORMAT                                                
         XC    CSFORMAT,SINFORMT                                                
         GOTO1 VCOLY,BODMCB,('O#BILFRM',0),0,0                                  
         L     RF,BODMCB                                                        
         BASR  RE,RF                                                            
         OC    SINDA,SINDA                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    CSFORMAT,SINFORMT   RESTORE DEFAULT FORMST CODE                  
         XC    SINFORMT,CSFORMAT                                                
         XC    CSFORMAT,SINFORMT                                                
         MVC   FVMSGNO,=AL2(AI$ADVDB)                                           
         MVI   FVPARMS,L'CSBILNUM+1                                             
         MVC   FVPARMS+1(L'CSBILNUM),CSBILNUM                                   
*                                                                               
INS40    MVI   FVOMTYP,GTMINF                                                   
         LA    R1,INSCLIH          POSITION CURSOR TO CLIENT                    
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    *+8                                                              
         LA    R1,INSSUPCH         POSITION CURSOR TO SUPPLIER                  
         ST    R1,FVADDR                                                        
         SR    RF,RF               SET ALL FIELDS VALID                         
         ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+12                                                             
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         BXH   R1,RF,*-12                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CHANGE THE ITEM CURRENTLY DISPLAYED (JUST ADDED)       *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
CHA10    MVC   IODAOVER,SINDA      SET RECORD DISK ADDRESS                      
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO3                                                          
         USING TRNELD,R3                                                        
         XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         SR    RE,RE                                                            
         IC    RE,TRNRFST+(TRNLN-TRNELD)                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNELD(0),TRNRFST                                                
         CLI   TRNTYPE,99                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RE,1,SINNARL                                                     
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNNARR(0),SINNAR1                                               
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',TRNRECD),0                 
         MVI   TRNEL,1             ENSURE THIS IS FIRST ELEMENT                 
         GOTO1 (RF),(R1),(C'P',ACCMST),TRNRECD,TRNELD                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNRFST,TRNELQ                                                   
*                                                                               
         BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
*                                                                               
CHA20    GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO1,LSPRATAS,LSPRATA                 
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(AI$RECCH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,INSCLIH          POSITION CURSOR TO CLIENT                    
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    *+8                                                              
         LA    R1,INSSUPCH         POSITION CURSOR TO SUPPLIER                  
         ST    R1,FVADDR                                                        
         SR    RF,RF               SET ALL FIELDS VALID                         
         ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+12                                                             
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         BXH   R1,RF,*-12                                                       
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              UPDATE TRANSACTION PTAEL AND DISPLAY AMOUNTS           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
         USING TRNELD,R3                                                        
UPDTRN   NTR1                                                                   
         GOTO1 AGETOPT,BODMCB,AIO3                                              
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    UPD04                                                            
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    UPD04                                                            
         LA    R0,CSEXCVAL                                                      
UPD04    GOTO1 APRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA,0              
         LA    RE,LSPRATAS                                                      
         LA    RF,PR$LNQ                                                        
         LA    R0,LSPRATA                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                  SET UP TSAR RECORD (USED BY ALLTRN)          
         GOTO1 ASETTRN,BOPARM,(C'S',IIOKEY),AIO3,LSPRATA                        
*                                                                               
UPD10    MVI   FVMINL,0            TEST IF ANY COMM INPUT                       
         GOTO1 AFVAL,INSALCMH                                                   
         BNL   UPD20               YES - SINCOM CONTAINS COMMISSION             
*                                                                               
         L     RF,AGOPBLK          NO - CALC. IT ON ALLOCATED NET AMT           
         ZAP   BOPL81(16),SINNET                                                
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),GOAGYCOM-GOBLOCKD(L'GOAGYCOM,RF)                      
         SRP   BOPL81(16),64-8,5                                                
         ZAP   SINCOM,BOPL81(16)                                                
*                                                                               
UPD20    GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               (1,SINNET),0                                                     
         BE    UPD22                                                            
         LA    RE,INSALNTH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
UPD22    GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               0,(1,SINCOM)                                                     
         BE    *+16                                                             
         LA    RE,INSALCMH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
*                                                                               
         L     R2,BODMCB           R2=A(PTAEL) AFTER ALLTRN CALLS               
         USING PTAELD,R2                                                        
         TM    SININD1,SIN1HRS     TEST HOURS                                   
         BZ    UPD24                                                            
         OC    SINFRHRS,SINFRHRS   TEST FREEFORM HOURS                          
         BZ    *+6                                                              
         DC    H'0'                CANNOT HAVE BOTH TYPES OF HOURS              
         MVC   PTAHOURS,SINHOURS   SET HOURS IN PTAEL                           
         B     UPD30                                                            
*                                                                               
UPD24    OC    SINFRHRS,SINFRHRS   TEST FREEFORM HOURS                          
         BZ    UPD30                                                            
         TM    SININD1,SIN1HRS     TEST HOURS                                   
         BZ    *+6                                                              
         DC    H'0'                CANNOT HAVE BOTH TYPES OF HOURS              
         MVC   PTAHOURS,SINFRHRS   SET HOURS IN PTAEL                           
*                                                                               
UPD30    TM    SININD1,SIN1WUC     TEST WRITE-UP                                
         BZ    UPD38                                                            
         OI    PTASTAT2,PTASWRUP   SET WRITE-UP                                 
         ZAP   PTAWUAMT,SINWUCOM   AMOUNT (WRITE-UP COMMISSION)                 
         MVC   PTAWUHRS,SINWUHRS   WRITE-UP HOURS                               
         SR    R0,R0               SET WRITE-UP DATA IN PRORATA BLOCK           
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    UPD36                                                            
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    UPD36                                                            
         LA    R0,CSEXCVAL                                                      
UPD36    GOTO1 APRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA,0              
*                                                                               
UPD38    CURED SINNET,(14,INSALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALNTH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
         CURED SINCOM,(14,INSALCM),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALCMH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
UPD40    L     R3,AIO3             DELETE RATE AND TIME SCIELS                  
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING SCIELD,R3                                                        
         XR    RF,RF                                                            
UPD42    CLI   SCIEL,0                                                          
         BE    UPD50                                                            
         CLI   SCIEL,SCIELQ                                                     
         BNE   UPD48                                                            
         CLI   SCITYPE,SCITSRAT                                                 
         BE    *+12                                                             
         CLI   SCITYPE,SCITSJHR                                                 
         BNE   UPD48                                                            
         GOTO1 ATOBCHA,BOPARM,AIO3,SCIELD,0                                     
         B     UPD40               START AGAIN                                  
UPD48    IC    RF,SCILN                                                         
         BXH   R3,RF,UPD42                                                      
*                                                                               
UPD50    DS    0H                                                               
         ZAP   BODUB1,SINRATE      SET RATE                                     
         AP    BODUB1,SINFHRAT     OR FREEFORM RATE                             
         CP    BODUB1,BCPZERO      TEST NO RATE                                 
         BE    UPD60                                                            
         CP    BODUB1,SINRATE      TEST SCANDANAVIAN RATE                       
         BE    UPD52                                                            
         CP    BODUB1,SINFHRAT     TEST GERMAN RATE                             
         BE    UPD52                                                            
         DC    H'0'                CAN'T HAVE BOTH RATE TYPES                   
UPD52    DS    0H                                                               
         LA    R3,BOELEM           BUILD NEW SCIEL TO CARRY RATE                
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITSRAT                                                 
         ZAP   SCIAMNT,BODUB1                                                   
         GOTO1 ATOBCHA,BOPARM,AIO3,0,SCIELD                                     
*                                                                               
UPD60    LH    R1,SINHOURS         TEST HOURS                                   
         LTR   R1,R1                                                            
         BNZ   UPD62                                                            
         LH    R1,SINFRHRS         OR FREEFORM HOURS                            
         LTR   R1,R1                                                            
         BZ    UPD70                                                            
UPD62    CVD   R1,BODUB1           CARRY SJ HOURS IN SCIEL                      
         LA    R3,BOELEM           BUILD NEW SCIEL TO CARRY SJ HOURS            
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITSJHR                                                 
         ZAP   SCIAMNT,BODUB1                                                   
         GOTO1 ATOBCHA,BOPARM,AIO3,0,SCIELD                                     
*                                                                               
UPD70    DS    0H                                                               
*&&UK                                                                           
         TM    LSINDS2,LSTOBACC    TEST RECORD IS CONVERTED                     
         BNO   UPDTRNX                                                          
         GOTO1 VTOBACCO,BOPARM,('TOBAACVB',TOBCUR),AIO3,ACOM,0,0                
         NI    LSINDS2,FF-LSTOBACC                                              
*&&                                                                             
UPDTRNX  B     EXITY                                                            
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              EXIT POINTS ETC                                        *         
***********************************************************************         
         SPACE 1                                                                
EXITY    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITNC   DS    0H                                                               
*XITNC   XC    INSCLIN,INSCLIN                                                  
*        XC    INSPRON,INSPRON                                                  
*        XC    INSJOBN,INSJOBN                                                  
*        OI    INSCLINH+(FVOIND-FVIHDR),FVOXMT                                  
*        OI    INSPRONH+(FVOIND-FVIHDR),FVOXMT                                  
*        OI    INSJOBNH+(FVOIND-FVIHDR),FVOXMT                                  
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              LITERAL POOL/CONSTANTS                                 *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
ACCMST   DC    C'ACCMST  '                                                      
MAXHOUR  DC    F'32760'                                                         
MINHOUR  DC    F'-32760'                                                        
WC       DC    C'WC      '                                                      
SV       DC    C'SV'                                                            
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              DSECTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
IWORKD   DSECT                                                                  
ILST     DS    XL(L'TLST)                                                       
IKEYINP  DS    CL1                                                              
IEXCVAL  DS    CL(L'CSEXCVAL)                                                   
ITYPEXC  DS    CL(L'CSTYPEXC)                                                   
IDATEXC  DS    CL(L'CSDATEXC)                                                   
IRECACT  DS    0XL2                                                             
IREC     DS    XL1                                                              
IACT     DS    XL1                                                              
IIOKEY   DS    XL(L'IOKEY)                                                      
IWORKL   EQU   *-IWORKD                                                         
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
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF9BD                                                      
         ORG   OSVALS                                                           
SINSAVE  DS    0XL(SINSAVL)                                                     
         DS    0H                                                               
SINHOURS DS    H                   HOURS ALLOCATION                             
SINFRHRS DS    H                   FREEFORM HOURS ALLOCATION                    
SINRATE  DS    PL8                 RATE                                         
SINFHRAT DS    PL8                 FREEFORM HOURS RATE                          
SINSUPP  DS    CL14                SUPPLIER CODE                                
SINSUPN  DS    CL36                SUPPLIER NAME                                
SINWC    DS    CL2                 WORKCODE                                     
SINREF   DS    CL6                 REFERENCE                                    
SINDATE  DS    PL3                 DATE                                         
SINNET   DS    PL8                 NET ALLOCATION                               
SINCOM   DS    PL8                 COMM ALLOCATION                              
SINNARL  DS    CL1                                                              
SINNAR1  DS    CL(L'INSNAR1)                                                    
SINNAR2  DS    CL(L'INSNAR2)                                                    
SINNAR3  DS    CL(L'INSNAR3)                                                    
SINNAR4  DS    CL(L'INSNAR4)                                                    
SINDA    DS    CL4                 LAST INSERTED TRANSACTION D/A                
SINWUCOM DS    PL8                 WRITE-UP COMMISSION ALLOCATION               
SINWUHRS DS    XL2                 WRITE-UP HOURS ALLOCATION                    
SINWCSTA DS    XL1                 WORKCODE STATUS (WCOSTAT IN WCOEL)           
SININD1  DS    XL1                 INDICATOR - 1                                
SIN1RAT  EQU   X'80'               NON-ZERO RATE                                
SIN1NET  EQU   X'40'               NON-ZERO NETE                                
SIN1HRS  EQU   X'20'               NON-ZERO HOURS                               
SIN1COM  EQU   X'10'               NON-ZERO COMMISSION                          
SIN1WUC  EQU   X'08'               NON-ZERO WRITE-UP COMMISSION                 
SIN1WUH  EQU   X'04'               NON-ZERO WRITE-UP HOURS                      
SINDRF   DS    XL1                 DRAFT BILL REQUIREMENT                       
SINDRFY  EQU   X'80'               ADD DRAFT BILL AUTOMATICALLY                 
SINSAVL  EQU   *-SINSUPP                                                        
SINFORMT DS    XL1                 OVERRIDE FORMAT CODE                         
ILASTJOB DS    CL12                LAST JOB-CODE                                
         ORG   OSVALS+OSVALSL                                                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'119ACCLB09B  12/22/99'                                      
         END                                                                    
