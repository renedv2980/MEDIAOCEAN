*          DATA SET ACCLB52    AT LEVEL 187 AS OF 08/16/00                      
*PHASE T62152A                                                                  
*&&      SET   NOP=N                                                            
CLB52    TITLE '- PC COMMS - FORMAT RECORD'                                     
CLB52    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB52**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
         B     SNDEL               SEND ELEMENT                                 
         B     SNDFLD              SEND ELEMENT FIELD                           
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB                                                        
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         ICM   RF,15,ORCVHDRF                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR MAP HEADER RECIEVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         ICM   RF,15,ORCVHDRL                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         CLI   RECMODE,RECMWRTQ    TEST WRITE RECORD                            
         BNE   EXITY                                                            
         GOTO1 PUTBFM              PUT BACK LAST RECORD                         
         PUSH  USING                                                            
         USING BFMRECD,KEYRCV      DELETE ANY RECORDS AFTER THIS                
         MVC   BFMKLVL,BCEFFS                                                   
         GOTO1 DELRECS,BOPARM,LASTKEY,BFMKEY                                    
         POP   USING                                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         CLI   RECMODE,RECMRDQ     TEST READ RECORD                             
         BNE   EXITY                                                            
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* SEND ELEMENT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SNDEL    DS    0H                                                               
         ICM   RF,15,OSNDEL                                                     
         BNZR  RF                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* SEND ELEMENT FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
SNDFLD   DS    0H                                                               
         ICM   RF,15,OSNDFLD                                                    
         BNZR  RF                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* RECEIVE FORMAT KEY                                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BFMRECD,KEYRCV      INITIALIZE RECORD KEY                        
RCVFMTF  DS    0H                                                               
         MVI   PUTINDS,0                                                        
         XC    BFMKEY,BFMKEY                                                    
         MVI   BFMKTYP,BFMKTYPQ                                                 
         MVC   BFMKCPY,CUABIN                                                   
         MVI   BFMKSUB,BFMKSUBQ                                                 
         B     EXITY                                                            
*                                                                               
RCVFMT   MVC   BFMKFMT,DATA                                                     
         B     EXITY                                                            
RCVLANG  MVC   BFMKLANG,DATA                                                    
         B     EXITY                                                            
*                                                                               
RCVOVER  CLI   DATA,C'Y'                                                        
         BNE   *+8                                                              
         OI    PUTINDS,PUTIOVER    TEST FOR OVERWRITE EXISTING RECORD           
         B     EXITY                                                            
*                                                                               
         PUSH  USING                                                            
         USING BEDRECD,IOKEY                                                    
RCVBILL  DS    0H                  ** GET FORMAT FROM BILL NUMBER **            
         MVI   BEDPTYP,BEDPTYPQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVC   BEDPBLNO,DATA                                                    
         GOTO1 AIO,IOHIGH+IOACCDIR                                              
         BNE   *+14                                                             
         CLC   BEDPAS(BEDPIND-BEDPAS),IOKEYSAV                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         MVC   BFMKFMT,BEDPFORM    GET FORMAT FOR BILL                          
*        MVC   BFMKLANG,??                                                      
         B     EXITY                                                            
         POP   USING                                                            
*                                                                               
         PUSH  USING                                                            
         USING BEDKEY,IOKEY                                                     
RCVLJOB  DS    0H                  ** GET FORMAT FROM LAST JOB BILL **          
         XC    BEDKEY,BEDKEY                                                    
         MVI   BEDKTYP,BEDKTYPQ                                                 
         MVC   BEDKCPY,CUABIN                                                   
         MVI   BEDKSUB,BEDKSUBQ                                                 
         MVC   BEDKJOB,DATA                                                     
*                                                                               
RLJOB02  GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BNE   RCVLJOBN                                                         
         CLC   BEDKEY(BEDKJSEQ-BEDKEY),IOKEYSAV                                 
         BNE   RCVLJOBN                                                         
         OC    BEDKLVL(BEDKSTA-BEDKLVL),BEDKLVL TEST IS BILL HEADER             
         BNZ   *+14                                                             
         OC    BEDKBILD,BEDKBILD   TEST DRAFT BILL                              
         BZ    RLJOB04                                                          
         ICM   RF,3,BEDKSEQ        NO - READ FOR NEXT DRAFT BILL                
         LA    RF,1(RF)                                                         
         STCM  RF,3,BEDKSEQ                                                     
         OC    BEDKSEQ,BEDKSEQ     TEST LAST BILL READ                          
         BZ    RCVLJOBN                                                         
         XC    BEDKLVL(BEDKSTA-BEDKLVL),BEDKLVL                                 
         B     RLJOB02                                                          
         POP   USING                                                            
*                                                                               
RLJOB04  GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         LA    R3,BEDRFST-BEDRECD(R3)                                           
         USING BLHELD,R3                                                        
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
         MVC   BFMKFMT,BLHFORM     GET FORMAT FOR BILL                          
         MVC   BFMKLANG,BLHLANG                                                 
         B     EXITY                                                            
         DROP  R3                                                               
*                                                                               
RCVLJOBN MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
*                                                                               
RCVFMTL  DS    0H                                                               
         CLI   RECMODE,RECMWRTQ    TEST WRITING RECORD                          
         BNE   EXITY               YES - TEST RECORD ALREADY ON FILE            
*                                                                               
         MVC   IOKEY(L'KEYRCV),KEYRCV                                           
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   EXITY                                                            
         TM    PUTINDS,PUTIOVER                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXITN                                                            
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PUTINDS,PUTIROF                                                  
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('RACELQ',AIO1),            *        
               (L'RACTYPE,=AL1(RACTADD))                                        
         CLI   12(R1),0                                                         
         BNE   EXITY                                                            
         L     RF,12(R1)                                                        
         MVC   ADDRAC,0(RF)        SAVE ADDED RECORD ACTIVITY EL                
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* RECIEVE RECORD KEY DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
RCVKEYF  DS    0H                                                               
         CLI   RECMODE,RECMWRTQ    TEST WRITING RECORD                          
         BNE   EXITY                                                            
         GOTO1 PUTBFM              WRITE PREVIOUS RECORD                        
         B     EXITY                                                            
*                                                                               
         PUSH  USING                                                            
         USING BFMRECD,KEYRCV                                                   
RCVKEY1  MVC   BFMKLVL,DATA                                                     
         XC    BFMKWHER(BFMKSTA-BFMKWHER),BFMKWHER                              
         B     EXITY                                                            
RCVKEY2  MVC   BFMKWHER,DATA                                                    
         XC    BFMKSECT(BFMKSTA-BFMKSECT),BFMKSECT                              
         B     EXITY                                                            
RCVKEY3  MVC   BFMKWHER,=AL2(BFMKWBDQ)                                          
         MVC   BFMKSECT,DATA                                                    
         XC    BFMKSWHR(BFMKSTA-BFMKSWHR),BFMKSWHR                              
         B     EXITY                                                            
RCVKEY4  IC    RE,BFMKSWSQ                                                      
         XC    BFMKSWSQ(BFMKSTA-BFMKSWSQ),BFMKSWSQ                              
         CLC   BFMKSWHR,DATA                                                    
         BE    *+14                                                             
         MVC   BFMKSWHR,DATA                                                    
         B     EXITY                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BFMKSWSQ                                                      
         B     EXITY                                                            
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* RECIEVE BFSBODYS                                                   *          
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BFSELD,ELEM                                                      
RCVBFS   DS    0H                  ADD RECEIVED BODY TO LIST                    
         XR    RE,RE                                                            
         IC    RE,BFSLN                                                         
         LA    RF,BFSELD(RE)                                                    
         MVC   0(L'BFSBODYS,RF),DATA                                            
         LA    RE,1(RE)                                                         
         STC   RE,BFSLN                                                         
         B     EXITY                                                            
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* RECIEVE RACELD ELEMENT CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RACELD,ELEM                                                      
RCVRAC   DS    0H                  ONLY RCVTIME IS SENT                         
         MVI   RACTYPE,RACTCHA                                                  
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CUPASS                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,BCTODAYP                                                 
         B     EXITY                                                            
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* RECIEVE BSDELD ELEMENT CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVBSD   DS    0H                  SET SECTION NUMBER                           
         MVC   ELEM+(BSDSEC-BSDELD)(L'BSDSEC),KEYRCV+(BFMKSECT-BFMKEY)          
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* RECIEVE BFMELD TRAILING SPACES                                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BFMELD,ELEM                                                      
RCVSPACE DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,BFMLN            RF = L(ELEMENT)                              
         LA    R1,BFMELD(RF)       R1 = A(END OF ELEMENT)                       
         XR    RE,RE                                                            
         ICM   RE,1,DATA                                                        
         BZ    EXITY                                                            
         AR    RF,RE                                                            
         STC   RF,BFMLN            SET NEW L(ELEMENT)                           
*                                                                               
         MVI   0(R1),C' '          PUT TRAILING SPACES IN                       
         LA    R1,1(R1)                                                         
         BCT   RE,*-8                                                           
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SEND FORMAT RECORD DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
SNDFMT   DS    0H                                                               
         LA    R2,IOKEY                                                         
         USING BFMRECD,R2                                                       
         MVC   BFMKEY,KEYRCV                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECNF)                                           
         B     EXITN                                                            
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
*                                  IN CASE WAS SENT BILL NUMBER                 
         GOTO1 ASNDHDR,BOPARM,MH#FMT   SEND FORMAT CODE                         
         GOTO1 ASNDDATA,(R1),1,BFMKFMT                                          
         GOTO1 (RF),(R1),2,BFMKLANG                                             
*                                                                               
         GOTO1 ASNDREC,BOPARM,AIO1                                              
*                                                                               
SFMT02   GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BNE   SFMT10                                                           
         CLC   BFMKEY(BFMKLVL-BFMKEY),IOKEYSAV                                  
         BNE   SFMT10                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BFMKEY(BFMKSEQ-BFMKEY),IOKEYSAV                                  
         BE    SFMT04              TEST SAME KEY EXCEPT FOR SEQ NO.             
         GOTO1 SNDKEY              SEND KEY DETAILS                             
*                                                                               
SFMT04   DS    0H                                                               
         GOTO1 ASNDREC,BOPARM,AIO1 SEND RECORD DETAILS                          
         B     SFMT02                                                           
*                                                                               
SFMT10   DS    0H                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SEND RECORD KEY                                                     *         
***********************************************************************         
         SPACE 1                                                                
SNDKEY   NTR1  ,                                                                
         GOTO1 ASNDHDR,BOPARM,2                                                 
*                                                                               
OLD      USING BFMRECD,IOKEYSAV                                                 
NEW      USING BFMRECD,IOKEY                                                    
*                                                                               
         DS    0H                  TEST FOR CHANGE IN LEVEL                     
         CLC   NEW.BFMKLVL,OLD.BFMKLVL                                          
         BE    SKEY02                                                           
         GOTO1 ASNDDATA,BOPARM,1,NEW.BFMKLVL                                    
         XC    OLD.BFMKEY,OLD.BFMKEY                                            
*                                                                               
SKEY02   DS    0H                  TEST FOR CHANGE IN WHERE                     
         CLC   NEW.BFMKWHER,OLD.BFMKWHER                                        
         BE    SKEY04                                                           
         XC    OLD.BFMKEY,OLD.BFMKEY                                            
         CLI   NEW.BFMKSECT,0      TEST FOR BILL BODY SECTION                   
         BNE   SKEY04                                                           
         GOTO1 ASNDDATA,BOPARM,2,NEW.BFMKWHER                                   
         B     EXIT                                                             
*                                                                               
SKEY04   DS    0H                  TEST FOR CHANGE IN SECTION NUMBER            
         CLC   NEW.BFMKSECT,OLD.BFMKSECT                                        
         BE    SKEY06                                                           
         GOTO1 ASNDDATA,BOPARM,3,NEW.BFMKSECT                                   
         B     EXIT                                                             
*                                                                               
SKEY06   DS    0H                  TEST FOR CHANGE IN WHERE IN SECTION          
         CLC   NEW.BFMKSWHR(L'BFMKSWHR+L'BFMKSWSQ),OLD.BFMKSWHR                 
         BE    SKEY08                                                           
         GOTO1 ASNDDATA,BOPARM,4,NEW.BFMKSWHR                                   
         B     EXIT                                                             
*                                                                               
SKEY08   DS    0H                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  OLD,NEW                                                          
         EJECT                                                                  
***********************************************************************         
* SEND BSDDATA - FIELD IS EITHER STRING OR NUMBER                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BSDELD,ELEM                                                      
SNDBSD#2 DS    0H                  TEST BSDDATA IS STRING                       
         LA    RF,BSDCHARS                                                      
         B     *+8                                                              
SNDBSD#3 DS    0H                  TEST BSDDATA IS NUMBER                       
         LA    RF,BSDNUMS                                                       
*                                                                               
SBSD02   CLI   0(RF),EOT                                                        
         BE    EXITN               NOT EQUAL - DO NOT SEND                      
         CLC   BSDSTYP,0(RF)                                                    
         BE    EXITY               FOUND MATCH SO SEND                          
         LA    RF,1(RF)                                                         
         B     SBSD02                                                           
*                                                                               
BSDCHARS DS    0X                  BSDATA IS CHARACTER                          
         DC    AL1(BSDSWCG,BSDSWC,BSDSCAC,BSDSCAT,EOT)                          
BSDNUMS  DS    0X                  BSDDATA IS NUMBER                            
         DC    AL1(BSDSWCT,BSDSTRN,EOT)                                         
         DS    0H                                                               
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND RACPERS - GET PERSON RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
SNDPERS  DS    0H                                                               
         GOTO1 AGETPID,DATA                                                     
         GOTO1 ASNDDATA,BOPARM,AMDEL,BCWORK                                     
         B     EXITN               CC=NOT EQUAL ALREADY SENT                    
         SPACE 1                                                                
***********************************************************************         
* SEND TEXT PLUS NUMBER OF TRAILING SPACES                            *         
***********************************************************************         
         SPACE 1                                                                
SNDTEXT  DS    0H                                                               
         LH    R0,DATALEN                                                       
         LTR   R0,R0               DON'T SEND IF ZERO LENGTH                    
         BZ    EXITN                                                            
*                                                                               
         LA    RF,DATA             FIND NUMBER OF TRAILING SPACES               
         AH    RF,DATALENX                                                      
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LTR   R0,R0                                                            
         BZ    STEXT02                                                          
         GOTO1 ASNDDATA,BOPARM,AMDEL,((R0),DATA)                                
*                                                                               
STEXT02  DS    0H                                                               
         LH    RE,DATALEN                                                       
         SR    RE,R0                                                            
         BZ    EXITN                                                            
         STC   RE,BOWORK1                                                       
         GOTO1 ASNDDATA,BOPARM,22,BOWORK1                                       
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT TO RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
ADDELEND LA    R3,ADDEND           ADD ELEMENT AT END OF RECORD                 
         B     ADDEL00                                                          
ADDELCOD LA    R3,ADDCODE          ADD ELEMENT AT CODE POSITION                 
*                                                                               
ADDEL00  L     R2,AIO1                                                          
         USING BFMRECD,R2                                                       
         TM    PUTINDS,PUTIRECI                                                 
         BO    ADDEL02                                                          
         XC    BFMRECD(BFMRFST+1-BFMRECD),BFMRECD                               
         MVC   BFMKEY,KEYRCV                                                    
         MVC   BFMRLEN,=AL2(BFMRFST+1-BFMRECD)                                  
         OI    PUTINDS,PUTIRECI                                                 
*                                                                               
ADDEL02  DS    0H                                                               
         XR    RF,RF               TEST RECORD WILL BE TOO BIG                  
         ICM   RF,3,BFMRLEN                                                     
         XR    RE,RE                                                            
         IC    RE,ELEM+(BFMLN-BFMELD)                                           
         AR    RF,RE                                                            
         CHI   RF,2000                                                          
         BNL   ADDEL04                                                          
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),BFMRECD,ELEM,(R3)                    
         CLI   12(R1),0                                                         
         BE    ADDELX                                                           
         DC    H'0'                                                             
*                                                                               
ADDEL04  DS    0H                                                               
         OI    BFMRSTAT,BFMSCONT   SET RECORD CONTINUED                         
         BAS   RE,PUTBFM           WRITE CURRENT RECORD                         
KEY      USING BFMRECD,KEYRCV                                                   
         IC    RE,KEY.BFMKSEQ      INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)                                                         
         STC   RE,KEY.BFMKSEQ                                                   
         CLI   KEY.BFMKSEQ,0                                                    
         BNE   ADDEL00             ADD ELEMENT TO NEW RECORD                    
         DC    H'0'                                                             
         DROP  KEY                                                              
*                                                                               
ADDELX   DS    0H                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT BFM RECORD TO FILE                                   *         
***********************************************************************         
         SPACE 1                                                                
PUTBFM   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING BFMRECD,R2                                                       
         TM    PUTINDS,PUTIRECI    TEST ANY ELEMENTS ADDED TO RECORD            
         BO    PBFM02                                                           
         TM    PUTINDS,PUTIFST     MASTER RECORD MUST HAVE ELEMENTS             
         BO    EXIT                                                             
         DC    H'0'                                                             
PBFM02   CLC   BFMKEY,KEYRCV       TEST FOR SCREW UP                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    PUTINDS,PUTIFST     TEST PUT FIRST RECORD                        
         BO    PBFM10                                                           
         CLI   ADDRAC,RACELQ       PUT BACK ADD RACELD                          
         BE    PBFM04              OR IF FIRST TIME COPY CHANGE RACELD          
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('RACELQ',AIO1),            *        
               (L'RACTYPE,=AL1(RACTCHA))                                        
         CLI   12(R1),0                                                         
         BNE   PBFM20                                                           
         L     RF,12(R1)                                                        
         MVC   ADDRAC,0(RF)                                                     
         MVI   ADDRAC+(RACTYPE-RACELD),RACTADD                                  
PBFM04   GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,ADDRAC,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PBFM20                                                           
*                                  DELETE RECORDS IN BETWEEN                    
PBFM10   GOTO1 DELRECS,BOPARM,LASTKEY,BFMRECD                                   
*                                                                               
PBFM20   DS    0H                                                               
         GOTO1 WRTREC                                                           
         OI    PUTINDS,PUTIFST                                                  
         NI    PUTINDS,FF-PUTIRECI                                              
         MVC   LASTKEY,BFMKEY                                                   
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE RECORD                                             *         
*                                                                     *         
* NTRY: IO1 = RECORD TO BE WRITTEN                                    *         
* EXIT: RECORD WRITTEN/ADDED TO FILE                                  *         
***********************************************************************         
         SPACE 1                                                                
WRTREC   NTR1  ,                                                                
DIR      USING BFMRECD,IOKEY                                                    
         L     R2,AIO1                                                          
NEW      USING BFMRECD,R2                                                       
         L     R3,AIO2                                                          
OLD      USING BFMRECD,R3                                                       
*                                                                               
         MVC   DIR.BFMKEY,NEW.BFMKEY                                            
         GOTO1 AIO,IORDD+IOACCDIR+IO2                                           
         BE    WREC02                                                           
         TM    IOERR,IOEDEL                                                     
         BO    WREC02                                                           
         TM    IOERR,IOERNF        TEST RECORD NOT ON FILE                      
         BO    *+6                                                              
         DC    H'0'                                                             
         TM    NEW.BFMRSTA,BFMSDELT TEST NEW RECORD DELETED                     
         BO    WRTRECX             YES - DON'T ADD IT THEN                      
         GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    WRTRECX                                                          
         DC    H'0'                                                             
*                                                                               
WREC02   DS    0H                  TEST CHANGE IN STATUS AREA                   
         CLC   DIR.BFMKSTA,NEW.BFMRSTA                                          
         BE    WREC04                                                           
         GOTO1 AIO,IORDUPD+IOACCDIR+IO2   YES - UPDATE DIRECTORY REC            
         MVC   DIR.BFMKSTA,NEW.BFMRSTA                                          
         GOTO1 AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WREC04   DS    0H                                                               
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,NEW.BFMRECD      COPY RECORD TO IO2                           
         XR    RF,RF                                                            
         ICM   RF,3,NEW.BFMRLEN                                                 
         LA    R0,OLD.BFMRECD                                                   
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 AIO,IOPUT+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRTRECX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  DIR,NEW,OLD                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE ALL INTERMEDIATE RECORDS IN BETWEEN BUT NOT       *         
* INCLUDING KEY1 AND KEY2                                             *         
*                                                                     *         
* NTRY: P1 = A(KEY 1)                                                 *         
*       P2 = A(KEY 2)                                                 *         
***********************************************************************         
         SPACE 1                                                                
DELRECS  NTR1  ,                                                                
         LM    RE,RF,0(R1)                                                      
KEY      USING BFMRECD,IOKEY                                                    
         MVC   KEY.BFMKEY,0(RE)                                                 
         MVC   DRUPPER,0(RF)                                                    
         GOTO1 INCKEY,KEY.BFMKEY                                                
*                                                                               
         CLC   KEY.BFMKEY,DRUPPER                                               
         BE    EXIT                                                             
         BL    *+6                                                              
         DC    H'0'                RECORDS MUST COME IN ASCENDING ORDER         
*                                                                               
         LA    R1,IOHIGH+IOACCDIR+IO3+IOLOCK                                    
         B     *+8                                                              
DRECS02  LA    R1,IOSEQ+IOACCDIR+IO3+IOLOCK                                     
         GOTO1 AIO                                                              
         BNE   DELRECSX                                                         
         CLC   KEY.BFMKEY,DRUPPER                                               
         BNL   DELRECSX                                                         
         OI    KEY.BFMKSTA,BFMSDELT                                             
*                                                                               
         CLI   KEY.BFMKTYP,BFMKTYPQ                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY.BFMKCPY,CUABIN                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY.BFMKSUB,BFMKSUBQ                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         OI    BFMRSTA-BFMRECD(RF),BFMSDELT                                     
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DRECS02                                                          
*                                                                               
DELRECSX B     EXIT                                                             
         DROP  KEY                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INCREASE RECORD KEY VALUE BY 1                           *         
*                                                                     *         
* NTRY: R1 = A(RECORD KEY)                                            *         
***********************************************************************         
         SPACE 1                                                                
INCKEY   NTR1  ,                                                                
         LA    R1,L'ACCKEY-1(R1)                                                
         LA    R0,L'ACCKEY                                                      
*                                                                               
IKEY02   IC    RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   INCKEYX                                                          
         BCTR  R1,0                                                             
         BCT   R0,IKEY02                                                        
         DC    H'0'                                                             
*                                                                               
INCKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
ADDEND   DC    C'ADD=END'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
         SPACE 1                                                                
M#FMT    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#FMT)         ELEMENT CODE                                 
         DC    AL2(M#FMTX+1-M#FMT) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVFMTF-CLB52)  FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(RCVFMTL-CLB52)  LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDFMT-CLB52)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(26)             MAPPING CODE FOR RECORD MODE                 
         DC    CL5'MODE'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDIMODE)        INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR FORMAT NUMBER               
         DC    CL5'FMTNO'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'PBCKFMT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVFMT-CLB52)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE FOR FORMAT LANGUAGE             
         DC    CL5'FLANG'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'PBCKLANG)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVLANG-CLB52)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE FOR OVERWRITE TEST              
         DC    CL5'OVER '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVOVER-CLB52)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE FOR BILL NUMBER                 
         DC    CL5'BILL#'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BLHBLNO)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBILL-CLB52)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE FOR LATEST FOR JOB              
         DC    CL5'LJOB '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BCJOBCOD)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVLJOB-CLB52)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#FMTX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** RECORD KEY **                             
M#KEY    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(2)              ELEMENT CODE                                 
         DC    AL2(M#KEYX+1-M#KEY) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVKEYF-CLB52)  FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'LVL  '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BFMKLVL)      DATA LENGTH                                  
         DC    AL1(BFMKLVL-BFMKEY) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY1-CLB52)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WHER '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BFMKWHER)     DATA LENGTH                                  
         DC    AL1(BFMKWHER-BFMKEY) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY2-CLB52)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'SECT '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BFMKSECT)     DATA LENGTH                                  
         DC    AL1(BFMKSECT-BFMKEY) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY3-CLB52)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'SWHR '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'BFMKSWHR)     DATA LENGTH                                  
         DC    AL1(BFMKSWHR-BFMKEY) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVKEY4-CLB52)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#KEYX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** NAME ELEMENT **                           
M#NAM    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(NAMELQ)         ELEMENT CODE                                 
         DC    AL2(M#NAMX+1-M#NAM) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(NAMELQ,NAMLN1Q) ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELCOD-CLB52) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'NAME '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(NAMEREC-NAMELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#NAMX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** BOFELD **                                 
M#BOF    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(BOFELQ)         ELEMENT CODE                                 
         DC    AL2(M#BOFX+1-M#BOF) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(BOFELQ,BOFLNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELCOD-CLB52) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'INDS1'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BOFINDS1)     DATA LENGTH                                  
         DC    AL1(BOFINDS1-BOFELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'INDS2'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BOFINDS2)     DATA LENGTH                                  
         DC    AL1(BOFINDS2-BOFELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'WCORD'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'BOFWCORD)     DATA LENGTH                                  
         DC    AL1(BOFWCORD-BOFELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
*        DC    AL1(MDELDL2)        ITEM LENGTH                                  
*        DC    AL2(04)             MAPPING CODE                                 
*        DC    CL5'PARSP'          TEXT IDENTIFIER                              
*        DC    AL1(MDTBIQ)         DATA TYPE                                    
*        DC    AL1(L'BOFPARSP)     DATA LENGTH                                  
*        DC    AL1(BOFPARSP-BOFELD) DATA DISPLACEMENT                           
*        DC    AL1(MDIELFLD)       INDICATORS                                   
*        DC    AL2(0)              RECEIVE ROUTINE                              
*        DC    AL2(0)              SEND ROUTINE                                 
*        DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'MAXLN'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BOFMAXLN)     DATA LENGTH                                  
         DC    AL1(BOFMAXLN-BOFELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'MAXWD'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BOFMAXWD)     DATA LENGTH                                  
         DC    AL1(BOFMAXWD-BOFELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'PAGSP'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BOFPAGSP)     DATA LENGTH                                  
         DC    AL1(BOFPAGSP-BOFELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'INDS3'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(BOFINDS3-BOFELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#BOFX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** SECTION DEFINITION ELEMENT **             
M#BSD    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(BSDELQ)         ELEMENT CODE                                 
         DC    AL2(M#BSDX+1-M#BSD) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(BSDELQ,BSDLN1Q) ELEMENT CODE/LENGTH                          
         DC    AL2(RCVBSD-CLB52)   FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELCOD-CLB52) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'TYPE '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BSDSTYP)      DATA LENGTH                                  
         DC    AL1(BSDSTYP-BSDELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'CHDA '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(BSDDATA-BSDELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(SNDBSD#2-CLB52) SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'BIDA '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(BSDDATA-BSDEL)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(SNDBSD#3-CLB52) SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#BSDX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** BILL SORT ELEMENT **                      
M#BFS    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(BFSELQ)         ELEMENT CODE                                 
         DC    AL2(M#BFSX+1-M#BFS) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(BFSELQ,BFSLNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELCOD-CLB52) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(EXITN-CLB52)    SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'LVL  '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BFSLVL)       DATA LENGTH                                  
         DC    AL1(BFSLVL-BFSELD)  DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'BODYS'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BFSBODYS)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBFS-CLB52)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#BFSX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** BFPELD **                                 
M#BFP_RCVHDRL  EQU ADDELEND-CLB52                                               
       ++INCLUDE ACCLB5#BFP                                                     
*                                                                               
*                                  ** BFMELD AND BFXELD **                      
M#BFM_RCVHDRL  EQU ADDELEND-CLB52                                               
M#BFM_RCVSPACE EQU RCVSPACE-CLB52                                               
M#BFM_SNDTEXT  EQU SNDTEXT-CLB52                                                
       ++INCLUDE ACCLB5#BFM                                                     
*                                                                               
*                                  ** BILL TOTALS ELEMENT **                    
M#BTT    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(BTTELQ)         ELEMENT CODE                                 
         DC    AL2(M#BTTX+1-M#BTT) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(BTTELQ,BTTLNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(ADDELCOD-CLB52) LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'MAINL'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BTTMAINL)     DATA LENGTH                                  
         DC    AL1(BTTMAINL-BTTELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'MAINW'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BTTMAINW)     DATA LENGTH                                  
         DC    AL1(BTTMAINW-BTTELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'VATL '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BTTVATL)      DATA LENGTH                                  
         DC    AL1(BTTVATL-BTTELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'PREVL'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BTTPREVL)     DATA LENGTH                                  
         DC    AL1(BTTPREVL-BTTELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'INDS1'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BTTINDS1)     DATA LENGTH                                  
         DC    AL1(BTTINDS1-BTTELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'INDS2'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'BTTINDS2)     DATA LENGTH                                  
         DC    AL1(BTTINDS2-BTTELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#BTTX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** RECORD ACTIVITY ELEMENT **                
M#RAC_RCVHDRF  EQU RCVRAC-CLB52                                                 
M#RAC_RCVHDRL  EQU ADDELCOD-CLB52                                               
M#RAC_SNDPERS  EQU SNDPERS-CLB52                                                
       ++INCLUDE ACCLB5#RAC                                                     
*                                                                               
         SPACE 1                                                                
MAPTABX  DC    X'00'               E-O-T                                        
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
KEYRCV   DS    XL(L'BFMKEY)        BILL FORMAT RECORD KEY RECEIVED              
*                                                                               
PUTINDS  DS    XL1                 PUT RECORD INDICATORS                        
PUTIROF  EQU   X'80'               RECORD PREVIOUSLY ON FILE                    
PUTIFST  EQU   X'40'               FIRST RECORD PUT TO FILE                     
PUTIRECI EQU   X'20'               RECORD INITIALIZED                           
PUTIOVER EQU   X'10'               TEST FOR OVERWITE EXISTING RECORD            
ADDRAC   DS    XL(RACLNQ)          SAVED RACELD FOR ADD                         
*                                                                               
LASTKEY  DS    XL(L'BFMKEY)        LAST KEY (USED BY PUTBFM)                    
*                                                                               
DRUPPER  DS    XL(L'ACCKEY)        UPPER KEY (USED BY DELRECS)                  
*                                                                               
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'187ACCLB52   08/16/00'                                      
         END                                                                    
