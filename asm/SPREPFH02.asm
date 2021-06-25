*          DATA SET SPREPFH02  AT LEVEL 030 AS OF 10/01/13                      
*PHASE SPFH02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE BINSR31                                                                
         TITLE 'SPFH02 - DOWNLOAD EXTRACTED EZLOAD RECORDS'                     
SPFH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFH02,R8,RC                                                   
         LHI   RC,8192                                                          
         AR    RC,RB                                                            
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LHI   R9,4096                                                          
         AR    R9,RA                                                            
         USING DLCBD,DLBLK                                                      
*                                                                               
         LA    RF,DNPRINT                                                       
         ST    RF,DLCBAPR          USER SUPPLIED PRINT ROUTINE                  
         LA    RF,P                                                             
         ST    RF,DLCBAPL          USER SUPPLIED PRINT LINE                     
         OI    DLCBFLG1,DLCBFXTN   EXTENDED TEXT FIELD USED                     
         MVC   DLCXTND(7),MAXLINE                                               
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         BRAS  RE,INIT             DO INITIALISATION                            
         BRAS  RE,BLDEMTAB         BUILD EMAIL ID TABLE                         
*                                                                               
MAIN02   LA    R2,RECBUFF                                                       
         GET   EZLOAD,0(R2)        GET INPUT INTO RECBUFF                       
*                                                                               
         LA    R0,RECIN            COPY RECBUFF TO RECIN                        
         LHI   R1,RECINLQ                                                       
         LA    RE,RECBUFF                                                       
         LHI   RF,RECINLQ                                                       
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ATRTAB           TRANSLATE ALL FUNNIES OUT OF RECIN           
         TR    RECIN(RECINLQ),0(R2)                                             
         USING EZBILLD,RECIN                                                    
*                                                                               
         BRAS  RE,LINEOK           CHECK LINE IS OK                             
         BE    MAIN04              IT IS                                        
*                                                                               
         LHI   RF,RECINLQ                                                       
         BCTR  RF,0                                                             
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CHI   RF,L'PLINE                                                       
         BNH   *+8                                                              
         LHI   RF,L'PLINE                                                       
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),RECBUFF                                                 
         BRAS  RE,PLBAD            PRINT OUT BAD LINE                           
         B     MAIN02                                                           
*                                                                               
MAIN04   GOTO1 DATCON,DMCB,(0,EZBTODAY),(2,FULL)                                
*                                                                               
         OC    BQSTARTP,BQSTARTP   CHECK DATE WITHIN RANGE                      
         BZ    *+14                                                             
         CLC   BQSTARTP,FULL                                                    
         BH    MAIN02              NO - TOO EARLY                               
*                                                                               
         OC    BQENDP,BQENDP                                                    
         BZ    *+14                                                             
         CLC   BQENDP,FULL                                                      
         BL    EODAD               NO - TOO LATE (FILE IN DATE ORDER)           
*                                                                               
         BRAS  RE,PROCSRC          BUILD TABLE OF SRC/INVOICE ENTRIES           
         BRAS  RE,DOLINE           FORMAT AND WRITE OUTPUT LINE                 
         B     MAIN02              NEXT RECORD                                  
*                                                                               
EODAD    CLOSE EZLOAD              END OF FILE - CLOSE IT                       
         MVC   P,SPACE                                                          
         MVI   DLCBACT,C'R'        SET END OF REPORT                            
         GOTO1 VDLFLD,DLBLK                                                     
         BRAS  RE,PXBAD            CLOSE BAD LINE OUTPUT                        
         BRAS  RE,CLSEPQ           CLOSE DOWNLOAD REPORT                        
*                                                                               
         BRAS  RE,WRTMAIL          SEND E-MAILS FOR ALL SOURCE ENTRIES          
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITALISATION                                                       *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         BRAS  RE,PIBAD                                                         
         XC    BQSTARTP,BQSTARTP                                                
         XC    BQENDP,BQENDP                                                    
*                                                                               
         ICM   RF,15,ABEZTAB       MAKE SURE ONLY ONCE                          
         BNZ   INIT02                                                           
         L     R0,=A(100*1024)      ALLOW SPACE FOR 100,000 RECORDS             
         STCM  R0,15,BP6                                                        
         MHI   R0,BEZRLQ                                                        
*                                                                               
         SRL   R0,12                                                            
         AHI   R0,1                                                             
         SLL   R0,12                                                            
         ST    R0,BEZTLEN                                                       
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
*                                                                               
         STCM  R1,15,ABEZTAB       SAVE A(TABLE)                                
         STCM  R1,15,BP2           SET UP BINSRCH PARAMETERS                    
         LHI   RF,BEZRLQ                                                        
         ST    RF,BP4                                                           
         LHI   RF,BEZKLQ                                                        
         ST    RF,BP5                                                           
*                                                                               
INIT02   ICM   RF,15,AEMTAB        MAKE SURE ONLY ONCE                          
         BNZ   INIT04                                                           
         L     R0,=A(8*1024)                                                    
         STCM  R0,15,BE6           ALLOW 8K WORTH OF SOURCES                    
         MHI   R0,EMRLQ                                                         
*                                                                               
         SRL   R0,12                                                            
         AHI   R0,1                                                             
         SLL   R0,12                                                            
         ST    R0,EMLEN                                                         
*                                                                               
         GETMAIN RU,LV=(0),LOC=(BELOW,ANY)                                      
*                                                                               
         STCM  R1,15,AEMTAB        SAVE A(TABLE)                                
         STCM  R1,15,BE2           SET UP BINSRCH PARAMETERS                    
         LHI   RF,EMRLQ                                                         
         ST    RF,BE4                                                           
         LHI   RF,EMKLQ                                                         
         ST    RF,BE5                                                           
*                                                                               
INIT04   MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACE                                                          
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         CLC   QSTART,SPACE                                                     
         BNH   INIT06                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BQSTARTP)                              
*                                                                               
INIT06   CLC   QEND,SPACE                                                       
         BNH   INIT08                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
*                                                                               
INIT08   OPEN  (EZLOAD,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE INCOMING INVOICE INFORMATION                               *         
***********************************************************************         
         SPACE 1                                                                
LINEOK   NTR1  ,                                                                
         LA    R1,EZBMOS           MAKE SURE MOS AND DATE ARE NUMERIC           
         LA    RF,L'EZBMOS         ELSE DATCON BLOWS UP                         
         CLI   0(R1),C'0'                                                       
         BL    EXITL               BAD MOS                                      
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
*                                                                               
         LA    R1,EZBTODAY                                                      
         LA    RF,L'EZBTODAY                                                    
         CLI   0(R1),C'0'                                                       
         BL    EXITL               BAD DATE                                     
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'0',EZBNSPTS),L'EZBNSPTS,0                        
         CLI   0(R1),0                                                          
         BNE   EXITL               BAD SPOT COUNT                               
*                                                                               
         GOTO1 (RF),(R1),(C'0',EZBGDOL),L'EZBGDOL,0                             
         CLI   0(R1),0                                                          
         BNE   EXITL               INVALID GROSS DOLLARS                        
*                                                                               
         GOTO1 (RF),(R1),(C'0',EZBNDOL),L'EZBNDOL,0                             
         CLI   0(R1),0                                                          
         BNE   EXITL               INVALID NET DOLLARS                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ADD SOURCE LINE TO LIST IN BINSRCH TABLE                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BEZD,BSREC                                                       
         USING EMRECD,EMREC                                                     
*                                                                               
PROCSRC  NTR1  ,                                                                
*                                                                               
* READ FOR SOURCE/STATION PAIRS FIRST                                           
*                                                                               
         MVC   BEZKEY,SPACE                                                     
         MVI   BEZTYP,EMTSTSR      USE STATION-SOURCE PAIR                      
         MVC   BEZSRC,EZBSRC                                                    
         MVC   BEZSTA,EZBCALL                                                   
*                                                                               
         XC    EMREC,EMREC                                                      
         MVI   EMTYPE,EMTSTSR      STATION-SOURCE PAIR                          
         MVC   EMSRC,EZBSRC                                                     
         MVC   EMSTA,EZBCALL                                                    
         MVI   EMSEQ,X'01'                                                      
         LA    R0,EMREC                                                         
         STCM  R0,15,BE1                                                        
         MVI   BE4,X'00'           READ FOR STATION-SOURCE PAIR                 
         GOTO1 VBINSRCH,BINPARM2                                                
         TM    BE1,X'80'           MATCH?                                       
         BZ    PRS02               YES                                          
*                                                                               
         MVC   BEZKEY,SPACE                                                     
         MVI   BEZTYP,EMTESRC      USE EXCEPTION SOURCE                         
         MVC   BEZSRC,EZBSRC                                                    
*                                                                               
         XC    EMREC,EMREC                                                      
         MVI   EMTYPE,EMTESRC      EXCEPTION SOURCE                             
         MVC   EMSRC,SPACE                                                      
         MVC   EMSRC,EZBSRC                                                     
         MVC   EMSTA,SPACE                                                      
         MVI   EMSEQ,X'01'                                                      
         LA    R0,EMREC                                                         
         STCM  R0,15,BE1                                                        
         MVI   BE4,X'00'           READ FOR EXCEPTION SOURCE                    
         GOTO1 VBINSRCH,BINPARM2                                                
         TM    BE1,X'80'           MATCH?                                       
         BZ    PRS02               YES                                          
*                                                                               
         MVC   BEZKEY,SPACE                                                     
         MVI   BEZTYP,EMTSTN       USE STATION                                  
         MVC   BEZSTA,EZBCALL                                                   
*                                                                               
         XC    EMREC,EMREC                                                      
         MVI   EMTYPE,EMTSTN       STATION                                      
         MVC   EMSTA,EZBCALL                                                    
         MVC   EMSRC,SPACE                                                      
         MVI   EMSEQ,X'01'                                                      
         LA    R0,EMREC                                                         
         STCM  R0,15,BE1                                                        
         MVI   BE4,X'00'           READ FOR STATION                             
         GOTO1 VBINSRCH,BINPARM2                                                
         TM    BE1,X'80'           MATCH?                                       
         BZ    PRS02               YES                                          
*                                                                               
         MVC   BEZKEY,SPACE        ELSE USE SOURCE                              
         MVI   BEZTYP,EMTSRC                                                    
         MVC   BEZSRC,EZBSRC                                                    
*                                                                               
PRS02    MVC   BEZSRCE,EZBSRC       KEY FIELDS                                  
         MVC   BEZMEDIA,EZBMEDIA                                                
         MVC   BEZAGY,EZBAGY                                                    
         MVC   BEZCALL,EZBCALL                                                  
         MVC   BEZNET,EZBNET                                                    
         MVC   BEZINVNO,EZBINVNO                                                
         MVC   BEZTODAY,EZBTODAY                                                
*                                                                               
         MVC   BEZMOS,EZBMOS       DATA FIELDS                                  
         MVC   BEZNSPTS,EZBNSPTS                                                
         MVC   BEZGDOL,EZBGDOL                                                  
         MVC   BEZNDOL,EZBNDOL                                                  
         MVC   BEZEST,EZBEST                                                    
         MVC   BEZREPOR,EZBREPOR                                                
         MVC   BEZSTAOR,EZBSTAOR                                                
         MVC   BEZADVCD,EZBADVCD                                                
         MVC   BEZPRDCD,EZBPRDCD                                                
         MVC   BEZLDATE,EZBLDATE                                                
         MVC   BEZHDATE,EZBHDATE                                                
*                                                                               
         LA    R0,BSREC                                                         
         ST    R0,BP1                                                           
         MVI   BP4,X'01'                                                        
         BRAS  RE,ON31                                                          
         GOTO1 VBINSRCH,BINPARM                                                 
         BRAS  RE,OFF31                                                         
         OC    BP1,BP1                                                          
         BNZ   EXITOK                                                           
         DC    H'0'                TABLE FULL - SIZE SET IN INIT                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT LINE FOR DOWNLOADING                              *         
***********************************************************************         
         SPACE 1                                                                
DOLINE   NTR1  ,                                                                
         MVC   DLCBFLD(L'EZBAGY),EZBAGY                                         
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBSRC),EZBSRC                                         
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBMEDIA),EZBMEDIA                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBCALL+L'EZBNET),EZBCALL                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DUB(L'EZBMOS),EZBMOS                                             
         MVC   DUB+4(2),=CL2'01'                                                
         GOTO1 DATCON,DMCB,(0,DUB),(X'20',DOUBLE)                               
         MVC   DLCBFLD(L'EZBMOS),DOUBLE                                         
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,EZBTODAY),(10,DLCBFLD)                            
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBADVNM),EZBADVNM                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBINVNO),EZBINVNO                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'0',EZBNSPTS),L'EZBNSPTS,0                        
         ZAP   DUB,DMCB+4(8)                                                    
         EDIT  (P8,DUB),(15,DLCBFLD),0,ALIGN=LEFT,ZERO=NOBLANK                  
         MVI   DLCBTYP,C'N'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'0',EZBNDOL),L'EZBNDOL,0                          
         ZAP   DUB,DMCB+4(8)                                                    
*        EDIT  (P8,DUB),(17,DLCBFLD),2,ALIGN=LEFT,COMMAS=YES,          +        
               ZERO=NOBLANK                                                     
         EDIT  (P8,DUB),(17,DLCBFLD),2,ALIGN=LEFT,ZERO=NOBLANK                  
         MVI   DLCBTYP,C'N'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'0',EZBGDOL),L'EZBGDOL,0                          
         ZAP   DUB,DMCB+4(8)                                                    
*        EDIT  (P8,DUB),(17,DLCBFLD),2,ALIGN=LEFT,COMMAS=YES,          +        
               ZERO=NOBLANK                                                     
         EDIT  (P8,DUB),(17,DLCBFLD),2,ALIGN=LEFT,ZERO=NOBLANK                  
         MVI   DLCBTYP,C'N'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBAGYNM),EZBAGYNM                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*****                                                                           
         MVC   DLCBFLD(L'EZBEST),EZBEST                                         
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBREPOR),EZBREPOR                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBSTAOR),EZBSTAOR                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBADVCD),EZBADVCD                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBPRDCD),EZBPRDCD                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBLDATE),EZBLDATE                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
         MVC   DLCBFLD(L'EZBHDATE),EZBHDATE                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLBLK                                                     
*                                                                               
*****                                                                           
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD,DLBLK                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DLFLD PRINT ROUTINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DNPRINT  NTR1  ,                                                                
         GOTO1 REPORT                                                           
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SEND EMAIL BASED ON SOURCE FIELDS                                   *         
***********************************************************************         
         SPACE 1                                                                
WRTMAIL  NTR1  ,                                                                
         ICM   R4,15,BP3           R0=# LINES                                   
         BZ    EXITOK              NOTHING                                      
         ICM   R2,15,BP2           R2=A(THIS LINE)                              
         LA    R5,BSWORK                                                        
*                                                                               
         BRAS  RE,ON31                                                          
         MVC   BSWORK(BEZRLQ),0(R2)                                             
         BRAS  RE,OFF31                                                         
*                                                                               
         USING BEZD,R5                                                          
*                                                                               
WRTM02   DS    0H                                                               
         BRAS  RE,ON31                                                          
         MVC   BSWORK(BEZRLQ),0(R2)                                             
         BRAS  RE,OFF31                                                         
*                                                                               
         CLC   SVSRC,BEZKEY        CHANGE OF SOURCE?                            
         BE    WRTM18              NO                                           
         OC    SVSRC,SVSRC         FIRST TIME?                                  
         BZ    WRTM04              YES                                          
*                                                                               
         BRAS  RE,DOTOTS           TOTALS DETAILS                               
         BRAS  RE,CLSEPQ           CLOSE REPORT                                 
*                                                                               
WRTM04   BRAS  RE,OPENPQ           OPEN FRESH E-MAIL REPORT                     
*                                                                               
         XC    NUMBEZ,NUMBEZ       RESET LINE COUNT                             
         ZAP   SPOTT,PZERO          AND TOTALS COUNTS                           
         ZAP   GDOLT,PZERO                                                      
         ZAP   NDOLT,PZERO                                                      
         MVC   SVSRC,BEZKEY        SET NEW SOURCE ID                            
*                                                                               
         MVC   P(L'HDRREC),HDRREC  HEADER                                       
         GOTO1 REPORT                                                           
         MVC   P(L'TRNREC),TRNREC  TRN                                          
         GOTO1 REPORT                                                           
*                                                                               
         MVI   EMTFLAG,C'0'        DEFAULT FLAG FOR THIS SOURCE IS 0            
*                                                                               
         XC    EMREC,EMREC                                                      
         MVC   EMREC(BEZKEYLQ),BEZKEY                                           
         MVI   EMREC+(EMSEQ-EMRECD),1   START WITH SEQ 1                        
*                                                                               
WRTM05   LA    RE,EMREC                                                         
         STCM  RE,15,BE1                                                        
         MVI   BE4,X'00'                                                        
*                                                                               
         GOTO1 VBINSRCH,BINPARM2                                                
         TM    BE1,X'80'                                                        
         BO    *+12                RECORD NOT FOUND                             
*                                                                               
         ICM   R3,15,BE1           EMAIL ADDRESS LINE FOUND, USE IT             
         B     WRTM06                                                           
*                                                                               
         CLI   EMREC+(EMSRC-EMRECD),1   WERE WE LOOKING FOR SEQ#1?              
         BNE   WRTM07              NO, WE'RE DONE                               
         LA    R3,UNKFLD           YES - UNKNOWN SOURCE                         
*                                                                               
         USING EMRECD,R3                                                        
WRTM06   MVC   P(L'RCPREC),RCPREC                                               
         MVC   P+L'RCPREC(L'EMADDR),EMADDR                                      
         GOTO1 REPORT                                                           
*                                                                               
* IF BE1 IS SET TO X'80' AT THIS POINT, THEN                                    
* IT MEANS WE COULDN'T FIND SEQ #1, AND WERE PROCESSING                         
* UNKNOWN SOURCE.  DON'T LOOK FOR NEXT SEQUENCE NUMBER, GO DO EMTREC            
         TM    BE1,X'80'           RECORD WASN'T FOUND?                         
         BO    WRTM07              YES - WE'RE DONE, PRINT EMTREC               
*                                                                               
         TM    EMFLAG,EMFATTQ      ATTACHMENT FLAG                              
         BZ    *+8                                                              
         MVI   EMTFLAG,C'1'                                                     
*                                                                               
         LLC   RF,EMREC+EMSEQ-EMRECD                                            
         AHI   RF,1                                                             
         STCM  RF,1,EMREC+EMSEQ-EMRECD                                          
         B     WRTM05                                                           
*                                                                               
WRTM07   MVC   P(L'EMTREC),EMTREC                                               
         MVC   P+L'EMTREC(L'EMTFLAG),EMTFLAG                                    
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R3                                                               
*                                                                               
         OC    CCFLD,CCFLD         .CC FIELD FOR ALL IDS DEFINED?               
         BZ    WRTM08              NO                                           
         MVC   P(L'CCRREC),CCRREC                                               
         MVC   P+L'CCRREC(L'EMADDR),CCFLD+(EMADDR-EMRECD)                       
         GOTO1 REPORT                                                           
*                                                                               
WRTM08   DS    0H                                                               
         MVC   P(L'FRMREC),FRMREC  .FROM                                        
         MVC   P+L'FRMREC(L'REPLYTO),REPLYTO                                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(L'RPYREC),RPYREC  .REPLYTO                                     
         MVC   P+L'RPYREC(L'REPLYTO),REPLYTO                                    
         GOTO1 REPORT                                                           
*                                                                               
         LA    RF,P                                                             
*                                                                               
         MVC   0(L'SUBREC,RF),SUBREC  .SUBJECT                                  
         LA    RF,L'SUBREC+1(RF)                                                
*                                                                               
         CLI   SVSRC,EMTSRC                                                     
         BE    *+12                                                             
         CLI   SVSRC,EMTESRC                                                    
         BNE   *+18                                                             
         MVC   0(L'BEZSRC,RF),SVSRC+BEZSRC-BEZKEY                               
         LA    RF,L'BEZSRC+1(RF)                                                
         B     WRTM10                                                           
*                                                                               
         CLI   SVSRC,EMTSTN                                                     
         BNE   *+18                                                             
         MVC   0(L'BEZSTA,RF),SVSRC+BEZSTA-BEZKEY                               
         LA    RF,L'BEZSTA+1(RF)                                                
         B     WRTM10                                                           
*                                                                               
         CLI   SVSRC,EMTSTSR                                                    
         BNE   WRTM10                                                           
         MVC   0(L'BEZSRC,RF),SVSRC+BEZSRC-BEZKEY                               
         LA    RF,L'BEZSRC+1(RF)                                                
         MVC   0(L'BEZSTA,RF),SVSRC+BEZSTA-BEZKEY                               
         LA    RF,L'BEZSTA+1(RF)                                                
*                                                                               
WRTM10   DS    0H                                                               
         MVC   0(L'SUBJECT,RF),SUBJECT                                          
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,P                INDIVIDUAL LINES HEADERS                     
         USING EMD,R3                                                           
         MVC   EMSIGN,=CL8'Agency  '                                            
         MVC   EMMEDIA,=CL5'Media'                                              
         MVC   EMSTATN,=CL9'Station'                                            
         MVC   EMINVNO,=CL10'Invoice #'                                         
         MVC   EMTODAY,=CL9'Date'                                               
         MVC   EMMOS,=CL4' MOS'                                                 
         MVC   EMSPOT,=CL11'    # Spots'                                        
         MVC   EMGDOL,=CL15'  Gross dollars'                                    
         MVC   EMNDOL,=CL15'    Net dollars'                                    
         GOTO1 REPORT                                                           
         DROP  R3                                                               
*                                                                               
WRTM18   L     RF,NUMBEZ           INCREMENT INDIVIDUAL LINE COUNTS             
         AHI   RF,1                                                             
         ST    RF,NUMBEZ                                                        
         BRAS  RE,DOEML            WRITE INDIVIDUAL LINE                        
*                                                                               
WRTM20   AHI   R2,BEZRLQ           NEXT ENTRY IN TABLE                          
         BCT   R4,WRTM02                                                        
         WTO   'Did it'                                                         
*                                                                               
         BRAS  RE,DOTOTS           OUTPUT TOTALS FOR LAST                       
         BRAS  RE,CLSEPQ           CLOSE PQ                                     
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT EMAIL NOTIFICATION MESSAGE TO PRINT LINE          *         
* NTRY: R2     = A(BEZD TO USE)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING BEZD,R5                                                          
DOEML    NTR1  ,                                                                
         LA    R3,P                                                             
         USING EMD,R3                                                           
         MVC   EMSIGN,BEZAGY                                                    
         MVC   EMMEDIA(L'BEZMEDIA),BEZMEDIA                                     
         MVC   EMSTATN(L'BEZCALL+L'EZBNET),BEZCALL                              
         MVC   EMINVNO(L'BEZINVNO),BEZINVNO                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,BEZTODAY),(10,EMTODAY)                            
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'0',BEZNSPTS),L'BEZNSPTS,0                        
         ZAP   DUB,DMCB+4(8)                                                    
         AP    SPOTT,DUB                                                        
         EDIT  (P8,DUB),(11,EMSPOT),0,ZERO=NOBLANK,COMMAS=YES                   
*                                                                               
         MVC   DUB(L'BEZMOS),BEZMOS                                             
         MVC   DUB+4(2),=CL2'01'                                                
         GOTO1 DATCON,DMCB,(0,DUB),(X'20',DOUBLE)                               
         MVC   EMMOS(L'BEZMOS),DOUBLE                                           
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'0',BEZGDOL),L'BEZGDOL,0                          
         ZAP   DUB,DMCB+4(8)                                                    
         AP    GDOLT,DUB                                                        
         EDIT  (P8,DUB),(15,EMGDOL),2,ZERO=NOBLANK,COMMAS=YES                   
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'0',BEZNDOL),L'BEZNDOL,0                          
         ZAP   DUB,DMCB+4(8)                                                    
         AP    NDOLT,DUB                                                        
         EDIT  (P8,DUB),(15,EMNDOL),2,ZERO=NOBLANK,COMMAS=YES                   
         GOTO1 REPORT                                                           
*                                                                               
         B     EXITOK                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT TOTAL INFORMATION                                  *         
***********************************************************************         
         SPACE 1                                                                
DOTOTS   NTR1  ,                                                                
         GOTO1 REPORT              LEAVE A SPACE LINE                           
         MVC   P(L'TOTDET),TOTDET  TOTALS DETAILS                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(TOTWIDE),NUMINV   NUMBER OF INVOICES                           
         EDIT  (B4,NUMBEZ),(15,P+TOTWIDE),0,COMMAS=YES,ZERO=NOBLANK             
         GOTO1 REPORT                                                           
         MVC   P(TOTWIDE),TOTT     TOTAL NUMBER OF SPOTS                        
         EDIT  (P8,SPOTT),(15,P+TOTWIDE),0,COMMAS=YES,ZERO=NOBLANK              
         GOTO1 REPORT                                                           
         MVC   P(TOTWIDE),TOTND    TOTAL NET DOLLARS                            
         EDIT  (P8,NDOLT),(15,P+TOTWIDE),2,COMMAS=YES,ZERO=NOBLANK              
         GOTO1 REPORT                                                           
         MVC   P(TOTWIDE),TOTGD    TOTAL GROSS DOLLARS                          
         EDIT  (P8,GDOLT),(15,P+TOTWIDE),2,COMMAS=YES,ZERO=NOBLANK              
         GOTO1 REPORT                                                           
         B     EXITOK                                                           
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO PRINT SOURCE AND NUMBER OF INVOICES TO JOB OUTPUT        *         
***********************************************************************         
         SPACE 1                                                                
DOEMT    NTR1  ,                                                                
         MVC   PLINE,SPACE         DETAIL E-MAIL FOR LAST SOURCE                
         MVC   PLINE(L'SVSRC-1),SVSRC+1                                         
         ICM   R0,15,NUMBEZ                                                     
         EDIT  (R0),(8,PLINE+5),0,ZERO=NOBLANK                                  
         MVC   PLINE+14(24),=CL24'Lines written - sent to '                     
         MVC   PLINE+38(L'EMADDR),EMREC+(EMADDR-EMRECD)                         
         BRAS  RE,PLEMAIL                                                       
         B     EXITOK                                                           
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* OPEN PRINT QUEUE FOR E-MAIL SENDING                                 *         
***********************************************************************         
         SPACE 1                                                                
OPENPQ   NTR1  ,                                                                
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     R7,MCVREMOT                                                      
         USING REMOTED,R7                                                       
         MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,=AL2(8093) DDSEMAIL ID RECORD - LIVE                    
*        MVC   REMOTDST,=AL2(6954) DDSEMAIL ID RECORD - TEST                    
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(10),=C'EZ REPORT'                                       
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'EZL'                                                 
*                                                                               
         MVI   LINE,99                                                          
         MVI   FORCEHED,C'Y'                                                    
         XC    PAGE,PAGE                                                        
         XC    SUBPAGE,SUBPAGE                                                  
         B     EXITOK                                                           
         DROP  R7,RF                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINT QUEUE                                                   *         
***********************************************************************         
         SPACE 1                                                                
CLSEPQ   NTR1  ,                                                                
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTING FOR BAD RECORDS                                 *         
***********************************************************************         
         SPACE 1                                                                
PIBAD    NTR1  ,                                                                
         OPEN  (BADRECS,OUTPUT)    PRINT INIT                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZAP   BLINE,PZERO         RESET LINECOUNT                              
         ZAP   BPAGE,PZERO                                                      
         BRAS  RE,PTBAD                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PRINT TITLE FOR BAD RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
PTBAD    NTR1  ,                                                                
         AP    BPAGE,PONE          BUMP PAGECOUNT                               
         ZAP   BLINE,PFOUR                                                      
         L     R2,ABT                                                           
         PUT   BADRECS,0(R2)       PRINT TITLE                                  
         L     R2,ABTU                                                          
         PUT   BADRECS,0(R2)       PRINT TITLE UNDERLINE                        
         PUT   BADRECS,SPACE       PRINT CLEAR SPACE UNDERNEATH                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PRINT BAD RECORD                                                    *         
***********************************************************************         
         SPACE 1                                                                
PLBAD    NTR1  ,                                                                
         AP    BLINE,PONE          BUMP LINECOUNT                               
         CP    BLINE,SMAXL         TEST FOR MAX LINES                           
         BL    *+8                                                              
         BRAS  RE,PTBAD            PRINT NEW TITLES                             
*                                                                               
         PUT   BADRECS,PLINE       PRINT LINE                                   
         MVC   PLINE,SPACE                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CLOSE PRINTING FOR BAD RECORDS                                      *         
***********************************************************************         
         SPACE 1                                                                
PXBAD    NTR1  ,                                                                
         CLOSE BADRECS                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF SOURCE/EMAIL ADDRESSES                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING EZMAILD,EWORK                                                    
BLDEMTAB NTR1  ,                                                                
         OPEN  (EMAIL,INPUT)                                                    
         LTR   RF,RF                                                            
         BZ    BEM02                                                            
         DC    H'0'                                                             
*                                                                               
         XC    SVEMKEY,SVEMKEY                                                  
         XC    SVSEQ,SVSEQ                                                      
*                                                                               
BEM02    GET   EMAIL,EWORK                                                      
         CLI   EWORK,C'*'          COMMENT FIELD                                
         BE    BEM02                                                            
*                                                                               
         CLC   SVEMKEY(BEZKEYLQ),EWORK                                          
         BE    *+10                                                             
         XC    SVSEQ,SVSEQ         IF SRC CHANGES, RE-SET SEQ NUM               
*                                                                               
         LLC   RF,SVSEQ                                                         
         AHI   RF,1                                                             
         CHI   RF,100                                                           
         BH    BEM02               IGNORE IF >99 RECEPIENTS                     
         STCM  RF,1,SVSEQ                                                       
         MVC   SVEMKEY(BEZKEYLQ),EWORK                                          
*                                                                               
         CLC   =C'CC  ',EZMSRC     THIS FIELD GETS APPENDED TO ALL              
         BNE   BEM04               EMAILS SENT OUT                              
CC       USING EMRECD,CCFLD                                                     
         MVC   CC.EMTYPE,EZMTYPE                                                
         MVC   CC.EMADDR,EZMMAIL                                                
         B     BEM02                                                            
         DROP  CC                                                               
*                                                                               
BEM04    CLC   =C'????',EZMSRC     THIS FIELD IS FOR SRC/STN COMBOS             
         BNE   BEM06               THAT DON'T HAVE A MATCH IN THE FILE          
UK       USING EMRECD,UNKFLD                                                    
         MVC   UK.EMTYPE,EZMTYPE                                                
         MVC   UK.EMADDR,EZMMAIL                                                
         B     BEM02                                                            
         DROP  UK                                                               
*                                                                               
         USING EMRECD,EMREC                                                     
BEM06    MVC   EMTYPE,EZMTYPE      SET TYPE                                     
         CLI   EMTYPE,EMTSRC                                                    
         BE    BEM08                                                            
         CLI   EMTYPE,EMTESRC                                                   
         BE    BEM08                                                            
         CLI   EMTYPE,EMTSTN                                                    
         BE    BEM08                                                            
         CLI   EMTYPE,EMTSTSR                                                   
         BE    BEM08                                                            
         DC    H'0'                INVALID TYPE                                 
*                                                                               
BEM08    MVC   EMSRC,EZMSRC        SET SOURCE                                   
         MVC   EMSTA,EZMSTA        SET STATION                                  
         MVC   EMSEQ,SVSEQ                                                      
         MVC   EMADDR,EZMMAIL      SET EMAIL ADDRESS                            
*                                                                               
         MVI   EMFLAG,X'00'                                                     
         CLI   EZMFLG,C'A'                                                      
         BNE   *+8                                                              
         OI    EMFLAG,EMFATTQ                                                   
*                                                                               
         LA    RF,EMREC                                                         
         ST    RF,BE1                                                           
         MVI   BE4,X'01'                                                        
         GOTO1 VBINSRCH,BINPARM2                                                
         OC    BE1,BE1                                                          
         BNZ   BEM02                                                            
         DC    H'0'                TABLE FULL                                   
*                                                                               
EODEMTAB CLOSE EMAIL                                                            
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
*                                                                               
ON31     O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* DCBS                                                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL8'EZLOAD**'                                                    
EZLOAD   DCB   DSORG=PS,MACRF=GM,DDNAME=EZLOAD,RECFM=FB,LRECL=256,     +        
               BLKSIZE=27904,EODAD=EODAD                                        
*                                                                               
         DS    0L                                                               
         DC    CL8'BADRECS*'                                                    
BADRECS  DCB   DSORG=PS,MACRF=PM,DDNAME=BADRECS,RECFM=FBA,LRECL=(166)           
*                                                                               
         DS    0L                                                               
         DC    CL8'NOTIFIED'                                                    
NOTIFIED DCB   DSORG=PS,MACRF=PM,DDNAME=NOTIFIED,RECFM=FBA,LRECL=(166)          
*                                                                               
         DS    0L                                                               
         DC    CL8'EMAIL***'                                                    
EMAIL    DCB   DSORG=PS,MACRF=GM,DDNAME=EMAIL,RECFM=FB,LRECL=80,       +        
               EODAD=EODEMTAB                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
VDLFLD   DC    V(DLFLD)            DLFLD                                        
CASHVAL  DC    V(CASHVAL)          CASHVAL                                      
VBINSRCH DC    V(BINSRCH)          BINSR31                                      
ATRTAB   DC    A(TRTAB)            TRANSLATE TABLE FOR INPUT RECORD             
ABT      DC    A(BTITLE)           BADRECS TITLE                                
ABTU     DC    A(BTITLEU)                                                       
AET      DC    A(ETITLE)           NOTIFIED TITLE                               
AETU     DC    A(ETITLEU)                                                       
*                                                                               
NUMBEZ   DC    F'0'                                                             
*                                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
*                                                                               
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
PFOUR    DC    P'4'                                                             
SMAXL    DC    P'60'                                                            
BLINE    DC    PL3'0'                                                           
BPAGE    DC    PL3'0'                                                           
ELINE    DC    PL3'0'                                                           
EPAGE    DC    PL3'0'                                                           
*                                                                               
INVCNT   DS    PL8                 NUMBER OF INVOICES                           
GDOLT    DS    PL8                 GROSS DOLLAR TOTAL                           
NDOLT    DS    PL8                 NET DOLLAR TOTAL                             
SPOTT    DS    PL8                 SPOT TOTAL                                   
*                                                                               
SVEMKEY  DS    CL(BEZKEYLQ)                                                     
SVSEQ    DS    X                                                                
*                                                                               
HDRREC   DC    C'    *HDR*                         WP                  +        
                              M'                                                
*                                                                               
TRNREC   DC    C'++DDS X XXXTRN'                                                
*                                                                               
RCPREC   DC    C'++DDS      RCP '                                               
CCRREC   DC    C'++DDS      CCR '                                               
BCCREC   DC    C'++DDS      BCC '                                               
SUBREC   DC    C'++DDS      SUB '                                               
RPYREC   DC    C'++DDS      RPY '                                               
FRMREC   DC    C'++DDS      FRM '                                               
SUBJECT  DC    C'EZ Load Information'                                           
REPLYTO  DC    C'EDIAdmin@mediaocean.com'                                       
EMTREC   DC    C'++DDS      EMT '                                               
EMTFLAG  DC    C'0'                                                             
*                                                                               
INVALID  DC    CL11'<INVALID #>'                                                
*                                                                               
TOTDET   DC    CL50'Totals information for this report'                         
TOTWIDE  EQU   40                                                               
NUMINV   DC    CL(TOTWIDE)'Number of invoices sent'                             
TOTT     DC    CL(TOTWIDE)'Total number of spots in invoices'                   
TOTGD    DC    CL(TOTWIDE)'Total of gross dollar amounts'                       
TOTND    DC    CL(TOTWIDE)'Total of net dollar amounts'                         
*                                                                               
EWORK    DC    CL80' '                                                          
SPACE    DC    166C' '                                                          
         EJECT                                                                  
***********************************************************************         
* CONSTANTS WITH EYECATCHERS (FOR DUMP ANALYSIS)                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL8'ABEZTAB*'                                                    
ABEZTAB  DC    A(0)                                                             
BEZTLEN  DC    A(0)                                                             
*                                                                               
         DC    CL8'AEMTAB**'                                                    
AEMTAB   DC    A(0)                                                             
EMLEN    DC    A(0)                                                             
*                                                                               
         DS    0L                                                               
         DC    CL8'PLINE***'                                                    
PLINE    DS    CL166                                                            
*                                                                               
         DS    0L                                                               
         DC    CL8'DLCB****'                                                    
DLBLK    DC    (DLCBXLX)X'00'                                                   
*                                                                               
         DS    0L                                                               
         DC    CL8'BINSRCH1'                                                    
BINPARM  DS    0XL32                                                            
BP1      DC    F'0'                                                             
BP2      DC    F'0'                                                             
BP3      DC    F'0'                                                             
BP4      DC    F'0'                                                             
BP5      DC    F'0'                                                             
BP6      DC    F'0'                                                             
BP7      DC    F'0'                                                             
BP8      DC    F'0'                                                             
*                                                                               
BSWORK   DS    XL(BEZRLQ)                                                       
*                                                                               
         DS    0L                                                               
         DC    CL8'BINSRCH2'                                                    
BINPARM2 DS    0XL32                                                            
BE1      DC    F'0'                                                             
BE2      DC    F'0'                                                             
BE3      DC    F'0'                                                             
BE4      DC    F'0'                                                             
BE5      DC    F'0'                                                             
BE6      DC    F'0'                                                             
BE7      DC    F'0'                                                             
BE8      DC    F'0'                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* RECORD I/O AREAS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'SVSRC***'                                                    
SVSRC    DC    XL(BEZKEYLQ)'00'                                                 
*                                                                               
         DS    0D                                                               
         DC    CL8'BSREC***'                                                    
BSREC    DS    CL(BEZRLQ)                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'EMREC***'                                                    
EMREC    DS    CL(EMRLQ)                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'CC REC**'                                                    
CCFLD    DC    XL(EMRLQ)'00'                                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'UNK REC*'                                                    
UNKFLD   DC    XL(EMRLQ)'00'                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'RECIN***RECIN***'                                           
RECIN    DS    256C                RECORD AFTER TRANSLATION                     
RECINLQ  EQU   *-RECIN                                                          
*                                                                               
         DS    0D                                                               
         DC    CL16'RECBUFF*RECBUFF*'                                           
RECBUFF  DS    256C                UNTRANSLATED RECORD FROM FILE                
         EJECT                                                                  
***********************************************************************         
* TITLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
BTITLE   DC    CL166'EZLOAD Invalid format records'                             
BTITLEU  DC    CL166'------ ------- ------ -------'                             
ETITLE   DC    CL166'EZLOAD E-Mail confirmation messages'                       
ETITLEU  DC    CL166'------ ------ ------------ --------'                       
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLE FOR INPUT DATA                                      *         
* TRANSLATES: ; TO .                                                  *         
*             " TO '                                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'*TRTAB**'                                                    
TRTAB    DC    XL16'40404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D4B5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040407A7B7C7D7E7D' 70-7F                     
         DC    XL16'40818283848586878889404040404040' 80-8F                     
         DC    XL16'40919293949596979899404040404040' 90-9F                     
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040' A0-AF                     
         DC    XL16'40404040404040404040404040404040' B0-BF                     
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040' C0-CF                     
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                     
         DC    XL16'E0E1E2E3E4E5E6E7E8E9404040404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF' F0-FF                     
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* BINSRCH RECORD BUILT FROM SOURCE ETC.                               *         
***********************************************************************         
*                                                                               
BEZKEYLQ EQU   L'EMTYPE+L'EMSRC+L'EMSTA                                         
*                                                                               
BEZD     DSECT                                                                  
*                                                                               
BEZKEY   DS    0CL(BEZKEYLQ)                                                    
BEZTYP   DS    CL(L'EMTYPE)                                                     
BEZSRC   DS    CL(L'EMSRC)                                                      
BEZSTA   DS    CL(L'EMSTA)                                                      
*                                                                               
BEZSRCE  DS    CL(L'EMSRC)                                                      
BEZMEDIA DS    CL(L'EZBMEDIA)                                                   
BEZAGY   DS    CL(L'EZBAGY)                                                     
BEZCALL  DS    CL(L'EZBCALL)                                                    
BEZNET   DS    CL(L'EZBNET)                                                     
BEZINVNO DS    CL(L'EZBINVNO)                                                   
BEZTODAY DS    CL(L'EZBTODAY)                                                   
BEZKLQ   EQU   *-BEZD              LENGTH OF KEY                                
BEZMOS   DS    CL(L'EZBMOS)                                                     
BEZNSPTS DS    CL(L'EZBNSPTS)                                                   
BEZGDOL  DS    CL(L'EZBGDOL)                                                    
BEZNDOL  DS    CL(L'EZBNDOL)                                                    
BEZEST   DS    CL(L'EZBEST)                                                     
BEZREPOR DS    CL(L'EZBREPOR)                                                   
BEZSTAOR DS    CL(L'EZBSTAOR)                                                   
BEZADVCD DS    CL(L'EZBADVCD)                                                   
BEZPRDCD DS    CL(L'EZBPRDCD)                                                   
BEZLDATE DS    CL(L'EZBLDATE)                                                   
BEZHDATE DS    CL(L'EZBHDATE)                                                   
BEZRLQ   EQU   *-BEZD              LENGTH OF RECORD                             
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* EMAIL RECORD BUILT FROM EZMAIL FILE                                 *         
***********************************************************************         
*                                                                               
EMRECD   DSECT                                                                  
EMTYPE   DS    CL1                 TYPE                                         
EMTSRC   EQU   C'S'                SOURCE                                       
EMTESRC  EQU   C'E'                EXCEPTION SOURCE (ALWAYS GETS MAIL)          
EMTSTN   EQU   C'N'                STATION                                      
EMTSTSR  EQU   C'P'                STATION/SOURCE PAIR                          
EMSRC    DS    CL4                 SOURCE                                       
EMSTA    DS    CL5                 STATION                                      
EMSEQ    DS    X                   SEQUENCE NUMBER                              
EMKLQ    EQU   *-EMRECD            LENGTH OF KEY                                
EMFLAG   DS    X                   FLAG BYTE                                    
EMFATTQ  EQU   X'01'               SEND DATA AS ATTACHMENT                      
EMADDR   DS    CL(L'EZMMAIL)                                                    
EMRLQ    EQU   *-EMRECD            LENGTH OF RECORD                             
*                                                                               
***********************************************************************         
* FORMAT OF EZMAIL INPUT FILE                                         *         
***********************************************************************         
*                                                                               
EZMAILD  DSECT                                                                  
EZMLINE  DS    0CL80                                                            
EZMSRC   DS    CL4                                                              
EZMSTA   DS    CL5                                                              
EZMTYPE  DS    C                                                                
         DS    CL2                 SPARE (WAS EZMTO)                            
EZMFLG   DS    C                   FLAG FIELD                                   
*                                  'A' - SEND DATA AS AN ATTACHMENT             
         DS    C                   SPARE                                        
*                                                                               
EZMMAIL  DS    CL(L'EZMLINE-(EZMMAIL-EZMLINE))                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* EMAIL LINE DETAIL DSECT                                             *         
***********************************************************************         
*                                                                               
EMD      DSECT                                                                  
EMSIGN   DS    CL8                                                              
         DS    C                                                                
EMMEDIA  DS    CL5                                                              
         DS    C                                                                
EMSTATN  DS    CL(L'EZBCALL+L'EZBNET)                                           
         DS    C                                                                
EMINVNO  DS    CL(L'EZBINVNO)                                                   
         DS    C                                                                
EMTODAY  DS    CL9                                                              
         DS    C                                                                
EMMOS    DS    CL4                                                              
         DS    C                                                                
EMSPOT   DS    CL11                                                             
         DS    C                                                                
EMNDOL   DS    CL15                                                             
         DS    C                                                                
EMGDOL   DS    CL15                                                             
         DS    C                                                                
         ORG   EMMEDIA+1                                                        
EMBEST   DS    CL10                                                             
         DS    C                                                                
EMBREPOR DS    CL10                                                             
         DS    C                                                                
EMBSTAOR DS    CL10                                                             
         DS    C                                                                
EMBADVCD DS    CL8                                                              
         DS    C                                                                
EMBPRDCD DS    CL8                                                              
         DS    C                                                                
EMBLDATE DS    CL9                                                              
         DS    C                                                                
EMBHDATE DS    CL9                                                              
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* DDDLCB DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* SPREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
* SPREPMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* EZBILLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE EZBILLD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPREPFH02 10/01/13'                                      
         END                                                                    
