*          DATA SET TAREP26    AT LEVEL 019 AS OF 04/09/14                      
*PHASE T70326C,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE TINVCON                                                                
         TITLE 'T70326 - P AND G INPUT TAPE FROM GREY'                          
T70326   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70326,R9,R8,R7                                    
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         L     R2,ASUBSYSD                                                      
         USING SUBSYSD,R2                                                       
         GOTO1 INITIAL,DMCB,0                                                   
         BAS   RE,MYCLEAR                                                       
         DROP  R2                                                               
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,OPENTAPE                                                      
         BAS   RE,PREP                                                          
         BAS   RE,CLOSTAPE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,SPLOPTH          ONLY VALIDATING OPTIONS SO FAR               
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         MVI   TRACOPT,C'N'                                                     
         MVI   SORTOPT,C'N'                                                     
         MVI   LISTOPT,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT6                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(4,R4),=C'SORT'   SORT OPTION                                  
         BNE   OPT8                                                             
         MVI   SORTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(4,R4),=C'LIST'   LIST OPTION                                  
         BNE   OPT10                                                            
         MVI   LISTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(6,R4),=C'AGENCY' CHANGE AGENCY FOR TESTING                    
         BNE   OPT12                                                            
         MVC   GREY,22(R4)                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(4,R4),=C'TAPE'   TAPE 'OPTION' DOESN'T DO ANYTHING            
*                                  NEED TO HAVE SOME INPUT!                     
         BNE   OPT18                                                            
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         SPACE 1                                                                
BADXIT   L     R5,ASUBSYSD                                                      
         USING SUBSYSD,R5          SYSTEM SPECIFIC WORK                         
         GOTO1 ERRXIT                                                           
         DROP  R5                                                               
         SPACE 1                                                                
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
TRALIMIT DC    PL6'0'                                                           
RECLIMIT DC    PL6'9999999'                                                     
REPLIMIT DC    PL6'9999999'                                                     
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,0),(1,PTODAY)                                     
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   MYSORTER,SORTER                                                  
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         MVC   MYTITLE,SPACES                                                   
         MVC   MYTITLE(17),=C'GREY PAYROLL TAPE'                                
         DROP  R5                                                               
         BAS   RE,SORTTAPE                                                      
         SPACE 1                                                                
MAINIO   GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    MYEOF                                                            
         LA    R4,PGREC                                                         
         USING PGD,R4                                                           
         MOVE  (PGREC,432),0(R2)                                                
         AP    CHKCOUNT,=P'1'                                                   
         CLI   LISTOPT,C'Y'        OPTION TO LIST ONLY                          
         BNE   MAINIO2                                                          
         MVC   MYP(132),PGREC                                                   
         BAS   RE,SPLAT                                                         
         B     MAINIO                                                           
         SPACE 1                                                                
MAINIO2  CP    CHKCOUNT,RECLIMIT                                                
         BH    MYEOF                                                            
         CP    CHKCOUNT,TRALIMIT                                                
         BH    MAINIO4                                                          
         BAS   RE,TRACEPG                                                       
         SPACE 1                                                                
MAINIO4  BAS   RE,GENCHEK          GENERATE A CHECK                             
         BAS   RE,REPCHEK          REPORT ON THE CHECK                          
         CLC   PGSSN,LASTSSN                                                    
         BE    *+8                                                              
         BAS   RE,ADDW4            MAY NEED TO ADD A W4 RECORD                  
         MVC   LASTSSN,PGSSN                                                    
         BAS   RE,GENINV           GENERATE AN INVOICE                          
         B     MAINIO                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              GENERATE A CHECK                                                 
         SPACE 3                                                                
GENCHEK  NTR1                                                                   
         LA    R4,PGREC                                                         
         USING PGD,R4                                                           
         LA    R6,NEWIO                                                         
         XC    NEWIO(41),NEWIO                                                  
         USING TLCKD,R6                                                         
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,GREY                                                     
         GOTO1 DATCON,DMCB,(0,PGQTRDTE),(1,SAVCDTE)                             
         GOTO1 DATCON,DMCB,(0,PGQTRDTE),(20,WORK)                               
         PACK  TLCKINV(4),WORK(7)                                               
         AP    CURRINV,=P'10'      CURRINV IS PL3                               
         MVC   TLCKINV+3(2),CURRINV    (PICK OFF THE TENS)                      
*                                                                               
         MVC   SAVINV,TLCKINV                                                   
         MVC   TLCKSSN,PGSSN                                                    
         MVI   TLCKSEQ,X'FF'                                                    
         SPACE 1                                                                
******   XC    ELEMENT,ELEMENT     AGENCY/INVOICE                               
******   LA    R5,ELEMENT                                                       
******   USING TAOID,R5                                                         
******   MVI   TAOIEL,TAOIELQ                                                   
******   MVI   TAOILEN,TAOILNQ                                                  
******   BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     CAST DETAIL                                  
         LA    R5,ELEMENT                                                       
         USING TACAD,R5                                                         
         MVI   TACAEL,TACAELQ                                                   
         MVI   TACALEN,TACALNQ                                                  
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     PAYMENT DETAIL                               
         LA    R5,ELEMENT                                                       
         USING TAPDD,R5                                                         
         MVI   TAPDEL,TAPDELQ                                                   
         MVI   TAPDLEN,TAPDLNQ                                                  
         MVC   TAPDINV,SAVINV                                                   
         MVI   TAPDW4TY,TAW4TYIN                                                
         MVI   TAPDOFF,C'1'                                                     
         OI    TAPDOPT3,TAPDOGRY                                                
         GOTO1 CONVCASH,DMCB,PGGRS,TAPDGRS                                      
         MVC   TAPDPAYI,TAPDGRS                                                 
         GOTO1 CONVCASH,DMCB,PGREXP,TAPDREXP                                    
         L     R1,TAPDGRS          ADJUST GROSS WITH REXP                       
         S     R1,TAPDREXP                                                      
         ST    R1,TAPDGRS                                                       
         L     R1,TAPDPAYI         ADJUST PAYMENT WITH REXP                     
         S     R1,TAPDREXP                                                      
         ST    R1,TAPDPAYI                                                      
         GOTO1 CONVCASH,DMCB,PGPNH,TAPDPNH                                      
         GOTO1 CONVCASH,DMCB,PGHNW,TAPDHNW                                      
         MVC   TAPDEMP,=C'PG '                                                  
         MVC   TAPDCLI,PANDG                                                    
         MVC   SAVEPD,ELEMENT                                                   
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     CHECK DETAIL                                 
         LA    R5,ELEMENT                                                       
         USING TACDD,R5                                                         
         MVI   TACDEL,TACDELQ                                                   
         MVI   TACDLEN,TACDLNQ                                                  
         GOTO1 DATCON,DMCB,(0,PGQTRDTE),(1,TACDDTE)                             
         MVC   TACDRUN,TACDDTE     RUN=CHECK                                    
         OI    TACDSTAT,TACDSGRY                                                
         GOTO1 CONVCASH,DMCB,PGGRS,TACDEARN                                     
         GOTO1 CONVCASH,DMCB,PGREXP,TACDNTAX                                    
         L     R1,TACDEARN         ADJUST GROSS WITH REXP                       
         S     R1,TACDNTAX                                                      
         ST    R1,TACDEARN                                                      
         GOTO1 CONVCASH,DMCB,PGNET,TACDNET                                      
         MVC   TACDSEQ,SAVINV+1    (USE INV YEAR/MON/SEQ)                       
         NI    TACDSEQ,X'FF'-X'80'                                              
         OI    TACDSEQ,TACDSGRY                                                 
         CLC   =C'0297',GREY       IF AGENCY NOT GREY                           
         BE    *+16                                                             
         L     R1,TACDSEQ          FORCE SEPERATE SEQUENCE NUMBERS              
         LA    R1,X'FFF'(R1)                                                    
         ST    R1,TACDSEQ                                                       
         SPACE 1                                                                
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     WITHHOLDING DETAIL                           
         LA    R5,ELEMENT                                                       
         USING TACWD,R5                                                         
         MVI   TACWEL,TACWELQ                                                   
         MVI   TACWLEN,TACWLNQ                                                  
         MVC   TACWUNIT,=C'FD '    FEDERAL                                      
         MVI   TACWMST,C'S'        ASSUMING SINGLE                              
         MVC   TACWEXS,=C'00'               ZERO EXEMPTIONS                     
         MVI   TACWSTAT,TACWSRES+TACWSWRK+TACWSTAX                              
         GOTO1 CONVCASH,DMCB,PGFIT,TACWTAX                                      
         GOTO1 CONVC7,DMCB,PGFICA,TACWFICA                                      
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         MVC   TACWUNIT(2),PGSTAWH STATE WH                                     
         MVI   TACWSTAT,TACWSTAX                                                
         CLC   PGSTAWH,PGSTAWK                                                  
         BNE   *+8                                                              
         OI    TACWSTAT,TACWSWRK                                                
         CLC   PGSTAWH,PGSTAABR                                                 
         BNE   *+8                                                              
         OI    TACWSTAT,TACWSRES                                                
         GOTO1 CONVCASH,DMCB,PGSIT,TACWTAX                                      
         GOTO1 CONVCASH,DMCB,PGSDI,TACWSDI                                      
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    SAVLOCAL,SAVLOCAL                                                
         MVC   TACWUNIT,PGLOCWH    LOCAL WH                                     
         GOTO1 CONVCASH,DMCB,PGLIT,TACWTAX                                      
         CLC   PGLOCWH,=C'000'     IF 000 PROVIDED                              
         BNE   GC1                                                              
         OC    TACWTAX,TACWTAX        AND THERE IS LOCAL TAX                    
         BZ    GC2                                                              
         MVC   TACWUNIT,=C'NYC'    USE NYC IF STATE = NY                        
         CLC   PGSTAWH(2),=C'NY'                                                
         BE    GC1                                                              
         MVC   TACWUNIT,=C'CIN'    USE CIN IF STATE = OH                        
         CLC   PGSTAWH(2),=C'OH'                                                
         BE    GC1                                                              
         DC    H'0'                TAKE A HIT                                   
         SPACE 1                                                                
GC1      MVI   TACWSTAT,TACWSTAX                                                
         CLC   TACWUNIT,PGLOCWK                                                 
         BNE   *+8                                                              
         OI    TACWSTAT,TACWSWRK                                                
         CLC   TACWUNIT,PGLOCID                                                 
         BNE   *+8                                                              
         OI    TACWSTAT,TACWSRES                                                
         XC    TACWSDI,TACWSDI                                                  
         MVC   SAVLOCAL,TACWUNIT                                                
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
GC2      XC    ELEMENT,ELEMENT     DUMMY YTDS                                   
         LA    R5,ELEMENT                                                       
         USING TACYD,R5                                                         
         MVI   TACYEL,TACYELQ                                                   
         MVI   TACYLEN,TACYLNQ                                                  
         MVC   TACYUNIT,=C'FD '    FED                                          
         BAS   RE,ADDEL                                                         
         MVC   TACYUNIT(2),PGSTAWH STATE                                        
         BAS   RE,ADDEL                                                         
         OC    SAVLOCAL,SAVLOCAL   LOCAL                                        
         BZ    GC4                                                              
         MVC   TACYUNIT,SAVLOCAL                                                
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
GC4      XC    ELEMENT,ELEMENT     OTHER DEDUCTIONS                             
         LA    R5,ELEMENT                                                       
         USING TAODD,R5                                                         
         MVI   TAODEL,TAODELQ                                                   
         MVI   TAODLEN,TAODLNQ                                                  
         MVI   TAODTYPE,TAODTYPM   MPR                                          
         GOTO1 CONVCASH,DMCB,PGMPR,TAODAMT                                      
         OC    TAODAMT,TAODAMT                                                  
         BZ    *+8                                                              
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         MVI   TAODTYPE,TAODTYPP   PERM CHARITIES                               
         GOTO1 CONVCASH,DMCB,PGPERM,TAODAMT                                     
         OC    TAODAMT,TAODAMT                                                  
         BZ    *+8                                                              
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         BAS   RE,CHECKIO                                                       
         CP    CHKCOUNT,TRALIMIT                                                
         BH    XIT                                                              
         MVC   RECTYPE,=CL16'CHECK RECORD'                                      
         BAS   RE,TRACENEW                                                      
         B     XIT                                                              
         EJECT                                                                  
*              REPORT ON THE CHECK                                              
         SPACE 3                                                                
REPCHEK  NTR1                                                                   
         LA    R4,PGREC                                                         
         USING PGD,R4                                                           
         LA    R5,MYP                                                           
         USING PRINTD,R5                                                        
         LA    R6,NEWIO                                                         
         USING TLCKD,R6                                                         
         SPACE 1                                                                
         MVC   PPERF(16),PGFRST                                                 
         MVC   PPERF+17(16),PGLAST                                              
         GOTO1 SQUASHER,DMCB,PPERF,33                                           
         MVC   PSSN(9),TLCKSSN                                                  
         MVC   PSSN+12(8),=C'INVOICE='                                          
         GOTO1 =V(TINVCON),DMCB,TLCKINV,PSSN+20                                 
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
REPC2    BAS   RE,NEXTEL                                                        
         BE    REPC4                                                            
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
REPC4    CLI   0(R6),TACDELQ                                                    
         BE    REPCD                                                            
         CLI   0(R6),TACWELQ                                                    
         BE    REPCW                                                            
         CLI   0(R6),TAODELQ                                                    
         BE    REPOD                                                            
         CLI   0(R6),TAPDELQ                                                    
         BE    REPPD                                                            
         B     REPC2                                                            
         SPACE 1                                                                
         USING TACDD,R6                                                         
REPCD    DS    0H                                                               
         EDIT  (4,TACDEARN),(12,PGROSS),2,MINUS=YES,ZERO=BLANK                  
         EDIT  (4,TACDNTAX),(12,PREXP),2,MINUS=YES,ZERO=BLANK                   
         EDIT  (4,TACDNET),(12,PNET),2,MINUS=YES,ZERO=BLANK                     
         L     R1,TACDEARN                                                      
         A     R1,TGROSS                                                        
         ST    R1,TGROSS                                                        
         L     R1,TACDNTAX                                                      
         A     R1,TREXP                                                         
         ST    R1,TREXP                                                         
         L     R1,TACDNET                                                       
         A     R1,TNET                                                          
         ST    R1,TNET                                                          
         B     REPC2                                                            
         SPACE 1                                                                
         USING TACWD,R6                                                         
REPCW    CLC   TACWUNIT,=C'FD '                                                 
         BE    REPCWFD                                                          
         CLI   TACWUNIT+2,C' '                                                  
         BNE   REPCWLOC                                                         
         MVC   PSTATE+1(2),TACWUNIT                                             
         EDIT  (4,TACWTAX),(12,PSIT),2,MINUS=YES,ZERO=BLANK                     
         EDIT  (4,TACWSDI),(12,PSDI),2,MINUS=YES,ZERO=BLANK                     
         L     R1,TACWTAX                                                       
         A     R1,TSIT                                                          
         ST    R1,TSIT                                                          
         L     R1,TACWSDI                                                       
         A     R1,TSDI                                                          
         ST    R1,TSDI                                                          
         B     REPC2                                                            
         SPACE 1                                                                
REPCWFD  EDIT  (4,TACWTAX),(12,PFIT),2,MINUS=YES,ZERO=BLANK                     
         EDIT  (4,TACWFICA),(12,PFICA),2,MINUS=YES,ZERO=BLANK                   
         L     R1,TACWTAX                                                       
         A     R1,TFIT                                                          
         ST    R1,TFIT                                                          
         L     R1,TACWFICA                                                      
         A     R1,TFICA                                                         
         ST    R1,TFICA                                                         
         B     REPC2                                                            
         SPACE 1                                                                
REPCWLOC EDIT  (4,TACWTAX),(12,PLIT),2,MINUS=YES,ZERO=BLANK                     
         L     R1,TACWTAX                                                       
         A     R1,TLIT                                                          
         ST    R1,TLIT                                                          
         MVC   PLOCAL+1(3),TACWUNIT                                             
         B     REPC2                                                            
         SPACE 1                                                                
         USING TAODD,R6                                                         
REPOD    DS    0H                                                               
         CLI   TAODTYPE,TAODTYPP                                                
         BE    REPODP                                                           
         EDIT  (4,TAODAMT),(12,PMPR),2,MINUS=YES,ZERO=BLANK                     
         B     REPC2                                                            
         SPACE 1                                                                
REPODP   EDIT  (4,TAODAMT),(12,PPERM),2,MINUS=YES,ZERO=BLANK                    
         B     REPC2                                                            
         SPACE 1                                                                
         USING TAPDD,R6                                                         
REPPD    DS    0H                                                               
         EDIT  (4,TAPDPNH),(12,PPNH),2,MINUS=YES,ZERO=BLANK                     
         EDIT  (4,TAPDHNW),(12,PHNW),2,MINUS=YES,ZERO=BLANK                     
         L     R1,TAPDPNH                                                       
         A     R1,TPNH                                                          
         ST    R1,TPNH                                                          
         L     R1,TAPDHNW                                                       
         A     R1,THNW                                                          
         ST    R1,THNW                                                          
         B     REPC2                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              GENERATE AN INVOICE                                              
         SPACE 3                                                                
GENINV   NTR1                                                                   
         LA    R4,PGREC                                                         
         USING PGD,R4                                                           
         LA    R6,NEWIO                                                         
         XC    NEWIO(41),NEWIO                                                  
         USING TLIND,R6                                                         
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,GREY                                                     
         MVC   TLININV,SAVINV                                                   
         XC    TLININV,=6X'FF'                                                  
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     AGENCY/INVOICE                               
         LA    R5,ELEMENT                                                       
         USING TAOID,R5                                                         
         MVI   TAOIEL,TAOIELQ                                                   
         MVI   TAOILEN,TAOILNQ                                                  
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     INVOICE STATUS                               
         LA    R5,ELEMENT                                                       
         USING TAIND,R5                                                         
         MVI   TAINEL,TAINELQ                                                   
         MVI   TAINLEN,TAINLNQ                                                  
         GOTO1 DATCON,DMCB,(0,PGQTRDTE),(1,TAINCDTE)                            
         OI    TAINSTAT,TAINSCHK                                                
         MVC   TAINCKRN,TAINCDTE                                                
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         MVC   ELEMENT,SAVEPD      PAY DETAILS - SAME AS CHECK                  
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     DUE DATE                                     
         LA    R5,ELEMENT                                                       
         USING TADDD,R5                                                         
         MVI   TADDEL,TADDELQ                                                   
         MVI   TADDLEN,TADDLNQ                                                  
         GOTO1 DATCON,DMCB,(0,PGQTRDTE),(1,TADDDATE)                            
         BAS   RE,ADDEL                                                         
         BAS   RE,INVIO                                                         
         SPACE 1                                                                
         CP    CHKCOUNT,TRALIMIT                                                
         BH    XIT                                                              
         MVC   RECTYPE,=CL16'INVOICE'                                           
         BAS   RE,TRACENEW                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ADD A W4 RECORD IF NECESSARY                                     
         SPACE 3                                                                
ADDW4    NTR1                                                                   
         LA    R4,PGREC                                                         
         USING PGD,R4                                                           
         LA    R6,NEWIO                                                         
         XC    NEWIO(41),NEWIO                                                  
         USING TLW4D,R6                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,PGSSN                                                    
         MVC   KEY,NEWIO                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    XIT                                                              
         SPACE 1                                                                
         MVI   TLW4STA2,TAW4TYIN   SET W4 TYPE IN 2ND STATUS BYTE               
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     NEW STYLE ADDRESS ELEMENT                    
         LA    R5,ELEMENT                                                       
         USING TAA2D,R5                                                         
         MVI   TAA2EL,TAA2ELQ                                                   
         MVI   TAA2LEN,TAA2LNQ                                                  
         LA    R0,3                THERE ARE 3 ADDRESS LINES                    
         LA    R1,PGADD1                                                        
         LA    R2,TAA2ADD1                                                      
AW40     CLC   0(L'PGADD1,R1),MYSPACES  IF THE ADDRESS LINE ISN'T BLANK         
         BE    AW41                                                             
         MVC   0(L'PGADD1,R2),0(R1)     SAVE IN ELEMENT                         
         OC    0(L'TAA2ADD1,R2),MYSPACES                                        
         LA    R2,L'TAA2ADD1(R2)   BUMP TO NEXT FIELD IN ELEMENT                
AW41     LA    R1,L'PGADD1(R1)     BUMP TO NEXT FIELD ON TAPE                   
         BCT   R0,AW40                                                          
         SPACE 1                                                                
         CLC   PGCTYSTA,MYSPACES                                                
         BNH   AW44                                                             
         LA    R1,PGCTYSTA+L'PGCTYSTA-1  START FROM END TO FIND STATE           
AW42     CLI   0(R1),C' '                                                       
         BH    AW43                                                             
         BCTR  R1,0                                                             
         B     AW42                                                             
         SPACE                                                                  
AW43     BCTR  R1,0                GET TO START OF 2 LETTER STATE CODE          
         MVC   TAA2ST,0(R1)                                                     
         SPACE                                                                  
         LA    R2,PGCTYSTA                                                      
         SR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAA2CITY(0),PGCTYSTA                                             
         OC    TAA2CITY,MYSPACES                                                
         SPACE                                                                  
AW44     CLC   PGZIP,MYSPACES         IF HAVE ZIP                               
         BE    AW45                                                             
         MVC   TAA2ZIP(5),PGZIP                                                 
         CLC   PGZIP+5(4),MYSPACES    IF HAVE EXTENDED ZIP                      
         BNH   *+14                                                             
         MVI   TAA2ZIP+5,C'-'         PUT "-"                                   
         MVC   TAA2ZIP+6(4),PGZIP+5   BEFORE IT                                 
         OC    TAA2ZIP,MYSPACES                                                 
AW45     BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     W4 ELEMENT                                   
         LA    R5,ELEMENT                                                       
         USING TAW4D,R5                                                         
         MVI   TAW4EL,TAW4ELQ                                                   
         MVI   TAW4LEN,TAW4LNQ                                                  
         MVI   TAW4TYPE,TAW4TYIN                                                
         MVC   TAW4NAM2,PGLAST                                                  
         MVC   TAW4NAM1,PGFRST                                                  
         MVI   TAW4SEX,C'M'                                                     
         MVC   TAW4RACE,MYSPACES                                                
         MVI   TAW4FREQ,C'W'                                                    
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     WH ELEMENTS                                  
         LA    R5,ELEMENT                                                       
         USING TAWHD,R5                                                         
         MVI   TAWHEL,TAWHELQ                                                   
         MVI   TAWHLEN,TAWHLNQ                                                  
******** MVC   TAWHEMP,=C'PG '                                                  
         MVC   TAWHEMP,=C'TP '                                                  
         MVC   TAWHUNIT,=C'FD '                                                 
         MVI   TAWHSTAT,C'S'                                                    
         BAS   RE,ADDEL                                                         
         MVC   TAWHUNIT(2),PGSTAABR                                             
         BAS   RE,ADDEL                                                         
         MVC   TAWHUNIT,PGLOCWH                                                 
         CLC   TAWHUNIT,=C'000'                                                 
         BE    *+8                                                              
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         LA    R5,MYP                                                           
         USING PRINTD,R5                                                        
         MVC   PGROSS,=CL12'W4 ADDED'                                           
         LA    R6,NEWIO                                                         
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAA2D,R6                                                         
         LA    R2,TAA2ADD1                                                      
         LA    R0,3                                                             
         LA    R3,PPERF                                                         
         SPACE 1                                                                
AW46     CLC   0(30,R2),MYSPACES      TEST FOR REMAINING LINES                  
         BE    AW48                                                             
         MVC   0(30,R3),0(R2)                                                   
         LA    R3,132(R3)                                                       
         LA    R2,30(R2)                                                        
         BCT   R0,AW46                                                          
         SPACE                                                                  
AW48     XC    WORK(35),WORK                                                    
         MVC   WORK(21),TAA2CITY      CITY (L'21 FROM GREY)                     
         MVC   WORK+22(2),TAA2ST      STATE                                     
         MVC   WORK+25(10),TAA2ZIP    ZIP                                       
         GOTO1 SQUASHER,DMCB,WORK,35                                            
         MVC   0(L'PPERF,R3),WORK                                               
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         BAS   RE,W4IO                                                          
         SPACE 1                                                                
         AP    AW4COUNT,=P'1'                                                   
         CP    AW4COUNT,TRALIMIT                                                
         BH    XIT                                                              
         MVC   RECTYPE,=CL16'W4'                                                
         BAS   RE,TRACENEW                                                      
         B     XIT                                                              
         EJECT                                                                  
*              END OF FILE PROCEDURES                                           
         SPACE 3                                                                
MYEOF    LA    R5,MYP                                                           
         USING PRINTD,R5                                                        
         MVC   PPERF(16),=C'QUARTERLY TOTALS'                                   
         EDIT  (P8,CHKCOUNT),(8,PSSN)                                           
         MVC   PSSN+9(6),=C'CHECKS'                                             
         EDIT  (4,TGROSS),(12,PGROSS),2,MINUS=YES,ZERO=BLANK                    
         EDIT  (4,TREXP),(12,PREXP),2,MINUS=YES,ZERO=BLANK                      
         EDIT  (4,TFIT),(12,PFIT),2,MINUS=YES,ZERO=BLANK                        
         EDIT  (4,TSIT),(12,PSIT),2,MINUS=YES,ZERO=BLANK                        
         EDIT  (4,TLIT),(12,PLIT),2,MINUS=YES,ZERO=BLANK                        
         EDIT  (4,TSDI),(12,PSDI),2,MINUS=YES,ZERO=BLANK                        
         EDIT  (4,TNET),(12,PNET),2,MINUS=YES,ZERO=BLANK                        
         EDIT  (4,TFICA),(12,PFICA),2,MINUS=YES,ZERO=BLANK                      
         BAS   RE,SPLAT                                                         
         EDIT  (P8,AW4COUNT),(8,PPERF)                                          
         MVC   PPERF+9(9),=C'W4S ADDED'                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              TAPE & SORT ROUTINES                                             
         SPACE 3                                                                
OPENTAPE NTR1                                                                   
         L     R2,=A(TAPEIN)                                                    
         OPEN  ((2),INPUT)                                                      
         B     XIT                                                              
         SPACE 1                                                                
CLOSTAPE NTR1                                                                   
         L     R2,=A(TAPEIN)                                                    
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 1                                                                
SORTTAPE NTR1                                                                   
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         L     R2,ASUBSYSD                                                      
         USING SUBSYSD,R2                                                       
         SPACE 1                                                                
SORTT2   LA    R4,PGREC                                                         
         USING PGD,R4                                                           
         L     R1,=A(TAPEIN)                                                    
         GET   (1),(4)                                                          
         SPACE 1                                                                
         LA    RE,QTRTAB           ROUTINE TO CHECK DATE OK                     
SORTT3   CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAUSE DUMP (NEED TO LOOK AT TAPE)            
         CLC   PGQTRDTE+2(2),2(RE) IF MONTH OF DATE IS QTR END                  
         BE    SORTT8              DONE                                         
         CLC   TGTODAY0+2(2),0(RE) IF TODAY'S MTH IS MTH AFTER QTR END          
         BE    *+12                                                             
         LA    RE,L'QTRTAB(RE)                                                  
         B     SORTT3                                                           
*                                                                               
         MVC   PGQTRDTE+2(4),2(RE) SET TO LAST DAY OF PREVIOUS QTR              
         CLC   =C'12',PGQTRDTE+2   IF ADJUSTED TO LAST MONTH OF YEAR            
         BNE   SORTT8              ADJUST THE YEAR                              
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),WORK,-1                               
         MVC   PGQTRDTE(2),WORK                                                 
*                                                                               
SORTT8   MVC   PGQTRDTE,=C'991027' FUDGE QUARTER DATE                           
         CLI   PGCORP,C'Y'                                                      
         BNE   SORTT10                                                          
         MVC   MYP+1(19),=C'CORP RECORD DROPPED'                                
         BAS   RE,SPLAT                                                         
         BAS   RE,TRACEPG                                                       
         B     SORTT2                                                           
         SPACE 1                                                                
SORTT10  GOTO1 MYSORTER,DMCB,=C'PUT',PGREC                                      
         B     SORTT2                                                           
         DROP  R2                                                               
         SPACE 1                                                                
TAPEEOF  B     XIT                                                              
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,9,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(432)'                                 
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEPG  NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),=CL16'TAPE RECORD'                                    
         BAS   RE,SPLAT                                                         
         LA    R6,PGREC                                                         
         LA    R0,4                                                             
         SPACE 1                                                                
TPG2     LA    R2,100                                                           
         BAS   RE,TRACEL                                                        
         LA    R6,100(R6)                                                       
         BCT   R0,TPG2                                                          
         LA    R2,32                                                            
         BAS   RE,TRACEL                                                        
         B     XIT                                                              
         SPACE 1                                                                
TRACENEW NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+56(4),=C'NEW '                                               
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R6,NEWIO                                                         
         LA    R2,NEWDATA-NEWIO                                                 
         BAS   RE,TRACEL                                                        
         SPACE 1                                                                
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACE4   BAS   RE,NEXTEL                                                        
         BE    TRACE6                                                           
         B     XIT                                                              
         SPACE 1                                                                
TRACE6   ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACE4                                                           
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   MYP,MYSPACES                                                     
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
         OC    MYP,MYSPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),MYP3,132,=C'SEP'                                
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP2(0),MYP3                                                     
         MVC   MYP3,MYSPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP3(0),MYP4                                                     
         MVC   MYP4,MYSPACES                                                    
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              I/O ROUTINES                                                     
         SPACE 3                                                                
CHECKIO  NTR1                                                                   
         MVC   FILENAME,=CL8'CHKDIR'                                            
         MVC   KEY,NEWIO                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    CHECKIOX                                                         
         LH    R3,NEWLEN                                                        
         L     R2,AIO                                                           
         LA    R4,NEWIO                                                         
         MOVE  ((R2),(R3)),(R4)                                                 
         MVC   FILENAME,=CL8'CHKFIL'                                            
         GOTO1 ADDREC                                                           
         SPACE 1                                                                
         LA    R4,KEY              BUILD A PASSIVE POINTER                      
         USING TLDRD,R4                                                         
         LA    R5,PGREC                                                         
         USING PGD,R5                                                           
         XC    TLDRREC,TLDRREC                                                  
         MVC   TLDRDA,DMDSKADD                                                  
         USING TLCKPD,R4                                                        
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,PGSSN                                                   
         MVI   TLCKECUR,C'U'                                                    
         MVC   TLCKEEMP,=C'PG '                                                 
         GOTO1 DATCON,DMCB,(0,PGQTRDTE),(1,TLCKEDTE)                            
         XC    TLCKEDTE,=3X'FF'    COMP THE DATE                                
         MVC   TLCKESEQ,SAVINV+1                                                
         NI    TLCKESEQ,X'FF'-X'80'                                             
         OI    TLCKESEQ,TACDSGRY                                                
*                                                                               
         CLC   =C'0297',GREY       IF AGENCY NOT GREY                           
         BE    *+16                                                             
         ICM   R1,15,TLCKESEQ      FORCE SEPERATE SEQUENCE NUMBERS              
         LA    R1,X'FFF'(R1)                                                    
         STCM  R1,15,TLCKESEQ                                                   
*                                                                               
         XC    TLCKESEQ,=6X'FF'                                                 
         MVC   TLCKEAGY,GREY                                                    
         MVC   FILENAME,=CL8'CHKDIR'                                            
         GOTO1 ADD                                                              
         SPACE 1                                                                
         CP    CHKCOUNT,TRALIMIT                                                
         BH    CHECKIOX                                                         
         MVC   MYP+1(25),=CL25'ADDING CHECK PASSIVE'                            
         BAS   RE,SPLAT                                                         
         LA    R6,KEY                                                           
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         SPACE 1                                                                
CHECKIOX XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 1                                                                
W4IO     NTR1                                                                   
         LH    R3,NEWLEN                                                        
         L     R2,AIO                                                           
         LA    R4,NEWIO                                                         
         MOVE  ((R2),(R3)),(R4)                                                 
         GOTO1 ADDREC                                                           
         B    XIT                                                               
         SPACE 1                                                                
INVIO    NTR1                                                                   
         MVC   KEY,NEWIO                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    XIT                                                              
         LH    R3,NEWLEN                                                        
         L     R2,AIO                                                           
         LA    R4,NEWIO                                                         
         MOVE  ((R2),(R3)),(R4)                                                 
         GOTO1 ADDREC                                                           
******   GOTO1 ADDPTRS                                                          
******   B     INVIO2                                                           
         SPACE 1                                                                
         LA    R4,KEY              BUILD A PASSIVE POINTER                      
         USING TLDRD,R4                                                         
         XC    TLDRREC,TLDRREC                                                  
         MVC   TLDRDA,DMDSKADD                                                  
         USING TLINPD,R4                                                        
         MVI   TLINPCD,TLINKCDQ                                                 
         MVC   TLINKDTE,SAVCDTE                                                 
         MVC   TLINKAGY,GREY                                                    
         MVC   TLINKINV,SAVINV                                                  
         MVI   TLINKCUR,C'U'                                                    
         MVC   TLINKEMP,=C'PG '                                                 
         MVI   TLINKOFF,C'1'                                                    
         MVC   TLINKCLI,PANDG                                                   
         GOTO1 ADD                                                              
         SPACE 1                                                                
INVIO2   CP    CHKCOUNT,TRALIMIT                                                
         BH    XIT                                                              
         MVC   MYP+1(20),=CL20'ADDING PASSIVE KEY'                              
         BAS   RE,SPLAT                                                         
         LA    R6,KEY                                                           
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
ADDEL    NTR1                      ADD ELEMENT TO NEWIO                         
*                                  UPDATING LENGTH                              
         GOTO1 =V(HELLO),DMCB,(C'P',=C'TALFIL'),NEWIO,ELEMENT                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R1,NEWIO                                                         
         AH    R1,NEWLEN                                                        
         MVI   0(R1),0                                                          
         B     XIT                                                              
         SPACE 1                                                                
DELEL    NTR1                      REMOVE ELEMENT FROM NEWIO                    
*                                  UPDATING LENGTH                              
         GOTO1 =V(HELLO),DMCB,(C'D',=C'TALFIL'),(ELCODE,NEWIO),0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R1,NEWIO                                                         
         AH    R1,NEWLEN                                                        
         MVI   0(R1),0                                                          
         B     XIT                                                              
         EJECT                                                                  
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
CONVCASH NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         XC    0(4,R3),0(R3)                                                    
         CLI   0(R2),C' '                                                       
         BE    XIT                                                              
         PACK  DUB,0(9,R2)                                                      
         CVB   R1,DUB                                                           
         BCTR  R2,0                                                             
         CLI   0(R2),C'0'                                                       
         BE    *+6                                                              
         LCR   R1,R1                                                            
         ST    R1,0(R3)                                                         
         B     XIT                                                              
         SPACE 1                                                                
CONVC7   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         XC    0(4,R3),0(R3)                                                    
         CLI   0(R2),C' '                                                       
         BE    XIT                                                              
         PACK  DUB,0(7,R2)                                                      
         CVB   R1,DUB                                                           
         BCTR  R2,0                                                             
         CLI   0(R2),C'0'                                                       
         BE    *+6                                                              
         LCR   R1,R1                                                            
         ST    R1,0(R3)                                                         
         B     XIT                                                              
         SPACE 1                                                                
SETSKIP  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R5                                                               
         B     XIT                                                              
         SPACE 1                                                                
SETPAGE  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   PAGE,=H'1'                                                       
         DROP  R5                                                               
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         GOTO1 SPOOL,DMCB,(R5)                                                  
         DROP  R5                                                               
         BAS   RE,MYCLEAR                                                       
         B     XIT                                                              
         SPACE 1                                                                
MYCLEAR  NTR1                                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   H1+52(24),MYTITLE                                                
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H6,MYH6                                                          
         MVC   H7,MYH7                                                          
         L     R6,ABOX                                                          
         USING BOXD,R6                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,MYCOLS                                                   
         MVC   BOXROWS,MYROWS                                                   
         MVI   BOXOFF,0                                                         
         XIT1                                                                   
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         SPACE 1                                                                
MYROWS   DC    C'    T  M'                                                      
         DC    CL50' '                                                          
         DC    CL80'B'                                                          
         SPACE 1                                                                
MYH6     DS    0D                                                               
         DC    CL01' '                                                          
         DC    CL33'            PERFORMER'                                      
         DC    CL01' '                                                          
         DC    CL12'   GROSS'                                                   
         DC    CL01' '                                                          
         DC    CL12'    FIT'                                                    
         DC    CL01' '                                                          
         DC    CL12'   P && H'                                                  
         DC    CL01' '                                                          
         DC    CL05'STATE'                                                      
         DC    CL01' '                                                          
         DC    CL12'    SIT'                                                    
         DC    CL01' '                                                          
         DC    CL12'  CANADIAN'                                                 
         DC    CL01' '                                                          
         DC    CL12'    PERM'                                                   
         DC    CL01' '                                                          
         DC    CL12'    MISC'                                                   
         DC    CL01' '                                                          
         SPACE 1                                                                
MYH7     DS    0D                                                               
         DC    CL01' '                                                          
         DC    CL33' '                                                          
         DC    CL01' '                                                          
         DC    CL12' REIMBURSED'                                                
         DC    CL01' '                                                          
         DC    CL12'    FICA'                                                   
         DC    CL01' '                                                          
         DC    CL12'   H && W'                                                  
         DC    CL01' '                                                          
         DC    CL05'LOCAL'                                                      
         DC    CL01' '                                                          
         DC    CL12'    SDI'                                                    
         DC    CL01' '                                                          
         DC    CL12'    LIT'                                                    
         DC    CL01' '                                                          
         DC    CL12'    MPR'                                                    
         DC    CL01' '                                                          
         DC    CL12'    NET'                                                    
         DC    CL01' '                                                          
         SPACE 1                                                                
MYCOLS   DS    0D                                                               
         DC    CL01'L'                                                          
         DC    CL33' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL05' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'R'                                                          
*              LTORG ETC                                                        
         SPACE 3                                                                
QTRTAB   DS    0CL6                2-TODAYS DATE,4-END DATE OF QTR              
         DC    C'040331'           APRIL OR MARCH ->MARCH 31                    
         DC    C'070630'           JULY OR JUNE   ->JUNE 30                     
         DC    C'100930'           OCT. OR SEP.   ->SEPTEMBER 30                
         DC    C'011231'           JAN. OR DEC.   ->DECEMBER 31                 
         DC    X'FF'                                                            
         SPACE 2                                                                
CHKCOUNT DC    PL8'0'                                                           
AW4COUNT DC    PL8'0'                                                           
TPCH     DC    H'1878'                                                          
GREY     DC    CL6'0297'                                                        
PANDG    DC    CL6'00600'                                                       
CURRINV  DC    PL3'0'                                                           
TGROSS   DC    F'0'                                                             
TFICA    DC    F'0'                                                             
TFIT     DC    F'0'                                                             
TSIT     DC    F'0'                                                             
TSDI     DC    F'0'                                                             
TLIT     DC    F'0'                                                             
TNET     DC    F'0'                                                             
TPNH     DC    F'0'                                                             
THNW     DC    F'0'                                                             
TREXP    DC    F'0'                                                             
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 3                                                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=TAPEEOF,        X        
               RECFM=FU,LRECL=432,BUFNO=2,BLKSIZE=0                             
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
PGREC    DS    432C                IO AREA FOR TAPE RECORDS                     
         SPACE 1                                                                
MYREGS   DS    16F                                                              
MYSORTER DS    V                                                                
MYTITLE  DS    CL32                                                             
*                                  OPTIONS                                      
SORTOPT  DS    CL1                 Y=SORT W4 FOR DUPLICATES                     
TRACOPT  DS    CL1                 Y=TRACE                                      
LISTOPT  DS    CL1                 Y=LIST AND COUNT ONLY                        
         DS    CL7                 SPARE                                        
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYSPACES DS    CL132                                                            
RECTYPE  DS    CL16                                                             
SAVINV   DS    CL6                                                              
SAVCDTE  DS    PL3                                                              
SAVLOCAL DS    CL3                                                              
SAVEPD   DS    CL200                                                            
LASTSSN  DS    CL9                                                              
         SPACE 1                                                                
*                                  FILTERS                                      
PTODAY   DS    PL3                                                              
LTODAY   DS    PL3                                                              
THISSSN  DS    CL9                                                              
THISAGY  DS    CL6                                                              
         SPACE 1                                                                
         DS    0D                                                               
NEWIO    DS    0C                                                               
NEWKEY   DS    CL32                                                             
NEWLEN   DS    CL2                                                              
NEWSTAT  DS    CL1                                                              
         DS    CL5                                                              
NEWDATA  DS    2000C                                                            
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT FOR P&G TAPE FROM GREY                                     
PGD      DSECT                                                                  
PGSSN    DS    CL9                                                              
PGSTAID  DS    CL2                                                              
PGLOCID  DS    CL3                                                              
PGSTAWK  DS    CL2                                                              
PGLOCWK  DS    CL3                                                              
PGCORP   DS    CL1                                                              
PGLOCLUN DS    CL4                                                              
PGSTAWH  DS    CL2                                                              
PGLOCWH  DS    CL3                                                              
PGCHECK  DS    CL8                                                              
PGQTRDTE DS    CL6                                                              
PGCHKDTE DS    CL6                                                              
         DS    CL1                                                              
PGGRS    DS    CL9                                                              
         DS    CL1                                                              
         DS    CL9                                                              
         DS    CL1                                                              
PGREXP   DS    CL9                                                              
         DS    CL1                                                              
PGFIT    DS    CL9                                                              
         DS    CL1                                                              
PGFICA   DS    CL7                                                              
         DS    CL1                                                              
PGSIT    DS    CL9                                                              
         DS    CL1                                                              
PGSDI    DS    CL9                                                              
         DS    CL1                                                              
PGPERM   DS    CL9                                                              
         DS    CL1                                                              
PGMISC   DS    CL9                                                              
         DS    CL1                                                              
PGNET    DS    CL9                                                              
PGHRS    DS    CL5                                                              
PGOT     DS    CL5                                                              
PGPREM   DS    CL5                                                              
PGDAYS   DS    CL3                                                              
         DS    CL30                                                             
         DS    CL1                                                              
PGMPR    DS    CL9                                                              
         DS    CL1                                                              
PGLIT    DS    CL9                                                              
         DS    CL1                                                              
PGCANTAX DS    CL9                                                              
         DS    CL1                                                              
PGPNH    DS    CL9                                                              
         DS    CL1                                                              
PGHNW    DS    CL9                                                              
PGCID    DS    CL10                                                             
PGRUNNO  DS    CL6                                                              
PGAGENCY DS    CL4                                                              
         DS    CL27                                                             
PGLAST   DS    CL16                                                             
PGFRST   DS    CL16                                                             
PGADD1   DS    CL24                                                             
PGADD2   DS    CL24                                                             
PGADD3   DS    CL24                                                             
PGCTYSTA DS    CL24                                                             
PGZIP    DS    CL9                                                              
PGSTAABR DS    CL2                                                              
         DS    CL1                                                              
         EJECT                                                                  
         SPACE 1                                                                
*              DSECT FOR PRINT LINE                                             
PRINTD   DSECT                                                                  
         DC    CL01' '                                                          
PPERF    DC    CL33' '                                                          
         DC    CL01' '                                                          
PGROSS   DC    CL12' '                                                          
         DC    CL01' '                                                          
PFIT     DC    CL12' '                                                          
         DC    CL01' '                                                          
PPNH     DC    CL12' '                                                          
         DC    CL01' '                                                          
PSTATE   DC    CL05' '                                                          
         DC    CL01' '                                                          
PSIT     DC    CL12' '                                                          
         DC    CL01' '                                                          
PCAN     DC    CL12' '                                                          
         DC    CL01' '                                                          
PPERM    DC    CL12' '                                                          
         DC    CL01' '                                                          
PMISC    DC    CL12' '                                                          
         DC    CL01' '                                                          
         SPACE 1                                                                
PLINE2   DC    CL01' '             THIS IS ON LINE 2                            
PSSN     DC    CL33' '                                                          
         DC    CL01' '                                                          
PREXP    DC    CL12' '                                                          
         DC    CL01' '                                                          
PFICA    DC    CL12' '                                                          
         DC    CL01' '                                                          
PHNW     DC    CL12' '                                                          
         DC    CL01' '                                                          
PLOCAL   DC    CL05' '                                                          
         DC    CL01' '                                                          
PSDI     DC    CL12' '                                                          
         DC    CL01' '                                                          
PLIT     DC    CL12' '                                                          
         DC    CL01' '                                                          
PMPR     DC    CL12' '                                                          
         DC    CL01' '                                                          
PNET     DC    CL12' '                                                          
         DC    CL01' '                                                          
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TAREPFFD                                                                       
         PRINT OFF                                                              
         USING SUBSYSD,R9                                                       
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPE6D                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019TAREP26   04/09/14'                                      
         END                                                                    
