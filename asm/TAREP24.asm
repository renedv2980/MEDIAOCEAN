*          DATA SET TAREP24    AT LEVEL 014 AS OF 04/08/14                      
*PHASE T70324A                                                                  
*INCLUDE PERVERT                                                                
         TITLE 'T70324 - MCDONALDS LIFE OF COMMERCIALS'                         
T70324   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70324,R7,R8                                       
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         BAS   RE,MYCLEAR                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
*                                  OPTIONAL FIELDS                              
         SPACE 1                                                                
         LA    R2,SPLPERH          PERIOD                                       
         GOTO1 ANY                                                              
         ST    R2,APERH                                                         
         GOTO1 VALPERD                                                          
         SPACE 1                                                                
         LA    R2,SPLAGGH          AGENCY GROUP                                 
         XC    TIFAGG,TIFAGG                                                    
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC4                                                            
         GOTO1 RECVAL,DMCB,TLAGCDQ,(R2),0                                       
         MVC   TIFAGG,TGAGG                                                     
         SPACE 1                                                                
VREC4    LA    R2,SPLAGYH          AGENCY                                       
         XC    TIFAGY,TIFAGY                                                    
         TM    WHEN,X'20'          REQUIRED IF SOON                             
         BZ    VREC5                                                            
         GOTO1 ANY                                                              
         B     *+12                                                             
VREC5    CLI   5(R2),0                                                          
         BE    VREC6                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC6                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2),0                                       
         MVC   TIFAGY,TGAGY                                                     
         SPACE 1                                                                
VREC6    LA    R2,SPLCLIH          CLIENT                                       
         XC    TIFCLI,TIFCLI                                                    
         TM    WHEN,X'20'          REQUIRED IF SOON                             
         BZ    VREC7                                                            
         GOTO1 ANY                                                              
         B     *+12                                                             
VREC7    CLI   5(R2),0                                                          
         BE    VREC8                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC8                                                            
         GOTO1 RECVAL,DMCB,(X'40',TLCLCDQ),(R2)                                 
         CLI   8(R2),C'@'          TEST FOR FLIST                               
         BE    *+14                                                             
         MVC   TIFCLI,TGCLI                                                     
         B     *+14                                                             
         MVC   TIFCLI,TGLST                                                     
         NI    TIFCLI,X'7F'        SET CLIENT FILTER IS FLIST RECORD            
         SPACE 1                                                                
VREC8    LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
         LA    R2,SPLTITLH         ANY TITLE?                                   
         MVC   MYTITLE,MYSPACES                                                 
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   MYTITLE,WORK                                                     
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         MVI   TRACOPT,C'N'                                                     
         MVI   BLANKOPT,C'Y'                                                    
         ZAP   TRALIMIT,=P'0'                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT4                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(5,R4),=C'BLANK'  BLANK OPTION                                 
         BNE   OPT6                                                             
         MVC   BLANKOPT,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
         SPACE 1                                                                
OPT6     DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         GOTO1 ERRXIT                                                           
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
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         MVC   MYSORTER,SORTER                                                  
         DROP  R5                                                               
         CLI   MYTITLE,C' '                                                     
         BH    *+10                                                             
         MVC   MYTITLE(34),=C'MCDONALD TV COMMERCIAL LIFE REPORT'               
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         SPACE 1                                                                
         MVI   TIREAD,TLUHCDQ      SET TO READ USAGE HISTORY                    
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   SORTFRST,C'Y'         TEST NOTHING TO REPORT ON                  
         BE    XIT                                                              
         BAS   RE,DOREPS                                                        
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         AP    USGCOUNT,=P'1'                                                   
         CP    USGCOUNT,TRALIMIT                                                
         BH    IOHOOK2                                                          
         BAS   RE,TRACEINP                                                      
         SPACE 1                                                                
IOHOOK2  CLI   TIMODE,PROCCOMM     SYSIO WILL PAS THE COMMERCIAL                
         BE    IOHOOK4                                                          
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         BAS   RE,PROCUH                                                        
         B     XIT                                                              
IOHOOK4  BAS   RE,COMEND                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS USAGE HISTORY                                            
         SPACE 3                                                                
PROCUH   NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TLUHD,R4                                                         
         CLC   TLUHUSE,=C'FGN'     IGNORE FOREIGN                               
         BE    XIT                                                              
         CLC   TLUHUSE,=C'FGR'     IGNORE FOREIGN REUSE                         
         BE    XIT                                                              
         CLC   TLUHUSE,=C'FGM'     IGNORE FOREIGN REUSE                         
         BE    XIT                                                              
*IRV 4/24CLC   TLUHUSE,=C'BSS'     SESSION                                      
****     BE    XIT                                                              
         CLC   TLUHUSE,=C'MUS'     OR MUSIC                                     
         BE    XIT                                                              
         CLC   TLUHUSE,=C'AUD'     OR AUD                                       
         BE    XIT                                                              
         LR    R6,R4                                                            
         MVI   ELCODE,TAUHELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAUHD,R6                                                         
         CLC   TLUHUSE,=C'HLD'     IF THIS IS A HOLDING FEE                     
         BE    PROCUH3                                                          
         CLC   TLUHUSE,=C'BSS'     OR SESSION                                   
         BNE   PROCUH5                                                          
PROCUH3  CLC   TAUHEND,THISLHCY    IS THIS THE LATEST HF END CYCLE?             
         BL    *+10                                                             
         MVC   THISLHCY,TAUHEND    YES - SO SAVE IT                             
         B     XIT                                                              
         SPACE                                                                  
PROCUH5  CLC   TAUHEND,THISLUCY    CHECK AGAINST LATEST USE CYCLE               
         BL    *+10                                                             
         MVC   THISLUCY,TAUHEND    AND SAVE THAT                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR END OF COMMERCIAL                                   
         SPACE 3                                                                
COMEND   NTR1                                                                   
         OC    THISFAD,THISFAD                                                  
         BZ    COMFIRST                                                         
         XC    SORTIO,SORTIO                                                    
         MVC   SORTCID,THISCID                                                  
         MVC   SORTTITL,THISTITL                                                
         MVC   SORTFAD,THISFAD                                                  
         MVC   SORTIND,THISIND                                                  
         SPACE                                                                  
         CLC   THISLHCY,TIQPSTR    DOES THE LATEST HOLD END CYCLE FALL          
         BL    COMEND5             WITHIN THE REQUESTED PERIOD?                 
         CLC   THISLHCY,TIQPEND                                                 
         BH    COMEND5                                                          
         MVI   SORTTYPE,1                                                       
         MVC   SORTLAST,THISLHCY   FIND LATEST OF HOLD OR USE CYCLE             
         CLC   THISLHCY,THISLUCY                                                
         BNL   *+10                                                             
         MVC   SORTLAST,THISLUCY                                                
         BAS   RE,CHKSRT                                                        
         SPACE                                                                  
         MVI   SORTTYPE,2                                                       
         MVC   SORTLAST,THISLUCY                                                
         BAS   RE,CHKSRT                                                        
         SPACE                                                                  
COMEND5  B     COMFIRST                                                         
         SPACE 1                                                                
CHKSRT   NTR1                                                                   
         XC    SORTDAYS,SORTDAYS   INIT DAYS TO 0                               
         OC    SORTLAST,SORTLAST   TEST HAVE DATE                               
         BZ    CHKSRT5                                                          
         GOTO1 DATCON,DMCB,(1,SORTLAST),(0,ELAST)                               
         GOTO1 DATCON,DMCB,(1,SORTFAD),(0,EFAD)                                 
         CLC   ELAST,EFAD                                                       
         BNH   XIT                                                              
         GOTO1 =V(PERVERT),DMCB,EFAD,ELAST                                      
         MVC   SORTDAYS,DMCB+8     (RETURNS HALF-WORD N'DAYS)                   
CHKSRT5  XC    SORTDAYS,=X'FFFF'   NEED REPORT IN DESCENDING SEQ.               
         BAS   RE,SORTPUT                                                       
         B     XIT                                                              
         SPACE 1                                                                
COMFIRST L     R6,TIAREC                                                        
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         USING TACOD,R6                                                         
         BNE   COMNO                                                            
         MVC   THISCID,TACOCID     PICK UP                                      
         MVC   THISFAD,TACOAIR     AND FIRST AIR DATE                           
         MVI   THISIND,C' '                                                     
         OC    THISFAD,THISFAD                                                  
         BNZ   COMF2                                                            
         CLI   BLANKOPT,C'N'       OPTION TO DROP THESE                         
         BE    COMNO                                                            
         MVC   THISFAD,TACOFCYC    OR FIRST FIXED CYCLE IF NO AIR               
         MVI   THISIND,C'*'                                                     
         SPACE 1                                                                
COMF2    OC    THISFAD,THISFAD     WE NEED ONE OF THE DATES                     
         BZ    COMNO                                                            
         CLC   THISFAD,TIQPEND     AND FORGET IF TOO RECENT                     
         BH    COMNO                                                            
         CLI   TACOMED,TACOMEDT    TV ONLY                                      
         BE    COMF5                                                            
         CLI   TACOMED,TACOMEDI    OR INTERNET                                  
         BE    COMF5                                                            
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA                                 
         BNE   COMNO                                                            
COMF5    CLI   TACOTYPE,C'M'       NO TYPE 'M'                                  
         BE    COMNO                                                            
         CLI   TACOTYPE,C'D'       OR TYPE 'D'                                  
         BE    COMNO                                                            
         L     R6,TIAREC                                                        
         MVC   THISTITL,MYSPACES                                                
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   COMYES                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     COMYES                                                           
         MVC   THISTITL(0),TANANAME                                             
         SPACE 1                                                                
COMNO    MVI   TIMODE,PROCNOUH     COMMERCIAL IS NO GOOD,                       
*                                  SO TELL SYSIO TO FORGET ABOUT USAGE          
         SPACE 1                                                                
COMYES   XC    THISLUCY,THISLUCY                                                
         XC    THISLHCY,THISLHCY                                                
         B     XIT                                                              
         EJECT                                                                  
*              DO THE REPORTS                                                   
         SPACE 3                                                                
DOREPS   NTR1                                                                   
         MVI   LASTTYPE,0                                                       
         LA    R4,1                (SEQUENCE NUMBER)                            
         XC    NUMDAYS,NUMDAYS                                                  
         SPACE 1                                                                
DOREPS2  GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BNZ   DOREPS4                                                          
         BAS   RE,REPTOTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
DOREPS4  MVC   SORTIO,0(R2)                                                     
         CLI   SORTTYPE,2          FIRST FOR REPORT 2?                          
         BNE   DOREPS6                                                          
         CLI   LASTTYPE,2                                                       
         BE    DOREPS6                                                          
         BAS   RE,REPTOTS          THEN FINISH REPORT 1                         
         LA    R4,1                (SEQUENCE NUMBER)                            
         XC    NUMDAYS,NUMDAYS                                                  
         SPACE 1                                                                
DOREPS6  MVC   LASTTYPE,SORTTYPE                                                
         LA    R3,MYP                                                           
         USING PRINTD,R3                                                        
         MVC   PCID(12),SORTCID    FORMAT A REPORT LINE                         
         MVC   PTITLE,SORTTITL                                                  
         GOTO1 DATCON,DMCB,(1,SORTFAD),(8,PFAD+3)                               
         MVC   PFAD+11(1),SORTIND                                               
         GOTO1 DATCON,DMCB,(1,SORTLAST),(8,PLAST+3)                             
         XC    SORTDAYS,=X'FFFF'   UNCOMPLEMENT DAYS                            
         EDIT  (2,SORTDAYS),(6,PLIFE)                                           
         EDIT  (R4),(6,PSEQ)                                                    
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         LA    R4,1(R4)            +1 SEQUENCE                                  
         MVC   DUB,SORTDAYS                                                     
         LH    R1,DUB                                                           
         A     R1,NUMDAYS                                                       
         ST    R1,NUMDAYS                                                       
         B     DOREPS2                                                          
         SPACE 1                                                                
REPTOTS  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         CLI   LINE,53                                                          
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,SPLAT                                                         
         MVC   PCID(12),=C'** TOTALS **'                                        
         BCTR  R4,0                                                             
         EDIT  (R4),(7,PTITLE)                                                  
         MVC   PTITLE+8(11),=C'COMMERCIALS'                                     
         BAS   RE,SPLAT                                                         
         EDIT  (4,NUMDAYS),(7,PTITLE)                                           
         MVC   PTITLE+8(14),=C'NUMBER OF DAYS'                                  
         BAS   RE,SPLAT                                                         
         L     R1,NUMDAYS                                                       
         M     R0,=F'200'                                                       
         DR    R0,R4                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,PTITLE),2                                                
         MVC   PTITLE+8(22),=C'AVERAGE NUMBER OF DAYS'                          
         BAS   RE,SPLAT                                                         
         MVC   PTITLE,=CL30'(* FIRST FIXED CYCLE DATE)'                         
         BAS   RE,SPLAT                                                         
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEINP NTR1                                                                   
         L     R6,TIAREC                                                        
         MVC   RECTYPE,=CL16'USAGE HISTORY'                                     
         CLI   0(R6),TLCOCDQ                                                    
         BNE   *+10                                                             
         MVC   RECTYPE,=CL16'COMMERCIAL'                                        
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACEREC NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
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
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 3                                                                
SORTPUT  NTR1                                                                   
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
SORTPUT2 GOTO1 MYSORTER,DMCB,=C'PUT',SORTIO                                     
         B     XIT                                                              
         SPACE 1                                                                
         DS    0F                                                               
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,03,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(60)'                                  
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
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
         MVC   H1+40(50),MYTITLE                                                
         GOTO1 CENTER,DMCB,H1+40,50                                             
         GOTO1 UNDERLIN,DMCB,(50,H1+40),(X'BF',H2+40)                           
         MVC   H6,MYH6                                                          
         MVC   H7,MYH7                                                          
         MVC   H3+50(29),=CL29'LAST USE OR HOLDING FEE CYCLE'                   
         CLI   LASTTYPE,2                                                       
         BNE   HK20                                                             
         MVC   H6+MYH6CYC-MYH6(L'MYH6CYC),MYH6CYC2                              
         MVC   H7+MYH7CYC-MYH7(L'MYH7CYC),MYH7CYC2                              
         MVC   H3+50(29),=CL29'        LAST USE CYCLE'                          
         SPACE 1                                                                
HK20     L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,MYCOLS                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         DROP  R2                                                               
         DROP  R5                                                               
HKX      XIT1                                                                   
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         SPACE 1                                                                
APERH    DS    A                                                                
MYH6     DS    0F                                                               
         DC    CL18' '                                                          
         DC    CL01' '                                                          
         DC    CL13'COMMERCIAL ID'                                              
         DC    CL01' '                                                          
         DC    CL30'COMMERCIAL TITLE'                                           
         DC    CL01' '                                                          
         DC    CL15'FIRST AIR DATE'                                             
         DC    CL01' '                                                          
MYH6CYC  DC    CL15'   LAST USE/'                                               
         DC    CL01' '                                                          
         DC    CL08'LIFE IN'                                                    
         DC    CL01' '                                                          
         DC    CL08'SEQUENCE'                                                   
         DC    CL01' '                                                          
         DC    CL18' '                                                          
MYH7     DS    0F                                                               
         DC    CL18' '                                                          
         DC    CL01' '                                                          
         DC    CL13' '                                                          
         DC    CL01' '                                                          
         DC    CL30' '                                                          
         DC    CL01' '                                                          
         DC    CL15' '                                                          
         DC    CL01' '                                                          
MYH7CYC  DC    CL15' HOLDING CYCLE'                                             
         DC    CL01' '                                                          
         DC    CL08'  DAYS'                                                     
         DC    CL01' '                                                          
         DC    CL08' NUMBER'                                                    
         DC    CL01' '                                                          
         DC    CL18' '                                                          
MYH6CYC2 DC    CL15'   LAST USE'                                                
MYH7CYC2 DC    CL15'  CYCLE DATE'                                               
MYCOLS   DS    0F                                                               
         DC    CL18' '                                                          
         DC    CL01'L'                                                          
         DC    CL13' '                                                          
         DC    CL01'C'                                                          
         DC    CL30' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'C'                                                          
         DC    CL15' '                                                          
         DC    CL01'C'                                                          
         DC    CL08' '                                                          
         DC    CL01'C'                                                          
         DC    CL08' '                                                          
         DC    CL01'R'                                                          
         DC    CL18' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
TRACOUNT DC    PL6'0'                                                           
USGCOUNT DC    PL6'0'                                                           
RECCOUNT DC    PL6'0'                                                           
         DS    0D                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
SORTIO   DS    0CL60                                                            
SORTTYPE DS    XL1                                                              
SORTDAYS DS    XL2                                                              
SORTCID  DS    CL12                                                             
SORTTITL DS    CL30                                                             
SORTFAD  DS    PL3                                                              
SORTIND  DS    XL1                                                              
SORTLAST DS    PL3                                                              
         DS    CL60                                                             
         SPACE 1                                                                
THISTITL DS    CL36                                                             
THISCID  DS    CL12                                                             
THISLUCY DS    PL3                                                              
THISLHCY DS    PL3                                                              
THISFAD  DS    PL3                                                              
THISIND  DS    PL3                                                              
THISDAYS DS    H                                                                
LASTTYPE DS    XL1                                                              
BLANKOPT DS    CL1                                                              
NUMCOMS  DS    F                                                                
NUMDAYS  DS    F                                                                
MYREGS   DS    16F                                                              
MYTITLE  DS    CL50                                                             
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYSPACES DS    CL132                                                            
ODDAREA  DS    CL12                                                             
ELAST    DS    CL6                                                              
EFAD     DS    CL6                                                              
         DS    0D                                                               
MYSORTER DS    A                                                                
         SPACE 1                                                                
*                                  OPTIONS                                      
TRACOPT  DS    CL1                 Y=TRACE                                      
         DS    CL6                 SPARE                                        
         SPACE 1                                                                
         SPACE 1                                                                
RECTYPE  DS    CL16                                                             
PTODAY   DS    PL3                                                              
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 3                                                                
PRINTD   DSECT                                                                  
         DC    CL18' '                                                          
         DC    CL01' '                                                          
PCID     DC    CL13' '                                                          
         DC    CL01' '                                                          
PTITLE   DC    CL30' '                                                          
         DC    CL01' '                                                          
PFAD     DC    CL15' '                                                          
         DC    CL01' '                                                          
PLAST    DC    CL15' '                                                          
         DC    CL01' '                                                          
PLIFE    DC    CL08' '                                                          
         DC    CL01' '                                                          
PSEQ     DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL18' '                                                          
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPD4D                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014TAREP24   04/08/14'                                      
         END                                                                    
