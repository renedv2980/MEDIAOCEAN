*          DATA SET TAREP2A    AT LEVEL 042 AS OF 04/08/14                      
*PHASE T7032AA                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'T7032A - FIX PROGRAM'                                           
T7032A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T7032A,R7,R8                                       
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
         CLI   5(R2),0                                                          
         BE    VREC2                                                            
         ST    R2,APERH                                                         
         GOTO1 VALPERD                                                          
         MVI   TIQDTYPE,TIQDCHK    (FILTER ON CHECK DATE)                       
         SPACE 1                                                                
         SPACE 1                                                                
VREC2    LA    R2,SPLAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         BE    VREC8                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC8                                                            
         GOTO1 ANY                                                              
         MVC   TIFAGY,WORK                                                      
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2),0                                       
         SPACE 1                                                                
VREC8    LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         MVI   TRACOPT,C'N'                                                     
         MVI   LISTOPT,C'N'                                                     
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
         BNE   OPT3                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT3     CLC   12(4,R4),=C'LIST'   LIST OPTION                                  
         BNE   OPT4                                                             
         MVI   LISTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT8                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   REPLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     DS    0H                                                               
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
RECLIMIT DC    PL6'9999999'                                                     
REPLIMIT DC    PL6'9999999'                                                     
FIXCOUNT DC    PL6'0000000'                                                     
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,0),(1,PTODAY)                                     
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         MVC   MYSORTER,SORTER                                                  
         DROP  R5                                                               
         MVC   MYTITLE,MYSPACES                                                 
         MVC   MYTITLE(18),=C'COPIED COMMERCIALS'                               
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         SPACE 1                                                                
******   MVI   TIREAD,TLCACDQ      SET TO READ CAST RECORDS                     
PREP2    MVI   TIREAD,TLCOCDQ      SET TO READ COMM RECORDS                     
***?**** MVI   TIFINCVS,2          CONVERTED RECORDS ONLY                       
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         EDIT  (P6,FIXCOUNT),(9,MYP)                                            
         MVC   MYP+10(13),=C'RECORDS FIXED'                                     
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
*****    CLI   TIMODE,PROCCOMM     FIRST TIME FOR COMMERCIAL                    
*****    BE    IOHOOK4                                                          
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         B     IOHOOK4                                                          
         SPACE 1                                                                
         CP    FIXCOUNT,RECLIMIT                                                
         BNL   XIT                                                              
         AP    CSTCOUNT,=P'1'                                                   
         CP    CSTCOUNT,TRALIMIT                                                
         BH    IOHOOK2                                                          
         BAS   RE,TRACEINP                                                      
         SPACE 1                                                                
IOHOOK2  BAS   RE,PROCCAST                                                      
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK4  CP    FIXCOUNT,RECLIMIT                                                
         BNL   NOCAST                                                           
         L     R6,TIAREC           LOOK FOR COPIED                              
         MVI   ELCODE,TAOCELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
IOHOOK6  BAS   RE,NEXTEL                                                        
         BNE   NOCAST                                                           
         USING TAOCD,R6                                                         
         OC    TAOCDTE,TAOCDTE                                                  
         BZ    IOHOOK6                                                          
         TM    TAOCSTAT,TAOCSFRO   FROM                                         
         BNO   IOHOOK6                                                          
         MVC   OLDCOMM,TAOCCOM                                                  
         MVC   OLDAGY,TAOCAGY                                                   
         MVC   OLDCID,TAOCCID                                                   
         MVC   COPYDATE,TAOCDTE                                                 
         XC    COPYDATE,=X'FFFFFF'                                              
         CLI   LISTOPT,C'Y'                                                     
         BNE   XIT                                                              
         SPACE 1                                                                
         XC    KEY,KEY             ANY USAGE ON NEW COMM?                       
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ                                                   
******   MVC   TLUHAGY,TIAGY                                                    
         MVC   TLUHCOM,TICOM                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    IOHOOKX             YES                                          
         SPACE 1                                                                
         XC    KEY,KEY             ANY USAGE ON OLD COMM?                       
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ                                                   
*******  MVC   TLUHAGY,OLDAGY                                                   
         MVC   TLUHCOM,OLDCOMM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BNE   IOHOOKX             YES                                          
         SPACE 1                                                                
         CLC   TIAGY,LASTAGY                                                    
         BE    IOHOOK8                                                          
         MVC   LASTAGY,TIAGY                                                    
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R5                                                               
IOHOOK8  MVC   MYP(6),TIAGY        SHOW AGENCY                                  
         MVC   MYP+7(12),TICID     CID                                          
         L     R6,TIAREC           LOOK FOR COPIED                              
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   IOHOOK12                                                         
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP+20(0),TANANAME                                               
         SPACE 1                                                                
IOHOOK12 MVC   MYP+52(11),=C'COPIED FROM'                                       
         MVC   MYP+64(6),OLDAGY                                                 
         MVC   MYP+71(12),OLDCID                                                
         GOTO1 DATCON,DMCB,(1,COPYDATE),(8,MYP+84)                              
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
IOHOOKX  MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         B     XIT                                                              
         SPACE 1                                                                
NOCAST   MVI   TIMODE,PROCNOCA                                                  
         B     XIT                                                              
         SPACE 1                                                                
OLDAGY   DS    CL6                                                              
OLDCID   DS    CL12                                                             
OLDCOMM  DS    CL4                                                              
THISSSN  DS    CL9                                                              
THISCAT  DS    CL3                                                              
COPYDATE DS    CL3                                                              
LASTAGY  DC    CL6' '                                                           
         EJECT                                                                  
*              PROCESS CAST RECORD                                              
         SPACE 3                                                                
PROCCAST NTR1                                                                   
         CLC   TIUN,=C'AFM'        NOT FOR AFM                                  
         BE    XIT                                                              
         CLC   TIUN,=C'ACT'        OR FOR ACTRA                                 
         BE    XIT                                                              
*****    CLI   TIAGY,C'0'          ONLY T&R AGENCIES                            
*****    BL    XIT                                                              
         GOTO1 CATVAL,DMCB,TICAT                                                
         TM    TGCATYPE,X'40'      NO HOLDS                                     
         BO    XIT                                                              
         CLC   TIONOF,=C'OFF'                                                   
         BNE   PROCCA2                                                          
         TM    TGCATYPE,X'20'                                                   
         BNO   PROCCA2                                                          
         TM    TGCATYPE,X'10'                                                   
         BO    XIT                                                              
         SPACE 1                                                                
PROCCA2  L     R4,TIAREC                                                        
         USING TLCAD,R4                                                         
         LR    R6,R4                                                            
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         BE    XIT                 OK IF THERE IS A TACR ELEMENT                
         B     OLDCAST                                                          
         SPACE 1                                                                
****                               THIS CODE IS FOR CHECKS                      
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING TLCKPD,R5                                                        
         MVI   TLCKPCD,TLCKHCDQ    ELSE GO AND READ CHECKS                      
         MVC   TLCKHCOM,OLDCOMM    FOR OLD COMMERCIAL                           
         MVC   TLCKHSSN,TLCASSN             SSN                                 
         MVC   TLCKHCAT,TLCACAT             CATEGORY                            
         MVC   MYP(9),TLCASSN                                                   
         MVC   MYP+10(3),TLCACAT                                                
         MVC   SYSDIR,=C'CHKDIR  '                                              
         MVC   SYSFIL,=C'CHKFIL  '                                              
         GOTO1 HIGH                                                             
         B     PCA4                                                             
         SPACE 1                                                                
PCA2     GOTO1 SEQ                                                              
         SPACE 1                                                                
PCA4     CLC   KEY(23),KEYSAVE     LOOK FOR FIRST HLD BSS SSS OR SHL            
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PCA2                                                             
         USING TAPDD,R6                                                         
         CLC   TAPDUSE,=C'HLD'                                                  
         BE    PCA6                                                             
         CLC   TAPDUSE,=C'BSS'                                                  
         BE    PCA6                                                             
         CLC   TAPDUSE,=C'SSS'                                                  
         BE    PCA6                                                             
         CLC   TAPDUSE,=C'SHL'                                                  
         BNE   PCA2                                                             
         SPACE 1                                                                
PCA6     MVC   SAVEUSE,TAPDUSE                                                  
         MVC   SAVECYC,TAPDCYCS                                                 
         SPACE 1                                                                
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING TACRD,R6                                                         
         MVI   TACREL,TACRELQ                                                   
         MVI   TACRLEN,TACRLNQ                                                  
         MVC   TACRSTRT(6),SAVECYC                                              
         MVC   TACRUSE,SAVEUSE                                                  
         MVC   SYSDIR,=C'TALDIR  '                                              
         MVC   SYSFIL,=C'TALFIL  '                                              
         MVC   KEY,TIKEY                                                        
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         BAS   RE,ADDEL                                                         
         GOTO1 PUTREC                                                           
         AP    FIXCOUNT,=P'1'                                                   
         CP    FIXCOUNT,REPLIMIT                                                
         BH    XIT                                                              
*****    MVC   MYP+20(3),SAVEUSE                                                
*****    GOTO1 DATCON,DMCB,(1,SAVECYC),(8,MYP+30)                               
*****    GOTO1 DATCON,DMCB,(1,SAVECYC+3),(8,MYP+39)                             
*****    BAS   RE,SPLAT                                                         
         LA    R6,KEY                                                           
         LA    R2,38                                                            
         BAS   RE,TRACEL                                                        
         B     XIT                                                              
         EJECT                                                                  
*              GO AND GET TACRS FROM OLD CAST RECORD                            
         SPACE 3                                                                
OLDCAST  XC    KEY,KEY                                                          
         MVC   THISSSN,TLCASSN     SAVE CURRENT SS#                             
         MVC   THISCAT,TLCACAT                  AND CAT                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ                                                   
******** MVC   TLCAAGY,OLDAGY      CHANGE TO OLD COMMERCIAL                     
         MVC   TLCACOM,OLDCOMM                                                  
         XC    TLCASEQ,TLCASEQ                                                  
         GOTO1 HIGH                                                             
         B     OCA4                                                             
         SPACE 1                                                                
OCA2     GOTO1 SEQ                                                              
         SPACE 1                                                                
OCA4     CLC   KEY(12),KEYSAVE     CHECK ITS FOR THIS COMMERCIAL                
         BE    OCA6                                                             
         SPACE 1                                                                
OCA5     MVC   KEY,TIKEY           NO GOOD                                      
         GOTO1 HIGH                RESTORE SEQUENCE                             
         B     XIT                 AND GIVE UP                                  
         SPACE 1                                                                
OCA6     CLC   TLCASSN,THISSSN     CHECK FOR SAME SS#                           
         BNE   OCA2                                                             
         CLC   TLCACAT,THISCAT               AND CAT                            
         BNE   OCA2                                                             
         MVC   AIO,AIO2            READ OLD CAST INTO IO2                       
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,TACRELQ MAKE SURE THIS HAS A TACR                         
         BAS   RE,GETEL                                                         
         BNE   OCA5                                                             
         AP    FIXCOUNT,=P'1'                                                   
         L     R6,AIO                                                           
         MVC   RECTYPE,=CL16'FROM CAST REC'                                     
         CP    FIXCOUNT,REPLIMIT                                                
         BH    *+8                                                              
         BAS   RE,TRACEREC                                                      
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                RESTORE NEW CAST SEQUENCE                    
         MVC   AIO,AIO1            READ NEW CAST INTO IO1                       
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   RECTYPE,=CL16'TO CAST - BEFORE'                                  
         CP    FIXCOUNT,REPLIMIT                                                
         BH    *+8                                                              
         BAS   RE,TRACEREC                                                      
         L     R6,AIO2             LOOK FOR TACR ELEMENTS IN IO2                
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
OCA8     BAS   RE,NEXTEL                                                        
         BNE   OCA10                                                            
         MVC   ELEMENT,0(R6) COPY ELEMENTS                                      
         BAS   RE,ADDEL                                                         
         ZIC   R2,1(R6)                                                         
         CP    FIXCOUNT,REPLIMIT                                                
         BH    OCA8                                                             
         BAS   RE,TRACEL                                                        
         B     OCA8                                                             
         SPACE 1                                                                
OCA10    GOTO1 PUTREC                                                           
         L     R6,AIO                                                           
         MVC   RECTYPE,=CL16'TO CAST - AFTER'                                   
         CP    FIXCOUNT,REPLIMIT                                                
         BH    *+8                                                              
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEINP NTR1                                                                   
         L     R6,TIAMAIN                                                       
         MVC   RECTYPE,=CL16'INVOICE'                                           
         BAS   RE,TRACEREC                                                      
         L     R6,TIAREC                                                        
         MVC   RECTYPE,=CL16'CHECK'                                             
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
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         L     R2,AIO                                                           
         GOTO1 =V(HELLO),DMCB,(C'P',=C'TALFIL'),(R2),ELEMENT                    
         B     XIT                                                              
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
SORTMOVE NTR1                                                                   
         LA    R4,SORTIO                                                        
         MOVE  (SORTIO,700),0(R2)                                               
         B     XIT                                                              
         DS    0F                                                               
LASTSKEY DC    XL20'00'                                                         
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(700)'                                 
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
         MVC   H1+52(24),MYTITLE                                                
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
****     MVC   H6,MYH6                                                          
****     MVC   H7,MYH7                                                          
         DROP  R5                                                               
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
APERH    DS    A                                                                
         SPACE 1                                                                
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
TRACOUNT DC    PL6'0'                                                           
CSTCOUNT DC    PL6'0'                                                           
RECCOUNT DC    PL6'0'                                                           
         DS    0D                                                               
THSCOUNT DC    PL8'0'                                                           
TAPCOUNT DC    PL6'0'                                                           
REPCOUNT DC    PL6'0'                                                           
TPCH     DC    H'1878'                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYREGS   DS    16F                                                              
MYTITLE  DS    CL32                                                             
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYSPACES DS    CL132                                                            
ODDAREA  DS    CL12                                                             
         DS    0D                                                               
SAVECYC  DS    CL6                                                              
SAVEUSE  DS    CL3                                                              
MYSORTER DS    A                                                                
         SPACE 1                                                                
LASTCHEK DS    CL32                                                             
*                                  OPTIONS                                      
TRACOPT  DS    CL1                 Y=TRACE                                      
LISTOPT  DS    CL1                                                              
         DS    CL6                 SPARE                                        
         SPACE 1                                                                
         SPACE 1                                                                
RECTYPE  DS    CL16                                                             
PTODAY   DS    PL3                                                              
SAVEEL   DS    CL1                                                              
         SPACE 1                                                                
         DS    0D                                                               
SORTIO   DS    700C                                                             
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
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
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPEAD                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042TAREP2A   04/08/14'                                      
         END                                                                    
