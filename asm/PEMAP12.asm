*          DATA SET PEMAP12    AT LEVEL 016 AS OF 05/01/02                      
*PHASE TE1B12A                                                                  
         TITLE 'TE1B12 - MANPOWER REPORT'                                       
TE1B12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MNRP**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LIST                                                             
         EJECT                                                                  
*              VALIDATE KEY FOR REPORTS                                         
         SPACE 3                                                                
VKEY     XC    QPERS,QPERS         PRESET FIELDS                                
         XC    QDATE,QDATE                                                      
         SPACE 1                                                                
         LA    R2,PERPERSH         PERSON                                       
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         GOTO1 VALIPERS                                                         
         MVC   QPERS,WORK                                                       
         SPACE 1                                                                
VKEY2    LA    R2,PERDATEH         DATE                                         
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 VALIDATE,DMCB,QDATE                                              
         B     XIT                                                              
         EJECT                                                                  
*              REPORT RECORDS                                                   
         SPACE 3                                                                
LIST     LA    R4,KEY                                                           
         L     R2,=A(SORTC)                                                     
         A     R2,RELO                                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(41,(R2))                           
         BAS   RE,MONSET                                                        
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   MAPKTYP,X'06'                                                    
         GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   LISTEND                                                          
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO              GET TASK DETAILS                             
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TSKELD,R6                                                        
         SPACE 1                                                                
         CLI   QPERS,0             MAY BE FILTERING ON PERSON                   
         BE    LIST6                                                            
         CLC   QPERS,TSKWHO                                                     
         BNE   LIST2                                                            
         SPACE 1                                                                
LIST6    DS    0H                                                               
         XC    SORTREC,SORTREC                                                  
         BAS   RE,MONANAL          TASK QUALIFIES - FILL RECORD                 
         OC    SORTTOT,SORTTOT                                                  
         BZ    LIST2                                                            
         MVC   SORTPERS,TSKWHO     PERSON                                       
         MVC   SORTSYST,TSKSYS     SYSTEM                                       
         MVC   SORTPROJ,TSKPROJ    PROJECT                                      
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     LIST2                                                            
         SPACE 1                                                                
LISTEND  BAS   RE,REP2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR MONTHLY ANALYSIS - SET LIST                         
         SPACE 3                                                                
MONSET   NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         OC    QDATE,QDATE         REQUESTABLE START OR TODAY                   
         BZ    *+10                                                             
         MVC   WORK(6),QDATE                                                    
         MVC   WORK+4(2),=C'01'    GET FIRST MONDAY INTO WORK                   
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,1                                                           
         BE    MON2                                                             
         ZIC   R2,DMCB                                                          
         LA    R3,8                                                             
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,WORK,DUB,(R3)                                         
         MVC   WORK(6),DUB                                                      
         SPACE 1                                                                
MON2     LA    R2,MONLIST                                                       
         LA    R3,6                                                             
         SR    R4,R4               TOTAL WEEKS IN PERIOD                        
         SPACE 1                                                                
MON4     MVC   0(6,R2),WORK        FIRST MONDAY IN MONTH                        
         LA    R5,4                                                             
         GOTO1 ADDAY,DMCB,WORK,DUB,28                                           
         CLC   WORK(4),DUB                                                      
         BNE   MON6                                                             
         MVC   WORK(6),DUB                                                      
         GOTO1 ADDAY,DMCB,WORK,DUB,7                                            
         LA    R5,5                                                             
         SPACE 1                                                                
MON6     MVC   WORK(6),DUB                                                      
         STC   R5,6(R2)            SAVE N'WEEKS IN MONTH                        
         AR    R4,R5                                                            
         STC   R4,TOTWEEKS                                                      
         LA    R2,7(R2)                                                         
         BCT   R3,MON4                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR MONTHLY ANALYSIS - TASK 1                           
         SPACE 3                                                                
MONANAL  NTR1                                                                   
         MVC   WORK(6),MONLIST     DEFAULT FOR OPEN ITEMS IS START              
         OC    TSKSTART,TSKSTART                                                
         BZ    MON12                                                            
         GOTO1 DATCON,DMCB,(1,TSKSTART),(0,WORK)                                
         SPACE 1                                                                
MON12    ZIC   R0,TSKWEEKS                                                      
         LTR   R0,R0               NUMBER OF WEEKS                              
         BNZ   *+8                                                              
         LA    R0,27                                                            
         SPACE 1                                                                
MON13    LA    R2,MONLIST          FIND A MONTH FOR THIS WEEK                   
         LA    R3,6                                                             
         LA    R4,SORTMONS                                                      
         SPACE 1                                                                
MON14    CLC   WORK(4),0(R2)                                                    
         BE    MON16                                                            
         LA    R2,7(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,MON14                                                         
         B     MON18                                                            
         SPACE 1                                                                
MON16    ZIC   R1,TSKPCT           GOT A MATCH                                  
         A     R1,0(R4)            ADD INTO MONTH SLOT                          
         ST    R1,0(R4)                                                         
         ZIC   R1,TSKPCT                                                        
         A     R1,SORTTOT          AND INTO TOTALS                              
         ST    R1,SORTTOT                                                       
         SPACE 1                                                                
MON18    GOTO1 ADDAY,DMCB,WORK,DUB,7                                            
         MVC   WORK(6),DUB                                                      
         BCT   R0,MON13                                                         
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL SORT READING                                             
         SPACE 3                                                                
REP2     NTR1                                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    PROJTOTS,PROJTOTS                                                
         XC    PERSTOTS,PERSTOTS                                                
         XC    LASTREC,LASTREC                                                  
         SPACE 1                                                                
REP4     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BNZ   REP6                                                             
         BAS   RE,JTOT                                                          
         BAS   RE,PTOT                                                          
         B     XIT                                                              
         SPACE 1                                                                
REP6     MVC   SORTREC,0(R6)                                                    
         OC    LASTREC,LASTREC                                                  
         BZ    REP12                                                            
         CLC   SORTREC(8),LASTREC            PERSON C/B                         
         BE    REP8                                                             
         BAS   RE,JTOT                                                          
         BAS   RE,PTOT                                                          
         B     REP12                                                            
         SPACE 1                                                                
REP8     CLC   SORTREC(24),LASTREC           PROJECT C/B                        
         BE    REP12                                                            
         BAS   RE,JTOT                                                          
         SPACE 1                                                                
REP12    MVC   LASTREC,SORTREC               SAVE THIS RECORD                   
         LA    R2,SORTMONS                                                      
         LA    R3,PROJTOTS                                                      
         LA    R4,PERSTOTS                                                      
         LA    R0,7                                                             
         SPACE 1                                                                
REP14    L     R1,0(R2)            ADD IN PROJECT                               
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         L     R1,0(R2)            AND PERSON ACCUMS                            
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,REP14                                                         
         B     REP4                                                             
         EJECT                                                                  
*              PRINTING LINE                                                    
         SPACE 3                                                                
JTOT     NTR1                      PROJECT TOTAL                                
         LA    R4,KEY                                                           
         CLC   LASTREC(8),PERSSAVE                                              
         BE    JTOT2                                                            
         MVC   P+2(8),LASTREC                                                   
         XC    KEY,KEY                                                          
         MVC   PERSSAVE,LASTREC    NEED PERSON NAME                             
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   PERKTYP,X'02'                                                    
         MVC   PERCODE,PERSSAVE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   JTOT2                                                            
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'20'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING CHATELD,R6                                                       
         ZIC   R1,CHATLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     JTOT2                                                            
         MVC   P+2(0),CHAT                                                      
         SPACE 1                                                                
JTOT2    MVC   P+24(8),LASTREC+8   SYSTEM                                       
         MVC   P+34(8),LASTREC+16  PROJECT                                      
         XC    KEY,KEY             GET PROJECT                                  
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   PRJKTYP,X'05'                                                    
         MVC   PRJSYS(16),LASTREC+8                                             
         MVC   P+44(4),=C'OPEN'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   JTOT4                                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'52'        AND DIG OUT SCHEDULE DATES                   
         BAS   RE,GETEL                                                         
         BNE   JTOT4                                                            
         USING PROJELD,R6                                                       
         OC    PROJSCST,PROJSCST                                                
         BZ    JTOT4                                                            
         LA    R5,P+44                                                          
         GOTO1 DATCON,DMCB,(1,PROJSCST),(9,(R5))                                
*&&US*&& MVC   3(3,R5),4(R5)                                                    
         CLI   PROJSCND,0                                                       
         BE    JTOT4                                                            
         CLC   PROJSCND,PROJSCST                                                
         BE    JTOT4                                                            
         MVC   WORK(2),PROJSCND                                                 
         MVI   WORK+2,1                                                         
         MVI   3(R5),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,WORK),(9,4(R5))                                   
*&&US*&& MVC   7(3,R5),8(R5)                                                    
         SPACE 1                                                                
JTOT4    LA    R2,PROJTOTS                                                      
         BAS   RE,FORMAT                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
PTOT     NTR1                                PERSON TOTALS                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+44(6),=C'TOTALS'                                               
         LA    R2,PERSTOTS                                                      
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         LA    R3,MONLIST                                                       
         LA    R4,P+53                                                          
         LA    R5,7                                                             
         SPACE 1                                                                
FOR2     L     RF,0(R2)                      NEED PERCENT                       
         LTR   RF,RF                                                            
         BZ    FOR4                                                             
         ZIC   R1,6(R3)                                                         
         M     RE,=F'2'                                                         
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,0(R4))                                                   
         MVI   5(R4),C'%'                                                       
         SPACE 1                                                                
FOR4     XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,FOR2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GENDISP  ZIC   R1,0(R2)            GENERAL DISPLAY                              
         SH    R1,=H'9'                                                         
         EX    R1,GDCLC                                                         
         BE    GENDISP2                                                         
         EX    R1,GDMVC                                                         
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
GENDISP2 MVC   WORK,SPACES                                                      
         BR    RE                                                               
         SPACE 1                                                                
GDCLC    CLC   8(0,R2),WORK                                                     
GDMVC    MVC   8(0,R2),WORK                                                     
         SPACE 1                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H6+02(6),=C'PERSON'                                              
         MVC   H6+24(6),=C'SYSTEM'                                              
         MVC   H6+34(7),=C'PROJECT'                                             
         MVC   H6+44(8),=C'SCHEDULE'                                            
         LA    R2,MONLIST                                                       
         LA    R3,H6+55                                                         
         LA    R0,6                                                             
         SPACE 1                                                                
HOOK2    GOTO1 DATCON,DMCB,(0,(R2)),(9,0(R3))                                   
*&&US*&& MVC   3(3,R3),4(R3)                                                    
         LA    R2,7(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,HOOK2                                                         
         MVC   0(5,R3),=C'TOTAL'                                                
         SPACE 1                                                                
         L     R4,ABOX             BOXES, IF AROUND                             
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXROWS+04,C'T'                                                  
         MVI   BOXROWS+06,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   00(R2),C'L'                                                      
         MVI   22(R2),C'C'                                                      
         MVI   32(R2),C'C'                                                      
         MVI   42(R2),C'C'                                                      
         MVI   53(R2),C'C'                                                      
         MVI   61(R2),C'C'                                                      
         MVI   69(R2),C'C'                                                      
         MVI   77(R2),C'C'                                                      
         MVI   85(R2),C'C'                                                      
         MVI   93(R2),C'C'                                                      
         MVI   101(R2),C'C'                                                     
         MVI   109(R2),C'R'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS SSPEC H1,2,C'MAP SYSTEM'                                               
         SSPEC H2,2,C'----------'                                               
         SSPEC H1,40,C'MANPOWER REPORT'                                         
         SSPEC H2,40,C'---------------'                                         
         SSPEC H1,77,REPORT                                                     
         SSPEC H1,96,REQUESTOR                                                  
         SSPEC H2,77,RUN                                                        
         SSPEC H2,103,PAGE                                                      
         DC    X'00'               END MARKER FOR SPECS                         
         SPACE 1                                                                
RELO     DS    A                                                                
RELOC    DC    A(*)                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,24,A),FORMAT=BI,WORK=1 '                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=60'                                    
         LTORG                                                                  
         SPACE 1                                                                
SORTC    CSECT                                                                  
         DS    44000C                                                           
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPD2D                                                       
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
QPERS    DS    CL8                                                              
QDATE    DS    CL6                                                              
         SPACE 1                                                                
         DS    0D                                                               
SORTREC  DS    0CL60               SORT RECORD                                  
SORTPERS DS    CL8                                                              
SORTSYST DS    CL8                                                              
SORTPROJ DS    CL8                                                              
SORTMONS DS    CL24                                                             
SORTTOT  DS    CL4                                                              
         DS    CL8                                                              
         SPACE 1                                                                
LASTREC  DS    CL60                                                             
MONLIST  DS    CL42                                                             
TOTDUMMY DS    CL6                                                              
TOTWEEKS DS    CL1                                                              
PROJTOTS DS    CL28                                                             
PERSTOTS DS    CL28                                                             
PERSSAVE DS    CL8                                                              
PERSNAME DS    CL20                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016PEMAP12   05/01/02'                                      
         END                                                                    
