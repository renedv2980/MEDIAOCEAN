*          DATA SET PEMAP15    AT LEVEL 021 AS OF 05/01/02                      
*PHASE TE1B15A                                                                  
         TITLE 'TE1B15 - PROJECT MANAGEMENT REPORT'                             
TE1B15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PMAN**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R1,=A(ASORT)                                                     
         ST    R1,ASORTAR          THIS IS SORTERS AREA                         
         B     REPORT                                                           
         EJECT                                                                  
*              VALIDATE REPORT REQUEST                                          
         SPACE 3                                                                
VKEY     XC    QSYS,QSYS           PRESET FIELDS                                
         SPACE 1                                                                
         LA    R2,PJLSYSH          SYSTEM                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 VALISYS                                                          
         MVC   QSYS,SAVSYSCD                                                    
         B     XIT                                                              
         EJECT                                                                  
*              GENERATE SORT RECORDS FOR REPORT                                 
         SPACE 3                                                                
REPORT   LA    R4,KEY                                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(43,ASORTAR)                        
         GOTO1 DATCON,DMCB,(5,0),ETODAY           SET UP TODAYS DATE            
         GOTO1 DATCON,DMCB,(0,ETODAY),(1,PTODAY)                                
         USING MAPKEYD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,MAPSYSQ                                                   
         MVI   MAPKTYP,PRJKTYPQ                                                 
         MVC   PRJSYS,QSYS                                                      
         GOTO1 HIGH                                                             
         B     SORT4                                                            
*                                                                               
SORT2    GOTO1 SEQ                                                              
*                                                                               
SORT4    LA    R4,KEY                                                           
         CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   SORTEND                                                          
         CLI   QSYS,0              IF SYSTEM WAS REQUESTED                      
         BE    SORT6                                                            
         CLC   PRJSYS,KEYSAVE+(PRJSYS-MAPKEY) WE SHOULD MATCH                   
         BNE   SORTEND                                                          
*                                                                               
SORT6    GOTO1 GETREC                                                           
         XC    SORTREC,SORTREC                                                  
         MVC   SORTSYS,PRJSYS                                                   
         MVC   SORTPROJ,PRJCODE                                                 
         MVC   SORTADDR,DMDSKADD                                                
         MVI   ELCODE,X'52'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PROJELD,R6                                                       
         CLI   PROJOKPC,100        IGNORE COMPLETED                             
         BE    SORT2                                                            
         OC    PROJSCST,PROJSCST   AND UNSCHEDULED PROJECTS                     
         BZ    SORT2                                                            
         MVC   SORTCOOR,PROJCOOR   SORT NEEDS COORDINATOR                       
         MVC   SORTDATE(2),PROJSCND           END DATE                          
         OC    PROJSCND,PROJSCND                                                
         BNZ   *+10                                                             
         MVC   SORTDATE(2),PROJSCST                                             
         MVC   SORTDATE+2(2),PROJSCST         AND START DATE                    
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     SORT2                                                            
         EJECT                                                                  
*              READ BACK THE SORT RECORDS                                       
         SPACE 3                                                                
SORTEND  XC    LASTSORT,LASTSORT                                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         SPACE 1                                                                
GET      GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,DMCB+4                                                        
         LTR   R5,R5               ANY RECORDS?                                 
         BZ    XIT                                                              
         MVC   SORTREC,0(R5)                                                    
         CLC   SORTCOOR,LASTSORT                                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       SKIP ON MANAGER C/B                          
         MVC   LASTSORT,SORTREC                                                 
         MVC   KEY+38(4),SORTADDR                                               
         GOTO1 GETREC                                                           
         L     R1,AIO              RESTORE KEY                                  
         MVC   KEY(36),0(R1)                                                    
         BAS   RE,FORMPROJ                                                      
         MVI   KEY+2,X'06'         SET UP TO READ TASKS FOR PROJECT             
         GOTO1 HIGH                                                             
         CLC   KEY(19),KEYSAVE                                                  
         BE    GET4                                                             
         MVC   P+93(26),=C'*** NO TASKS ALLOCATED ***'                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     GET                                                              
         SPACE 1                                                                
GET2     GOTO1 SEQ                                                              
         CLC   KEY(19),KEYSAVE                                                  
         BNE   GET                                                              
         SPACE 1                                                                
GET4     BAS   RE,FORMTASK                                                      
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     GET2                                                             
         EJECT                                                                  
*              FORMAT A PROJECT                                                 
         SPACE 3                                                                
FORMPROJ NTR1                                                                   
         MVC   DUB(2),SORTDATE+2   SCHEDULE DATES                               
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,P+2)                                      
*&&US*&& MVC   P+5(3),P+6                                                       
         CLC   SORTDATE(2),SORTDATE+2                                           
         BE    FP2                                                              
         MVI   P+5,C'-'                                                         
         MVC   DUB(2),SORTDATE                                                  
         GOTO1 DATCON,DMCB,(1,DUB),(9,P+6)                                      
*&&US*&& MVC   P+9(3),P+10                                                      
         SPACE 1                                                                
FP2      CLC   SORTDATE(2),PTODAY  CHECK IF PROJECT IS LATE                     
         BNL   FP4                                                              
         MVC   P2+2(6),=C'(LATE)'                                               
         SPACE 1                                                                
FP4      MVC   P+14(8),SORTSYS                                                  
         MVC   P2+14(8),SORTPROJ                                                
         MVI   ELCODE,X'50'        SHOW DESCRIPTION                             
         LA    R2,P+24                                                          
         BAS   RE,FORMCHAT                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'52'                                                     
         BAS   RE,GETEL                                                         
         USING PROJELD,R6                                                       
         CLI   PROJOKPC,0                                                       
         BE    XIT                                                              
         EDIT  (1,PROJOKPC),(3,P+56)                                            
*                                  SHOW PERCENT COMPLETE                        
         MVI   P+59,C'%'           ON DATE...                                   
         GOTO1 DATCON,DMCB,(1,PROJOKDT),(7,P2+56)                               
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT A TASK                                                    
         SPACE 3                                                                
FORMTASK NTR1                                                                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         L     R6,AIO                                                           
         MVC   P+65(8),TSKCODE     CODE AND TYPE                                
         MVC   P2+65(7),TSKTYPE+1                                               
         MVI   ELCODE,X'62'                                                     
         LA    R2,P+93                                                          
         BAS   RE,FORMCHAT         DESCRIPTION                                  
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         USING TSKELD,R6                                                        
         MVC   P+76(8),TSKWHO      WHO IS ASSIGNED                              
         OC    TSKSTART,TSKSTART                                                
         BZ    FT2                                                              
         GOTO1 DATCON,DMCB,(1,TSKSTART),(8,P2+76) AND WHEN                      
         EDIT  (1,TSKWEEKS),(3,P+86)                                            
         CLI   TSKOKPCT,100                                                     
         BE    FT2                                                              
         ZIC   R2,TSKWEEKS         FIGURE OUT END DATE                          
         MH    R2,=H'7'                                                         
         BCTR  R2,0                                                             
         GOTO1 DATCON,DMCB,(1,TSKSTART),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         CLC   WORK+6(6),ETODAY                                                 
         BNL   *+10                                                             
         MVC   P3+76(6),=C'(LATE)'                                              
         SPACE 1                                                                
FT2      CLI   TSKPCT,0                                                         
         BE    FT4                                                              
         EDIT  (1,TSKPCT),(3,P2+86)                                             
         MVI   P2+89,C'%'                                                       
         SPACE 1                                                                
FT4      CLI   TSKOKPCT,0                                                       
         BE    XIT                                                              
         EDIT  (1,TSKOKPCT),(3,P+125)                                           
*                                  SHOW PERCENT COMPLETE                        
         MVI   P+128,C'%'          ON DATE...                                   
         GOTO1 DATCON,DMCB,(1,TSKOKDAT),(7,P2+125)                              
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
FORMCHAT NTR1                                                                   
         L     R6,AIO                                                           
         USING CHATELD,R6                                                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
FC2      BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R1,CHATLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CHAT                                                     
         LA    R2,132(R2)                                                       
         B     FC2                                                              
         SPACE 1                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+2(9),=C'MANAGER -'                                            
         MVC   H4+12(8),SORTCOOR                                                
         L     R5,ABOX             TURN ON BOXES                                
         USING BOXD,R5                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXCOLS+000,C'L'                                                 
         MVI   BOXCOLS+012,C'C'                                                 
         MVI   BOXCOLS+022,C'C'                                                 
         MVI   BOXCOLS+054,C'C'                                                 
         MVI   BOXCOLS+062,C'C'                                                 
         MVI   BOXCOLS+063,C'C'                                                 
         MVI   BOXCOLS+074,C'C'                                                 
         MVI   BOXCOLS+085,C'C'                                                 
         MVI   BOXCOLS+091,C'C'                                                 
         MVI   BOXCOLS+123,C'C'                                                 
         MVI   BOXCOLS+131,C'R'                                                 
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              LTORG ETC.                                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,30,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(34,,34)'                              
RELO     DS    A                                                                
QSYS     DS    CL8                                                              
ETODAY   DS    CL6                                                              
PTODAY   DS    CL3                                                              
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
HEDSPECS SSPEC H1,3,C'MAP SYSTEM'                                               
         SSPEC H2,3,C'----------'                                               
         SSPEC H1,51,C'PROJECT MANAGEMENT REPORT'                               
         SSPEC H2,51,C'-------------------------'                               
         SSPEC H4,48,C'(SCHEDULED, INCOMPLETE PROJECTS)'                        
         SSPEC H5,48,C'(PROJECTS SEQUENCED BY END DATE)'                        
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,118,REQUESTOR                                                 
         SSPEC H2,99,RUN                                                        
         SSPEC H2,125,PAGE                                                      
         SSPEC H8,3,C'SCHEDULED   SYSTEM'                                       
         SSPEC H9,15,C'PROJECT'                                                 
         SSPEC H8,29,C'DESCRIPTION OF PROJECT'                                  
         SSPEC H8,57,C'%DONE'                                                   
         SSPEC H9,57,C'(ON) '                                                   
         SSPEC H8,68,C'TASK'                                                    
         SSPEC H9,68,C'TYPE'                                                    
         SSPEC H8,77,C'BY WHOM?   WKS'                                          
         SSPEC H9,77,C'WHEN?       %'                                           
         SSPEC H8,99,C'DESCRIPTION OF TASK'                                     
         SSPEC H8,126,C'%DONE'                                                  
         SSPEC H9,126,C'(ON) '                                                  
         DC    X'00'               END MARKER FOR SPECS                         
         EJECT                                                                  
       ++INCLUDE PEMAPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPC5D                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* PEMAPFILE                                                                     
* PEMAPWORKD ********************* MUST BE THE LAST ++INCLUDE                   
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
ASORTAR  DS    A                                                                
*                                                                               
LASTSORT DS    CL34                                                             
SORTREC  DS    0CL34                                                            
SORTKEY  DS    0CL30                                                            
SORTCOOR DS    CL8                 COORDINATOR                                  
SORTDATE DS    CL6                 END/START                                    
SORTSYS  DS    CL8                                                              
SORTPROJ DS    CL8                                                              
SORTADDR DS    CL4                                                              
         SPACE 3                                                                
TE1B15   CSECT                                                                  
         ENTRY ASORT                                                            
         DC    C'**SORT**'                                                      
ASORT    DS    50000C              AREA FOR SORT                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021PEMAP15   05/01/02'                                      
         END                                                                    
