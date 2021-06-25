*          DATA SET PEMAP04    AT LEVEL 012 AS OF 05/01/02                      
*PHASE TE1B04A                                                                  
         TITLE 'TE1B04 - SYSTEM RECORD MAINTENANCE'                             
TE1B04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SYMN**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LIST                                                             
         EJECT                                                                  
*              VALIDATE AND DISPLAY KEY                                         
         SPACE 3                                                                
VKEY     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         CLI   ACTEQU,1                                                         
         BNE   XIT                                                              
         USING MAPKEY,R4                                                        
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   MAPKTYP,X'04'                                                    
         LA    R2,SYSCODH                                                       
         GOTO1 ANY                                                              
         MVC   SYSCODE,WORK                                                     
         B     XIT                                                              
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,SYSCODH                                                       
         MVC   8(8,R2),SYSCODE                                                  
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
         SPACE 3                                                                
VREC     DS    0H                                                               
DREC     DS    0H                                                               
         LA    R2,SYSMANH                                                       
         LA    R3,X'40'                                                         
         MVI   OPTION,0                                                         
         LA    R0,4                                                             
         SPACE 1                                                                
VREC2    MVI   MAX,1                                                            
         STC   R3,ELCODE                                                        
         GOTO1 VALICHAT            MANAGER                                      
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         LA    R3,1(R3)                                                         
         MVI   MAX,2                                                            
         STC   R3,ELCODE                                                        
         GOTO1 VALICHAT            SUPPORT                                      
         BCT   R0,*+8                                                           
         B     XIT                                                              
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         LA    R3,1(R3)                                                         
         B     VREC2                                                            
         B     XIT                                                              
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LIST4                                                            
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   MAPKTYP,X'04'                                                    
         CLI   SYSCODH+5,0                                                      
         BE    LIST1                                                            
         LA    R2,SYSCODH                                                       
         GOTO1 ANY                                                              
         MVC   SYSCODE,WORK                                                     
         SPACE 1                                                                
LIST1    GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         CLI   MODE,PRINTREP                                                    
         BE    REP2                                                             
         MVC   LISTAR,SPACES       SHOW CODE                                    
         LA    R3,LISTAR                                                        
         LA    R5,SYLHED                                                        
         OI    SYLHEDH+6,X'80'                                                  
         MVC   0(8,R3),SYSCODE                                                  
         MVC   0(6,R5),=C'SYSTEM'                                               
         SPACE 1                                                                
         GOTO1 LISTMON                                                          
         B     LIST2                                                            
         EJECT                                                                  
*              PRINT A REPORT FOR SYSTEM                                        
         SPACE 3                                                                
REP2     LA    R2,DEPTLIST                                                      
         LA    R3,64               ELEMENTS X'40' THRU X'47'                    
         LA    R0,4                                                             
         LA    R4,KEY                                                           
         MVC   P+14(8),SYSCODE                                                  
         SPACE 1                                                                
REP4     MVC   P+25(14),0(R2)                                                   
         STC   R3,ELCODE                                                        
         MVI   OPTION,C'W'                                                      
         MVI   MAX,1                                                            
         GOTO1 DISPCHAT                                                         
         MVC   P+42(20),WORK       MANAGER                                      
         LA    R3,1(R3)                                                         
         STC   R3,ELCODE                                                        
         GOTO1 DISPCHAT                                                         
         MVC   P+65(30),WORK       SUPPORT                                      
         MVI   MAX,2                                                            
         GOTO1 DISPCHAT                                                         
         MVC   P2+65(30),WORK                                                   
         CLC   P+42(50),SPACES                                                  
         BE    REP6                                                             
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
REP6     LA    R2,14(R2)                                                        
         LA    R3,1(R3)                                                         
         BCT   R0,REP4                                                          
         MVC   P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LIST2                                                            
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
         L     R4,ABOX             FILL BOXES                                   
         LTR   R4,R4                                                            
         BZ    HOOK2                                                            
         USING BOXD,R4                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXCOLS+12,C'L'                                                  
         MVI   BOXCOLS+23,C'C'                                                  
         MVI   BOXCOLS+40,C'C'                                                  
         MVI   BOXCOLS+63,C'C'                                                  
         MVI   BOXCOLS+96,C'R'                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+59,C'B'                                                  
         SPACE 1                                                                
HOOK2    MVC   H6+14(6),=C'SYSTEM'                                              
         MVC   H6+25(10),=C'DEPARTMENT'                                         
         MVC   H6+42(7),=C'MANAGER'                                             
         MVC   H6+65(12),=C'SUPPORT TEAM'                                       
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS SSPEC H1,2,C'MAP SYSTEM'                                               
         SSPEC H2,2,C'----------'                                               
         SSPEC H1,40,C'SYSTEM REPORT'                                           
         SSPEC H2,40,C'-------------'                                           
         SSPEC H1,77,REPORT                                                     
         SSPEC H1,96,REQUESTOR                                                  
         SSPEC H2,77,RUN                                                        
         SSPEC H2,103,PAGE                                                      
         DC    X'00'               END MARKER FOR SPECS                         
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
DEPTLIST DC    CL14'SYSTEMS'                                                    
         DC    CL14'CLIENT SERVICE'                                             
         DC    CL14'DOCUMENTATION'                                              
         DC    CL14'DATA CONTROL'                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPF4D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPE4D                                                       
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012PEMAP04   05/01/02'                                      
         END                                                                    
