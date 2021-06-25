*          DATA SET PEMAP03    AT LEVEL 019 AS OF 05/01/02                      
*PHASE TE1B03A                                                                  
         TITLE 'TE1B03 - USER RECORD MAINTENANCE'                               
TE1B03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**USMN**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R1,USERDICT                                                      
         ST    R1,ADTADICT                                                      
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
         MVI   USRKTYP,X'03'                                                    
         LA    R2,USRCODH                                                       
         GOTO1 ANY                                                              
         MVC   USRCODE,WORK                                                     
         B     XIT                                                              
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,USRCODH                                                       
         MVC   8(8,R2),USRCODE                                                  
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
         SPACE 3                                                                
VREC     LA    R2,USRNAMH                                                       
         GOTO1 ANY                                                              
         SPACE 1                                                                
DREC     LA    R2,USRNAMH          NAME                                         
         MVI   OPTION,0                                                         
         MVI   ELCODE,X'30'                                                     
         MVI   MAX,1                                                            
         GOTO1 VALICHAT                                                         
         SPACE 1                                                                
         LA    R2,USRPADDH         ADDRESS (DICTIONARY)                         
         LA    R3,USRPTLXH                                                      
         SPACE 1                                                                
VREC2    GOTO1 VALIDATA                                                         
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         CR    R2,R3                                                            
         BNH   VREC2                                                            
         SPACE 1                                                                
         LA    R2,USRKEYPH         KEY PERSONNEL                                
         MVI   OPTION,0                                                         
         MVI   ELCODE,X'34'                                                     
         MVI   MAX,8                                                            
         GOTO1 VALICHAT                                                         
         B     XIT                                                              
         SPACE 3                                                                
*              DATA DICTIONARY FOR USER ADDRESS                                 
         SPACE 1                                                                
USERDICT DS    0F                                                               
LINE1    DICT  C,32,02,26,SCREEN='ADDRESS LINE 1'                               
LINE2    DICT  C,32,28,26,SCREEN='ADDRESS LINE 2'                               
CITY     DICT  C,32,54,20,SCREEN='ADDRESS CITY'                                 
ZIP      DICT  C,32,74,20,SCREEN='STATE/ZIP'                                    
PHONE    DICT  C,32,94,20,SCREEN='TELEPHONE NUMBER'                             
TELEX    DICT  C,32,114,20,SCREEN='TELEX NUMBER'                                
         DC    X'00'                                                            
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LIST4                                                            
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   USRKTYP,X'03'                                                    
         CLI   USRCODH+5,0                                                      
         BE    LIST1                                                            
         LA    R2,USRCODH                                                       
         GOTO1 ANY                                                              
         MVC   USRCODE,WORK                                                     
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
         LA    R5,USLHED                                                        
         OI    USLHEDH+6,X'80'                                                  
         MVC   0(8,R3),USRCODE                                                  
         MVC   0(4,R5),=C'USER'                                                 
         SPACE 1                                                                
         LA    R3,10(R3)           NAME                                         
         LA    R5,10(R5)                                                        
         MVI   ELCODE,X'30'                                                     
         MVI   OPTION,C'W'                                                      
         MVI   MAX,1                                                            
         GOTO1 DISPCHAT                                                         
         MVC   0(4,R5),=C'NAME'                                                 
         MVC   0(20,R3),WORK                                                    
         LA    R3,21(R3)           TELEPHONE                                    
         LA    R5,21(R5)                                                        
         MVC   0(9,R5),=C'TELEPHONE'                                            
         GOTO1 DIGDATA,DMCB,=C'PHONE   ',(R3)                                   
         GOTO1 LISTMON                                                          
         B     LIST2                                                            
         EJECT                                                                  
*              REPORT USER                                                      
         SPACE 3                                                                
REP2     LA    R2,P+11                                                          
         LA    R4,KEY                                                           
         MVC   2(8,R2),USRCODE                                                  
         MVI   ELCODE,X'30'                                                     
         MVI   OPTION,C'W'                                                      
         MVI   MAX,1                                                            
         GOTO1 DISPCHAT                                                         
         MVC   13(20,R2),WORK      NAME                                         
         LA    R3,36(R2)                                                        
         LA    R4,ADDLIST                                                       
         LA    R0,4                                                             
         BAS   RE,OUTLIST                                                       
         LA    R3,65(R2)                                                        
         LA    R4,TELLIST                                                       
         LA    R0,2                                                             
         BAS   RE,OUTLIST                                                       
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LIST2                                                            
         SPACE 1                                                                
OUTLIST  NTR1                                                                   
         SPACE 1                                                                
OUT      GOTO1 DIGDATA,DMCB,(R4),(R3)                                           
         OC    0(20,R3),SPACES                                                  
         CLC   0(20,R3),SPACES                                                  
         BE    *+8                                                              
         LA    R3,132(R3)                                                       
         LA    R4,8(R4)                                                         
         BCT   R0,OUT                                                           
         B     XIT                                                              
         SPACE 1                                                                
ADDLIST  DC    CL8'LINE1'                                                       
         DC    CL8'LINE2'                                                       
         DC    CL8'CITY'                                                        
         DC    CL8'ZIP'                                                         
         SPACE 1                                                                
TELLIST  DC    CL8'PHONE'                                                       
         DC    CL8'TELEX'                                                       
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
         LA    R2,H6+11                                                         
         MVC   02(04,R2),=C'USER'                                               
         MVC   13(09,R2),=C'USER NAME'                                          
         MVC   36(12,R2),=C'USER ADDRESS'                                       
         MVC   65(15,R2),=C'TELEPHONE/TELEX'                                    
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS+11                                                    
         MVI   00(R2),C'L'                                                      
         MVI   11(R2),C'C'                                                      
         MVI   34(R2),C'C'                                                      
         MVI   63(R2),C'C'                                                      
         MVI   86(R2),C'R'                                                      
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS SSPEC H1,2,C'MAP SYSTEM'                                               
         SSPEC H2,2,C'----------'                                               
         SSPEC H1,40,C'USER REPORT'                                             
         SSPEC H2,40,C'-----------'                                             
         SSPEC H1,77,REPORT                                                     
         SSPEC H1,96,REQUESTOR                                                  
         SSPEC H2,77,RUN                                                        
         SSPEC H2,103,PAGE                                                      
         DC    X'00'               END MARKER FOR SPECS                         
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPF3D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPE3D                                                       
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PEMAP03   05/01/02'                                      
         END                                                                    
