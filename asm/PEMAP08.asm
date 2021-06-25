*          DATA SET PEMAP08    AT LEVEL 003 AS OF 08/22/00                      
*PHASE TE1B08A                                                                  
         TITLE 'TE1B08 - SCRATCH RECORD MAINTENANCE'                            
TE1B08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SCMN**                                                       
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
         MVI   SCRKTYP,X'08'                                                    
         LA    R2,SCRPERH                                                       
         GOTO1 VALIPERS                                                         
         MVC   SCRKPERS,WORK                                                    
*                                  OBTAIN THE CODE                              
         LA    R2,SCRCODH                                                       
         GOTO1 ANY                                                              
         MVC   SCRKCODE,WORK                                                    
*                                  OBTAIN THE SUB-CODE                          
         LA    R2,SCRSUBH                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   SCRKSUBC,WORK                                                    
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,SCRPERH          DIARIST                                      
         MVC   8(8,R2),SCRKPERS                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,SCRCODH          CODE                                         
         MVC   SCRCOD,SCRKCODE                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,SCRSUBH          SUB-CODE                                     
         MVC   SCRSUB,SCRKSUBC                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
         SPACE 3                                                                
VREC     LA    R2,SCRDETH          VALIDATE DIARY DETAILS                       
         GOTO1 ANY                 MUST HAVE AT LEAST 1 LINE                    
         B     DVREC                                                            
         SPACE 1                                                                
DREC     LA    R2,SCRDETH          DISPLAY                                      
DVREC    MVI   ELCODE,X'80'                                                     
         MVI   MAX,10                                                           
         MVI   OPTION,0                                                         
         GOTO1 VALICHAT                                                         
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
         MVI   SCRKTYP,X'08'                                                    
         LA    R2,SCLPERH          MANDATORY PERSON INPUT                       
         GOTO1 VALIPERS                                                         
         MVC   SCRKPERS,WORK                                                    
*                                                                               
         CLI   SCLCODH+5,0         OPTIONAL START CODE                          
         BE    LIST1                                                            
         ZIC   R1,SCLCODH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     LIST1                                                            
         MVC   SCRKCODE(0),SCLCOD                                               
         SPACE 1                                                                
LIST1    GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(11),KEYSAVE     CHECKMAIN C/B                                
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES       SHOW CODE                                    
         LA    R3,LISTAR                                                        
         LA    R5,SCLHED                                                        
         OI    SCLHEDH+6,X'80'                                                  
         MVC   0(8,R3),SCRKCODE                                                 
         MVC   0(2,R5),=C'ID'                                                   
         SPACE 1                                                                
         LA    R3,10(R3)           SUB-CODE                                     
         LA    R5,10(R5)                                                        
         MVC   0(8,R3),SCRKSUBC                                                 
         MVC   0(6,R5),=C'SUB ID'                                               
         OC    0(8,R3),SPACES                                                   
         SPACE 1                                                                
LIST6    LA    R3,12(R3)           DETAILS                                      
         LA    R5,12(R5)                                                        
         MVI   ELCODE,X'80'                                                     
         MVI   OPTION,C'W'                                                      
         MVI   MAX,1                                                            
         GOTO1 DISPCHAT                                                         
         MVC   0(7,R5),=C'DETAILS'                                              
         MVC   0(32,R3),WORK                                                    
         SPACE 1                                                                
         GOTO1 LISTMON                                                          
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
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS SSPEC H1,2,C'SYSTEM CONTROL'                                           
         SSPEC H2,2,C'--------------'                                           
         SSPEC H1,40,C'PERSON REPORT'                                           
         SSPEC H2,40,C'-------------'                                           
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,REPORT                                                     
         SSPEC H5,77,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         DC    X'00'               END MARKER FOR SPECS                         
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPF8D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPE8D                                                       
         EJECT                                                                  
*PEMAPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PEMAP08   08/22/00'                                      
         END                                                                    
