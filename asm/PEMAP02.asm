*          DATA SET PEMAP02    AT LEVEL 014 AS OF 08/22/00                      
*PHASE TE1B02A                                                                  
         TITLE 'TE1B02 - PERSON RECORD MAINTENANCE'                             
TE1B02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PEMN**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         CLI   MODE,XRECPUT        *                                            
         BNE   *+6                 *TEMP TO TEST RECOVERY                       
         DC    H'0'                *                                            
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
         MVI   PERKTYP,X'02'                                                    
         LA    R2,PERCODH                                                       
         GOTO1 ANY                                                              
         MVC   PERCODE,WORK                                                     
         B     XIT                                                              
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,PERCODH                                                       
         MVC   8(8,R2),PERCODE                                                  
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
         SPACE 3                                                                
VREC     LA    R2,PERSECH          VALIDATE SECURITY                            
         GOTO1 VALISEC                                                          
         LA    R2,PERNAMEH                                                      
         GOTO1 ANY                                                              
         B     DVREC                                                            
         SPACE 1                                                                
DREC     LA    R2,PERSECH          DISPLAY                                      
         GOTO1 TESTSEC             CHECK SECURITY                               
         BNE   TRAPERR                                                          
         GOTO1 DISPSEC             DISPLAY SECURITY                             
         SPACE 1                                                                
DVREC    LA    R2,PERNAMEH         VALIDATE NAME                                
         MVI   ELCODE,X'20'                                                     
         MVI   MAX,1                                                            
         MVI   OPTION,0                                                         
         GOTO1 VALICHAT                                                         
         LA    R2,PERPOSH          POSITION                                     
         MVI   ELCODE,X'22'                                                     
         GOTO1 VALICHAT                                                         
         SPACE 1                                                                
         LA    R2,PERNOTEH         NOTES                                        
         MVI   ELCODE,X'24'                                                     
         MVI   MAX,10                                                           
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
         MVI   PERKTYP,X'02'                                                    
         CLI   PELCODH+5,0                                                      
         BE    LIST1                                                            
         LA    R2,PELCODH                                                       
         GOTO1 ANY                                                              
         MVC   PERCODE,WORK                                                     
         SPACE 1                                                                
LIST1    GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         GOTO1 TESTSEC                                                          
         BNE   LIST2                                                            
         MVC   LISTAR,SPACES       SHOW CODE                                    
         LA    R3,LISTAR                                                        
         LA    R5,PELHED                                                        
         OI    PELHEDH+6,X'80'                                                  
         MVC   0(8,R3),PERCODE                                                  
         MVC   0(6,R5),=C'PERSON'                                               
         SPACE 1                                                                
         LA    R3,10(R3)           NAME                                         
         LA    R5,10(R5)                                                        
         MVI   ELCODE,X'20'                                                     
         MVI   OPTION,C'W'                                                      
         MVI   MAX,1                                                            
         GOTO1 DISPCHAT                                                         
         MVC   0(4,R5),=C'NAME'                                                 
         MVC   0(20,R3),WORK                                                    
         SPACE 1                                                                
         LA    R3,21(R3)           POSITION                                     
         LA    R5,21(R5)                                                        
         MVI   ELCODE,X'22'                                                     
         GOTO1 DISPCHAT                                                         
         MVC   0(20,R3),WORK                                                    
         MVC   0(8,R5),=C'POSITION'                                             
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
         SSPEC H4,96,REQUESTOR                                                  
         SSPEC H5,77,RUN                                                        
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
       ++INCLUDE PEMAPF2D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPE2D                                                       
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014PEMAP02   08/22/00'                                      
         END                                                                    
