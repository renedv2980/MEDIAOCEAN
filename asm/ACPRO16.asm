*          DATA SET ACPRO16    AT LEVEL 017 AS OF 09/12/02                      
*PHASE T60B16A,*                                                                
         TITLE 'T60B16 - USER FIELD SELECT LIST'                                
T60B16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B16**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR USER RECORDS                                      
         SPACE 3                                                                
         LA    R4,KEY                                                           
         USING ACUFKEY,R4                                                       
         CLC   KEY(2),=X'2C10'     ARE WE HANDLING USERS YET                    
         BE    LIST2                                                            
         MVI   OPTION,0                                                         
         LA    R2,PROOGRH          OPTIONAL OFFICE GROUP FILTER                 
         CLI   5(R2),0                                                          
         BE    KEY2                                                             
         GOTO1 VALOG                                                            
         SPACE 1                                                                
KEY2     LA    R2,PROOFFH          OPTIONAL OFFICE FILTER                       
         CLI   5(R2),0                                                          
         BE    KEY4                                                             
         GOTO1 VALOFF                                                           
         SPACE 1                                                                
KEY4     LA    R2,PROCLIH          OPTIONAL CLIENT FILTER                       
         CLI   5(R2),0                                                          
         BE    KEY6                                                             
         GOTO1 VALCLI                                                           
         SPACE 1                                                                
KEY6     LA    R2,PROPROH          OPTIONAL PRODUCT FILTER                      
         CLI   5(R2),0                                                          
         BE    KEY8                                                             
         GOTO1 VALPROD                                                          
         SPACE 1                                                                
KEY8     LA    R2,PROMGRH          OPTIONAL MED. GROUP FILTER                   
         CLI   5(R2),0                                                          
         BE    KEY10                                                            
         GOTO1 VALMG                                                            
         SPACE 1                                                                
KEY10    LA    R2,PROMEDH          OPTIONAL MEDIA FILTER                        
         CLI   5(R2),0                                                          
         BE    KEY12                                                            
         GOTO1 VALMED                                                           
         SPACE 1                                                                
KEY12    LA    R2,PROJOBH          OPTIONAL JOB FILTER                          
         CLI   5(R2),0                                                          
         BE    LIST1                                                            
         GOTO1 VALJOB                                                           
         SPACE 1                                                                
LIST1    XC    ACUFKEY,ACUFKEY                                                  
         MVI   ACUFRTYP,ACUFEQU                                                 
         MVI   ACUFSREC,ACUFSEQU                                                
         MVC   ACUFCUL,CUL                                                      
         MVC   ACUFOG,PROOGR                                                    
         MVC   ACUFOFC,PROOFF                                                   
         CLI   ACUFOFC,0           TEST FOR AN OFFICE INPUT                     
         BE    *+8                                                              
         OI    ACUFOFC+1,C' '      SPACE PAD THE SECOND BYTE                    
         MVC   ACUFCLI,PROCLI                                                   
         MVC   ACUFPRO,PROPRO                                                   
         MVC   ACUFMG,PROMGR                                                    
         MVC   ACUFMED,PROMED                                                   
         MVC   ACUFJOB,PROJOB                                                   
         SPACE 1                                                                
LIST2    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   ACUFKEY(ACUFOG-ACUFKEY),KEYSAVE  CHECK C/B                       
         BNE   XIT                                                              
         MVC   LISTAR,SPACES                                                    
         CLI   PROOGRH+5,0         CHECK FILTERS                                
         BE    LIST8                                                            
         CLC   ACUFOG,PROOGR                                                    
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST8    CLI   PROOFFH+5,0                                                      
         BE    LIST10                                                           
         LA    R2,PROOFFH                                                       
         GOTO1 ANY                                                              
         CLC   ACUFOFC,WORK                                                     
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST10   CLI   PROCLIH+5,0                                                      
         BE    LIST12                                                           
         LA    R2,PROCLIH                                                       
         GOTO1 ANY                                                              
         CLC   ACUFCLI,WORK                                                     
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST12   CLI   PROPROH+5,0                                                      
         BE    LIST14                                                           
         LA    R2,PROPROH                                                       
         GOTO1 ANY                                                              
         CLC   ACUFPRO,WORK                                                     
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST14   CLI   PROMGRH+5,0                                                      
         BE    LIST16                                                           
         CLC   ACUFMG,PROMGR                                                    
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST16   CLI   PROMEDH+5,0                                                      
         BE    LIST18                                                           
         CLC   ACUFMED,PROMED                                                   
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST18   CLI   PROJOBH+5,0                                                      
         BE    LIST20                                                           
         LA    R2,PROJOBH                                                       
         GOTO1 ANY                                                              
         CLC   ACUFJOB,WORK                                                     
         BNE   LIST4                                                            
         EJECT                                                                  
LIST20   MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETELIO                                                       
         SPACE 1                                                                
         USING ACUFD,R6                                                         
LIST22   BNE   LIST4                                                            
         MVC   LISTAR,SPACES                                                    
         USING LSTDSECT,R3                                                      
         LA    R3,LISTAR                                                        
         MVC   LSTOG,ACUFOG                                                     
         MVC   LSTOFF,ACUFOFC                                                   
         MVC   LSTCLI,ACUFCLI                                                   
         MVC   LSTPRO,ACUFPRO                                                   
         MVC   LSTMG,ACUFMG                                                     
         MVC   LSTMED,ACUFMED                                                   
         MVC   LSTJOB,ACUFJOB                                                   
         MVC   LSTCODE,ACUFCODE                                                 
         MVC   LSTDESC,ACUFDESC                                                 
         MVC   LSTEDIT,ACUFEDIT                                                 
         EDIT  (1,ACUFMXLN),(2,LSTMAX)                                          
         MVI   LSTSTA1,C'N'                                                     
         TM    ACUFSTAT,X'80'                                                   
         BNO   *+8                                                              
         MVI   LSTSTA1,C'Y'                                                     
         MVI   LSTSTA2,C'N'                                                     
         TM    ACUFSTAT,X'40'                                                   
         BNO   *+8                                                              
         MVI   LSTSTA2,C'Y'                                                     
*                                                                               
         OC    ACUFOG(ACUFJOB+6-ACUFOG),ACUFOG                                  
         BNZ   *+10                                                             
         MVC   0(3,R3),=C'ALL'                                                  
         GOTO1 LISTMON                                                          
         BAS   RE,NEXTEL                                                        
         B     LIST22                                                           
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
LSTDSECT DSECT                                                                  
         DS    CL1                                                              
LSTOG    DS    CL1                                                              
         DS    CL3                                                              
LSTOFF   DS    CL2                                                              
         DS    CL1                                                              
LSTCLI   DS    CL6                                                              
         DS    CL1                                                              
LSTPRO   DS    CL6                                                              
         DS    CL3                                                              
LSTMG    DS    CL1                                                              
         DS    CL3                                                              
LSTMED   DS    CL1                                                              
         DS    CL3                                                              
LSTJOB   DS    CL6                                                              
         DS    CL1                                                              
LSTCODE  DS    CL2                                                              
         DS    CL3                                                              
LSTDESC  DS    CL12                                                             
         DS    CL2                                                              
LSTEDIT  DS    CL1                                                              
         DS    CL2                                                              
LSTMAX   DS    CL2                                                              
         DS    CL3                                                              
LSTSTA1  DS    CL1                                                              
         DS    CL3                                                              
LSTSTA2  DS    CL1                                                              
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROE6D                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACPRO16   09/12/02'                                      
         END                                                                    
