*          DATA SET ACPRO18    AT LEVEL 004 AS OF 09/12/02                      
*PHASE T60B18A,*                                                                
         TITLE 'T60B18 - MEDIA LIST'                                            
T60B18   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B18**,RA                                                    
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
*              HANDLE I/O FOR MEDIA RECORDS                                     
         SPACE 3                                                                
         CLI   KEY,X'09'           ARE WE HANDLING MEDIA RECORDS YET?           
         BE    LIST2                                                            
         LA    R2,PROMGRH          OPTIONAL GROUP FILTER                        
         CLI   5(R2),0                                                          
         BE    LIST1                                                            
         MVI   OPTION,0                                                         
         GOTO1 VALMG                                                            
         SPACE 1                                                                
LIST1    XC    KEY,KEY                                                          
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),CUL                                                     
         B     LIST3                                                            
         SPACE 1                                                                
LIST2    ZIC   R1,KEY+2                                                         
         LA    R1,1(R1)            BUMP KEY AHEAD TO GET NEXT MEDIA             
         STC   R1,KEY+2                                                         
         SPACE 1                                                                
LIST3    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   KEY(2),KEYSAVE      CHECK C/B                                    
         BNE   XIT                                                              
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR+2                                                      
         L     R4,AIO                                                           
         MVC   0(1,R3),2(R4)       SHOW MEDIA CODE                              
         LA    R3,LISTAR+9                                                      
         MVI   ELCODE,ACMDELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACMEDIAD,R6                                                      
         MVC   0(1,R3),ACMDGRP     MEDIA GROUP                                  
         CLI   PROMGR,X'41'        OPTIONAL GROUP FILTER                        
         BL    LIST8                                                            
         CLC   PROMGR(1),0(R3)                                                  
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST8    LA    R3,LISTAR+14        DESCRIPTION                                  
         MVC   0(12,R3),ACMDDESC+3                                              
         LA    R3,LISTAR+29                                                     
         MVC   0(14,R3),ACMDCOMM+1                                              
         LA    R3,LISTAR+48                                                     
         MVC   0(1,R3),ACMDANAL                                                 
         GOTO1 LISTMON                                                          
         B     LIST4                                                            
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
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
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
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROE8D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACPRO18   09/12/02'                                      
         END                                                                    
