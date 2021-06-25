*          DATA SET SPSFM10    AT LEVEL 038 AS OF 05/01/02                      
*PHASE T21710A                                                                  
         TITLE 'T21710  REASON RECORD'                                          
T21710   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21710                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         LA    R2,SREMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         LA    R2,SRECODEH         CODE FIELD                                   
         GOTO1 ANY                                                              
         MVC   MYCODE,WORK                                                      
         OC    MYCODE,SPACES                                                    
*                                                                               
         XC    MYOFF,MYOFF                                                      
         LA    R2,SREOFFH          OFFICE FIELD                                 
         CLI   5(R2),0             ****NEED TO VALIDATE *******                 
         BE    VK10                                                             
         MVC   MYOFF,8(R2)                                                      
*                                                                               
         USING RSNRECD,R6                                                       
VK10     XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   RSNKTYPE,=X'0D77'   REECORD TYPE                                 
         MVC   RSNKAGY,AGENCY                                                   
         MVC   RSNKMED,QMED                                                     
         MVC   RSNKCODE,MYCODE                                                  
         MVC   RSNKOFF,MYOFF                                                    
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING RSNRECD,R6                                                       
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SRETEXTH                                                      
         GOTO1 ANY                                                              
         XC    ELEM,ELEM                                                        
         USING RSNEL01,R6                                                       
         LA    R6,ELEM                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSNTEXT(0),8(R2)                                                 
         MVI   RSNEL01,X'01'                                                    
         MVI   RSNELNQ,RE01ELLN                                                 
         GOTO1 ADDELEM                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       TWAXC SRETEXTH                                                         
         L     R6,AIO                                                           
         USING RSNEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         MVC   SRETEXT,RSNTEXT                                                  
*                                                                               
DRX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        DISPLAY KEY                                                            
*                                                                               
         USING RSNRECD,R6                                                       
DK       L     R6,AIO                                                           
*                                                                               
DK10     MVC   SREMED,RSNKMED     SET MEDIA                                     
         MVC   SRECODE,RSNKCODE       CODE                                      
         MVC   SREOFF,RSNKOFF         OFFICE                                    
         OI    SREMEDH+6,X'80'                                                  
         OI    SRECODEH+6,X'80'                                                 
         OI    SREOFFH+6,X'80'                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        MISC & ERROR ROUTINES                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX               NEVER TO RETURN                              
         EJECT                                                                  
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME0D                                                       
         EJECT                                                                  
         ORG   SREWORK                                                          
MYOFF    DS    CL2                                                              
MYCODE   DS    CL6                                                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENREAS                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SPSFM10   05/01/02'                                      
         END                                                                    
