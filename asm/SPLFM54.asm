*          DATA SET SPLFM54    AT LEVEL 060 AS OF 05/01/02                      
*PHASE T21954A                                                                  
         TITLE 'SPLFM54 - CLIENT GROUP DEFINITIONS'                             
T21954   CSECT                                                                  
         NMOD1 0,T21954                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R6,REC                                                           
         ST    R6,AREC                                                          
*                                                                               
         LA    R1,CGRLISTH                                                      
         LA    R2,CGRCLTLH                                                      
         TWAXC (R1),(R2),PROT=Y                                                 
*                                                                               
         LA    R2,LFMACTH                                                       
         MVI   ERRCD,INVERR                                                     
         CLI   SVFMTSW,0           TEST CHANGE OR DISPLAY?                      
         BNE   LFMERR              CHANGE IS INVALID                            
         EJECT                                                                  
*                                                                               
         LA    R2,CGRLISTH                                                      
         USING CLTD,R2                                                          
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING GRPRECD,R3                                                       
         MVI   GRPKTYP,GRPKTYPQ                                                 
         MVI   GRPKSTYP,GRPKCTYQ                                                
         MVC   GRPKAGMD,SVKEY+1    AGENCY MEDIA                                 
         LA    R2,ONEH                                                          
         GOTO1 HIGH                                                             
         B     CGR10                                                            
CGR05    GOTO1 SEQ                                                              
*                                                                               
CGR10    CLC   KEYSAVE(3),KEY                                                   
         BNE   CGRX                                                             
         LA    R3,KEYSAVE                                                       
*                                                                               
         GOTO1 GETREC                                                           
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'30'        VALUE ELEMENT                                
CGR15    BAS   RE,NEXTEL                                                        
         BNE   CGR05                                                            
         CLC   2(3,R6),SVEBCCLT                                                 
         BNE   CGR15                                                            
*                                                                               
         LA    R3,REC                                                           
         MVC   8(1,R2),GRPKID                                                   
         MVC   FULL,GRPKCODE                                                    
         L     R1,FULL                                                          
         SRL   R1,4                                                             
         ST    R1,FULL                                                          
         OI    FULL+2,X'0F'                                                     
         UNPK  9(4,R2),FULL(3)                                                  
         OI    ONEH+6,X'80'         TRANSMIT                                    
         LA    R2,NEXTL                                                         
         LA    R2,ONEH                                                          
         LA    R1,CGRCLTLH                                                      
         CR    R2,R1                                                            
         BL    CGR05               GO AGAIN IF THERE'S ROOM                     
*                                                                               
CGRX     B     EXIT                                                             
         EJECT                                                                  
LFMERR   GOTO1 ERROR                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
*SPLFMB4                                                                        
       ++INCLUDE SPLFMB4D                                                       
         EJECT                                                                  
         ORG   SVAPPL                                                           
DATADISP DS    H                                                                
CTKEY    DS    CL28                                                             
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
CLTD     DSECT                                                                  
ONEH     DS    CL8                                                              
ONE      DS    CL5                                                              
NEXTL    DS    0C                                                               
*                                                                               
 END                                                                            
