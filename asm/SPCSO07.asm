*          DATA SET SPCSO07    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T21807A                                                                  
         TITLE 'T21807 - CHILD SPOT REPORT COMMENTS MAINTENANCE'                
T21807   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21807                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MYOVNUM,X'07'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    COMMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'07'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,COMMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    COMCLTH+4,X'DF'                                                  
         NI    COMSTAH+4,X'DF'                                                  
         NI    COMESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,COMCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    COMSTAH+4,X'DF'                                                  
         NI    COMESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,COMSTAH          VALIDATE MARKET FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    COMESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
         B     VKEST                                                            
*                                                                               
VKEST    LA    R2,COMESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
VKX      LA    R6,KEY              BUILD KEY                                    
         USING COMKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   COMKTYPE,COMKTYPQ   REPORT COMMENTS RECORD TYPE                  
         MVI   COMKSTYP,COMKSTPQ   RECORD SUB-TYPE                              
         MVC   COMKAM,BAGYMD                                                    
         MVC   COMKCLT,BCLT                                                     
         MVC   COMKMKT(5),BMKTSTA                                               
         MVC   COMKEST,BMEST                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       L     R6,AIO                                                           
         USING COMRECD,R6                                                       
         CLI   ACTNUM,ACTADD       TEST IF ADD RECORD                           
         BNE   VR5                                                              
         MVC   COMLEN,DATADISP     INSERT RECORD LENGTH                         
         MVC   COMAGYA,AGENCY      AND ALPHA AGENCY CODE                        
         B     VR10                                                             
         DROP  R6                                                               
*                                                                               
VR5      MVI   ELCODE,HEDCODEQ     REMOVE OLD ELEMENTS                          
         GOTO1 REMELEM                                                          
         MVI   ELCODE,FOTCODEQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
VR10     LA    R2,COMHEADH         VALIDATE HEADLINE COMMENTS AND               
         LA    R6,ELEM                 BUILD HEADLINE ELEMENTS                  
         USING COMHEDEL,R6                                                      
         MVI   HEDCODE,HEDCODEQ                                                 
         MVI   HEDLEN,HEDLENQ                                                   
         SR    R5,R5               R5 = LINE COUNTER                            
*                                                                               
VR20     MVC   HEDTEXT,8(R2)                                                    
         OC    HEDTEXT,SPACES                                                   
         STC   R5,HEDREF                                                        
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST NO MORE COMMENTS                        
         BE    VR30                                                             
         LA    R5,1(R5)                                                         
         C     R5,=F'6'            NO MORE THAN SIX HEADLINES                   
         BL    VR20                                                             
*                                                                               
VR30     LA    R2,COMFOOTH         VALIDATE FOOTLINE COMMENTS AND               
         LA    R6,ELEM                 BUILD FOOTLINE ELEMENTS                  
         USING COMFOTEL,R6                                                      
         MVI   FOTCODE,FOTCODEQ                                                 
         MVI   FOTLEN,FOTLENQ                                                   
         SR    R5,R5               R5 = LINE COUNTER                            
*                                                                               
VR40     MVC   FOTTEXT,8(R2)                                                    
         OC    FOTTEXT,SPACES                                                   
         STC   R5,FOTREF                                                        
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST NO MORE COMMENTS                        
         BE    VRX                                                              
         LA    R5,1(R5)                                                         
         C     R5,=F'8'            NO MORE THAN EIGHT FOOTLINES                 
         BL    VR40                                                             
*                                                                               
VRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,COMHEADH),COMLAST                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,HEDCODEQ     DISPLAY HEADLINE COMMENTS                    
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         USING COMHEDEL,R6                                                      
         LA    R2,COMHEADH                                                      
*                                                                               
DR10     MVC   8(L'HEDTEXT,R2),HEDTEXT                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BAS   RE,NEXTEL           GET NEXT HEADLINE                            
         BE    DR10                                                             
*                                                                               
DR20     L     R6,AIO                                                           
         MVI   ELCODE,FOTCODEQ     DISPLAY FOOTLINE COMMENTS                    
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         USING COMFOTEL,R6                                                      
         LA    R2,COMFOOTH                                                      
*                                                                               
DR30     MVC   8(L'FOTTEXT,R2),FOTTEXT                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BAS   RE,NEXTEL           GET NEXT FOOTLINE                            
         BE    DR30                                                             
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
         SPACE 4                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOF7D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPCSO07   05/01/02'                                      
         END                                                                    
