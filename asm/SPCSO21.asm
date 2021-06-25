*          DATA SET SPCSO21    AT LEVEL 081 AS OF 05/01/02                      
*PHASE T21821A,*                                                                
         TITLE 'T21821 - CHILD SPOT PROGRAM RECORD MAINTENANCE'                 
T21821   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21821                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MYOVNUM,X'21'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    SHOCODEH+4,X'DF'    FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'21'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,SHOCODEH         VALIDATE SHOW CODE FIELD                     
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         MVC   LOCSHOW,MYSPACES    IF ACTION LIST THEN ALLOW                    
         CLI   ACTNUM,ACTLIST          NOTHING ENTERED                          
         BNE   VK10                                                             
         CLI   5(R2),0                                                          
         BE    VKX                                                              
*                                                                               
VK10     GOTO1 ANY                 OTHERWISE MUST BE ENTERED                    
         MVC   LOCSHOW,8(R2)       SAVE FOR BUILDING KEY                        
         OC    LOCSHOW,MYSPACES                                                 
         OI    4(R2),X'20'                                                      
*                                                                               
VKX      LA    R6,KEY              BUILD KEY                                    
         USING SHOKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   SHOKTYPE,SHOKTYPQ   SHOW RECORD TYPE                             
         MVI   SHOKSTYP,SHOKSTPQ   SHOW RECORD SUB-TYPE                         
         MVC   SHOKCODE,LOCSHOW                                                 
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,SHCODEQ      REMOVE OLD SHOW ELEMENT                      
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM             BUILD SHOW ELEMENT                           
         XC    ELEM,ELEM                                                        
         USING SHELEM,R6                                                        
         MVI   SHCODE,SHCODEQ                                                   
         MVI   SHLEN,SHLENQ                                                     
*                                                                               
         LA    R2,SHONAMEH         VALIDATE NAME FIELD                          
         MVC   SHNAME,SHONAME                                                   
         OC    SHNAME,MYSPACES                                                  
*                                                                               
         LA    R2,SHODURH          VALIDATE DURATION FIELD                      
         GOTO1 ANY                                                              
         TM    4(R2),X'08'                                                      
         BZ    INVERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         C     R3,=F'255'                                                       
         BH    INVERR                                                           
         STC   R3,SHDUR                                                         
*                                                                               
         GOTO1 ADDELEM             ADD SHOW ELEMENT                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO              GET SHOW ELEMENT                             
         MVI   ELCODE,SHCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SHELEM,R6                                                        
*                                                                               
         XC    SHONAME,SHONAME     DISPLAY NAME                                 
         MVC   SHONAME,SHNAME                                                   
         OI    SHONAMEH+6,X'80'                                                 
*                                                                               
         XC    SHODUR,SHODUR       DISPLAY DURATION                             
         EDIT  (1,SHDUR),(3,SHODUR),ALIGN=LEFT,ZERO=NOBLANK                     
         OI    SHODURH+6,X'80'                                                  
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING SHOKEY,R6                                                        
*                                                                               
         XC    SHOCODE,SHOCODE     DISPLAY CODE                                 
         MVC   SHOCODE,SHOKCODE                                                 
         OI    SHOCODEH+6,X'80'                                                 
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING SHOKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   SHOKTYPE,SHOKTYPQ   RECORD TYPE                                  
         MVI   SHOKSTYP,SHOKSTPQ   RECORD SUB-TYPE                              
         MVC   SHOKCODE,LOCSHOW    STARTING SHOW CODE                           
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     MVI   RDUPDATE,C'N'       FIRST RECORD                                 
         GOTO1 HIGH                                                             
         B     LR30                                                             
*                                                                               
LR20     MVI   RDUPDATE,C'N'       NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(2),SAVEKEY      TEST STILL SHOW KEYS                         
         BNE   LRX                                                              
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
*                                                                               
         MVC   LSTCODE,SHOKCODE    DISPLAY CODE FROM KEY                        
*                                                                               
         MVI   RDUPDATE,C'N'       GET RECORD                                   
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              GET SHOW ELEMENT                             
         MVI   ELCODE,SHCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SHELEM,R6                                                        
*                                                                               
         MVC   LSTNAME,SHNAME      DISPLAY NAME AND DURATION                    
         EDIT  (1,SHDUR),(3,LSTDUR),ALIGN=LEFT,ZERO=NOBLANK                     
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ERRRNF   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
MYSPACES DC    CL80' '                                                          
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
       ++INCLUDE SPCSOD1D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
LOCSHOW  DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCODE  DS    CL8                                                              
         DS    CL5                                                              
LSTNAME  DS    CL30                                                             
         DS    CL2                                                              
LSTDUR   DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081SPCSO21   05/01/02'                                      
         END                                                                    
