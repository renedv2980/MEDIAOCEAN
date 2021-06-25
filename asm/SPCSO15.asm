*          DATA SET SPCSO15    AT LEVEL 078 AS OF 05/01/02                      
*PHASE T21815A,*                                                                
         TITLE 'T21815 - CHILD SPOT PROGRAM RECORD EXTENSION MAINT'             
T21815   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21815                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MYOVNUM,X'15'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    PROMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'15'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         DC    H'0'                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       MVI   KEYCHANG,C'N'                                                    
*                                                                               
         LA    R2,PROMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    PROCLTH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,PROCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    PROSTAH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,PROSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    PROESTH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,PROESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKESTX                                                           
         NI    PROREFH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
VKESTX   DS    0H                                                               
*                                                                               
VKREF    LA    R2,PROREFH          VALIDATE REFERENCE FIELD                     
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALIREF                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKX      LA    R6,LOCALKEY         BUILD KEY AND SAVE LOCALLY                   
         USING CSOKEY,R6                                                        
         XC    LOCALKEY,LOCALKEY                                                
         MVI   CSOKTYPE,CSOKTYPQ   CSO RECORD TYPE                              
         MVI   CSOKSTYP,CSOKSTPQ   CSO RECORD SUB-TYPE                          
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT(5),BMKTSTA                                               
         MVC   CSOKEST,BMEST                                                    
         MVC   CSOKREF,BREF                                                     
         DROP  R6                                                               
*                                                                               
         CLI   KEYCHANG,C'Y'       IF KEY CHANGED                               
         BE    DR                  THEN DISPLAY ONLY                            
         EJECT                                                                  
* VALIDATE RECORD AND SAVE CHANGES                                              
*                                                                               
VR       MVC   KEY,LOCALKEY        READ RECORD FOR UPDATE                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,EXCODEQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM             BUILD PROGRAM EXTENTION ELEMENT              
         XC    ELEM,ELEM                                                        
         USING CSOEXEL,R6                                                       
         MVI   EXCODE,EXCODEQ                                                   
         MVI   EXLEN,EXLENQ                                                     
*                                                                               
         LA    R2,PROFTDH          VALIDATE FTD                                 
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(1,8(R2)),EXFTD                                      
         OC    DMCB(4),DMCB                                                     
         BZ    INVERR                                                           
         CLC   EXFTD,QSTART                                                     
         BL    ESTERR                                                           
         CLC   EXFTD,QEND                                                       
         BH    ESTERR                                                           
*                                                                               
         LA    R2,PROLTDH          VALIDATE LTD                                 
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(1,8(R2)),EXLTD                                      
         OC    DMCB(4),DMCB                                                     
         BZ    INVERR                                                           
         CLC   EXLTD,QSTART                                                     
         BL    ESTERR                                                           
         CLC   EXLTD,QEND                                                       
         BH    ESTERR                                                           
*                                                                               
         LA    R2,PRODURH          VALIDATE DURATION                            
         GOTO1 ANY                                                              
         TM    4(R2),X'08'                                                      
         BZ    INVERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         C     R3,=F'99'                                                        
         BH    INVERR                                                           
         STC   R3,EXDUR                                                         
*                                                                               
         LA    R2,PROSEGH          VALIDATE SEGMETS                             
         GOTO1 ANY                                                              
         TM    4(R2),X'08'                                                      
         BZ    INVERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         C     R3,=F'99'                                                        
         BH    INVERR                                                           
         STC   R3,EXSEGS                                                        
*                                                                               
         GOTO1 ADDELEM             ADD PROGRAM EXTENTION ELEMENT                
*                                                                               
         GOTO1 PUTREC              WRITE RECORD BACK                            
         B     DR                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BNE   DR10                                                             
         MVC   KEY,LOCALKEY        THEN READ RECORD                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                  CLEAR AND TRANSMIT ALL DATA FLDS             
DR10     GOTO1 CLEARF,DMCB,(0,PROFTDH),PROLAST                                  
*                                                                               
         L     R6,AIO              DISPLAY PROGRAM EXTENTION ELEMENT            
         MVI   ELCODE,EXCODEQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRX                 DISPLAY NOTHING IF NO ELEMENT                
         USING CSOEXEL,R6                                                       
*                                  DISPLAY EACH FIELD                           
         GOTO1 DATCON,DMCB,(0,EXFTD),(5,PROFTD)                                 
         GOTO1 DATCON,DMCB,(0,EXFTD),(5,PROLTD)                                 
         EDIT  (1,EXDUR),(3,PRODUR),ALIGN=LEFT,ZERO=NOBLANK                     
         EDIT  (1,EXSEGS),(3,PROSEG),ALIGN=LEFT,ZERO=NOBLANK                    
*                                                                               
DRX      B     EXIT                                                             
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
ESTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ERROR - DATE OUTSIDE OF MASTER ESTIMATE'          
         B     EXITX                                                            
*                                                                               
EXIT     XC    CONHEAD,CONHEAD                                                  
         CLI   KEYCHANG,C'Y'                                                    
         BNE   EXIT2                                                            
         MVC   CONHEAD(30),=C'DATA DISPLAYED - ENTER CHANGES'                   
         LA    R2,PROFTDH                                                       
         B     EXITX                                                            
EXIT2    MVC   CONHEAD(35),=C'RECORD CHANGED - ENTER NEXT REQUEST'              
         LA    R2,PROMEDH                                                       
*                                                                               
EXITX    OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         GOTO1 ERREX2                                                           
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
       ++INCLUDE SPCSOE5D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
KEYCHANG DS    C                                                                
LOCALKEY DS    CL48                                                             
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
*GEND     DSECT                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078SPCSO15   05/01/02'                                      
         END                                                                    
