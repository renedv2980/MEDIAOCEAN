*          DATA SET SPCSO22    AT LEVEL 107 AS OF 05/07/02                      
*PHASE T21822A,*                                                                
         TITLE 'T21822 - CHILD SPOT CONTRACT RECORD MAINTENANCE'                
T21822   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21822                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MYOVNUM,X'22'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    CNTMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'22'       STORE OVERLAY NUMBER                         
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
VK       LA    R2,CNTMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    CNTCLTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,CNTCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    CNTSTAH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,CNTSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKDATE                                                           
         NI    CNTDATEH+4,X'DF'                                                 
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BNE   VKSTA10                                                          
         XC    BMKTSTA(5),BMKTSTA                                               
         CLI   5(R2),0             THEN STATION IS OPTIONAL                     
         BE    VKSTAX                                                           
*                                                                               
VKSTA10  GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
*                                                                               
VKSTAX   OI    4(R2),X'20'                                                      
*                                                                               
VKDATE   LA    R2,CNTDATEH         VALIDATE DATE FIELD                          
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BNE   VKDATE10                                                         
         XC    DATEFMT2,DATEFMT2                                                
         CLI   5(R2),0             THEN DATE IS OPTIONAL                        
         BE    VKDATEX                                                          
*                                                                               
VKDATE10 BAS   RE,VALMEST                                                       
*                                                                               
VKDATEX  OI    4(R2),X'20'                                                      
*                                                                               
VKX      LA    R6,KEY              BUILD KEY                                    
         USING CNTKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   CNTKTYPE,CNTKTYPQ   CONTRACT RECORD TYPE                         
         MVI   CNTKSTYP,CNTKSTPQ   CONTRACT RECORD SUB-TYPE                     
         MVC   CNTKAM,BAGYMD                                                    
         MVC   CNTKCLT,BCLT                                                     
         MVC   CNTKMKT(5),BMKTSTA                                               
         MVC   CNTKDATE,DATEFMT2                                                
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVC   MYKEY,KEY           SAVE KEY FOR RESTORE READ                    
*                                                                               
         L     R6,AIO              BUILD RECORD FROM KEY                        
         USING CNTRECD,R6                                                       
         XC    0(25,R6),0(R6)                                                   
         MVC   CNTKEY,KEY                                                       
         MVC   CNTLEN,DATADISP     INSERT RECORD LENGTH                         
         MVC   CNTAGYA,AGENCY      AND ALPHA AGENCY CODE                        
*                                                                               
         LA    R6,ELEM             BUILD CONTRACT ELEMENT                       
         XC    ELEM,ELEM                                                        
         USING COELEM,R6                                                        
         MVI   COCODE,COCODEQ                                                   
         MVI   COLEN,COLENQ                                                     
*                                                                               
         LA    R2,CNTL1H           R2 = A(FIRST LINE OF DATA)                   
*                                                                               
VR10     LA    RF,CNTLAST          IF END OF SCREEN OR SHOW IS BLANK            
         CR    RF,R2                   THEN DONE                                
         BNH   VR100                                                            
         CLI   5(R2),0                                                          
         BE    VR100                                                            
*                                                                               
         BAS   RE,VALSHOW          VALIDATE SHOW FIELD                          
         BNE   ERRSHOW                                                          
         MVC   COSHOW,8(R2)                                                     
         OC    COSHOW,MYSPACES                                                  
*                                                                               
         ZIC   R0,0(R2)            BUMP TO FTD                                  
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                  VALIDATE FTD                                 
         GOTO1 DATVAL,DMCB,(0,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         GOTO1 DATCON,DMCB,(0,THISDATE),(3,COFTD)                               
*                                                                               
         ZIC   R0,0(R2)            BUMP TO LTD                                  
         AR    R2,R0                                                            
*                                  VALIDATE LTD                                 
         GOTO1 DATVAL,DMCB,(0,8(R2)),NEXTDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         CLC   NEXTDATE,THISDATE   MAKE SURE LTD COMES AFTER FTD                
         BL    ERRDATES                                                         
         GOTO1 DATCON,DMCB,(0,NEXTDATE),(3,COLTD)                               
*                                                                               
         ZIC   R0,0(R2)            BUMP TO SEGMENTS                             
         AR    R2,R0                                                            
*                                                                               
         GOTO1 ANY                 VALIDATE SEGMENTS                            
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
         STC   R3,COSEG                                                         
*                                                                               
         ZIC   R0,0(R2)            BUMP TO DAYS                                 
         AR    R2,R0                                                            
*                                                                               
         GOTO1 ANY                 VALIDATE DAYS                                
         XC    CODAYS,CODAYS                                                    
         XC    COTIMES,COTIMES                                                  
         CLC   =C'TBD',WORK        IF 'TDB' ENTERED THEN SET TO ZEROS           
         BE    VR30                                                             
         ZIC   R3,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R3),8(R2)),CODAYS,MYWORK                           
         CLI   CODAYS,0                                                         
         BE    INVERR                                                           
*                                                                               
VR30     ZIC   R0,0(R2)            BUMP TO TIMES                                
         AR    R2,R0                                                            
*                                                                               
         CLI   CODAYS,0            IF 'TDB' ENTERED FOR DAYS THEN               
         BE    VR40                    SKIP TIMES                               
*                                                                               
         GOTO1 ANY                 ELSE VALIDATE TIMES                          
         ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R3),8(R2)),MYWORK                                  
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         MVC   COTIMES,MYWORK                                                   
*                                                                               
VR40     ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT AND LOOP BACK                    
         BE    VR10                                                             
*                                                                               
VR100    CLI   ACTNUM,ACTADD       IF NOT ACTION ADD                            
         BE    VR110                                                            
         MVC   KEY(13),MYKEY       THEN RESTORE READ SEQUENCE                   
         GOTO1 READ                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
VR110    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE DATE ENTERED IN THE FIELD POINTED TO BY            
* R2 AND MAKES SURE THERE IS A MASTER ESTIMATE LIST RECORD WITH THAT            
* DATE.  IF SO, THE DATE IS SAVED IN 2-BYTE FORMAT FOR KEY BUILDING.            
*                                                                               
VALMEST  NTR1                                                                   
         GOTO1 ANY                 VALIDATE FIELD INTO 6-BYTE FORMAT            
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATEFMT0                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
*                                                                               
         LA    R4,KEY              READ FOR MASTER ESTIMATE KEY                 
         USING MASKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   MASKTYPE,MASKTYPQ                                                
         MVI   MASKSTYP,MASKSTPQ                                                
         MVC   MASKAM,BAGYMD                                                    
         MVC   MASKCLT,BCLT                                                     
         MVC   MASKDATE,DATEFMT0                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRMEST                                                          
*                                  SAVE DATE IN 2-BYTE FORMAT                   
         GOTO1 DATCON,DMCB,(0,DATEFMT0),(2,DATEFMT2)                            
*                                                                               
VMX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE SHOW ENTERED IN THE FIELD POINTED TO BY            
* R2 BY READING FOR THE SHOW RECORD, AND IF IT IS FOUND IT FILLS IN THE         
* FIELD THAT FOLLOWS WITH THE SHOW NAME AND RETURNS CONDITION CODE EQ.          
* OTHERWISE IT RETURNS NE.                                                      
*                                                                               
VALSHOW  NTR1                                                                   
         GOTO1 ANY                 FIELD IS REQUIRED                            
*                                                                               
         LA    R4,KEY              READ FOR SHOW KEY                            
         USING SHOKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   SHOKTYPE,SHOKTYPQ                                                
         MVI   SHOKSTYP,SHOKSTPQ                                                
         MVC   SHOKCODE,WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NO                  IF NOT FOUND THEN RETURN NE                  
*                                                                               
         MVC   AIO,AIO2            READ RECORD                                  
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2             R6 = A(SHOW ELEMENT)                         
         MVI   ELCODE,SHCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SHELEM,R6                                                        
*                                                                               
         ZIC   R0,0(R2)            BUMP R2 TO NAME FIELD                        
         AR    R2,R0                                                            
         MVC   8(22,R2),SHNAME     FILL IN NAME                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         B     YES                 RETURN EQ                                    
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,CNTL1H),CNTLAST                                   
*                                                                               
         L     R6,AIO              DISPLAY CONTRACT ELEMENTS                    
         MVI   ELCODE,COCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING COELEM,R6                                                        
*                                                                               
         LA    R2,CNTL1H           R2 = A(FIRST LINE OF DATA)                   
*                                                                               
DR10     LA    RF,CNTLAST          IF END OF SCREEN THEN ERROR                  
         CR    RF,R2                                                            
         BNH   OVERERR                                                          
*                                                                               
         MVC   8(8,R2),COSHOW      DISPLAY SHOW                                 
         MVI   5(R2),8                                                          
*                                                                               
         BAS   RE,VALSHOW          IF SHOW RECORD NOT FOUND                     
         BE    DR20                                                             
         ZIC   R0,0(R2)            THEN DISPLAY ERROR IN NAME FIELD             
         AR    R2,R0                                                            
         MVC   8(19,R2),=C'** RECORD NOT FOUND'                                 
         B     DR30                                                             
*                                                                               
DR20     ZIC   R0,0(R2)            ELSE BUMP PAST NAME FIELD                    
         AR    R2,R0                                                            
*                                                                               
DR30     ZIC   R0,0(R2)            BUMP TO FTD AND DISPLAY                      
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,COFTD),(5,8(R2))                                  
*                                                                               
         ZIC   R0,0(R2)            BUMP TO LTD AND DISPLAY                      
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,COLTD),(5,8(R2))                                  
*                                                                               
         ZIC   R0,0(R2)            BUMP TO SEGMENTS AND DISPLAY                 
         AR    R2,R0                                                            
         EDIT  (1,COSEG),(3,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                      
*                                                                               
         ZIC   R0,0(R2)            BUMP TO DAYS AND DISPLAY                     
         AR    R2,R0                                                            
         MVC   8(3,R2),=C'TBD'                                                  
         CLI   CODAYS,0                                                         
         BE    DR40                                                             
         GOTO1 UNDAY,DMCB,CODAYS,8(R2)                                          
*                                                                               
DR40     ZIC   R0,0(R2)            BUMP TO TIMES AND DISPLAY                    
         AR    R2,R0                                                            
         CLI   CODAYS,0                                                         
         BE    DR50                                                             
         GOTO1 UNTIME,DMCB,COTIMES,8(R2)                                        
*                                                                               
DR50     ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
*                                                                               
         BAS   RE,NEXTEL           BUMP TO NEXT ELEMENT                         
         BE    DR10                LOOP BACK IF MORE ELEMENTS                   
*                                                                               
DR100    B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING CNTKEY,R6                                                        
*                                  FILL IN KEY FIELDS                           
         GOTO1 CLEARF,DMCB,(0,CNTMEDH),CNTKENDH                                 
         MVC   CNTMED(L'QMED),QMED                                              
         MVC   CNTCLT(L'QCLT),QCLT                                              
* GET WHOLE STATION, EVEN IF CABLE                                              
         GOTO1 MSUNPK,DMCB,('80',CNTKMKT),FULL,CNTSTA                           
         GOTO1 DATCON,DMCB,(2,CNTKDATE),(5,CNTDATE)                             
         GOTO1 DATCON,DMCB,(2,CNTKDATE),(0,DATEFMT0)                            
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY              R4 = A(KEY)                                  
         USING CNTKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   CNTKTYPE,CNTKTYPQ   RECORD TYPE                                  
         MVI   CNTKSTYP,CNTKSTPQ   RECORD SUB-TYPE                              
         MVC   CNTKAM,BAGYMD       AGY/MED                                      
         MVC   CNTKCLT,BCLT        CLIENT                                       
         MVC   CNTKMKT(5),BMKTSTA  MARKET/STATION                               
         MVC   CNTKDATE,DATEFMT2   DATE                                         
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     MVI   RDUPDATE,C'N'       FIRST RECORD                                 
         GOTO1 HIGH                                                             
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(5),SAVEKEY      TEST SAME TYPE/AGY/MED/CLI                   
         BNE   LRX                                                              
*                                                                               
         OC    BMKTSTA(5),BMKTSTA  IF STATION SPECIFIED                         
         BZ    LR40                                                             
         CLC   CNTKMKT(5),BMKTSTA  THEN KEY MUST MATCH                          
         BNE   LRX                                                              
*                                                                               
LR40     OC    DATEFMT2,DATEFMT2   IF DATE SPECIFIED                            
         BZ    LR50                                                             
         CLC   CNTKDATE,DATEFMT2   THEN KEY MUST BE >=                          
         BL    LRX                                                              
*                                                                               
LR50     XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         GOTO1 MSUNPK,DMCB,CNTKMKT,FULL,LSTSTA                                  
         GOTO1 DATCON,DMCB,(2,CNTKDATE),(5,LSTDATE)                             
*                                                                               
         GOTO1 GETREC              READ RECORD FOR GENCON                       
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
ERRMEST  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'ERROR - MASTER EST LIST RECORD NOT FOUND'         
         GOTO1 ERREX2                                                           
*                                                                               
ERRSHOW  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'ERROR - SHOW RECORD NOT FOUND'                    
         GOTO1 ERREX2                                                           
*                                                                               
ERRDATES XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'ERROR - LTD MUST COME AFTER FTD'                  
         GOTO1 ERREX2                                                           
*                                                                               
OVERERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'ERROR - RECORD DOES NOT FIT IN SCREEN'            
         LA    R2,CNTMEDH                                                       
         GOTO1 ERREX2                                                           
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
       ++INCLUDE SPCSOD2D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
DATEFMT0 DS    CL6                                                              
DATEFMT2 DS    XL2                                                              
MYKEY    DS    XL13                                                             
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTSTA   DS    CL8                                                              
         DS    CL2                                                              
LSTDATE  DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107SPCSO22   05/07/02'                                      
         END                                                                    
