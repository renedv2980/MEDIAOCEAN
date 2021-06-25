*          DATA SET TAGEN1B    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T7021BA                                                                  
         TITLE 'T7021B - EMPLOYER MAINTENANCE'                                  
T7021B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7021B                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB                                               
*                                                                               
         CLI   MODE,VALKEY         IF FIRST TIME IN                             
         BE    VK                  VALIDATE KEY                                 
         SPACE 3                                                                
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    EMP15                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    EMP15                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   EMP20                                                            
EMP15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
EMP20    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   XIT                                                              
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE THE KEY                                                 
*                                                                               
VK       GOTO1 FLDVAL,DMCB,(X'04',SEMEMPNH),SEMLHDRH    UNPROTECT FLDS          
         OI    SEMEMPNH+1,X'01'   SET MODIFY BIT ON                             
*                                                                               
         CLI   RECNUM,EM           IF EMPLOYER RECORD                           
         BNE   VK10                                                             
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'40',SEMEMPH)    BUILD THE KEY             
         B     XIT                                                              
*                                                                               
VK10     LA    R2,SEMEMPH                                                       
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'24',(R2))  ELSE READ RECORD               
         BE    *+16                                                             
         CLI   ERROR,NOTFOUND      IF EMPLOYER RECORD NOT FOUND                 
         BE    EMPNTFND            THEN CANNOT ACCESS EMTAX RECORD              
         B     ERRXIT                                                           
         BAS   RE,DISPHDR          DISPLAY REG EMP. INFO NOW                    
         GOTO1 FLDVAL,DMCB,(X'08',SEMEMPNH),SEMLHDRH  PROTECT ALL FLDS          
         NI    SEMEMPNH+1,X'FE'                       SET MODIFY BIT ON         
         GOTO1 RECVAL,DMCB,TLEXCDQ,(X'40',(R2))       BLD EMP TAX KEY           
         LA    R2,SEMSIDH                                                       
         ST    R2,AFRSTREC         RESET TO FIRST UNPROTECTED FIELD             
         B     XIT                                                              
         SPACE                                                                  
*                                                                               
*              DISPLAY THE KEY                                                  
*                                                                               
DK       L     R4,AIO                                                           
         USING TLEMD,R4                                                         
         MVC   SEMEMP,TLEMEMP                                                   
         OI    SEMEMPH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         CLI   RECNUM,EX          FOR EMTAX RECORD                              
         BE    *+8                                                              
         BAS   RE,DISPHDR         HEADER INFO ALREADY DISPLAYED                 
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'01',SEMSIDH),(X'10',SEMLFLDH)  NORM,CLR           
         GOTO1 ACTVOUT,DMCB,SEMLCHGH                         LST CHANG          
*                                                                               
         LA    R2,SEMSIDH                                                       
         L     R4,AIO              FIND ID ELEMENTS                             
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         B     DR40                                                             
*                                                                               
DR30     BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
*                                                                               
         USING TATID,R4                                                         
DR40     CLI   TATITYPE,TATITYTX                                                
         BE    DR50                                                             
         CLI   TATITYPE,TATITYUN                                                
         BNE   DR30                                                             
DR50     CLC   TATIUNIT,=C'FD '    IRS ID NUMBER                                
         BE    DR30                SKIP IT                                      
*                                                                               
         LA    R1,2                                                             
         CLI   TATIUNIT+2,C' '     STATE CODE = 2 OR 3                          
         BH    DR60                                                             
         GOTO1 TAXVAL,DMCB,(2,TATIUNIT)                                         
         TM    TGTASTAT,TALUNTAX   IF NO INCOME TAX FOR THIS STATE              
         BZ    *+8                                                              
         OI    1(R2),X'08'         SET FIELD TO HIGH INTENSITY                  
         LA    R1,1                LENGTH OF MOVE - 1                           
*                                                                               
DR60     EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),TATIUNIT    **  EXECUTED                                 
         AR    R1,R2                LOCATION TO PUT '='                         
         AH    R1,=H'9'                                                         
         MVI   0(R1),C'='                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'TATIID,R1),TATIID                                            
*                                                                               
         SR    R0,R0               BUMP TO NEXT FIELD                           
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     DR30                                                             
*                                                                               
DRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY HEADER INFO, PROTECT/UNPROTECT FLDS AS NECESSARY         
*                                                                               
DISPHDR  NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TANAELQ,SEMEMPNH  EMPLOYER NAME                     
*                                                                               
         MVI   ELCODE,TATIELQ      IRS NUMBER                                   
         MVI   WORK,TATITYUN                                                    
         MVC   WORK+1(3),=C'FD '                                                
         GOTO1 GETL,DMCB,(4,WORK)                                               
         BE    *+6                                                              
         DC    H'0'                MUST BE IRS NUMBER                           
         L     R4,TGELEM           GET IRS NUMBER FROM ID ELEM                  
         USING TATID,R4                                                         
         MVC   SEMIRS(L'TATIID),TATIID                                          
         OI    SEMIRSH+6,X'80'                                                  
*                                                                               
         GOTO1 CHAROUT,DMCB,TANUELQ,SEMGST#H,TANUTGST  GST NUMBER               
*                                                                               
         L     R4,AIO              GET EMPLOYER DETAILS ELEMENT                 
         MVI   ELCODE,TAEDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DSPH30                                                           
         USING TAEDD,R4                                                         
         MVC   SEMDST,TAEDDST                      DEFAULT STATE                
         OI    SEMDSTH+6,X'80'                                                  
*                                                                               
DSPH30   GOTO1 CHAROUT,DMCB,TAADELQ,(3,SEMADDRH)   EMPLOYER ADDRESS             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         MVI   ELCODE,TATIELQ     DELETE ALL TAX OR UNEMP  ID'S                 
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   RECNUM,EX          FOR REG. EMPLOYER RECORD                      
         BE    *+8                                                              
         BAS   RE,BLDHDR          & DELETE/ADD HEADER INFO                      
*                                                                               
         GOTO1 ACTVIN,DMCB,SEMLCHGH    LAST CHANGED                             
         LA    R2,SEMSIDH         ANY STATE ID INPUT?                           
BLD20    CLI   5(R2),0                                                          
         BE    BLD60                                                            
         LA    R5,SCANBLK                                                       
         USING SCAND,R5                                                         
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TATID,R4                                                         
         MVI   TATIEL,TATIELQ     ELEMENT CODE                                  
         MVI   TATILEN,TATILNQ    ELEMENT LENGTH                                
         MVI   TATITYPE,TATITYTX  TAX CODE                                      
         CLI   RECNUM,EX          IF EMTAX RECORD                               
         BE    *+8                                                              
         MVI   TATITYPE,TATITYUN  ELSE UNEMPLOYMENT CODE                        
*                                                                               
         GOTO1 SCANNER,DMCB,(14,(R2)),(1,(R5))                                  
         CLI   RECNUM,EX          IF EMTAX RECORD                               
         BNE   *+14                                                             
         CLC   =C'FD',SCDATA1                                                   
         BE    INVERR             FD NOT VALID STATE                            
*                                                                               
         ZIC   R3,SCLEN1                                                        
         GOTO1 TAXVAL,DMCB,((R3),SCDATA1)                                       
         BNE   INVERR                                                           
         BCTR  R3,0                FOR MOVE                                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TATIUNIT(0),SCDATA1                                              
         OC    TATIUNIT,SPACES                                                  
*                                                                               
         MVI   ELCODE,TATIELQ     CHECK FOR DUPLICATE STATE ENTRY               
         GOTO1 GETL,DMCB,(4,TATITYPE)                                           
         BE    INVERR                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SCLEN2         LENGTH OF ID NUM                             
         BZ    INVERR              MUST HAVE SECOND HALF                        
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TATIID(0),SCDATA2                                                
         GOTO1 ADDELEM                                                          
*                                                                               
BLD60    SR    R0,R0               BUMP TO NEXT FIELD ON SCREEN                 
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         LA    R6,SEMLFLDH                                                      
         CR    R2,R6               REACHED END OF SCREEN                        
         BNH   BLD20                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              BUILD THE HEADER                                                 
*                                                                               
BLDHDR   NTR1                                                                   
         GOTO1 NAMIN,DMCB,TANAELQ,SEMEMPNH         EMPLOYER NAME                
*                                                                               
         GOTO1 NAMIN,DMCB,TANUELQ,SEMGST#H,TANUTGST  GST NUMBER                 
*                                                                               
         LA    R2,SEMIRSH                          IRS NUMBER                   
         GOTO1 ANY                                                              
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TATID,R4                                                         
         MVI   TATIEL,TATIELQ                      ELEMENT CODE                 
         MVI   TATILEN,TATILNQ                     ELEMENT LENGTH               
         MVI   TATITYPE,TATITYUN                   UNEMPLOYMENT CODE            
         MVC   TATIUNIT,=C'FD '                    FEDERAL UNIT                 
         MVC   TATIID,WORK                                                      
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,TAEDELQ                      DELETE DEFAULT STATE         
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SEMDSTH                          DEFAULT STATE                
         GOTO1 ANY                                                              
         GOTO1 TAXVAL,DMCB,(2,8(R2))                                            
         BNE   INVERR                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAEDD,R4                                                         
         MVI   TAEDEL,TAEDELQ                      ELEMENT CODE                 
         MVI   TAEDLEN,TAEDLNQ                     ELEMENT LENGTH               
         MVC   TAEDDST,8(R2)                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ADDRIN,DMCB,(3,SEMADDRH)            EMPLOYER ADDRESS             
         B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
EMPNTFND MVI   ERROR,EREMNFND                                                   
*                                                                               
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                 PFKEY TABLE                                   
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'EMPLOYER',CL8'LIST    '                               
PF13     DC    AL1(0,0),AL2(0)                                                  
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR1BD                                                       
         ORG   SEMWORK                                                          
SCANBLK  DS    CL50                SCANNER BLOCK                                
         EJECT                                                                  
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGEN1B   05/01/02'                                      
         END                                                                    
