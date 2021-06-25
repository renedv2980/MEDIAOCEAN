*          DATA SET SPTRA56    AT LEVEL 003 AS OF 08/05/04                      
*PHASE T21656A                                                                  
         TITLE 'T21656 STATION APPROVAL RECORD'                                 
*                                                                               
*                  (FOR STARCOM)                                                
***********************************************************************         
*             PROGRAM - SPTRA56 /MAINT - SPTRA5F /LIST - SPTRA5E                
*                                                                               
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 -                                                            
*             AIO3 -                                                            
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE                                                       
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
*                         CHANGE LOG                                  *         
*                                                                     *         
*  LEV   3 SMUR AUG04/04 SOX                                          *         
***********************************************************************         
         SPACE                                                                  
T21656   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21656*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         SPACE                                                                  
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    DELREC                                                           
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    DELREC                                                           
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
         SPACE                                                                  
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
         SPACE                                                                  
VK01     TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     CLI   ACTNUM,ACTDEL       DELETE IS INVALID                            
         BE    DELREC                                                           
         SPACE                                                                  
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         XC    NETWORK,NETWORK     NETWORK                                      
         SPACE                                                                  
         LA    R2,TRASTAH          STATION                                      
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK20                                                             
         SPACE                                                                  
VK10     GOTO1 ANY                                                              
         SPACE                                                                  
         BAS   RE,VNET                                                          
         SPACE                                                                  
*                                                                               
* BUILD THE KEY                                                                 
*                                                                               
VK20     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATRECD,R4         STATION RECORD                               
         MVC   STATKID,=X'29'      RECORD ID                                    
         MVC   STATKAM,BAGYMD      AGY/MED                                      
         MVC   STATKNET,NETWORK                                                 
         SPACE                                                                  
         MVC   MYSVKEY,KEY         SAVE THE KEY                                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE 2                                                                
VR       DS    0H                                                               
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    L     R4,AIO                                                           
         USING STATKEY,R4                                                       
         MVC   BAGYMD,STATKAM                                                   
         MVC   NETWORK,STATKNET    NETWORK                                      
         DROP  R4                                                               
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING STADATEL,R6                                                      
         SPACE                                                                  
         MVI   STADATEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   STADATLN,STADATEQ-STADATEL ELEMENT LENGTH                        
         SPACE                                                                  
         LA    R2,TRAADTH          ACTIVE DATE                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,TRAADT),WORK                                      
         SPACE                                                                  
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         SPACE                                                                  
         GOTO1 DATCON,(R1),(0,WORK),(3,STAADATE)                                
         SPACE                                                                  
         MVC   STAIDATE,=X'FFFFFF' INIT - NO INACTIVE DATE                      
         SPACE                                                                  
         LA    R2,TRAIDTH          INACTIVE DATE                                
         CLI   5(R2),0             ANY DATE                                     
         BE    VR10                 NO                                          
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,TRAIDT),WORK                                      
         SPACE                                                                  
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         SPACE                                                                  
         GOTO1 DATCON,(R1),(0,WORK),(3,IDATE)                                   
         SPACE                                                                  
         CLC   STAADATE,IDATE      COMPARE ACTIVE AND INACTIVE DATES            
         BNL   INVPER              INVALID PERIOD                               
         XC    IDATE,=X'FFFFFF'    INVERT INACTIVE DATE                         
         MVC   STAIDATE,IDATE                                                   
         SPACE                                                                  
VR10     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,STAMDATE) MAINT DATE (TODAY'S)              
         SPACE                                                                  
         L     R6,AIO                                                           
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    VR15                                                             
         SPACE                                                                  
         CLI   ACTNUM,ACTADD       ADD RECORD?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELEM+STASEQ-STADATEL,X'FF'  FIRST SEQUENCE NUMBER                
         B     VR20                                                             
         SPACE                                                                  
         USING STADATEL,R6                                                      
         SPACE                                                                  
VR15     ZIC   R1,STASEQ           ELEMENT SEQUENCE NUMBER                      
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                MORE THAN 255 ELEMENTS                       
         SPACE                                                                  
         STC   R1,ELEM+STASEQ-STADATEL                                          
         SPACE                                                                  
VR20     CLC   ELEM+STAIDATE-STADATEL(STADATEQ-STAIDATE),STAIDATE               
         BE    DR                   SAME ELEMENT DO NOT ADD                     
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BE    VR20                                                             
         SPACE                                                                  
         LA    R6,ELEM                                                          
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         CLI   ACTNUM,ACTADD       ADD RECORD?                                  
         BNE   DR                                                               
         SPACE                                                                  
* ASSIGN CODE EQUVALIENT TO THIS STATION                                        
         SPACE                                                                  
         BAS   RE,GCODE            GET LAST STATION CODE USED                   
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING STACODEL,R6                                                      
         SPACE                                                                  
         MVI   STACODEL,X'20'      STATION CODE ELEMENT                         
         MVI   STACODLN,STACODEQ-STACODEL ELEMENT LENGTH                        
         SPACE                                                                  
         OC    SVSTACDE,SVSTACDE   ANY STATION CODE FOUND                       
         BNZ   VR35                                                             
         OI    SVSTACDE,X'80'      NO, TURN ON X'80' IN FIRST BYTE              
         B     VR40                                                             
         SPACE                                                                  
VR35     LM    R0,R1,SVSTACDE      LAST STATION CODE USED                       
         SRDL  R0,1                                                             
         STM   R0,R1,SVSTACDE      SAVE STATION CODE FOR THIS STA               
         SPACE                                                                  
VR40     MVC   STACODE,SVSTACDE                                                 
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         B     DR                                                               
         SPACE                                                                  
INVPER   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPERMS),INVPERMS                                     
         B     ERREXIT                                                          
INVPERMS DC    C'* ERROR * INACTIVE DATE MUST BE AFTER ACTIVE DATE *'           
         SPACE                                                                  
DATERR   MVI   ERROR,INVDATE                                                    
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
DELREC   MVI   ERROR,INVACT        DELETE IS INVALID                            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         B     TRAPERR                                                          
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       L     R6,AIO                                                           
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         USING STADATEL,R6                                                      
         SPACE                                                                  
         XC    TRAADT,TRAADT                                                    
         OI    TRAADTH+6,X'80'     TRANSMIT                                     
         XC    TRAIDT,TRAIDT                                                    
         OI    TRAIDTH+6,X'80'     TRANSMIT                                     
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,STAADATE),(5,WORK)                                
         SPACE                                                                  
         MVC   TRAADT,WORK                                                      
         SPACE                                                                  
         CLC   STAIDATE,=X'FFFFFF' ANY INACTIVE DATE                            
         BE    DR20                 NO                                          
         SPACE                                                                  
         MVC   IDATE,STAIDATE                                                   
         XC    IDATE,=X'FFFFFF'                                                 
         GOTO1 DATCON,DMCB,(3,IDATE),(5,WORK)                                   
         SPACE                                                                  
         MVC   TRAIDT,WORK                                                      
         SPACE                                                                  
DR20     DS    0H                                                               
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,STAMDATE),(5,WORK)                                
         SPACE                                                                  
         MVC   TRAMDT,WORK         MAINTENANCE DATE                             
         OI    TRAMDTH+6,X'80'     TRANSMIT                                     
         SPACE                                                                  
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE                                                                  
DK       L     R4,AIO                                                           
         USING STATKEY,R4                                                       
*                                                                               
         XC    TRASTA,TRASTA                                                    
         MVC   TRASTA,STATKNET     DISPLAY NETWORK                              
         OI    TRASTAH+6,X'80'     SET ON TRANSMIT BIT                          
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST                                                                   
         SPACE                                                                  
LR       LA    R4,KEY                                                           
         USING STATKEY,R4                                                       
         OC    KEY(20),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                NO, GOTO HIGH                                
         SPACE                                                                  
         MVC   KEY,MYSVKEY         MOVE IN SAVED KEY                            
         SPACE                                                                  
* DO READHI                                                                     
         SPACE                                                                  
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   MYSVKEY(2),KEY      ANY RECS FOR THIS AGENCY/MEDIA               
         BNE   EXIT                                                             
         B     LR22                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         CLC   MYSVKEY(2),KEY      AT THE END OF THIS AGENCY                    
         BNE   EXIT                                                             
         SPACE                                                                  
LR22     OC    NETWORK,NETWORK     SEE IF NETWORK WAS ENTERED                   
         BZ    *+14                                                             
         CLC   NETWORK,STATKNET    IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
LR50     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         SPACE                                                                  
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
         SPACE                                                                  
LRL      LA    R5,LISTAR           ADDRESS OF WORK AREA                         
         MVC   LISTAR,SPACES                                                    
         USING LSTLINE,R5                                                       
         MVC   LNET,STATKNET       NETWORK                                      
         SPACE                                                                  
         MVI   ELCODE,X'10'        STATION DATA ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         USING STADATEL,R6                                                      
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,STAADATE),(5,WORK)                                
         SPACE                                                                  
         MVC   LADATE,WORK                                                      
         SPACE                                                                  
         CLC   STAIDATE,=X'FFFFFF' ANY INACTIVE DATE                            
         BE    LRL10                NO                                          
         SPACE                                                                  
         MVC   IDATE,STAIDATE      INACTIVE DATE                                
         XC    IDATE,=X'FFFFFF'                                                 
         GOTO1 DATCON,DMCB,(3,IDATE),(5,WORK)                                   
         SPACE                                                                  
         MVC   LIDATE,WORK         INACTIVE DATE                                
         SPACE                                                                  
LRL10    GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
         EJECT                                                                  
*                                                                               
* GET STATION CODE                                                              
*                                                                               
GCODE    NTR1                                                                   
         SPACE                                                                  
         XC    SVSTACDE,SVSTACDE   INIT STATION CODE                            
         SPACE                                                                  
         USING STATRECD,R4         STATION RECORD                               
         SPACE                                                                  
         MVC   BYTE,ELCODE         SAVE ELCODE VALUE                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   STATKID,=X'29'      RECORD ID                                    
         MVC   STATKAM,BAGYMD      AGY/MED                                      
         SPACE                                                                  
         GOTO1 HIGH                                                             
         SPACE                                                                  
GCODE10  CLC   KEY(2),KEYSAVE                                                   
         BNE   GCODEX                                                           
         SPACE                                                                  
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVI   ELCODE,X'20'                                                     
         SPACE                                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         USING STACODEL,R6                                                      
         SPACE                                                                  
         OC    SVSTACDE,SVSTACDE                                                
         BZ    GCODE15                                                          
         SPACE                                                                  
         CLC   SVSTACDE,STACODE    SAVE LOWER BIT OF THE TWO                    
         BL    *+10                                                             
GCODE15  MVC   SVSTACDE,STACODE                                                 
         SPACE                                                                  
         GOTO1 SEQ                                                              
         B     GCODE10                                                          
GCODEX   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   ELCODE,BYTE                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE NETWORK                                                              
*                                                                               
VNET     NTR1                                                                   
         USING STARECD,R4          LOOK UP NETWORK IN STATION RECORD            
*                                                                               
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVI   STAKTYPE,C'S'       STATION RECORD TYPE                          
         MVI   STAKMED,C'N'        MEDIA NETWORK                                
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     R4,AIO                                                           
         CLC   0(9,R4),KEY         TEST NETWORK IS ON FILE                      
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,BNET             SAVE NETWORK MARKET NUMBER                   
*                                                                               
         B     EXIT                                                             
         SPACE                                                                  
         DROP  R4                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
NETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERRMS),NETERRMS                                     
         B     ERREXIT                                                          
NETERRMS DC    C'* ERROR * NETWORK NOT FOUND *'                                 
         SPACE                                                                  
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE                                                                  
*ONLINE LIST LINE                                                               
         SPACE                                                                  
LSTLINE  DSECT                                                                  
LNET     DS    CL4                                                              
         DS    CL9                                                              
LADATE   DS    CL8                                                              
         DS    CL4                                                              
LIDATE   DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE SPTRNSTA                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA5FD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         SPACE                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
FLDH     DS    XL8                                                              
FLD      DS    CL64                                                             
MYSVKEY  DS    CL48                                                             
NETWORK  DS    CL4                                                              
BNET     DS    H                                                                
IDATE    DS    CL3                                                              
SVSTACDE DS    D                   SAVE LAST STATION CODE USED                  
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPTRA56   08/05/04'                                      
         END                                                                    
