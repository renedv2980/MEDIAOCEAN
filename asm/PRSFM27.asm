*          DATA SET PRSFM27    AT LEVEL 010 AS OF 01/10/05                      
*PHASE T41C27C                                                                  
*                                                                               
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMYE  04/04   MODIFY FOR 2-CHARACTER CLIENT GROUP ID                          
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T41C27 - PUB/CLIENT GROUP ASSIGNMENT DISPLAY          *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS DISPLAY ONLY                                 *         
*                                                                     *         
*  INPUTS       SCREEN T41CB7 (PUB ASSIGNMENT DISPLAY)                *         
*               SCREEN T41CB3 (CLIENT ASSIGNMENT DISPLAY)             *         
*                                                                     *         
*  OUTPUTS      NONE - DISPLAYS CLIENT/PUB GROUP ASSIGNMENTS          *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- SVSPARE                                         *         
*               R4 -- WORK                                            *         
*               R5 -- MINBLKD                                         *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - PUB/CLIENT GROUP RECORD                         *         
*                   - GROUP DEFINITION RECORD                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T41C27 - PUB/CLIENT GROUP ASSIGNMENT DISPLAY'                   
T41C27   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C27,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         OI    SFSMEDH+1,X'01'     SET MODIFIED BIT EVERY TIME                  
         CLI   ACTNUM,ACTDIS       ONLY ACTION DISPLAY ALLOWED                  
         BE    CM                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
CM       DS    0H                                                               
*                                                                               
         LH    R3,=Y(SVSPARE-T41CFFD)                                           
         AR    R3,RA               POINT TO TWA0 END FOR SAVES                  
         USING SVSPARED,R3                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                  DISPLAY RECORDS                              
         DC    H'0'                ONLY MODES ALLOWED                           
*                                                                               
* VALIDATE KEY  * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VK       EQU   *                                                                
*                                                                               
         TM    CONRECH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    CONKEYH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    SFSMEDH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    SFSPUBH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    SFSSIDH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    SFCCLIH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    SFCSIDH+4,X'80'     INPUT THIS TIME ?                            
         BNO   VK02                YES - CLEAR LIST START FIELDS                
*                                                                               
VK01     DS    0H                                                               
         XC    SORTLAST,SORTLAST   CLEAR LIST START FIELD AND                   
         XC    DISPON,DISPON       DISPLAY IN PROCESS SWITCH                    
*                                                                               
         LA    R0,SORTDATA         CLEAR THIS AREA FOR STORING ALL              
         LHI   R1,SORTDATX-SORTDATA  MEDIA/ID CODES TO BE SORTED                
         SR    RE,RE                 IN VALREC                                  
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VK02     DS    0H                                                               
         LA    R2,SFSMEDH          MEDIA                                        
         GOTO1 VALIMED             VALIDATE THE MEDIA CODE                      
*                                                                               
         CLI   RECNUM,41           PUB REQUEST                                  
         BNE   VKCCLI                                                           
         LA    R2,SFSPUBH          PUB                                          
         GOTO1 VALIPUB             VALIDATE THE PUB CODE                        
         LA    R2,SFSSIDH          START ID                                     
         B     VKID                                                             
*                                                                               
VKCCLI   CLI   RECNUM,35           CLIENT REQUEST                               
         BE    *+6                                                              
         DC    H'0'                ONLY VALUES ALLOWED                          
         LA    R2,SFCCLIH          CLIENT                                       
         GOTO1 VALICLT             VALIDATE THE CLIENT CODE                     
         LA    R2,SFCSIDH          START ID                                     
*                                                                               
VKID     CLI   8(R2),C' '          ENTERED?                                     
         BH    VKA                 YES                                          
         MVC   STARTID,SPACES      CLEAR                                        
         MVC   8(2,R2),SPACES      DEFAULT TO BEGINNING                         
         OI    6(R2),X'80'                                                      
         B     VKX                 NO - DONE                                    
VKA      TM    4(R2),X'04'         DATA ALPHABETIC?                             
         BO    VKAX                YES - OK                                     
         MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
VKAX     DS    0H                                                               
         CLI   RECNUM,35           CLIENT REQUEST ?                             
         BE    VKAX10              YES                                          
         CLI   5(R2),1             NO - PUB REQUEST                             
         BNE   BADGRPID            ONLY 1-CHARACTER ALLOWED                     
* TRANSLATE 2 CHAR CODE TO ONE CHAR                                             
VKAX10   MVC   WORK(2),8(R2)                                                    
         OI    WORK+1,C' '                                                      
         LA    RE,SPCGRTAB                                                      
         LHI   RF,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
VK22     CLC   WORK(2),0(RE)                                                    
         BE    VK24                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,VK22                                                          
         B     BADGRPID                                                         
*                                                                               
VK24     MVC   STARTID,0(RE)        SET 2 BYTE START ID                         
*                                                                               
VKX      B     XIT                                                              
*                                                                               
* VALIDATE RECORDS  * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VR       DS    0H                                                               
*                                                                               
         OC    SORTLAST,SORTLAST   ANYTHING THERE ?                             
         BNZ   LR30                YES - CONTINUE FROM TABLE                    
*                                                                               
         LA    R0,SORTDATA         PREP CLIENT ID SORT TABLE                    
         LHI   R1,SORTDATX-SORTDATA     R5 IS POINTER FOR                       
         SR    RE,RE                    THIS TABLE                              
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   SORTLAST,0                                                       
         LA    R5,SORTDATA                                                      
*                                  SELECT REQUIRED IDS FOR SORT                 
         XC    KEY,KEY                                                          
         LA    R4,KEY              ---------------------                        
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,AGENCY                                                   
         MVC   GRPPMED,QMED                                                     
         MVC   GRPPCODE,=X'0000'                                                
         MVC   GRPPVAL,SPACES                                                   
*                                                                               
         CLI   RECNUM,35           CLIENT GROUP?                                
         BNE   SPSTA                                                            
         MVI   GRPPTYP,GRPPCGQ     CLIENT                                       
         MVC   GRPPVAL(3),QCLT                                                  
         LA    R2,SFCLINH          FIRST CLIENT GROUP RECORD FIELD              
         B     SPSV                                                             
*                                                                               
SPSTA    MVI   GRPPTYP,GRPPBGQ     PUB GROUP                                    
         MVC   NEWID,SFSSID                                                     
         MVC   GRPPVAL(6),BPUB                                                  
         LA    R2,SFSLINH          FIRST PUB GROUP RECORD FIELD                 
SPSV     DS    0H                                                               
         MVI   GRPPID,0            START AT BEGINNING                           
*                                                                               
         TWAXC (R2),PROT=Y         CLEAR SCREEN                                 
*                                                                               
SVRLOOP  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GET PASSIVE POINTER                          
         CLC   KEY(13),KEYSAVE     SAME CLIENT / PUB                            
         BNE   SVREND              NO - DONE - GO SORT                          
*                                                                               
* TRANSLATE 1 CHAR CODE TO 2 CHARS                                              
         LA    RE,SPCGRTAB                                                      
         LHI   RF,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
SVR20    CLC   GRPPID,2(RE)                                                     
         BE    SVR30                                                            
         LA    RE,3(RE)                                                         
         BCT   RF,SVR20                                                         
         DC    H'0'                                                             
*                                                                               
SVR30    DS    0H                                                               
         CLC   0(2,RE),STARTID     COMPARE TO START VALUE                       
         BL    SVR40                                                            
         MVC   0(3,R5),0(RE)       SAVE BOTH CODES IN SORTDATA                  
         AHI   R5,3                                                             
*                                                                               
SVR40    DS    0H                                                               
         MVI   KEY+14,X'FF'        FORCE NEXT GROUP                             
         B     SVRLOOP             GET NEXT PASSIVE POINTER                     
*                                                                               
SVREND   DS    0H                                                               
*                                                                               
LR20     LR    R1,R5                                                            
         LA    R0,SORTDATA                                                      
         SR    R1,R0               GIVES LENGTH USED                            
         BZ    VRX1                NOTHING SAVED                                
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LR    R0,R1                                                            
*                                                                               
         GOTO1 QSORT,DMCB,SORTDATA,(R0),3,2,0                                   
         XC    SORTLAST,SORTLAST   CLEAR LAST ID PROCESSED AND                  
         MVI   DISPON,C' '         CLEAR DISPLAY-IN-PROCESS SWITCH              
         EJECT                                                                  
*=============================================================                  
* NOW DISPLAY DATA FROM SORTED LIST                                             
*=============================================================                  
                                                                                
LR30     LA    R5,SORTDATA         FIND LAST ENTRY DISPLAYED                    
         LHI   R0,(SORTDATX-SORTDATA)/3  LOOP PREVENTION                        
         CLI   SORTLAST,0                                                       
         BE    LR34                                                             
*                                                                               
LR32     CLC   SORTLAST,2(R5)                                                   
         BE    LR34                                                             
         AHI   R5,3                                                             
         BCT   R0,LR32                                                          
         DC    H'0'                                                             
*                                                                               
LR34     MVC   SORTLAST,2(R5)      SET START POINT                              
         MVC   BYTEID,2(R5)         FOR KEYS                                    
         MVC   NEWID,0(R5)          FOR DISPLAY                                 
*                                                                               
         OC    0(3,R5),0(R5)       TEST MORE DATA                               
         BZ    VRX1                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ---------------------                        
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,AGENCY                                                   
         MVC   GRPPMED,QMED                                                     
         MVC   GRPPCODE,=X'0000'                                                
         MVC   GRPPVAL,SPACES                                                   
*                                                                               
         CLI   RECNUM,35           CLIENT GROUP?                                
         BNE   PSTA                                                             
         MVI   GRPPTYP,GRPPCGQ     CLIENT                                       
         MVC   GRPPID,BYTEID       FROM SORTDATA                                
         MVC   GRPPVAL(3),QCLT                                                  
         CLI   DISPON,C'Y'         DISPLAY STARTED ?                            
         BE    PSVHI               YES - SKIP CLEAR & LEAVE R2 ALONE            
         LA    R2,SFCLINH          FIRST GROUP RECORD FIELD                     
         B     PSV                                                              
*                                                                               
PSTA     MVI   GRPPTYP,GRPPBGQ     PUB GROUP                                    
         MVC   GRPPID,BYTEID       FROM SORTDATA                                
         MVC   GRPPVAL(6),BPUB                                                  
         CLI   DISPON,C'Y'         DISPLAY STARTED ?                            
         BE    PSVHI               YES - SKIP CLEAR & LEAVE R2 ALONE            
         LA    R2,SFSLINH          FIRST GROUP RECORD FIELD                     
*                                                                               
PSV      TWAXC (R2),PROT=Y         CLEAR SCREEN                                 
*                                                                               
PSVHI    DS    0H                                                               
         MVC   SAVEPKEY,GRPPKEY                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GET FIRST PASSIVE POINTER                    
*                                                                               
*                                                                               
VRLOOP   CLC   SAVEPKEY(13),GRPPKEY SAME CLIENT / PUB                           
         BNE   VRX1                NO - DONE                                    
         MVC   SAVEPKEY,GRPPKEY                                                 
         MVC   LISTAR,SPACES       LINE AREA                                    
*                                                                               
         LA    R1,SFSSIDH                                                       
         CLI   RECNUM,35           CLIENT REQUEST                               
         BNE   *+8                                                              
         LA    R1,SFCSIDH                                                       
         MVC   8(2,R1),NEWID       FROM SORTDATA                                
         OI    6(R1),X'80'                                                      
*                                                                               
         MVC   LSTID,NEWID         GROUP ID/CODE FROM SORTDATA                  
         MVC   SAVECODE,GRPPCODE   XL2 PWOS                                     
         ICM   R7,B'1100',GRPPCODE        FROM PASSIVE PTR                      
         SRL   R7,12                DD DD ?? ??  =>  00 0D DD D?                
         ST    R7,FULL                                                          
         OI    FULL+3,X'0F'         00 0D DD DS                                 
         UNPK  CODECHAR(5),FULL+1(3)             =>  Z0 ZD ZD ZD ZD             
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              --------------------------                   
         USING GRPKEY,R4           BUILD GROUP DEFINITION KEY                   
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKMED,QMED                                                     
         MVC   GRPKID,BYTEID       ID FROM SORTDATA                             
         MVC   GRPKCODE,=X'0000'   GROUP DEF RECORD                             
         MVI   GRPKRCOD,GRPKCTYQ                                                
         CLI   RECNUM,35           CLIENT GROUP?                                
         BE    KSV                                                              
         MVI   GRPKRCOD,GRPKBTYQ   PUB GROUP                                    
KSV      MVC   SAVEKKEY,GRPKEY                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GET GROUP DEFINITION RECORD                  
*                                                                               
         CLC   SAVEKKEY(10),GRPKEY                                              
         BE    *+6                 GET IT?                                      
         DC    H'0'                NO - VERY BAD NEWS                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO              GROUP DEFINITION RECORD                      
         LA    R6,GRPEL            A(FIRST ELEMENT)                             
         USING GRPBRKD,R6                                                       
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R7,GRPBK1LN         L'BREAK CODES                                
         ZIC   R0,GRPBK2LN                                                      
         AR    R7,R0               L'WHOLE GROUP CODE                           
         BCTR  R7,0                                                             
         EX    R7,C2L                                                           
         B     BKS                                                              
C2L      MVC   LSTCODE(0),CODECHAR+1 CODE TO SCREEN LINE BLANK PADDED           
*                                                                               
BKS      MVC   LSTBRK1(12),GRPBK1  BREAK TITLES TO SCREEN                       
         OC    GRPBK2,GRPBK2       MAY BE ONLY ONE                              
         BZ    BKDN                                                             
         MVC   LSTBRK2(12),GRPBK2                                               
         DROP  R4                                                               
*                                                                               
BKDN     XC    KEY,KEY                                                          
         LA    R4,KEY              ----------------------                       
         USING GRPKEY,R4           BUILD GROUP RECORD KEY                       
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKMED,QMED                                                     
         MVC   GRPKID,BYTEID       ID/CODE FROM SORTDATA                        
         MVC   GRPKCODE,SAVECODE   GROUP RECORD                                 
         MVI   GRPKRCOD,GRPKCTYQ                                                
         CLI   RECNUM,35           CLIENT GROUP?                                
         BE    KSV2                                                             
         MVI   GRPKRCOD,GRPKBTYQ   PUB GROUP                                    
KSV2     MVC   SAVENKEY,GRPKEY                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GET GROUP RECORD                             
*                                                                               
         CLC   SAVENKEY(10),GRPKEY                                              
         BE    *+6                 NOT THERE?                                   
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO              GROUP RECORD                                 
         LA    R6,GRPEL            A(FIRST ELEMENT)                             
         USING GRPGRPD,R6                                                       
         MVI   ELCODE,GRPGRPCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   LSTNAME1(20),GRPGNAM1  GROUP NAMES TO SCREEN                     
         OC    GRPGNAM2,GRPGNAM2                                                
         BZ    NMDN                                                             
         MVC   LSTNAME2(20),GRPGNAM2                                            
         DROP  R4                                                               
*                                                                               
NMDN     DS    0H                                                               
         MVI   DISPON,C'Y'         DISPLAY IS STARTED                           
         MVC   8(79,R2),LISTAR     TO SCREEN LINE                               
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,SFSLSTH          LAST                                         
         CLI   RECNUM,41           PUB REQUEST                                  
         BE    NDS                                                              
         LA    RF,SFCLSTH          LAST                                         
NDS      CR    R2,RF               END OF SCREEN?                               
         BH    VRX                 YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVEPKEY    RESTORE KEY TO PASSIVE PTR                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SAVEPKEY(25),KEY                                                 
         BE    VRNXT                                                            
         DC    H'0'                                                             
*                                                                               
VRNXT    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 NEXT PASSIVE POINTER                         
         LA    R4,KEY                                                           
         CLC   KEY+13(1),KEYSAVE+13   SAME ID ?                                 
         BE    VRLOOP              YES - NEXT PASSIVE                           
         AHI   R5,3                                                             
         B     LR34                NO - TEST NEXT SORTDATA ENTRY                
*                                                                               
VRX1     LA    RF,SFSSIDH                                                       
         CLI   RECNUM,41           PUB REQUEST                                  
         BE    *+8                                                              
         LA    RF,SFCSIDH                                                       
         MVC   8(2,RF),SPACES      BACK TO BEGINNING                            
         OI    6(RF),X'80'                                                      
VRX      DS    0H                                                               
         MVI   DISPON,C' '         CLEAR DISPLAY-IN-PROCESS SWITCH              
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
RELO     DS    F                                                                
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
BADGRPID DS    0H                                                               
         LA    R2,SFSSIDH                                                       
         CLI   RECNUM,35           CLIENT REQUEST                               
         BNE   *+8                                                              
         LA    R2,SFCSIDH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADGRPM),BADGRPM                                       
         B     GOERREX2                                                         
BADGRPM  DC    C'* ERROR * START ID IS NOT VALID'                               
         SPACE 2                                                                
GOERREX2 GOTO1 ERREX2                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPCGRTAB                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PRSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMB3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMB7D                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
* PROGRAM SAVED STORAGE AT BOTTOM OF TWA0                                       
*                                                                               
         ORG   T41CFFD+TWAENDLQ    ORG TO BOTTOM OF TWA0                        
*                                                                               
STSAVE   EQU   *                                                                
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
SVSPARE  DS    CL(TWAMXLEN-(*-STSAVE))  SPARE - DEFINE NEW AREAS ABOVE          
*                                                                               
SVSPARED DSECT                     DATA SAVED IN TWA                            
*                                                                               
SORTLAST DS    XL1                 LAST  GRPID DISPLAYED                        
*                                                                               
SORTDATA DS    XL768               MAX 256 3 BYTE CODES                         
SORTDATX EQU   *                   ALPHA CODE(2)/GRPID(1)                       
*                                                                               
SVSPAREL EQU   *-SVSPARED                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PRSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SAVECODE DS    XL2                 GROUP CODE (PWOS)                            
CODECHAR DS    XL5                 UNPK AREA                                    
SAVEPKEY DS    XL32                                                             
SAVEKKEY DS    XL32                                                             
SAVENKEY DS    XL32                                                             
BYTEID   DS    XL1                 1-CHARACTER GROUP ID                         
NEWID    DS    CL2                 2-CHARACTER GROUP ID                         
DISPON   DS    CL1                 "Y" = DISPLAY IN PROCESS                     
STARTID  DS    XL2                 2-CHARACTER START GROUP ID                   
         EJECT                                                                  
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
*                                                                               
GEND     DSECT                   SCREEN LINE WORK AREA                          
         ORG   LISTAR             (LISTMON NOT USED)                            
LSTID    DS    CL2                                                              
         DS    CL1                                                              
LSTCODE  DS    CL4                                                              
         DS    CL2                                                              
LSTBRK1  DS    CL12                                                             
         DS    CL2                                                              
LSTNAME1 DS    CL20                                                             
         DS    CL2                                                              
LSTBRK2  DS    CL12                                                             
         DS    CL2                                                              
LSTNAME2 DS    CL20                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PRSFM27   01/10/05'                                      
         END                                                                    
*                                                                               
