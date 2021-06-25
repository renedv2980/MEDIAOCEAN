*          DATA SET SPSFM27    AT LEVEL 016 AS OF 11/08/04                      
*PHASE T21727A                                                                  
*INCLUDE RECUP                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T21727 - STATION/CLIENT GROUP ASSIGNMENT DISPLAY      *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS DISPLAY ONLY                                 *         
*                                                                     *         
*  INPUTS       SCREEN T217C8 (STATION ASSIGNMENT DISPLAY)            *         
*               SCREEN T217C9 (CLIENT ASSIGNMENT DISPLAY)             *         
*                                                                     *         
*  OUTPUTS      NONE - DISPLAYS CLIENT/STATION GROUP ASSIGNMENTS      *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
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
*  I/O AREAS    IO1 - STATION/CLIENT GROUP RECORD                     *         
*                   - GROUP DEFINITION RECORD                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21727 - STATION/CLIENT GROUP ASSIGNMENT DISPLAY'               
T21727   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21727,RR=R3                                                   
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
CM       CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                  DISPLAY RECORDS                              
         DC    H'0'                ONLY MODES ALLOWED                           
*                                                                               
* VALIDATE KEY  * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VK       EQU   *                                                                
         LA    R2,SFSMEDH          MEDIA                                        
         GOTO1 VALIMED             VALIDATE THE MEDIA CODE                      
*                                                                               
         CLI   RECNUM,32           STATION REQUEST                              
         BNE   VKCCLI                                                           
         LA    R2,SFSSTNH          STATION                                      
         GOTO1 VALISTA             VALIDATE THE STATION CODE                    
         LA    R2,SFSSIDH          START ID                                     
         B     VKID                                                             
*                                                                               
VKCCLI   CLI   RECNUM,35           CLIENT REQUEST                               
         BE    *+6                                                              
         DC    H'0'                ONLY VALUES ALLOWED                          
         LA    R2,SFCCLIH          CLIENT                                       
         GOTO1 VALICLT             VALIDATE THE CLIENT CODE                     
*                                                                               
         LA    R2,SFCSIDH          START ID                                     
VKID     CLI   5(R2),0             ENTERED?                                     
         BNE   VKA                 YES                                          
         MVI   GRPID,0             DEFAULT TO BEGINNING                         
         B     VKX                 NO - DONE                                    
*                                                                               
VKA      MVI   ERROR,NOTALPHA                                                   
         TM    4(R2),X'04'         DATA ALPHABETIC?                             
         BZ    TRAPERR                                                          
         MVC   SAVECODE,8(R2)                                                   
         OI    SAVECODE+1,C' '     MAKE SURE 2 CHARS                            
*                                                                               
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
VKA2     CLC   SAVECODE,0(R1)      MATCH 2 CHAR CODE                            
         BE    VKA4                                                             
         AHI   R1,3                                                             
         BCT   R0,VKA2                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VKA4     MVC   GRPID,2(R1)                                                      
*                                                                               
VKX      B     XIT                                                              
*                                                                               
* VALIDATE RECORDS  * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VR       EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY              ---------------------                        
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVI   GRPPTYP,GRPPTYPQ                                                 
         MVC   GRPPAGMD,BAGYMD                                                  
         MVC   GRPPCODE,=X'0000'                                                
         MVC   GRPPVAL,SPACES                                                   
*                                                                               
         CLI   RECNUM,35           CLIENT GROUP?                                
         BNE   PSTA                                                             
         MVI   GRPPSTYP,GRPPCTYQ   CLIENT                                       
         MVC   GRPPID,GRPID                                                     
         MVC   GRPPVAL(3),QCLT                                                  
         LA    R2,SFCLINH          FIRST GROUP RECORD FIELD                     
         B     PSV                                                              
*                                                                               
PSTA     MVI   GRPPSTYP,GRPPSTYQ   STATION GROUP                                
         MVC   GRPPID,GRPID                                                     
         MVC   GRPPVAL(5),QSTA                                                  
         LA    R2,SFSLINH          FIRST GROUP RECORD FIELD                     
PSV      MVC   SAVEPKEY,GRPPKEY                                                 
         TWAXC (R2),PROT=Y         CLEAR SCREEN                                 
*                                                                               
         GOTO1 HIGH                GET FIRST PASSIVE POINTER                    
*                                                                               
VRLOOP   CLC   SAVEPKEY(9),GRPPKEY SAME CLIENT / STATION                        
         BNE   VRX1                NO - DONE                                    
         MVC   SAVEPKEY,GRPPKEY                                                 
         MVC   LISTAR,SPACES       LINE AREA                                    
*                                                                               
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
VRLOOP2  CLC   GRPPID,2(R1)        MATCH 1 CHAR CODE                            
         BE    VRLOOP4                                                          
         AHI   R1,3                                                             
         BCT   R0,VRLOOP2                                                       
         DC    H'0'                                                             
*                                                                               
VRLOOP4  MVC   LSTID,0(R1)         MOVE 2 CHARS TO DISPLAY AREA                 
*                                                                               
         MVC   SAVEID,GRPPID       SAVE 1 CHAR GROUP ID                         
         MVC   SAVECODE,GRPPCODE   XL2 PWOS                                     
         UNPK  DUB,GRPPCODE(3)     UNPACK 1 EXTRA CHAR                          
         MVC   CODECHAR(5),DUB+2                                                
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              --------------------------                   
         USING GRPKEY,R4           BUILD GROUP DEFINITION KEY                   
         MVI   GRPKTYP,GRPKTYPQ    BUILD KEY                                    
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,SAVEID       ID FROM PASSIVE PTR                          
         MVC   GRPKCODE,=X'0000'   GROUP DEF RECORD                             
         MVI   GRPKSTYP,GRPKCTYQ                                                
         CLI   RECNUM,35           CLISNT GROUP?                                
         BE    KSV                                                              
         MVI   GRPKSTYP,GRPKSTYQ   STATION GROUP                                
*                                                                               
KSV      MVC   SAVEKKEY,GRPKEY                                                  
*                                                                               
         GOTO1 HIGH                GET GROUP DEFINITION RECORD                  
*                                                                               
         CLC   SAVEKKEY(GRPKMSQL),GRPKEY                                        
         BE    *+6                 GET IT?                                      
         DC    H'0'                NO - VERY BAD NEWS                           
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
         MVI   GRPKTYP,GRPKTYPQ    BUILD KEY                                    
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,SAVEID       ID/CODE FROM PASSIVE POINTER                 
         MVC   GRPKCODE,SAVECODE   GROUP RECORD                                 
         MVI   GRPKSTYP,GRPKCTYQ                                                
         CLI   RECNUM,35           CLISNT GROUP?                                
         BE    KSV2                                                             
         MVI   GRPKSTYP,GRPKSTYQ   STATION GROUP                                
KSV2     MVC   SAVENKEY,GRPKEY                                                  
*                                                                               
         GOTO1 HIGH                GET GROUP RECORD                             
*                                                                               
         CLC   SAVENKEY(GRPKMSQL),GRPKEY                                        
         BE    *+6                 NOT THERE?                                   
         DC    H'0'                                                             
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
NMDN     MVC   8(79,R2),LISTAR     TO SCREEN LINE                               
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,SFSLSTH          LAST                                         
         CLI   RECNUM,32           STATION REQUEST                              
         BE    NDS                                                              
         LA    RF,SFCLSTH          LAST                                         
NDS      CR    R2,RF               END OF SCREEN?                               
         BH    VRX                 YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEPKEY    RESTORE KEY TO PASSIVE PTR                   
         GOTO1 HIGH                                                             
         CLC   SAVEPKEY(13),KEY                                                 
         BE    VRNXT                                                            
         DC    H'0'                                                             
*                                                                               
VRNXT    GOTO1 SEQ                 NEXT PASSIVE POINTER                         
         LA    R4,KEY                                                           
         B     VRLOOP                                                           
VRX1     LA    RF,SFSSIDH                                                       
         CLI   RECNUM,32           STATION REQUEST                              
         BE    *+8                                                              
         LA    RF,SFCSIDH                                                       
         XC    8(2,RF),8(RF)                                                    
         OI    6(RF),X'80'                                                      
VRX      B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
RELO     DS    F                                                                
         SPACE 5                                                                
XIT      XIT1                                                                   
*                                                                               
         LTORG                                                                  
       ++INCLUDE SPCGRTAB                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMC8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMC9D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
GRPID    DS    X                                                                
SAVEID   DS    X                                                                
SAVECODE DS    XL2                 GROUP CODE (PWOS)                            
CODECHAR DS    XL5                 UNPK AREA                                    
SAVEPKEY DS    XL13                                                             
SAVEKKEY DS    XL13                                                             
SAVENKEY DS    XL13                                                             
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
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
**PAN#1  DC    CL21'016SPSFM27   11/08/04'                                      
         END                                                                    
*                                                                               
