*          DATA SET PRSFM2A    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T41C2AA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T41C2A - PRODUCT GROUP ASSIGNMENT DISPLAY             *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS DISPLAY ONLY                                 *         
*                                                                     *         
*  INPUTS       SCREEN T41CB5 (PRODUCT ASSIGNMENT DISPLAY)            *         
*                                                                     *         
*  OUTPUTS      NONE - DISPLAYS PRODUCT GROUP ASSIGNMENTS             *         
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
*  I/O AREAS    IO1 - PRODUCT GROUP RECORD                            *         
*                   - GROUP DEFINITION RECORD                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T41C2A - PRODUCT GROUP ASSIGNMENT DISPLAY'                      
T41C2A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C2A,RR=R3                                                   
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
         OI    SFCMEDH+1,X'01'     SET MODIFIED BIT EVERY TIME                  
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
         LA    R2,SFCMEDH          MEDIA                                        
         GOTO1 VALIMED             VALIDATE THE MEDIA CODE                      
*                                                                               
         LA    R2,SFCCLIH                                                       
         GOTO1 VALICLT                                                          
         LA    R2,SFCPRDH          PRODUCT                                      
         GOTO1 VALIPRD             VALIDATE THE PRODUCT                         
*                                                                               
VKID     LA    R2,SFCSIDH          START ID                                     
         CLI   5(R2),0             ENTERED?                                     
         BNE   VKA                 YES                                          
         MVI   8(R2),C' '          DEFAULT TO BEGINNING                         
         OI    6(R1),X'80'                                                      
         B     VKX                 NO - DONE                                    
VKA      TM    4(R2),X'04'         DATA ALPHABETIC?                             
         BO    *+12                YES - DONE                                   
         MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
VKX      B     XIT                                                              
*                                                                               
* VALIDATE RECORDS  * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VR       EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY              ---------------------                        
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,AGENCY                                                   
         MVC   GRPPMED,QMED                                                     
         MVC   GRPPCLT,QCLT                                                     
         MVC   GRPPCODE,=X'0000'                                                
         MVC   GRPPVAL,SPACES                                                   
*                                                                               
         MVI   GRPPTYP,GRPPPGQ                                                  
         MVC   GRPPID,SFCSID                                                    
         MVC   GRPPVAL(3),QPRD                                                  
         LA    R2,SFCLINH          FIRST GROUP RECORD FIELD                     
*                                                                               
PSV      MVC   SAVEPKEY,GRPPKEY                                                 
         TWAXC (R2),PROT=Y         CLEAR SCREEN                                 
*                                                                               
         GOTO1 HIGH                GET FIRST PASSIVE POINTER                    
*                                                                               
VRLOOP   CLC   SAVEPKEY(13),GRPPKEY SAME PRODUCT                                
         BNE   VRX1                NO - DONE                                    
         MVC   SAVEPKEY,GRPPKEY                                                 
         MVC   LISTAR,SPACES       LINE AREA                                    
*                                                                               
         LA    R1,SFCSIDH                                                       
         MVC   8(1,R1),GRPPID                                                   
         OI    6(R1),X'80'                                                      
*                                                                               
         MVC   LSTID,GRPPID        GROUP ID/CODE FROM PASSIVE PTR               
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
         MVC   GRPKCLT,QCLT                                                     
         MVC   GRPKID,LSTID        ID FROM PASSIVE PTR                          
         MVC   GRPKCODE,=X'0000'   GROUP DEF RECORD                             
         MVI   GRPKRCOD,GRPKPTYQ                                                
KSV      MVC   SAVEKKEY,GRPKEY                                                  
*                                                                               
         GOTO1 HIGH                GET GROUP DEFINITION RECORD                  
*                                                                               
         CLC   SAVEKKEY(10),GRPKEY                                              
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
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKMED,QMED                                                     
         MVC   GRPKCLT,QCLT                                                     
         MVC   GRPKID,LSTID        ID/CODE FROM PASSIVE POINTER                 
         MVC   GRPKCODE,SAVECODE   GROUP RECORD                                 
         MVI   GRPKRCOD,GRPKPTYQ                                                
KSV2     MVC   SAVENKEY,GRPKEY                                                  
*                                                                               
         GOTO1 HIGH                GET GROUP RECORD                             
*                                                                               
         CLC   SAVENKEY(10),GRPKEY                                              
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
         LA    RF,SFCLSTH          LAST                                         
NDS      CR    R2,RF               END OF SCREEN?                               
         BH    VRX                 YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVEPKEY    RESTORE KEY TO PASSIVE PTR                   
         GOTO1 HIGH                                                             
         CLC   SAVEPKEY(25),KEY                                                 
         BE    VRNXT                                                            
         DC    H'0'                                                             
*                                                                               
VRNXT    GOTO1 SEQ                 NEXT PASSIVE POINTER                         
         LA    R4,KEY                                                           
         B     VRLOOP                                                           
VRX1     LA    RF,SFCSIDH                                                       
         MVI   8(RF),C' '          BACK TO BEGINNING                            
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
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PRSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMB5D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
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
         EJECT                                                                  
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
*                                                                               
GEND     DSECT                   SCREEN LINE WORK AREA                          
         ORG   LISTAR             (LISTMON NOT USED)                            
LSTID    DS    CL1                                                              
         DS    CL2                                                              
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
**PAN#1  DC    CL21'011PRSFM2A   05/01/02'                                      
         END                                                                    
*                                                                               
