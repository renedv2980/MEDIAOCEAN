*          DATA SET TAGENF5    AT LEVEL 002 AS OF 01/27/15                      
*PHASE T702F5E,*                                                                
         TITLE 'T702F5 - ACHMAP MAINTENANCE'                                    
T702F5   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702F5,R7,R6                                                   
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
                                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
                                                                                
         CLI   ACTNUM,ACTDEL                                                    
         BE    ERACT                                                            
         CLI   ACTNUM,ACTREST                                                   
         BE    ERACT                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         JNE   *+8                                                              
         BRAS  RE,DE                                                            
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   *+8                                                              
         BRAS  RE,VR                                                            
                                                                                
         CLI   MODE,XRECADD        RECORD ADDED                                 
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         JNE   XIT                                                              
         BRAS  RE,DR                                                            
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ERINV    MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
ERACT    LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT        INVALID ACTION                               
         J     END                                                              
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERNFD    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     END                                                              
                                                                                
ERACC    LHI   RE,ERACCACT         POSTING ACCOUNT DOES NOT EXIST               
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
INFEND   MVI   MYMTYP,GTMINF       INFORMATION MESSAGE EXIT                     
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         J     END                                                              
                                                                                
END      GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        PF KEY TABLE                                                 *         
***********************************************************************         
                                                                                
PFTAB    DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'40',ACHEMPH),(X'80',ACHTYPH)                      
         JE    XIT                                                              
                                                                                
         GOTO1 RECVAL,DMCB,TLEMCDQ,ACHEMPH                                      
                                                                                
         LA    R2,ACHTYPH          VALID TYPES ARE                              
         GOTO1 ANY                                                              
         MVC   ACHDESC,=CL40'DISABILITY'                                        
         OI    ACHDESCH+6,X'80'                                                 
         CLI   8(R2),C'D'          D                                            
         JE    VK10                                                             
         MVC   ACHDESC,=CL40'FEDERAL/FICA'                                      
         CLI   8(R2),C'F'          F                                            
         JE    VK10                                                             
         MVC   ACHDESC,=CL40'TAXES'                                             
         CLI   8(R2),C'T'          T                                            
         JE    VK10                                                             
         MVC   ACHDESC,=CL40'UNEMPLOYMENT'                                      
         CLI   8(R2),C'U'          AND U                                        
         JNE   ERINV                                                            
                                                                                
         USING TLSYD,R3                                                         
VK10     LA    R3,KEY              BUILD SYSTEM RECORD KEY                      
         XC    KEY,KEY                                                          
         MVI   TLSYCD,TLSYCDQ                                                   
         MVI   TLSYTYPE,TLSYACHM                                                
         MVC   TLSYAEMP,TGEMP                                                   
         OC    TLSYAEMP,SPACES                                                  
         DROP  R3                                                               
                                                                                
         GOTO1 FLDVAL,DMCB,(X'20',ACHEMPH),(X'80',ACHTYPH)                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ACHSIDH                                                       
         GOTO1 FLDVAL,DMCB,(3,(R2)),(X'80',ACHLFLDH)                            
                                                                                
         GOTO1 ACTVOUT,DMCB,ACHLCHGH                                            
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATIELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DR10     BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         CLC   TATITYPE,ACHTYP                                                  
         JNE   DR10                                                             
                                                                                
         MVC   8(L'TATIUNIT,R2),TATIUNIT                                        
                                                                                
         LA    R1,10(R2)                                                        
         CLI   0(R1),C' '                                                       
         JNH   *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),C'='                                                       
                                                                                
         MVC   1(L'TATIID,R1),TATIID                                            
         DROP  R4                                                               
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     DR10                                                             
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         XC    SAVEKEY,SAVEKEY                                                  
         L     R4,AIO                                                           
         MVC   SAVEKEY,0(R4)                                                    
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO              DELETE ALL ELEMENTS OF THIS TYPE             
         MVI   ELCODE,TATIELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VR10     BRAS  RE,NEXTEL                                                        
         JNE   VR20                                                             
         CLC   TATITYPE,ACHTYP                                                  
         JNE   VR10                                                             
         MVI   TATIEL,X'FF'                                                     
         J     VR10                                                             
                                                                                
VR20     MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
                                                                                
***********************************************************************         
                                                                                
         USING TATID,R4                                                         
VR30     LA    R4,ELEMENT                                                       
         MVI   TATIEL,TATIELQ                                                   
         MVI   TATILEN,TATILNQ                                                  
         MVC   TATITYPE,ACHTYP                                                  
                                                                                
         USING SCAND,R5                                                         
         LA    R5,SCANBLK                                                       
                                                                                
         LA    R2,ACHSIDH                                                       
VR40     CLI   5(R2),0                                                          
         JE    VR60                                                             
         GOTO1 SCANNER,DMCB,(L'TATIID,(R2)),(1,(R5))                            
                                                                                
         ZIC   R3,SCLEN1                                                        
         GOTO1 TAXVAL,DMCB,((R3),SCDATA1)                                       
         JNE   ERINV                                                            
         MVC   TATIUNIT,SPACES                                                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TATIUNIT(0),SCDATA1                                              
                                                                                
         MVI   ELCODE,TATIELQ                                                   
         GOTO1 GETL,DMCB,(4,TATITYPE)                                           
         JE    ERINV                                                            
                                                                                
         SR    R1,R1                                                            
         ICM   R1,1,SCLEN2                                                      
         JZ    ERINV                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   TATIID(0),SCDATA2                                                
                                                                                
         CLC   =C'SE',TATIID                                                    
         JNE   ERINV                                                            
                                                                                
         XC    WORK(10),WORK       CALL USERVAL WITH SIGN-ON USER ID TO         
         MVC   WORK+8(2),TWAORIG       GET SE NUMBER AND HEXCOMP                
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
                                                                                
         MVC   KEY,SPACES          BUILD RECEIVABLE ACCOUNT KEY                 
         MVC   KEY(1),TGACCHX      HEX COMPONENT                                
         MVC   KEY+1(12),TATIID    ACCOUNT NUMBER                               
         OC    KEY+1(12),SPACES    BUILD RECEIVABLE ACCOUNT KEY                 
                                                                                
         GOTO1 READACC,DMCB,0                                                   
         JNE   ERACC                                                            
                                                                                
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
                                                                                
         CLI   RECNUM,ACTADD                                                    
         JE    VR50                                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
VR50     MVC   AIO,AIO1            RESTORE AIO                                  
         GOTO1 ADDELEM                                                          
         DROP  R4,R5                                                            
                                                                                
VR60     ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
                                                                                
         LA    R3,ACHLFLDH                                                      
         CR    R2,R3                                                            
         JL    VR40                                                             
                                                                                
         GOTO1 ACTVIN,DMCB,ACHLCHGH                                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES COMMERCIAL/VERSION                           *         
***********************************************************************         
                                                                                
DE       NTR1  BASE=*,LABEL=*                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DE ROUTINE                                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
                                                                                
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR93D                                                       
                                                                                
         DS    D                                                                
SCANBLK  DS    CL50                SCANNER BLOCK                                
SAVEKEY  DS    XL32                                                             
                                                                                
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAGENF5   01/27/15'                                      
         END                                                                    
