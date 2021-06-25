*          DATA SET TAGENFB    AT LEVEL 002 AS OF 06/27/12                      
*PHASE T702FBA,*                                                                
         TITLE 'T702FB - MEDIA MAINTENANCE AND LIST'                            
T702FB   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702FB                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+12                                                             
         BRAS  RE,VK                                                            
         J     XIT                                                              
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+12                                                             
         BRAS  RE,DK                                                            
         J     XIT                                                              
                                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         JNE   *+12                                                             
         BRAS  RE,LR                                                            
         J     XIT                                                              
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+12                                                             
         BRAS  RE,DR                                                            
         J     XIT                                                              
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   *+12                                                             
         BRAS  RE,VR                                                            
         J     XIT                                                              
                                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   *+12                                                             
         BRAS  RE,VR                                                            
         J     XIT                                                              
                                                                                
         CLI   MODE,RECREST        RESTORE RECORD                               
         JNE   *+12                                                             
         BRAS  RE,VR                                                            
         J     MODE10                                                           
                                                                                
         CLI   MODE,XRECADD        RECORD ADDED                                 
         JNE   *+12                                                             
         BRAS  RE,DR                                                            
         J     MODE10                                                           
                                                                                
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         JNE   *+12                                                             
         BRAS  RE,DR                                                            
         J     MODE10                                                           
                                                                                
         CLI   MODE,XRECDEL        RECORD DELETED                               
         JNE   *+12                                                             
         BRAS  RE,DR                                                            
         J     MODE10                                                           
                                                                                
         CLI   MODE,XRECREST       RECORD RESTORED                              
         JNE   XIT                                                              
         BRAS  RE,DR                                                            
                                                                                
MODE10   BRAS  RE,NFYVIT           NOTIFY VITA OF ACTION                        
                                                                                
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         MVI   TGMEEQU,INTERNET    SET GLOBAL MEDIA VARIABLE                    
         CLI   CONREC,C'I'                                                      
         BE    VK10                                                             
         MVI   TGMEEQU,NEWMEDIA                                                 
*                                                                               
VK10     CLI   ACTNUM,ACTLIST      IF LISTING RECORDS                           
         JNE   VK30                                                             
         CLC   TGMEEQU,SVMEEQU     ALWAYS VALIDATE FIRST TIME IN                
         JE    VK20                                                             
         NI    MELSTRTH+4,X'FF'-X'20'                                           
VK20     TM    MELSTRTH+4,X'20'    IF KEY ALREADY VALIDATED                     
         JO    VKX                 GO CONTINUE LIST                             
         OI    MELSTRTH+4,X'20'                                                 
         XC    LSTSTRT,LSTSTRT                                                  
         CLI   MELSTRTH+5,0        IF START IS ENTERED                          
         JE    VKX                 SAVE THE START CODE                          
         MVC   LSTSTRT,MELSTRT                                                  
         J     VKX                                                              
*                                                                               
VK30     LA    R2,MEDKEYH          IF NOT LISTING RECORDS                       
         CLI   MEDKEYH+5,0         JUST BUILD KEY                               
         JE    VKENTFLD                                                         
         CLC   MEDKEY,=X'C1D3D300' ALL IS NOT ALLOWED                           
         JE    VKFLDINV                                                         
         GOTO1 RECVAL,DMCB,TLMDCDQ,(X'40',(R2))                                 
*                                                                               
VKX      MVC   SVMEEQU,TGMEEQU     SAVE GLOBAL MEDIA VARIABLE                   
         J     XIT                                                              
         EJECT                                                                  
*              ERROR MESSAGES                                                   
*                                                                               
VKENTFLD MVI   MYMSGNO1,90                                                      
         OI    GENSTAT2,USGETTXT                                                
         J     VKEND                                                            
*                                                                               
VKFLDINV MVI   ERROR,INVALID                                                    
         J     VKEND                                                            
*                                                                               
VKEND    GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE KEY                                   *         
***********************************************************************         
                                                                                
DK       NTR1  BASE=*,LABEL=*                                                   
         USING TLMDD,R4                                                         
         L     R4,AIO                                                           
         MVC   MEDKEY,TLMDCODE                                                  
         OI    MEDKEYH+6,X'80'                                                  
*                                                                               
         MVC   TGMDCODE,MEDKEY     SET GLOBAL VARIABLES                         
*                                                                               
         MVI   TGMEEQU,INTERNET                                                 
         CLI   CONREC,C'I'                                                      
         JE    XIT                                                              
         MVI   TGMEEQU,NEWMEDIA                                                 
         J     XIT                                                              
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO LIST THE RECORDS                                  *         
***********************************************************************         
                                                                                
LR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(1,MELSELH),(X'40',999)                              
*                                                                               
         USING LISTD,R2                                                         
         LA    R2,LISTAR           R2=A(LIST LINE)                              
         MVC   LISTAR,SPACES                                                    
*                                                                               
         USING TLMDD,R4                                                         
         L     R4,AIO              R4=A(INTERNET/NEW MEDIA RECORD)              
*                                                                               
         GOTO1 RECVAL,DMCB,TLMDCDQ,(X'80',LSTSTRT)                              
         J     LR20                                                             
LR10     GOTO1 SEQ                                                              
LR20     CLC   KEY(TLMDCODE-TLMDD),KEYSAVE                                      
         JNE   LRNOMORE                                                         
LR30     GOTO1 GETREC                                                           
*                                                                               
         MVC   LSTSTRT,TLMDCODE                                                 
*                                                                               
         MVC   LISCODE,TLMDCODE                                                 
         GOTO1 CHAROUT,DMCB,TANAELQ,NAMEFLD                                     
         MVC   LISNAME,NAMEFLD+8                                                
         GOTO1 LISTMON                                                          
         J     LR10                                                             
         DROP  R2,R4                                                            
*                                                                               
LRNOMORE NI    MELSTRTH+4,X'FF'-X'20'                                           
         MVC   MYMSGNO,=H'262'                                                  
         J     LRXIT                                                            
*                                                                               
LRXIT    LA    R2,MELSTRTH                                                      
         MVI   MYMTYP,GTMINF                                                    
         MVI   BLOCK,0                                                          
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 CHAROUT,DMCB,TANAELQ,MEDNAMH                                     
         GOTO1 ACTVOUT,DMCB,MEDACTVH                                            
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         CLC   TGMDCODE,=C'N/A '                                                
         JE    VRINRACT                                                         
                                                                                
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    VR10                SAVE VITA-NOTIFYING MQ MESSAGE               
         GOTOR BLDMQMSG,DMCB,(X'80',0)     BASED ON INITIAL STATE               
                                                                                
VR10     GOTO1 NAMIN,DMCB,TANAELQ,MEDNAMH                                       
         GOTO1 ACTVIN,DMCB,0                                                    
         J     XIT                                                              
                                                                                
VRINRACT LA    R2,MEDKEYH                                                       
         MVI   ERROR,INVRCACT                                                   
         GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING MQ MESSAGE                     *         
*        ON ENTRY ... P1 BYTE 0 = X'80' SAVE TO WSSVR BLOCK           *         
*                     AIO1      = A(INTERNET/NEW MEDIA RECORD)        *         
***********************************************************************         
                                                                                
BLDMQMSG NTR1  BASE=*,LABEL=*                                                   
         MVC   TGBYTE,0(R1)        TGBYTE = WSSVR SAVE STATUS                   
                                                                                
         LA    R2,BLOCK            R2=A(MQ MESSAGE BLOCK)                       
         BAS   RE,BLDIMMSG         BUILD INSERT/MODIFY MESSAGE                  
         JE    BMQ10                                                            
         BAS   RE,BLDDMSG          OR BUILD DELETE MESSAGE                      
         JNE   XIT                 LENGTH RETURNED IN R3                        
                                                                                
         USING FAWSSVRD,R1                                                      
BMQ10    TM    TGBYTE,X'80'        IF SAVING TO WSSVR                           
         JZ    BMQX                DO SO NOW                                    
         LA    R1,WORK                                                          
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         ST    R2,FAWSADR                                                       
         STH   R3,FAWSLEN                                                       
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    BMQX                                                             
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
BMQYES   XR    RC,RC                                                            
BMQNO    LTR   RC,RC                                                            
BMQX     XIT1  REGS=(R3)                                                        
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING INSERT/MODIFY MESSAGE          *         
*        ON ENTRY ... AIO1 = A(LOCAL RECORD)                          *         
*                     R2   = A(MQ MESSAGE BLOCK)                      *         
***********************************************************************         
                                                                                
BLDIMMSG NTR1                                                                   
         CLI   MODE,XRECDEL        IF NOT DELETING RECORD                       
         JE    BMQNO                                                            
                                                                                
         USING IMMSGD,R2                                                        
         LHI   R3,IMMLNQ                                                        
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,IMMSG                                                         
         LR    R1,R3               COPY INSERT MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         MVC   IMREC,=C'internet'                                               
         CLI   CONREC,C'I'                                                      
         JE    BIMM10                                                           
         MVC   IMREC,=C'newmedia'                                               
                                                                                
BIMM10   MVC   IMREX,IMREC                                                      
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JE    BIMM20                                                           
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         JNE   BIMM30                                                           
BIMM20   MVC   IMROT,=C'<acpinsert>'                                            
         MVC   IMINS,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMMOD,BMQFALSE                                                   
         MVC   IMROX,=C'</acpinsert>'                                           
         J     BIMM40                                                           
                                                                                
BIMM30   MVC   IMROT,=C'<acpmodify>'                                            
         MVC   IMINS,BMQFALSE      IF ACTION IS CHANGE                          
         MVC   IMMOD,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMROX,=C'</acpmodify>'                                           
                                                                                
         USING TLMDD,R4                                                         
BIMM40   L     R4,AIO1                                                          
         MVC   IMCOD,TLMDCODE      COPY CODE INTO XML                           
         OC    IMCOD,SPACES                                                     
         GOTO1 ELIMCHAR,DMCB,(L'IMCOD,IMCOD)                                    
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              COPY NAME INTO XML                           
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   BMQYES                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   IMNAM(0),TANANAME                                                
         OC    IMNAM,SPACES                                                     
         GOTO1 ELIMCHAR,DMCB,(L'IMNAM,IMNAM)                                    
         J     BMQYES                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING DELETE MESSAGE                 *         
*        ON ENTRY ... AIO1 = A(UNION LOCAL RECORD)                    *         
*                     R2   = A(MQ MESSAGE BLOCK)                      *         
***********************************************************************         
                                                                                
BLDDMSG  NTR1                                                                   
         CLI   MODE,XRECDEL        IF DELETING RECORD                           
         JNE   BMQNO                                                            
                                                                                
         USING DMSGD,R2                                                         
         LHI   R3,DMLNQ                                                         
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,DMSG                                                          
         LR    R1,R3               COPY DELETE MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         MVC   DREC,=C'internet'                                                
         CLI   CONREC,C'I'                                                      
         JE    BDM10                                                            
         MVC   DREC,=C'newmedia'                                                
                                                                                
BDM10    MVC   DREX,DREC                                                        
                                                                                
         MVC   DCOD,MEDKEY         COPY CODE INTO XML                           
         GOTO1 ELIMCHAR,DMCB,(L'DCOD,DCOD)                                      
         J     BMQYES                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
BMQTRUE  DC    CL5'true'                                                        
BMQFALSE DC    CL5'false'                                                       
                                                                                
IMMSG    DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    CL11' '                                                          
         DC    C'<'                                                             
         DC    CL8' '                                                           
         DC    C' isinsert="'                                                   
         DC    CL5' '                                                           
         DC    C'" ismodify="'                                                  
         DC    CL5' '                                                           
         DC    C'" code="'                                                      
         DC    CL4' '                                                           
         DC    C'" name="'                                                      
         DC    CL36' '                                                          
         DC    C'">'                                                            
         DC    C'</'                                                            
         DC    CL8' '                                                           
         DC    C'>'                                                             
         DC    CL12' '                                                          
IMMLNQ   EQU   *-IMMSG                                                          
                                                                                
DMSG     DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    C'<acpdelete>'                                                   
         DC    C'<'                                                             
         DC    CL8' '                                                           
         DC    C' isdelete="true" '                                             
         DC    C'code="'                                                        
         DC    CL4' '                                                           
         DC    C'">'                                                            
         DC    C'</'                                                            
         DC    CL8' '                                                           
         DC    C'>'                                                             
         DC    C'</acpdelete>'                                                  
DMLNQ    EQU   *-DMSG                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS MQ MESSAGE                                   *         
*        ON ENTRY ... AIO1 = A(UNION LOCAL RECORD)                    *         
***********************************************************************         
                                                                                
NFYVIT   NTR1  BASE=*,LABEL=*                                                   
         GOTOR BLDMQMSG,DMCB,0     BUILD UPDATED MQ MESSAGE                     
                                                                                
         USING FAWSSVRD,R1                                                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         JNE   NV10                                                             
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSARST   RECALL INITIAL-STATE BASED                   
         MVC   FAWSADR,AIO3        MESSAGE INTO AIO3                            
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         LA    RE,BLOCK                                                         
         LR    RF,R3               IF CHANGE HAS BEEN MADE THAT                 
         L     R0,AIO3             WILL AFFECT THE MESSAGE                      
         LR    R1,R3               SEND THE UPDATED MESSAGE                     
         CLCL  RE,R0                                                            
         JE    XIT                                                              
                                                                                
NV10     GOTO1 NTFYVITA,DMCB,BLOCK,(R3),0                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR50D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR51D                                                       
         ORG   MELWORK                                                          
**********************************************************************          
* SAVED VARIABLES                                                    *          
**********************************************************************          
                                                                                
SVMEEQU  DS    XL(L'TGMEEQU)                                                    
LSTSTRT  DS    CL(L'MELSTRT)                                                    
NAMEFLD  DS    CL(L'MEDNAMH+L'MEDNAM)                                           
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**********************************************************************          
* DSECT TO COVER LIST LINE                                           *          
**********************************************************************          
                                                                                
LISTD    DSECT                                                                  
         DS    CL4                                                              
LISCODE  DS    CL4                                                              
         DS    CL6                                                              
LISNAME  DS    CL36                                                             
                                                                                
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING INSERT/UPDATE MESSAGES           *         
***********************************************************************         
                                                                                
IMMSGD   DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
IMROT    DS    CL11                                                             
         DS    CL1                                                              
IMREC    DS    CL8                                                              
         DS    CL11                                                             
IMINS    DS    CL5                                                              
         DS    CL12                                                             
IMMOD    DS    CL5                                                              
         DS    CL8                                                              
IMCOD    DS    CL4                                                              
         DS    CL8                                                              
IMNAM    DS    CL36                                                             
         DS    CL2                                                              
         DS    CL2                                                              
IMREX    DS    CL8                                                              
         DS    CL1                                                              
IMROX    DS    CL12                                                             
                                                                                
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING DELETE MESSAGES                  *         
***********************************************************************         
                                                                                
DMSGD    DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
         DS    CL11                                                             
         DS    CL1                                                              
DREC     DS    CL8                                                              
         DS    CL17                                                             
         DS    CL6                                                              
DCOD     DS    CL4                                                              
         DS    CL2                                                              
         DS    CL2                                                              
DREX     DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAGENFB   06/27/12'                                      
         END                                                                    
