*          DATA SET TAGEN1D    AT LEVEL 012 AS OF 06/27/12                      
*PHASE T7021DA                                                                  
         TITLE 'T7021D - ATTENTION MAINTENANCE'                                 
T7021D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7021D                                                         
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
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTABLE                                             
         BRAS  RE,NFYVIT           POSSIBLY NOTIFY VITA OF ACTION               
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   ATT10                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SATAGYH),SATAGYNH                     
         GOTO1 RECVAL,DMCB,TLATCDQ,(X'40',SATATTH)                              
         B     XIT                                                              
         SPACE 3                                                                
ATT10    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    ATT15                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    ATT15                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   ATT20                                                            
*                                                                               
ATT15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
ATT20    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   XIT                                                              
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*              DISPLAY THE KEY                                                  
*                                                                               
DK       MVC   SVKEY,KEY                                                        
         L     R4,AIO                                                           
         USING TLATD,R4                                                         
         MVC   SATAGY,TLATAGY                                                   
         OI    SATAGYH+6,X'80'                                                  
         MVC   AIO,AIO2            SWITCH IO AREA FOR RECVAL                    
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SATAGY),SATAGYNH                      
         MVC   SATATT,TLATATT                                                   
         OI    SATATTH+6,X'80'                                                  
         MVC   AIO,AIO1            RESTORE IO AREA                              
         MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         TWAXC SATNAMEH                                                         
         GOTO1 CHAROUT,DMCB,TANAELQ,(1,SATNAMEH)   BILLING NAME                 
         GOTO1 (RF),(R1),TAADELQ,(4,SATADDRH)      BILLING ADDRESS              
         GOTO1 (RF),(R1),TAFNELQ,(1,SATANAMH),TAFNTATT ATTN NAME                
         GOTO1 (RF),(R1),TACMELQ,(1,SATEMAIH),TACMTYPI EMAIL ADDRESS            
         GOTO1 ACTVOUT,DMCB,SATLCHGH               LAST CHANGED                 
         B     XIT                                                              
         SPACE 3                                                                
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    BR10                SAVE VITA-NOTIFYING MQ MESSAGE               
         GOTOR BLDMQMSG,DMCB,(X'80',0)     BASED ON INITIAL STATE               
                                                                                
BR10     GOTO1 NAMIN,DMCB,TANAELQ,(X'80',SATNAMEH) BILLING NAME                 
         GOTO1 ADDRIN,DMCB,(X'80',SATADDRH)        BILLING ADDRESS              
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SATANAMH),TAFNTATT                     
         BAS   RE,VREMAIL          VALIDATE OPTIONAL EMAIL ADDRESS              
         GOTO1 ACTVIN,DMCB,SATLCHGH                LAST CHANGED                 
         B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE OPTIONAL EMAIL ADDRESS                                               
*                                                                               
VREMAIL  NTR1                                                                   
         LA    R2,SATEMAIH                                                      
         CLI   5(R2),0                                                          
         BE    VREM40                                                           
         LA    RE,35                                                            
         LA    RF,SATEMAI                                                       
VREM20   CLI   0(RF),C'@'          THERE MUST BE A '@'                          
         BE    VREM30                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM20                                                        
         B     ERRINV                                                           
*                                                                               
         LA    RE,35                                                            
         LA    RF,SATEMAI                                                       
VREM30   CLI   0(RF),C'.'          THERE MUST BE A '.'                          
         BE    VREM40                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM30                                                        
         B     ERRINV                                                           
*                                                                               
VREM40   GOTO1 NAMIN,DMCB,TACMELQ,(X'C0',SATEMAIH),TACMTYPI                     
         B     XIT                                                              
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ERRORS                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'ATTN    ',CL8'LIST    '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'AGY     ',CL8'DISPLAY '                               
PF14X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING MQ MESSAGE                     *         
*        ON ENTRY ... P1 BYTE 0 = X'80' SAVE TO WSSVR BLOCK           *         
*                     AIO1      = A(ATTENTION RECORD)                 *         
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
*        ON ENTRY ... AIO1 = A(ATTENTION RECORD)                      *         
*                     R2   = A(RESERVED MQ MESSAGE BLOCK)             *         
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
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JE    BIMM10                                                           
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         JNE   BIMM20                                                           
BIMM10   MVC   IMROT,=C'<acpinsert>'                                            
         MVC   IMINS,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMMOD,BMQFALSE                                                   
         MVC   IMROX,=C'</acpinsert>'                                           
         J     BIMM30                                                           
                                                                                
BIMM20   MVC   IMROT,=C'<acpmodify>'                                            
         MVC   IMINS,BMQFALSE      IF ACTION IS CHANGE                          
         MVC   IMMOD,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMROX,=C'</acpmodify>'                                           
                                                                                
         USING TLATD,R4                                                         
BIMM30   L     R4,AIO1                                                          
         MVC   IMAGY,TLATAGY       COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'IMAGY,IMAGY)                                    
         MVC   IMCOD,TLATATT       COPY ATTENTION CODE INTO XML                 
         GOTO1 ELIMCHAR,DMCB,(L'IMCOD,IMCOD)                                    
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         MVI   ELCODE,TANAELQ      COPY BILL-TO NAME INTO XML                   
         BRAS  RE,GETEL                                                         
         BNE   BIMM40                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   IMNAM(0),TANANAME                                                
         J     BIMM50                                                           
                                                                                
         USING TAFND,R4                                                         
BIMM40   MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTATT))                                     
         BNE   BIMM50                                                           
         L     R4,TGELEM                                                        
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   IMNAM(0),TAFNNAME                                                
         DROP  R4                                                               
                                                                                
BIMM50   GOTO1 ELIMCHAR,DMCB,(L'IMNAM,IMNAM)                                    
         J     BMQYES                                                           
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING DELETE MESSAGE                 *         
*        ON ENTRY ... AIO1 = A(ATTENTION RECORD)                      *         
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
                                                                                
         MVC   DAGY,SATAGY         COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'DAGY,DAGY)                                      
         MVC   DCOD,SATATT         COPY ATTENTION CODE INTO XML                 
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
         DC    C'<agency isinsert="false" ismodify="false" code="'              
         DC    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'<billTo isinsert="'                                            
         DC    CL5' '                                                           
         DC    C'" ismodify="'                                                  
         DC    CL5' '                                                           
         DC    C'" code="'                                                      
         DC    CL2' '                                                           
         DC    C'" name="'                                                      
         DC    CL36' '                                                          
         DC    C'">'                                                            
         DC    C'</billTo>'                                                     
         DC    C'</agency>'                                                     
         DC    CL12' '                                                          
IMMLNQ   EQU   *-IMMSG                                                          
                                                                                
DMSG     DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    C'<acpdelete>'                                                   
         DC    C'<agency code="'                                                
         DC    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'<billTo isdelete="true" code="'                                
         DC    CL2' '                                                           
         DC    C'">'                                                            
         DC    C'</billTo>'                                                     
         DC    C'</agency>'                                                     
         DC    C'</acpdelete>'                                                  
DMLNQ    EQU   *-DMSG                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS MQ MESSAGE                                   *         
*        ON ENTRY ... AIO1 = A(ATTENTION RECORD)                      *         
***********************************************************************         
                                                                                
NFYVIT   NTR1  BASE=*,LABEL=*                                                   
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         JE    NV10                                                             
         CLI   MODE,XRECADD        OR ADDED RECORD                              
         JE    NV10                                                             
         CLI   MODE,XRECREST       OR RESTORED RECORD                           
         JE    NV10                                                             
         CLI   MODE,XRECDEL        OR DELETED RECORD                            
         JNE   XIT                 BUILD MQ MESSAGE BASED ON UPDATED            
NV10     GOTOR BLDMQMSG,DMCB,0     AGENCY RECORD INTO BLOCK                     
                                                                                
         USING FAWSSVRD,R1                                                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         JNE   NV20                                                             
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
                                                                                
NV20     GOTO1 NTFYVITA,DMCB,BLOCK,(R3),0                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR1DD                                                       
         ORG   SATWORK                                                          
*                                                                               
SVKEY    DS    CL38                                                             
*                                                                               
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING INSERT/UPDATE MESSAGES           *         
***********************************************************************         
                                                                                
IMMSGD   DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
IMROT    DS    CL11                                                             
         DS    CL48                                                             
IMAGY    DS    CL6                                                              
         DS    CL2                                                              
         DS    CL18                                                             
IMINS    DS    CL5                                                              
         DS    CL12                                                             
IMMOD    DS    CL5                                                              
         DS    CL8                                                              
IMCOD    DS    CL2                                                              
         DS    CL8                                                              
IMNAM    DS    CL36                                                             
         DS    CL2                                                              
         DS    CL9                                                              
         DS    CL9                                                              
IMROX    DS    CL12                                                             
                                                                                
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING DELETE MESSAGES                  *         
***********************************************************************         
                                                                                
DMSGD    DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
         DS    CL11                                                             
         DS    CL14                                                             
DAGY     DS    CL6                                                              
         DS    CL2                                                              
         DS    CL30                                                             
DCOD     DS    CL2                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012TAGEN1D   06/27/12'                                      
         END                                                                    
