*          DATA SET SPTRA57    AT LEVEL 003 AS OF 08/05/04                      
*PHASE T21657A                                                                  
         TITLE 'T21657 STUDIO RECORD DISPLAY,CHANGE,ADD,DELETE,LIST'            
*                                                                               
***********************************************************************         
*             PROGRAM - SPTRA57/ MAINT - SPTRA8E/ LIST - SPTRA9E                
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
*        R7 - WORK REG                                                          
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
*  LEV 03 SMUR 29JUL04 SOX                                            *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
T21657   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21657*                                                       
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
         CLI   MODE,VALREC         VALIDATE RECORD AFTER CHANGE SELECT          
         BE    ACTERR               ERROR, CANNOT CHANGE RECS                   
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BE    ACTERR                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
         SPACE                                                                  
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    ACTERR                                                           
         SPACE                                                                  
         LA    R2,TRAHSEH          HOUSE FIELD REQUIRED                         
         GOTO1 ANY                                                              
         MVC   SVHOUSE,WORK        SAVE HOUSE ID                                
         SPACE                                                                  
         XC    SVSTUDID,SVSTUDID   CLEAR STUDIO ID                              
         SPACE                                                                  
         LA    R2,TRASIDH          STUDIO CODE REQUIRED FOR DISPLAY             
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         CLI   ACTEQU,ACTLIST      NOT REQUIRED FOR LIST                        
         BE    VK10                YES                                          
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         MVC   SVSTUDID,WORK       SAVE STUDIO ID                               
         SPACE                                                                  
VK10     LA    R4,KEY              BUILD KEY                                    
         USING STUDKEYD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   STUDSYS,STUDSYSQ    RECORD CODE X'00'                            
         MVI   STUDTYP,STUDTYPQ    X'37'                                        
         MVC   STUDHSE,SVHOUSE     HOUSE ID                                     
         MVC   STUDID,SVSTUDID     STUDIO ID                                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       DS    0H                                                               
         SPACE                                                                  
         CLI   ACTEQU,ACTLIST      LIST ACTION?                                 
         BE    *+8                 YES, BYPASS                                  
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         SPACE                                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,STNAMELQ     ELEMENT CODE                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                REQUIRED ELEM                                
         SPACE                                                                  
         USING STNAMD,R6                                                        
         SPACE                                                                  
         MVC   TRANAME,STNAME      STATION NAME                                 
         OI    TRANAMEH+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
         MVC   TRAADDR,STADDR      ADDRESS LINE 1                               
         OI    TRAADDRH+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
         MVC   TRAADD2,STADD2      ADDRESS LINE 2                               
         OI    TRAADD2H+6,X'80'    TRANSMIT                                     
         DROP  R6                                                               
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE                                                                  
DK       DS    0H                                                               
         L     R4,AIO              SELECTED RECORD                              
         USING STUDKEYD,R4                                                      
         LA    R2,TRAHSEH                                                       
         MVC   8(L'TRAHSE,R2),STUDHSE DISPLAY HOUSE                             
         OI    6(R2),X'80'            TRANSMIT                                  
         SPACE                                                                  
         CLI   ACTEQU,ACTLIST      LIST ACTION?                                 
         BE    DKX                 YES                                          
         SPACE                                                                  
         LA    R2,TRASIDH                                                       
         MVC   8(L'TRASID,R2),STUDID DISPLAY STUDIO                             
         OI    6(R2),X'80'           TRANSMIT                                   
         SPACE                                                                  
DKX      B     EXIT                                                             
         EJECT                                                                  
************************                                                        
*  CLEAR SCREEN                                                                 
************************                                                        
         SPACE                                                                  
CLRSCRN  DS    0H                                                               
         SPACE                                                                  
         LA    R0,TRATAGH          LAST FIELD ON SCREEN                         
         LA    R2,TRANAMEH         FIRST FIELD HEADER                           
         SPACE                                                                  
CLR10    DS    0H                                                               
         ZIC   R1,0(R2)            LENGTH OF FIELD + HEADER                     
         LR    R3,R1                                                            
         SH    R1,=H'9'            MINUS HEADER AND 1 FOR EX                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         SPACE                                                                  
         OI    6(R2),X'80'         TRANSMIT                                     
CLR20    AR    R2,R3               BUMP TO THE NEXT FIELD                       
         CR    R2,R0               END OF SCREEN?                               
         BE    CLRX                                                             
         TM    1(R2),X'20'         NO, FIELD IS PROTECTED?                      
         BZ    CLR10               NO, CLEAR IT                                 
         ZIC   R1,0(R2)            LENGTH OF FIELD + HEADER                     
         LR    R3,R1                                                            
         B     CLR20               YES, BUMP TO NEXT FIELD                      
         SPACE                                                                  
CLRX     BR    RE                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       MVI   NLISTS,14           SET NUMBER OF LINES ON THE SCREEN            
         SPACE                                                                  
         LA    R4,KEY                                                           
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
         SPACE                                                                  
         MVI   STUDSYS,STUDSYSQ    SYSTEM X'00'                                 
         MVI   STUDTYP,STUDTYPQ    RECORD TYPE X'37'                            
         MVC   STUDHSE,SVHOUSE     HOUSE ID                                     
         MVC   STUDID,SVSTUDID     STUDIO ID                                    
         MVC   SAVEKEY,KEY                                                      
         SPACE                                                                  
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
         SPACE                                                                  
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
         SPACE                                                                  
LR30     CLC   KEY(2),SAVEKEY      SAME RECORD ID                               
         BNE   LRX                 NO, DONE                                     
         SPACE                                                                  
         CLC   STUDHSE,SVHOUSE     SAME HOUSE ID                                
         BNE   LRX                 NO, DONE                                     
         SPACE                                                                  
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         SPACE                                                                  
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSTUDID,STUDID                                                   
         SPACE                                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,STNAMELQ     ELEMENT CODE X'10'                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                ELEMENT IS REQUIRED                          
         USING STNAMD,R6                                                        
         MVC   LSTUNAME,STNAME     STUDIO NAME                                  
         MVC   LSTUADDR,STADDR     STUDIO ADDRESS                               
         DROP  R6                                                               
         SPACE                                                                  
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
         SPACE                                                                  
LRX      B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ACTERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         LA    R2,CONHEADH                                                      
         MVC   8(L'ACTMSG,R2),ACTMSG                                            
         GOTO1 ERREX2                                                           
ACTMSG   DC    C'*ERROR* NO ACTION ADD/DELETE/CHANGE, DISPLAY ONLY'             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE CTSTUDIOD                                                      
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA8ED                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         SPACE                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SVHOUSE  DS    CL(L'STUDHSE)       HOUSE ID                                     
SVSTUDID DS    CL(L'STUDID)        STUDIO CODE ID                               
SAVEKEY  DS    XL32                GENFILE KEY                                  
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL1                                                              
LSTUDID  DS    CL3                 STUDIO ID                                    
         DS    CL2                                                              
LSTUNAME DS    CL25                STUDIO NAME                                  
         DS    CL1                                                              
LSTUADDR DS    CL30                STUDIO ADDRESS                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPTRA57   08/05/04'                                      
         END                                                                    
