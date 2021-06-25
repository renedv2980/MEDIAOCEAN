*          DATA SET CTSFM37    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA0A37A,*                                                                
*                                                                               
* *********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM37 -- STUDIO CODE RECORDS MAINT/LIST/REPORT     *         
*                                                                     *         
*  COMMENTS:     MAINTAINS STUDIO RECORDS ON GENDIR/GENFIL            *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMAF (MAINTENANCE)                        *         
*                        CTSFM9F (LIST)                               *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- KEY                                            *         
*                R5 -- WORK (NOT USED)                                *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A37 - STUDIO CODE RECORD MAINT/LIST'                         
TA0A37   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A37**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TA0AFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         SPACE                                                                  
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
XIT      XIT1                                                                   
         EJECT                                                                  
************************                                                        
*      VALIDATE KEY                                                             
************************                                                        
         SPACE                                                                  
VK       DS    0H                                                               
         SPACE                                                                  
         XC    SVHOUSE,SVHOUSE                                                  
         XC    SVSTUDID,SVSTUDID                                                
         SPACE                                                                  
         CLI   ACTEQU,ACTREP       ACTION REPORT                                
         BE    VK10                YES, JUST BUILD THE KEY                      
         SPACE                                                                  
         LA    R2,STUHSEH          HOUSE FIELD REQUIRED                         
         GOTO1 ANY                                                              
         MVC   SVHOUSE,WORK        SAVE HOUSE ID                                
         SPACE                                                                  
         CLI   ACTEQU,ACTLIST      ACTION LIST                                  
         BE    VK10                YES                                          
         SPACE                                                                  
         LA    R2,STUSIDH          STUDIO CODE REQUIRED FOR DISPLAY             
         GOTO1 ANY                                                              
         MVC   SVSTUDID,WORK       SAVE STUDIO ID                               
         SPACE                                                                  
VK10     LA    R4,KEY              BUILD KEY                                    
         USING STUDKEYD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   STUDSYS,STUDSYSQ    RECORD CODE X'00'                            
         MVI   STUDTYP,STUDTYPQ    X'37'                                        
         MVC   STUDHSE,SVHOUSE     HOUSE ID                                     
         MVC   STUDID,SVSTUDID     STUDIO ID                                    
         B     XIT                                                              
         SPACE                                                                  
         DROP  R4                                                               
         EJECT                                                                  
***********************                                                         
*     VALIDATE RECORD                                                           
***********************                                                         
         SPACE                                                                  
VR       DS    0H                                                               
         L     R4,AIO              BUILD RECORD                                 
         USING STUDKEYD,R4                                                      
         MVC   SVHOUSE,STUHSE      SAVE HOUSE ID FOR LATER                      
         MVC   SVSTUDID,STUDID     AND THE STUDIO ID                            
         SPACE                                                                  
         MVI   ELCODE,STNAMELQ     ELEMENT X'10'                                
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         SPACE                                                                  
         LA    R6,ELEM                                                          
         USING STNAMD,R6                                                        
         SPACE                                                                  
         MVI   STNAMEL,STNAMELQ    ELEMENT CODE                                 
         MVI   STNAMLN,STNAMLNQ    ELEMENT LENGTH                               
         SPACE                                                                  
         LA    R2,STUNAMEH         STUDIO FULL NAME                             
         GOTO1 ANY                                                              
         MVC   STNAME,WORK                                                      
         SPACE                                                                  
         LA    R2,STUADDRH         STUDIO ADDRESS LINE 1                        
         GOTO1 ANY                                                              
         MVC   STADDR,WORK                                                      
         SPACE                                                                  
         LA    R2,STUADD2H         STUDIO ADDRESS LINE 2                        
         GOTO1 ANY                                                              
         MVC   STADD2,WORK                                                      
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         SPACE                                                                  
         B     DR                  DISPLAY THE RECORD                           
         DROP  R4                                                               
         EJECT                                                                  
**********************                                                          
*     DISPLAY RECORD                                                            
**********************                                                          
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
         MVC   STUNAME,STNAME      STATION NAME                                 
         OI    STUNAMEH+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
         MVC   STUADDR,STADDR      ADDRESS LINE 1                               
         OI    STUADDRH+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
         MVC   STUADD2,STADD2      ADDRESS LINE 2                               
         OI    STUADD2H+6,X'80'    TRANSMIT                                     
         DROP  R6                                                               
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
*************************                                                       
*      DISPLAY KEY                                                              
*************************                                                       
         SPACE                                                                  
DK       L     R4,AIO              SELECTED RECORD                              
         USING STUDKEYD,R4                                                      
         LA    R2,STUHSEH                                                       
         MVC   8(L'STUHSE,R2),STUDHSE DISPLAY HOUSE                             
         OI    6(R2),X'80'            TRANSMIT                                  
         SPACE                                                                  
         CLI   ACTEQU,ACTLIST      LIST ACTION?                                 
         BE    DKX                 YES                                          
         SPACE                                                                  
         LA    R2,STUSIDH                                                       
         MVC   8(L'STUSID,R2),STUDID DISPLAY STUDIO                             
         OI    6(R2),X'80'           TRANSMIT                                   
         SPACE                                                                  
DKX      B     XIT                                                              
         EJECT                                                                  
************************                                                        
*  CLEAR SCREEN                                                                 
************************                                                        
         SPACE                                                                  
CLRSCRN  DS    0H                                                               
         SPACE                                                                  
         LA    R0,STUTAGH          LAST FIELD ON SCREEN                         
         LA    R2,STUNAMEH         FIRST FIELD HEADER                           
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
***********************                                                         
*   ON-SCREEN LIST                                                              
***********************                                                         
         SPACE                                                                  
LR       LA    R4,KEY                                                           
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
         SPACE                                                                  
         MVI   STUDSYS,STUDSYSQ    SYSTEM X'00'                                 
         MVI   STUDTYP,STUDTYPQ    RECORD TYPE X'37'                            
         MVC   STUDHSE,SVHOUSE     HOUSE ID                                     
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
LRX      B     XIT                                                              
         EJECT                                                                  
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
RELO     DS    F                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMAFD                                                                      
* CTSFM9FD                                                                      
* CTMSTAFFD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMAFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM9FD                                                       
         EJECT                                                                  
       ++INCLUDE CTSTUDIOD                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE                                                        
*                                                                               
         ORG   SYSSPARE                                                         
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
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTSFM37   05/01/02'                                      
         END                                                                    
