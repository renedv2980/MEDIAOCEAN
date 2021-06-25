*          DATA SET SPSFM4D    AT LEVEL 052 AS OF 01/08/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE T2174DA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2174A  -- PRODUCT GROUP DEF. MAINTENANCE            *         
*                                                                     *         
*  COMMENTS:     MAINTAINS PDT GRP DEF RECS ON SPTFILE                *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFME4, AND SPSFME2                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED PDT GRP DEF RECS                             *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2174D - PRODUCT GROUP DEF. MAINTENANCE'                        
T2174D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**174D**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
         CLI   MODE,XRECPUT                                                     
         BE    XR                                                               
         CLI   MODE,XRECADD                                                     
         BE    XR                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                 LIST RECORDS                                 
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         B     LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         MVI   AGYX,0                 CLEAR AGENCY W/ NO MED                    
         MVI   AGYHI,0                 CLEAR AGENCY W/ NO MED                   
         XC    SVBCLT,SVBCLT                                                    
         XC    BCLT,BCLT                                                        
         MVC   MEDIA,SPACES                                                     
         MVC   CLIENT,SPACES                                                    
         MVC   PGRPID,SPACES                                                    
         XC    SAVESEL,SAVESEL                                                  
*                                                                               
         MVC   PDLMDN,SPACES           CLEAR ADDRESS FIELDS                     
         OI    PDLMDNH+6,X'80'                                                  
         MVC   PDLCLN,SPACES                                                    
         OI    PDLCLNH+6,X'80'                                                  
*                                                                               
         CLI   ACTEQU,ACTLIST           LIST?                                   
         BNE   VK50                                                             
*                                                                               
* VALIDATE KEY FOR ACTION LIST                                                  
         LA    R2,PDLMEDH               VALIDATE MEDIA                          
         CLI   5(R2),0                  PRESENT?                                
         BE    VK20                     NO, BUILD AGENCY NIBBLE                 
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
VK10     CLC   PDLMED,0(R4)                                                     
         BE    VK15                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   VK10                                                             
         B     ERRINV                                                           
*                                                                               
VK15     MVC   MEDIA,0(R4)                                                      
         CLI   SVAPROF+7,C'C'           CANADIAN?                               
         BE    VK16                     YES, ALL MEDIA OK                       
         CLI   8(R2),C'N'               MEDIA N?                                
         BE    ERRINV                   YES,ERROR                               
         CLI   8(R2),C'C'               NO, MEDIA C?                            
         BE    ERRINV                   YES, ERROR                              
*                                                                               
VK16     GOTO1 VALIMED                  FOR VALICLI CALL                        
         MVC   PDLMDN,MEDNM            DISPLAY MEDIA NAME                       
         OI    PDLMDNH+6,X'80'                                                  
         MVC   AGYHI,BAGYMD                                                     
         OI    AGYHI,X'0F'              HIGH MARKER FOR NEXT AGENCY             
         B     VK25                                                             
*                                                                               
VK20     XC    TEMPFLD,TEMPFLD           FOR BLANK MEDIA                        
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
         MVC   AGYHI,AGYX                                                       
         OI    AGYHI,X'0F'              HIGH MARKER FOR NEXT AGENCY             
*                                                                               
VK25     LA    R2,PDLCLTH               VALIDATE CLIENT                         
         CLI   5(R2),0                                                          
         BE    VK30                     NO CLIENT, BUILD KEY                    
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
         MVC   PDMCLN,CLTNM            DISPLAY CLIENT NAME                      
         OI    PDMCLNH+6,X'80'                                                  
** EXCEPTION FOR CLIENT POL ??? ***                                             
         MVC   CLIENT,8(R2)                                                     
         OC    CLIENT,SPACES                                                    
*                                                                               
VK30     LA    R2,PDLGIDH               VALIDATE PRODUCT GROUP ID               
         CLI   5(R2),0                                                          
         BE    VK100                                                            
**       CLI   8(R2),C'V'                                                       
**       BL    ERRINV                                                           
**       CLI   8(R2),C'Z'                                                       
**       BH    ERRINV                                                           
         CLI   8(R2),C'A'                                                       
         BL    ERRINV                                                           
         CLI   8(R2),C'Z'                                                       
         BH    ERRINV                                                           
         CLI   8(R2),C'N'                                                       
         BE    ERRINV                                                           
         MVC   PGRPID,8(R2)                                                     
         B     VK100                                                            
*                                                                               
VK50     DS    0H             VALIDATE FOR DISPLAY/ADD/CHANGE                   
         LA    R2,PDMMEDH               VALIDATE MEDIA                          
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
VK60     CLC   PDMMED,0(R4)                                                     
         BE    VK65                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   VK60                                                             
         B     ERRINV                                                           
*                                                                               
VK65     MVC   MEDIA,0(R4)                                                      
         CLI   SVAPROF+7,C'C'                                                   
         BNE   VK66                                                             
         CLI   8(R2),C'N'               MEDIA N?                                
         BE    *+12                     YES                                     
         CLI   8(R2),C'C'               NO, MEDIA C?                            
         BNE   *+12                     NO, ALL ACTIONS VALID                   
         CLI   ACTEQU,ACTDIS            ONLY DISPLAY VALID FOR C AND N          
         BNE   ERRINV                                                           
*                                                                               
VK66     GOTO1 VALIMED                                                          
         MVC   PDMMDN,MEDNM            DISPLAY MEDIA NAME                       
         OI    PDMMDNH+6,X'80'                                                  
*                                                                               
         LA    R2,PDMCLTH               VALIDATE CLIENT                         
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
         MVC   PDMCLN,CLTNM            DISPLAY CLIENT NAME                      
         OI    PDMCLNH+6,X'80'                                                  
** EXCEPTION FOR CLIENT POL ??? ***                                             
         MVC   CLIENT,8(R2)                                                     
         OC    CLIENT,SPACES                                                    
*                                                                               
         LA    R2,PDMGIDH               VALIDATE PRODUCT GROUP ID               
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
**       CLI   8(R2),C'V'                                                       
**       BL    ERRINV                                                           
**       CLI   8(R2),C'Z'                                                       
**       BH    ERRINV                                                           
         CLI   8(R2),C'A'                                                       
         BL    ERRINV                                                           
         CLI   8(R2),C'Z'                                                       
         BH    ERRINV                                                           
         CLI   8(R2),C'N'                                                       
         BE    ERRINV                                                           
         MVC   PGRPID,8(R2)                                                     
*                                                                               
         LA    R2,PDMGSTH               VALIDATE PGROUP START AT                
         LA    R3,8(R2)                 POINT R3 TO FIRST DISPLAY BYTE          
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         XC    GRPSTART,GRPSTART                                                
         B     VK100                                                            
         TM    4(R2),X'08'              VALID NUMERIC?                          
         BO    VK80                                                             
*                                                                               
* NOT ALL NUMERIC INPUT - VALIDATE FOR CORRECT FIRST CHARACTER                  
*                                                                               
         CLC   8(1,R2),PGRPID                                                   
         BNE   ERRINV                                                           
*                                                                               
         CLI   5(R2),1                  CHECK FOR GROUP # INPUT                 
         BE    ERRMIS                                                           
*                                                                               
* VALIDATE REMAINING INPUT FOR NUMERIC                                          
         LA    R3,9(R2)                 POINT TO FIRST DIGIT                    
         LA    RE,3                                                             
*                                                                               
VK70     DS    0H                                                               
         CLI   0(R3),0                  WHEN NO INPUT FOR NEXT BYTE             
         BE    VK75                     DON'T LOOK AT OTHER BYTES               
         CLI   0(R3),C'0'                                                       
         BL    ERRINV                                                           
         CLI   0(R3),C'9'                                                       
         BH    ERRINV                                                           
         LA    R3,1(R3)                 POINT TO NEXT BYTE                      
         BCT   RE,VK70                                                          
*                                                                               
VK75     LLC   R1,5(R2)                 GET LENGTH OF GROUP INPUT               
         BCTR  R1,0                     DECR. ONCE FOR GROUP ID                 
         LA    R3,9(R2)                 POINT TO 1ST INPUT DIGIT                
         B     VK85                                                             
*                                                                               
VK80     LLC   R1,5(R2)                 GET LENGTH OF GROUP # INPUT             
VK85     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(R3)                                                     
         PACK  GRPSTART(2),DUB(3)                                               
         NI    GRPSTART+1,X'FF'-X'0F'     TURN OFF LOW NIBBLE = SIGN            
*                                                                               
VK100    DS    0H                                                               
*                                                                               
         LA    R4,KEY              BUILD PRODUCT GROUP DEF. KEY                 
         USING PRGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'                                                 
*                                                                               
         CLI   AGYX,0                   LIST W/ NO MED INPUT?                   
         BE    *+14                     NO, USE AGYMED CODE                     
         MVC   PRGKAGMD,AGYX            YES, USE AGY W/ NO MED                  
         B     *+10                                                             
*                                                                               
         MVC   PRGKAGMD,BAGYMD          NOT LIST, BUILD AGENCY/MED              
         MVC   PRGKCLT,BCLT             BINARY CLIENT CODE                      
         MVC   PRGKID,PGRPID                                                    
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         DROP  R4                                                               
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         LA    R2,PDMMEDH                                                       
         CLI   SVAPROF+7,C'C'                                                   
         BNE   VR10                                                             
         CLI   8(R2),C'N'               MEDIA N?                                
         BE    *+12                     YES                                     
         CLI   8(R2),C'C'               NO, MEDIA C?                            
         BNE   *+12                     NO, ALL ACTIONS VALID                   
         CLI   ACTEQU,ACTDIS            ONLY DISPLAY VALID FOR C AND N          
         BNE   ERRINV                                                           
*                                                                               
VR10     CLI   ACTEQU,ACTADD                                                    
         BE    VR15                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'             BREAK DESC. ELEM                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PRGEL01,R6                                                       
         MVC   SVPGB1,PRGBK1            SAVE OLD DATA                           
         MVC   SVPGBL1,PRGBK1LN                                                 
         MVC   SVPGB2,PRGBK2                                                    
         MVC   SVPGBL2,PRGBK2LN                                                 
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM                                                          
         B     VR15                                                             
*                                                                               
VR15     LA    R5,ELEM                                                          
         USING PRGEL01,R5                                                       
         XC    ELEM,ELEM                                                        
         MVI   0(R5),X'01'                                                      
         MVI   1(R5),41                                                         
*                                                                               
         LA    R4,SVPGB1                POINT TO OLD BREAK TITLE                
         LA    R3,PRGBK1                POINT TO NEW BREAK TITLE                
         LA    R2,PDMBK1H                                                       
         XC    BYTE2,BYTE2              USE FOR BREAK LENGTH TOTAL              
         CLI   5(R2),0                  MUST BE 1ST BK ON ADD                   
         BE    ERRMIS                                                           
         BAS   RE,CHKBKLN               CHECK BREAK TITLE AND LENGTHS           
*                                                                               
         LA    R4,SVPGB2                                                        
         LA    R3,PRGBK2                POINT TO NEW BREAK TITLE                
         LA    R2,PDMBK2H                                                       
         BAS   RE,CHKBKLN               CHECK BREAK TITLE AND LENGTHS           
         DROP  R5                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR50     DS    0H                                                               
         MVI   ELCODE,PRGSCMCQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PRGSCMD,R6                                                       
         MVI   PRGSCMCD,PRGSCMCQ   X'12' SECURITY MANAGER ID ELEMENT            
         MVI   PRGSCMLN,PRGSCKLQ                                                
         LA    R5,PRGSCM                                                        
****  WE WILL FILL THE PRGSCM'S ONE BY ONE A BIT LATER                          
*                                                                               
         LA    R2,PDMSCM1H                                                      
         LA    R3,PDMSCM6H                                                      
*                                                                               
VR60     DS    0H                                                               
         CR    R2,R3                                                            
         BH    VR80                WE'RE DONE                                   
*                                                                               
         LR    RE,R2                                                            
         TM    1(RE),X'02'         DO WE HAVE AN EXTENDED FIELD HEADER?         
         BZ    SCMNEXT                                                          
         XR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         SHI   RE,8                POINT TO START OF EXTENDED HEADER            
         CLI   0(RE),111           IS IT ONE OF THE PID FIELDS?                 
         BNE   SCMNEXT                                                          
         CLI   5(R2),0             ANYTHING HERE?                               
         BE    SCMNEXT              - NOPE, MOVE TO THE NEXT SCMFIELD           
         MVC   PIDNAME,8(R2)                                                    
         OC    PIDNAME,=C'        '                                             
*                                                                               
         BRAS  RE,CHECKPID         WE NEED TO CHECK ALL THE PIDS                
         BNE   ERRPID                                                           
         MVC   0(L'PRGSCM,R5),PIDNUM   SAVE OFF THE PID NUMBER                  
         LA    R5,L'PRGSCM(R5)                                                  
*                                                                               
SCMNEXT  XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR60                                                             
*                                                                               
VR80     GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'01'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING PRGEL01,R6                                                       
         MVC   PDMBK1,PRGBK1       DISPLAY BREAK TITLES                         
         OI    PDMBK1H+6,X'80'                                                  
         MVC   PDMBK2,PRGBK2       DISPLAY BREAK TITLES                         
         OI    PDMBK2H+6,X'80'                                                  
*                                                                               
         EDIT  PRGBK1LN,PDMLN1          DISPLAY BREAK LENGTH                    
         OI    PDMLN1H+6,X'80'                                                  
         OI    PDMLN1H+4,X'20'          VALID'D BEFORE, FOR ACTCHA              
         EDIT  PRGBK2LN,PDMLN2          DISPLAY BREAK LENGTH                    
         OI    PDMLN2H+6,X'80'                                                  
         OI    PDMLN2H+4,X'20'                                                  
*                                                                               
         MVC   SVBKLNS+0(1),PRGBK1LN                                            
         MVC   SVBKLNS+1(1),PRGBK2LN                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PRGBK1LN                                                      
         SR    RE,RE                                                            
         IC    RE,PRGBK2LN                                                      
         AR    R0,RE                                                            
         STC   R0,BYTE2            SAVE TOTAL DIGITS                            
         DROP  R6                                                               
*                                                                               
         BAS   RE,CLRSCR                CLEAR SCREEN                            
*                                                                               
         LA    R2,PDMLISTH                                                      
         LA    R3,8(R2)                                                         
         LA    R4,KEY                                                           
         USING PRGRECD,R4                                                       
         MVC   KEY(13),SAVEKEY                                                  
         MVC   AIO,AIO2                                                         
         OC    GRPSTART,GRPSTART        PGROUP START AT PRESENT?                
         BZ    DR9                      NO DISPLAY GROUPS NORMALLY              
         MVC   PRGKGRP,GRPSTART                                                 
         GOTO1 HIGH                                                             
         B     DR15                                                             
*                                                                               
DR9      GOTO1 HIGH                                                             
DR10     GOTO1 SEQ                                                              
DR15     CLC   KEY(PRGKGRP-PRGKEY),KEYSAVE                                      
         BNE   DR50                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING PRGEL10,R6                                                       
*                                                                               
         MVC   0(1,R3),PRGKID                                                   
         UNPK  DUB,PRGKGRP(3)                                                   
         IC    RE,BYTE2            GET TOTAL DIGITS                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),DUB+3 ** EXECUTED **                                     
         MVC   6(24,R3),PRGNAM1                                                 
         CLI   SVBKLNS+1,0                                                      
         BE    DR20                                                             
         MVC   6(24,R3),PRGNAM2                                                 
         DROP  R4,R6                                                            
*                                                                               
DR20     DS    0H                                                               
         OI    6(R2),X'80'                                                      
         LA    R0,44(R2)                BEGIN OF SECOND GROUP                   
         CR    R3,R0                    DOES R3 POINT AT 2ND GROUP?             
         BE    DR25                     2ND GROUP PRINTED, BUMP R2...           
         LA    R3,36(R3)                POINT R3 TO SECOND GROUP                
         B     DR10                                                             
*                                                                               
DR25     BAS   RE,NXTSCRF               .... TO NEXT SCREEN FIELD               
         LA    R0,PDMDONEH              END OF SCREEN?                          
**       LA    R0,PDMLAST               END OF SCREEN?                          
         CR    R2,R0                                                            
         BE    DR50                     YES, DONE                               
         LA    R3,8(R2)                 NO, POINT TO FIRST GROUP ID             
         B     DR10                                                             
*                                                                               
DR50     DS    0H                                                               
         LA    R2,PDMSCM1H                                                      
         LA    R1,PDMSCM6H                                                      
*                                                                               
DR50G    CR    R2,R1               DONE WITH THE FIELDS?                        
         BH    DR55                 - YUP, WE'RE DONE                           
         TM    1(R2),X'20'         ARE WE PROTECTED?                            
         BNZ   DR50NX               - YES, DON'T TOUCH                          
         XC    8(8,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         RETRANSMIT                                   
*                                                                               
DR50NX   XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR50G                                                            
*                                                                               
*****      NEW SECURITY MANAGER ID ELEMENT      MHC  05/02/06                   
DR55     MVC   AIO,AIO1            MOVE THE AIO BACK, PUNK!!   MHC              
         BRAS  RE,SECMGREL                                                      
*                                                                               
         L     R6,AIO              *** THIS IS THE ENTRY FOR XREC ***           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BRAS  RE,GETEL            MUST BE THERE                                
         BNE   DR100               END IT IF NOT THERE, FOR NOW                 
*        BE    *+6                                                              
*        DC    H'0'                                                             
         USING ACTVD,R6                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,PDMCDTE)   ADDED DATE                
         OI    PDMCDTEH+6,X'80'                                                 
*                                                                               
         OC    ACTVCHDT,ACTVCHDT   HAS RECORD BEEN CHANGED BEFORE?              
         BZ    DR60                 NO, SKIP NEXT DATCON CALL                   
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,PDMADTE)   CHANGED DATE              
         OI    PDMADTEH+6,X'80'                                                 
*                                                                               
DR60     GOTO1 SECMGRID,DMCB,PDMCRTRH,ACTVADID                                  
*                                                                               
         GOTO1 SECMGRID,DMCB,PDMCWHOH,ACTVCHID                                  
***********************************************************************         
DR100    CLI   ACTEQU,ACTSEL            ACTION SELECT?                          
         BNE   DR100A                   NO, CHECK FOR ACTION CHANGE             
         CLI   SAVESEL,C'C'             YES,DON'T EXIT YET                      
         BE    DR101                                                            
DR100A   CLI   ACTEQU,ACTCHA                                                    
         BNE   DRX                                                              
DR101    MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
DRX      MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       DS    0H                                                               
         XC    GRPSTART,GRPSTART        DISPLAY FROM BEGIN. ON LIST             
         L     R6,AIO                                                           
         USING PRGRECD,R6                                                       
*                                                                               
         LA    R4,MEDTAB                                                        
         MVC   MEDX,PRGKAGMD                                                    
         NI    MEDX,X'FF'-X'F0'                                                 
*                                                                               
DK10     CLC   MEDX,1(R4)            COMPARE BINARY MEDIA W/ NO AGY             
         BE    DK15                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DK10                                                             
         DC    H'0'                  **** SHOULD DIE IF NOT T-X                 
*                                                                               
DK15     MVC   MEDIA,0(R4)             DISPLAY MEDIA CODE                       
         MVC   PDLMED,MEDIA                                                     
         OI    PDLMEDH+6,X'80'                                                  
         MVI   PDLMEDH+5,1                                                      
*                                                                               
         LA    R2,PDLMEDH                                                       
         CLI   SVAPROF+7,C'C'      CANADIAN?                                    
         BNE   DK17                                                             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BNE   DK17                                                             
         CLI   MEDIA,C'N'          CAN'T CHANGE MEDIA N                         
         BE    ERRINV                                                           
         CLI   MEDIA,C'C'          CAN'T CHANGE MEDIA C                         
         BE    ERRINV                                                           
*                                                                               
DK17     DS    0H                                                               
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   PDLMDN,MEDNM            DISPLAY MEDIA NAME                       
         OI    PDLMDNH+6,X'80'                                                  
*                                                                               
         GOTO1 CLUNPK,DMCB,PRGKCLT,CLIENT                                       
*** DISPLAY MEDIA AND CLIENT NAMES ????                                         
*                                                                               
         OC    CLIENT,SPACES                                                    
         MVC   PDLCLT,CLIENT                                                    
         OI    PDLCLTH+6,X'80'                                                  
         MVI   PDLCLTH+5,3                                                      
*                                                                               
         LA    R2,PDLCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   PDLCLN,CLTNM            DISPLAY CLIENT NAME                      
         OI    PDLCLNH+6,X'80'                                                  
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,PRGKCLT),CLIENT                           
         MVC   PDLCLT,CLIENT                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   PGRPID,PRGKID                                                    
         MVC   PDLGID,PGRPID                                                    
         OI    PDLGIDH+6,X'80'                                                  
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
DKX      CLI   ACTEQU,ACTSEL                                                    
         BNE   *+10                                                             
         MVC   SAVESEL,THISLSEL                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* ONLINE LIST                                                                   
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING PRGRECD,R6                                                       
         OC    KEY,KEY        IS THIS FIRST TIME AT LIST SCREEN?                
         BNZ   LR10           NO, DO NOT BUILD KEY                              
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
LR10     GOTO1 HIGH               FIND FIRST DIRECTORY REC                      
         B     LR25                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
         LA    R6,KEY                                                           
LR25     CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     DS    0H                                                               
         CLC   KEY(2),SAVEKEY     IF PAST PRODUCT GROUD DEF RECS -              
         BNE   LRX                STOP READING RECORDS                          
*                                                                               
LR32     CLC   AGYHI,KEY+2           ARE WE PAST THIS AGECNY'S RECS?            
         BL    LRX                                                              
*                                                                               
* CLIENT FILTER                                                                 
         OC    SVBCLT,SVBCLT            IF NO INPUT, SKIP THIS FILTER           
         BZ    *+14                                                             
         CLC   SVBCLT,KEY+3                                                     
         BNE   LR20                                                             
*                                                                               
* CHECK IF VALID ID 'V-Z'                                                       
**       CLI   PRGKID,C'V'                                                      
**       BL    LR20                                                             
**       CLI   PRGKID,C'Z'                                                      
**       BH    LR20                                                             
         CLI   PRGKID,C'A'                                                      
         BL    LR20                                                             
         CLI   PRGKID,C'Z'                                                      
         BH    LR20                                                             
*                                                                               
* CHECK IF STILL X'0000' REC                                                    
         OC    PRGKGRP,PRGKGRP           IS IT A PRGDEF REC?                    
         BNZ   LR20                     NO CHECK NEXT                           
*                                                                               
         MVC   LISTAR,SPACES      CLEAR PRINT LINE OF LIST ENTRIES              
*                                                                               
         LA    R4,MEDTAB                                                        
         MVC   MEDX,PRGKAGMD                                                    
         NI    MEDX,X'FF'-X'F0'                                                 
*                                                                               
LR35     CLC   MEDX,1(R4)           COMPARE BINARY AGENCY/MEDIA                 
         BE    LR40                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   LR35                                                             
         B     LRX                     PAST RELEVANT RECORDS                    
*                                                                               
LR40     DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    TEMPFLD,TEMPFLD           FOR BLANK MEDIA                        
         MVC   TEMPFLD(8),=XL8'0900000000030000'                                
         GOTO1 CLUNPK,DMCB,PRGKCLT,TEMPFLD+8                                    
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
         USING LISTD,R2                                                         
*                                                                               
         MVC   LSMED,0(R4)             DISPLAY MEDIA CODE                       
         MVC   LSPGID,PRGKID                                                    
*                                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,PRGKCLT),LSCLT                            
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         USING PRGRECD,R6                                                       
*                                                                               
         MVI   ELCODE,X'01'       FIND BREAK ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PRGEL01,R6                                                       
         MVC   LSBKT1,PRGBK1                                                    
         EDIT  PRGBK1LN,LSBKLN1                                                 
         MVC   LSBKT2,PRGBK2                                                    
         EDIT  PRGBK2LN,LSBKLN2                                                 
         DROP  R2                       DROP LISTD FROM R2                      
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   LR50                                                             
         GOTO1 LISTMON                                                          
         B     LR60                                                             
*                                                                               
LR50     GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR60     B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* XRECADD/XRECPUT                                                               
*                                                                               
XR       DS    0H                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   XRX                                                              
         CLI   MEDIA,C'T'                                                       
         BNE   XRX                                                              
*                                                                               
         L     R4,AIO1                                                          
         L     RE,AIO2                  COPY NEW REC TO AIO2                    
         L     R0,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(R4)              LENGTH OF REC                           
         LR    R1,RF                                                            
         MVCL  RE,R0               COPY AIO1 TO AIO2                            
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   XR50                                                             
*                                                                               
XR10     DS    0H                                                               
         L     R4,AIO2                  NEW MEDIA REC                           
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'03'              FOR NETWORK                             
         MVC   AIO,AIO2                                                         
         GOTO1 ADDREC                                                           
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'08'              FOR COMBINED                            
         GOTO1 ADDREC                                                           
         B     XRX                                                              
*                                                                               
XR50     MVC   KEY(13),SAVEKEY                                                  
         LA    R5,KEY                                                           
*                                                                               
         NI    2(R5),X'F0'                                                      
         OI    2(R5),X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XR60     MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO2                                                          
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'03'              FOR NETWORK                             
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC                                                           
*                                                                               
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'08'                                                      
         MVC   KEY,0(R4)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO2                  NEW MEDIA REC                           
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'08'              FOR NETWORK                             
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC                                                           
*                                                                               
XRX      MVC   KEY,SAVEKEY         RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*NVMEDAC MVC   ERRNUM,=AL2(INMDAC)                                              
         B     SPERREX                                                          
ERRBKLN  MVC   ERRNUM,=AL2(CHBKLN)                                              
         B     SPERREX                                                          
ERRNMBK  MVC   ERRNUM,=AL2(CHNUMBK)                                             
         B     SPERREX                                                          
ERRBKINV MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRBKTOT MVC   ERRNUM,=AL2(BKLNTOT)                                             
         B     SPERREX                                                          
ERRPID   MVC   ERRNUM,=AL2(BADPID)                                              
         B     SPERREX                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
*NMDAC   EQU   ???                                                              
CHBKLN   EQU   444                                                              
CHNUMBK  EQU   445                                                              
BKLNINV  EQU   446                                                              
BKLNTOT  EQU   447                                                              
BADPID   EQU   1284                                                             
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
         EJECT                                                                  
**** ROUTINE TO BUMP TO NEXT SCREEN FIELD ****                                  
NXTSCRF  DS    0H                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
****  CODE COPIED FROM SPSFM15 ADDRESS RECORD MAINTENANCE  MHC 05/02/06         
***********************************************************************         
*        SECMGREL SUBROUTINE                                          *         
***********************************************************************         
*****      NEW SECURITY MANAGER ID ELEMENT      MHC  05/02/06                   
SECMGREL NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,PRGSCMCQ     SECURITY MANAGER ID ELEMENT                  
         BRAS  RE,GETEL                                                         
         BNE   SMELX                                                            
*                                                                               
         USING PRGSCMD,R6                                                       
         LA    R2,PDMSCM1H                                                      
         LA    R3,PDMSCM6H                                                      
         LA    R4,PRGSCM1                                                       
*                                                                               
SECMGR10 OC    0(2,R4),0(R4)       ANYTHING IN THE PRGSCM?                      
         BZ    SMELX                - NOPE, WE'RE DONE                          
*                                                                               
         CR    R2,R3                                                            
         BH    SMELX               WE'RE DONE                                   
*                                                                               
         LR    RE,R2                                                            
         TM    1(RE),X'02'         DO WE HAVE AN EXTENDED FIELD HEADER?         
         BZ    SECMGRNX                                                         
         XR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         SHI   RE,8                POINT TO START OF EXTENDED HEADER            
         CLI   0(RE),111           IS IT ONE OF THE PID FIELDS?                 
         BNE   SECMGRNX             - NOPE                                      
*                                                                               
         GOTO1 SECMGRID,DMCB,(R2),(R4)                                          
*                                                                               
         LA    R4,L'PRGSCM(R4)     BUMP TO NEXT SECURITY MANAGER ID             
*                                                                               
SECMGRNX XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     SECMGR10                                                         
*                                                                               
SMELX    DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CHECKPID SUBROUTINE                                          *         
***********************************************************************         
CHECKPID NTR1                                                                   
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         USING SAPEREC,R6                                                       
         XC    SAPEKEY,SAPEKEY     BUILD PERSON KEY                             
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECALPHA                                                 
         MVC   SAPEPID,PIDNAME                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'CTFILE ',KEY2,AIO2               
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   CHKPIDNO                                                         
*                                                                               
         USING SAPWDD,R6                                                        
         MVI   ELCODE,SAPWDELQ     X'C4' - PERSON PASSWORD ELEM                 
         BRAS  RE,GETEL2                                                        
         BNE   CHKPIDNO                                                         
         MVC   PIDNUM,SAPWDNUM                                                  
*                                                                               
CHKPIDYS CR    RE,RE                                                            
         B     *+6                                                              
*                                                                               
CHKPIDNO LTR   RE,RE                                                            
         J     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SECMGRID SUBROUTINE                                          *         
*        P1 = SCREEN HEADER                                           *         
*        P2 = PID                                                     *         
***********************************************************************         
SECMGRID NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
* PERSONAL ID                                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SECALPHA                                                 
         MVC   PASSWD,0(R4)                                                     
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK   GET PERSONAL ID                             
         MVC   AIO,AIO1                                                         
         MVC   8(8,R2),PRSNLID                                                  
         MVI   5(R2),8             INPUT LENGTH OF 8                            
         OI    6(R2),X'80'         RETRANSMIT                                   
         DROP  R3                                                               
*                                                                               
SMIDX    DS    0H                                                               
         J     XIT                                                              
***********************************************************************         
*        CLEARS BOTTOM OF SCREEN                                                
***********************************************************************         
*                                                                               
CLRSCR   NTR1                                                                   
         LA    R2,PDMLISTH                                                      
         LA    R3,PDMDONEH                                                      
**       LA    R3,PDMLAST                                                       
*                                                                               
CLRSCR10 LLC   R1,0(R2)            FIELD LENGTH                                 
         SHI   R1,9                8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               PAST LAST OUTPUT FIELD?                      
         BL    CLRSCR10            NO, CONTINUE                                 
         B     XIT                      YES, DONE                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECKS BREAK NAMES AND LENGTHS                                         
***********************************************************************         
*                                                                               
CHKBKLN  NTR1                                                                   
         CLI   SAVESEL,C'C'             LIST CHANGE?                            
         BE    *+12                     VALIDATE PREVIOUS DATA                  
         CLI   ACTEQU,ACTCHA                                                    
         BNE   CBL10                   SKIP VALID ON PREVIOUS DATA              
*                                                                               
         CLC   0(L'SVPGB1,R4),SPACES    BREAK INPUT BEFORE?                     
         BH    CBL5                     YES                                     
         CLI   5(R2),0                  NO, CURRENT INPUT?                      
         BNE   ERRNMBK                  YES, ERROR                              
         B     CBLX                     NO CURRENT INPUT, EXIT                  
*                                                                               
CBL5     CLI   5(R2),0                  INPUT PREVIOUSLY, INPUT NOW?            
         BE    ERRNMBK                  NO, ERROR                               
*                                                                               
CBL10    CLI   5(R2),0                  CHECK 2ND BREAK ON ADD                  
         BE    CBLX                     NONE, DONE                              
         MVC   0(L'PRGBK1,R3),8(R2)     STORE NEW BREAK TITLE                   
*                                                                               
         BAS   RE,NXTSCRF                                                       
         BAS   RE,NXTSCRF               POINT TO SCREEN LENGTH                  
         TM    4(R2),X'08'              NUMERIC?                                
         BNO   ERRINV                                                           
         CLI   8(R2),C'3'                                                       
         BH    ERRBKINV                                                         
         CLI   8(R2),C'1'                                                       
         BL    ERRBKINV                                                         
*                                                                               
         PACK  DUB,8(1,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,SVBKLNB               SAVE BINARY BREAK LEN                   
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    *+14                                                             
         CLC   SVBKLNB,12(R4)             SAME AS PREVIOUS LEN DATA?            
         BNE   ERRBKLN                                                          
*                                                                               
**** CHECK FOR TOTAL = 3 HERE                                                   
         MVC   12(1,R3),SVBKLNB          STORE NEW BREAK LEN                    
         LLC   R0,BYTE2                                                         
         AR    R0,R1                    ADD NEW BREAK LEN TO TOTAL              
         STC   R0,BYTE2                                                         
         CLI   BYTE2,3                  IS TOTAL > 3 ?                          
         BH    ERRBKTOT                                                         
*                                                                               
CBLX     B     XIT                      YES, DONE                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST        DON'T ALLOW LIST DELETE                 
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**** CONSTANTS AND TABLES                                                       
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'PRGD LIST'                                               
         SSPEC H2,30,C'---------'                                               
         SPACE 1                                                                
         SSPEC H4,1,C'MD'                                                       
         SSPEC H4,5,C'CLI'                                                      
         SSPEC H4,10,C'ID'                                                      
         SSPEC H4,14,C'BREAK1 TITLE'                                            
         SSPEC H4,28,C'BKLN1'                                                   
         SSPEC H4,46,C'BREAK2 TITLE'                                            
         SSPEC H4,60,C'BKLN2'                                                   
         SPACE 1                                                                
         SSPEC H5,1,C'--'                                                       
         SSPEC H5,5,C'---'                                                      
         SSPEC H5,10,C'--'                                                      
         SSPEC H5,14,C'------------'                                            
         SSPEC H5,28,C'-----'                                                   
         SSPEC H5,46,C'------------'                                            
         SSPEC H5,60,C'-----'                                                   
         SPACE 1                                                                
         DC    X'00'                                                            
*                                                                               
**** TABLE FOR MEDIA AND BIN. AGYMD CODES ***                                   
MEDTAB   DS    0H                                                               
         DC    CL1'T',XL1'01'                                                   
MTABLQ   EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'           NETWORK TV FOR CANADIAN                 
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'           COMBINED MEDIA FOR CANADIAN             
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME4D               MAINT. SCREEN                           
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME2D               LIST SCREEN                             
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
CLIENT   DS    CL3                                                              
MEDIA    DS    CL1                                                              
MEDX     DS    CL1                      MEDIA CODE WITH AGY '0'                 
AGYX     DS    CL1                      AGENCY CODE WITH MEDIA   '0'            
AGYHI    DS    CL1                      AGENCY CODE WITH MED 'F'                
SVBCLT   DS    XL2                      SAVED BIN CLIENT                        
PGRPID   DS    CL1                                                              
SAVEKEY  DS    CL13                                                             
SVBKLNS  DS    XL2                      SAVED BREAK LENGTHS                     
SVBKLNB  DS    XL1                      SAVED BINARY BREAK LENGTH               
BYTE2    DS    XL1                      TOTAL BREAK LENGTHS                     
GRPSTART DS    XL2                      PGROUP START AT DISPLAY                 
PIDNAME  DS    CL8                                                              
PIDNUM   DS    XL2                                                              
SECALPHA DS    CL2                 SECURITY AGENCY ALPHA                        
KEY2     DS    CL50                                                             
*                                                                               
SVPGB1   DS    CL12                     SAVE FIELDS OF OLD 01 DATA              
SVPGBL1  DS    XL1                                                              
SVPGB2   DS    CL12                                                             
SVPGBL2  DS    XL1                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
TEMPFLD  DS    XL11                                                             
SAVESEL  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
*                                                                               
**** ONLINE LIST LINE                                                           
*                                                                               
LISTD    DSECT                                                                  
LSMED    DS    CL1                 STAFF MEMBER NAME (IN E-MAIL FORM)           
         DS    CL3                                                              
LSCLT    DS    CL3                                                              
         DS    CL2                                                              
LSPGID   DS    CL1                                                              
         DS    CL3                                                              
LSBKT1   DS    CL12                                                             
         DS    CL2                                                              
LSBKLN1  DS    CL1                                                              
         DS    CL17                                                             
LSBKT2   DS    CL12                                                             
         DS    CL2                                                              
LSBKLN2  DS    CL1                                                              
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPSFM4D   01/08/14'                                      
         END                                                                    
