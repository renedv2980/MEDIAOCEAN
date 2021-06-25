*          DATA SET SPSFM4E    AT LEVEL 107 AS OF 11/06/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE T2174EA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2174E  -- PRODUCT GROUP MAINTENANCE                 *         
*                                                                     *         
*  COMMENTS:     MAINTAINS PRODUCT GRP RECS ON SPTFILE                *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMFE, AND SPSFMFD                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED PDT GRP RECORDS                              *         
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
         TITLE 'T2174E - PRODUCT GROUP RECORD MAINTENANCE'                      
T2174E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**174E**,R7,RR=R3                                              
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
         CLI   MODE,XRECADD                                                     
         BE    XR                  ADD/DEL PASSIVE KEYS                         
         CLI   MODE,XRECPUT                                                     
         BE    XR                  ADD/DEL PASSIVE KEYS                         
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
         MVI   AGYX,0                                                           
         MVI   AGYHI,0                                                          
         MVI   MEDIA,C' '                                                       
         MVC   CLIENT,SPACES                                                    
         MVI   PGRPID,C' '                                                      
         XC    SVBAGYMD,SVBAGYMD                                                
         XC    BCLT,BCLT                                                        
         XC    SVBCLT,SVBCLT                                                    
         XC    PGRPNM,PGRPNM                                                    
         XC    SAVESEL,SAVESEL                                                  
         XC    DISKADD,DISKADD                                                  
*                                                                               
         MVC   PGLMDN,SPACES           CLEAR ADDRESS FIELDS                     
         OI    PGLMDNH+6,X'80'                                                  
         MVC   PGLCLN,SPACES                                                    
         OI    PGLCLNH+6,X'80'                                                  
*                                                                               
         CLI   ACTEQU,ACTLIST           LIST?                                   
         BNE   VK50                     NO, VALKEY FOR MAINT                    
*                                                                               
* VALIDATE KEY FOR ACTION LIST                                                  
         LA    R2,PGLMEDH               VALIDATE MEDIA                          
         CLI   5(R2),0                  PRESENT?                                
         BE    VK20                     NO, BUILD EMPTY KEY                     
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
VK10     CLC   PGLMED,0(R4)                                                     
         BE    VK15                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   VK10                                                             
         B     ERRINV                                                           
*                                                                               
VK15     MVC   MEDIA,0(R4)                                                      
         CLI   SVAPROF+7,C'C'           CANADAN?                                
         BE    VK16                     YES, ALL MEDIA OK FOR LIST              
         CLI   8(R2),C'N'               NOT CANADA, MEDIA N?                    
         BE    ERRINV                   YES, CAN'T UNLESS CANADIAN              
         CLI   8(R2),C'C'               SAME FOR MEDIA C ...                    
         BE    ERRINV                                                           
*                                                                               
VK16     GOTO1 VALIMED                  FOR VALICLI CALL                        
         MVC   PGLMDN,MEDNM            DISPLAY MEDIA NAME                       
         OI    PGLMDNH+6,X'80'                                                  
         MVC   AGYHI,BAGYMD                                                     
         OI    AGYHI,X'0F'                                                      
         B     VK25                                                             
*                                                                               
VK20     XC    TEMPFLD,TEMPFLD          FOR BLANK MEDIA                         
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
         MVC   AGYHI,AGYX                                                       
         OI    AGYHI,X'0F'                                                      
*                                                                               
VK25     LA    R2,PGLCLTH               VALIDATE CLIENT                         
         CLI   5(R2),0                                                          
         BE    VK30                     NO CLIENT, BUILD KEY                    
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT            *** FOR FILTER                            
         MVC   PGLCLN,CLTNM            DISPLAY CLIENT NAME                      
         OI    PGLCLNH+6,X'80'                                                  
** EXCEPTION FOR CLIENT POL ??? ***                                             
         MVC   CLIENT,8(R2)                                                     
         OC    CLIENT,SPACES                                                    
*                                                                               
VK30     LA    R2,PGLGRPH               VALIDATE PRODUCT GROUP ID               
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
*                                                                               
VK35     LA    R2,PGLGRPH               VALIDATE PRODUCT GROUP #                
         CLI   5(R2),1                  CHECK FOR GROUP # INPUT                 
         BE    VK100                                                            
         BAS   RE,VALGRPNM              VALIDATE GROUP #                        
         B     VK100                                                            
*                                                                               
VK50     DS    0H             VALIDATE FOR DISPLAY/ADD/CHANGE                   
         LA    R2,PGMMEDH               VALIDATE MEDIA                          
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
VK60     CLC   PGMMED,0(R4)                                                     
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
         MVC   SVBAGYMD,BAGYMD          SAVE AGENCY MEDIA CODE                  
         MVC   PGMMDN,MEDNM            DISPLAY MEDIA NAME                       
         OI    PGMMDNH+6,X'80'                                                  
*                                                                               
         LA    R2,PGMCLTH               VALIDATE CLIENT                         
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT            SAVE AGENCY MEDIA CODE                    
         MVC   PGMCLN,CLTNM            DISPLAY CLIENT NAME                      
         OI    PGMCLNH+6,X'80'                                                  
** EXCEPTION FOR CLIENT POL ??? ***                                             
         MVC   CLIENT,8(R2)                                                     
         OC    CLIENT,SPACES                                                    
*                                                                               
         LA    R2,PGMGRPH               VALIDATE PRODUCT GROUP ID               
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
         LA    R2,PGMGRPH               VALIDATE PRODUCT GROUP #                
         CLI   5(R2),1                  CHECK FOR GROUP # INPUT                 
         BE    ERRMIS                                                           
         BAS   RE,VALGRPNM              VALIDATE GROUP #                        
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
         MVC   PRGKAGMD,AGYX            YES, USE AGY W/O MEDIA                  
         B     *+10                                                             
         MVC   PRGKAGMD,BAGYMD                                                  
*                                                                               
         MVC   PRGKCLT,BCLT           BINARY CLIENT CODE                        
         MVC   PRGKID,PGRPID                                                    
         MVC   PRGKGRP,PGRPNM                                                   
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
         MVI   DPRDFLG,C'N'        SET PRODUCT DELETE FLAG                      
         MVI   XRECFLG,C'N'        SET VISITED XREC FLAG                        
         MVI   MISCFLG1,X'FF'-MF1UPD   RESET THIS FLAG                          
         XC    SAVNAM,SAVNAM                                                    
*                                                                               
*************  NEED TO SEE IF USER HAS ACCESS TO THIS  ****************         
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    *+12                NO                                           
         BRAS  RE,USERCHK                                                       
         BNE   ERRPID                                                           
*************  NEED TO SEE IF USER HAS ACCESS TO THIS  ****************         
         LA    R2,PGMMEDH                                                       
         CLI   SVAPROF+7,C'C'                                                   
         BNE   VR2                                                              
         CLI   8(R2),C'N'               MEDIA N?                                
         BE    *+12                     YES                                     
         CLI   8(R2),C'C'               NO, MEDIA C?                            
         BNE   *+12                     NO, ALL ACTIONS VALID                   
         CLI   ACTEQU,ACTDIS            ONLY DISPLAY VALID FOR C AND N          
         BNE   ERRINV                                                           
*                                                                               
VR2      XC    ADPRDS(ADPRDLQ),ADPRDS                                           
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VR5                                                              
*                                                                               
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING PRGEL10,R6                                                       
*                                                                               
         MVC   SAVNAM,PRGNAM1      SAVE NAME FOR MESSAGING                      
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VR5      XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING PRGEL10,R5                                                       
         MVI   0(R5),X'10'                                                      
         MVI   1(R5),74                                                         
*                                                                               
* VALIDATE BREAK NAMES                                                          
*                                                                               
         LA    R2,PGMNM1H                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   TWAOFFC,C'*'             DDS TERMINAL?                           
         BNE   VR7                      NO                                      
         CLC   =C'DELETE',PGMNM1   DID USER TYPE DELETE?                        
         BNE   VR7                 NO, CHANGE NAME NORMALLY                     
         MVC   PRGNAM1,SAVNAM                                                   
         B     VR8                                                              
*                                                                               
VR7      MVC   PRGNAM1,8(R2)                                                    
         OC    PRGNAM1,SPACES                                                   
*                                                                               
VR8      LA    R2,PGMNM2H                                                       
         TM    BKFLG,X'08'              SEC BREAK PRESENT?                      
         BNO   VR10                     NO SECOND BREAK                         
         CLI   5(R2),0                  # BK NAMES MUST = # BK TITLES           
         BE    ERRMIS                   SHOULD HAVE BREAK NAME                  
         MVC   PRGNAM2,8(R2)                                                    
         OC    PRGNAM2,SPACES                                                   
         B     VR15                                                             
         DROP  R5                                                               
*                                                                               
VR10     CLI   5(R2),0                  # BK NAMES MUST = # BK TITLES           
         BNE   ERRINV                   SHOULD NOT HAVE BREAK NAME              
*                                                                               
VR15     GOTO1 ADDELEM                                                          
*                                                                               
* VALIDATE BILLING ADDRESSES                                                    
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VR15A                                                            
         MVI   ELCODE,X'20'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VR15A    XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING PRGEL20,R5                                                       
         MVI   0(R5),X'20'                                                      
         MVI   1(R5),122                                                        
*                                                                               
         LA    R2,PGMADD1H                                                      
         CLI   5(R2),0                                                          
         BE    VR16                                                             
         MVC   PRGADDR1,8(R2)                                                   
         OC    PRGADDR1,SPACES                                                  
*                                                                               
VR16     LA    R2,PGMADD2H                                                      
         CLI   5(R2),0                                                          
         BE    VR17                                                             
         MVC   PRGADDR2,8(R2)                                                   
         OC    PRGADDR2,SPACES                                                  
*                                                                               
VR17     LA    R2,PGMADD3H                                                      
         CLI   5(R2),0                                                          
         BE    VR18                                                             
         MVC   PRGADDR3,8(R2)                                                   
         OC    PRGADDR3,SPACES                                                  
*                                                                               
VR18     LA    R2,PGMADD4H                                                      
         CLI   5(R2),0                                                          
         BE    VR19                                                             
         MVC   PRGADDR4,8(R2)                                                   
         OC    PRGADDR4,SPACES                                                  
         DROP  R5                                                               
*                                                                               
VR19     OC    ELEM+2(120),ELEM+2        IS ELEM BLANK?                         
         BZ    VR19A                                                            
         GOTO1 ADDELEM                  NO ADD THE ELEMENT                      
*                                                                               
* VALIDATE USER FIELD MASTER PRODUCT                                            
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VR19B                                                            
VR19A    MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VR19B    XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING PRGEL30,R5                                                       
         MVI   0(R5),X'30'                                                      
         MVI   1(R5),5                                                          
*                                                                               
         LA    R2,PGMUSERH                                                      
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         BAS   RE,VALMPRD               VALIDATE MASTER PRODUCT                 
         MVC   PRGUSER,INPRD                                                    
         DROP  R5                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR20     DS    0H                       CHECK ADD PRODUCT FIELDS                
         LA    R2,PGMPRD1H                                                      
         LA    R3,ADPRDS                POINT TO TABLE OF ADDED PROD.S          
*                                                                               
VR25     CLI   5(R2),0                                                          
         BE    VR30                     NO INPUT, CHECK NEXT FIELD              
         BAS   RE,VALPRD                ADD SINGLE PRODUCT                      
*                                                                               
* IF DDS TERMINAL AND P-KEY FOUND, GOTO DELTPRD                                 
         CLI   TWAOFFC,C'*'             DDS TERMINAL?                           
         BNE   VR27                     NO CHECK FOR DUP ASSIGN                 
         CLC   KEY(13),KEYSAVE          IS PASSIVE KEY FOUND?                   
         BNE   VR27                     NO, EXIT                                
         CLC   =C'DELETE',PGMNM1   DID USER TYPE DELETE?                        
         BNE   VR27                NO, CHECK DUP ASSIGN                         
         BAS   RE,DELTPRD               DELETE PRD FROM DDS TERMINAL            
         MVI   DPRDFLG,C'Y'        SET PRD DELETE FLAG                          
         B     VR30                                                             
*                                                                               
VR27     CLC   KEY(13),KEYSAVE          IS PASSIVE KEY FOUND?                   
         BE    DPGRPERR                 YES, DUPLICATE ASSIGN ERROR             
*                                                                               
         MVC   0(L'ADPRDS,R3),INPRD     MOVE PRODUCT INTO TABLE                 
         LA    R3,L'ADPRDS(R3)          POINT TO NEXT TABLE ENTRY               
*                                                                               
VR30     BAS   RE,NXTSCRF                                                       
         LA    R0,PGMPRDXH                                                      
         CR    R2,R0                                                            
         BNH   VR25                                                             
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        PROD GRP BREAK NAMES                         
         BAS   RE,GETEL                                                         
*        BE    *+6                 REQUIRED ELEMENT                             
*        DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         BAS   RE,CLRSCR                CLEAR SCREEN                            
*                                                                               
         USING PRGEL10,R6                                                       
         MVC   PGMNM1,PRGNAM1                                                   
         OI    PGMNM1H+6,X'80'                                                  
         MVC   PGMNM2,PRGNAM2                                                   
         OI    PGMNM2H+6,X'80'                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        PROD GRP ADDRESSES                           
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
*                                                                               
         USING PRGEL20,R6                                                       
         MVC   PGMADD1,PRGADDR1                                                 
         OI    PGMADD1H+6,X'80'                                                 
         MVC   PGMADD2,PRGADDR2                                                 
         OI    PGMADD2H+6,X'80'                                                 
         MVC   PGMADD3,PRGADDR3                                                 
         OI    PGMADD3H+6,X'80'                                                 
         MVC   PGMADD4,PRGADDR4                                                 
         OI    PGMADD4H+6,X'80'                                                 
*                                                                               
DR10     L     R6,AIO                                                           
         MVI   ELCODE,X'30'        PROD GRP USER FIELD                          
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
*                                                                               
         USING PRGEL30,R6                                                       
         MVC   PGMUSER,PRGUSER                                                  
         OI    PGMUSERH+6,X'80'                                                 
*                                                                               
         USING PRGRECD,R4                                                       
DR20     XC    KEY,KEY                                                          
         MVC   KEY(PRGKGRP-PRGKEY),SAVEKEY    LOOK FOR DEF REC                  
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE          IS THIS THE DEFINITION REC?             
         BNE   ERRINV                   NO DEF REC, ERROR                       
*                                                                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R6,AIO2                                                          
         MVI   ELCODE,X'01'        PROD GRP BREAK DESC                          
         BAS   RE,GETEL                                                         
*        BE    *+6                 REQUIRED ELEMENT                             
*        DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING PRGEL01,R6                                                       
         MVC   PGMBK1,PRGBK1                                                    
         OI    PGMBK1H+6,X'80'                                                  
         MVC   PGMBK2,PRGBK2                                                    
         OI    PGMBK2H+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
* DISPLAY PRODUCT LIST                                                          
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(8),SAVEKEY                                                   
         MVC   PRGPTYP,=X'0D81'                                                 
*                                                                               
         LA    R2,PGMLSH                                                        
         LA    R5,PGMSEPAH                                                      
         LA    R3,8(R2)                                                         
         ZAP   HALF,=P'18'         SET FOR 18 PRDS PER LINE                     
*                                                                               
         GOTO1 HIGH                                                             
         B     DR30                                                             
*                                                                               
DR25     GOTO1 SEQ                                                              
*                                                                               
DR30     CLC   KEY(PRGPPRD-PRGPTYP),KEYSAVE   TEST SAME THRU PRDGRP             
         BNE   DR50                                                             
*                                                                               
         MVC   0(3,R3),PRGPPRD                                                  
         LA    R3,4(R3)                                                         
         SP    HALF,=P'1'          ADJUST COUNTER                               
         BP    DR25                CONTINUE IF MORE ROOM                        
         OI    6(R2),X'80'                                                      
         BAS   RE,NXTSCRF                                                       
         CR    R2,R5               TEST E-O-PRD LIST                            
         BE    DR50                                                             
         LA    R3,8(R2)                                                         
         ZAP   HALF,=P'18'                                                      
         B     DR25                                                             
         DROP  R4                                                               
*                                                                               
DR50     L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BRAS  RE,GETEL            MUST BE THERE                                
         BNE   DR100               END IT IF NOT THERE, FOR NOW                 
*                                                                               
         USING ACTVD,R6                                                         
         XC    PGMADTE,PGMADTE     WIPE IT SQUEAKY CLEAN                        
         OC    ACTVCHDT,ACTVCHDT                                                
         BZ    DR52                                                             
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,PGMADTE)   CHANGED DATE              
DR52     OI    PGMADTEH+6,X'80'                                                 
         XC    PGMCWHO,PGMCWHO                                                  
         OC    ACTVCHID,ACTVCHID   ANY CHANGED PERSON?                          
         BZ    DR54                                                             
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SECALPHA    SECURITY AGENCY                              
         MVC   PASSWD,ACTVCHID     PERSON WHO CHANGED                           
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK  GET PERSONAL ID                              
         MVC   AIO,AIO1                                                         
         MVC   PGMCWHO,PRSNLID                                                  
DR54     OI    PGMCWHOH+6,X'80'                                                 
*                                                                               
DR100    CLI   ACTEQU,ACTSEL            ACTION SELECT?                          
         BNE   DR100A                   NO, CHECK FOR ACTION CHANGE             
         CLI   SAVESEL,C'C'             YES,DON'T EXIT YET                      
         BE    DR101                                                            
DR100A   CLI   ACTEQU,ACTCHA                                                    
         BNE   DRX                                                              
DR101    XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
DRX      MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
* DELETED PRODUCT CHECK - SO WE CAN DISPLAY MESSAGE                             
*                                                                               
         CLI   XRECFLG,C'Y'        DID WE CHANG REC'S YET?                      
         BNE   DRXX                NO, EXIT                                     
         CLI   DPRDFLG,C'Y'        DID WE DELETE A PRD?                         
         BNE   DRXX                NO, EXIT                                     
*                                                                               
         MVI   DPRDFLG,C'N'        RESET FLAG TO NO                             
         B     INFMSGX             INFORMATION MESSAGE EXIT                     
*                                                                               
DRXX     B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       DS    0H                                                               
         XC    BYTE2,BYTE2                                                      
         L     R6,AIO                                                           
         USING PRGRECD,R6                                                       
*                                                                               
         LA    R4,MEDTAB                                                        
         MVC   MEDX,PRGKAGMD                                                    
         NI    MEDX,X'FF'-X'F0'                                                 
*                                                                               
DK10     CLC   MEDX,1(R4)               COMPARE BINARY AGENCY/MEDIA             
         BE    DK15                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DK10                                                             
         B     ERRINV                                                           
*        DC    H'0'                     **** SHOULD DIE IF NOT T-X              
*                                                                               
DK15     MVC   MEDIA,0(R4)             DISPLAY MEDIA CODE                       
         MVC   PGMMED,MEDIA                                                     
         OI    PGMMEDH+6,X'80'                                                  
         MVI   PGMMEDH+5,1                                                      
         MVC   SVBAGYMD,PRGKAGMD        SAVE AGENCY MEDIA CODE                  
*                                                                               
         LA    R2,PGMMEDH                                                       
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
         MVC   SAVEKEY3,KEY                                                     
         GOTO1 VALIMED                                                          
         MVC   PGMMDN,MEDNM            DISPLAY MEDIA NAME                       
         OI    PGMMDNH+6,X'80'                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   SVBCLT,PRGKCLT         SAVE CLIENT CODE                          
*                                                                               
         GOTO1 CLUNPK,DMCB,PRGKCLT,CLIENT                                       
*                                                                               
         OC    CLIENT,SPACES                                                    
         MVC   PGMCLT,CLIENT                                                    
         OI    PGMCLTH+6,X'80'                                                  
         MVI   PGMCLTH+5,3                                                      
*                                                                               
         LA    R2,PGMCLTH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   PGMCLN,CLTNM            DISPLAY CLIENT NAME                      
         OI    PGMCLNH+6,X'80'                                                  
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,PRGKCLT),CLIENT                           
         MVC   PGMCLT,CLIENT                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   PGRPID,PRGKID                                                    
         MVC   PGMGRP(1),PGRPID                                                 
*                                                                               
         MVC   PGRPNM,PRGKGRP                                                   
         MVC   SAVEKEY2,SAVEKEY3                                                
         BAS   RE,GETGPNM                                                       
         MVC   PGMGRP+1(3),SAVGNUM                                              
         OI    PGMGRPH+6,X'80'                                                  
*                                                                               
         L     R6,AIO                                                           
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
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
         BNZ   LR10           NO, DO NOT BUILD KEY & DON'T CLEAR BYTE           
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
         CLC   KEY(2),SAVEKEY     IF PAST PRODUCT GROUP -                       
         BNE   LRX                STOP READING RECORDS                          
*                                                                               
LR32     CLC   AGYHI,KEY+2           ARE WE PAST THIS AGECNY'S RECS?            
         BL    LRX                                                              
*                                                                               
* CLIENT FILTER                                                                 
         OC    SVBCLT,SVBCLT                                                    
         BZ    *+14                                                             
         CLC   SVBCLT,KEY+3                                                     
         BNE   LR20                                                             
*                                                                               
* GROUP ID FILTER                                                               
         CLI   PGRPID,C' '                                                      
         BE    *+14                                                             
         CLC   PGRPID,KEY+5                                                     
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
         BZ    LR20                     YES, CHECK NEXT RECORD                  
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
*                                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,PRGKCLT),LSCLT                            
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BNE   LR20                CHECK NEXT                                   
*        BE    *+6                                                              
*        DC    H'0'                                                             
         L     R6,AIO                                                           
         USING PRGRECD,R6                                                       
*                                                                               
         MVI   ELCODE,X'10'       FIND BREAK NAME ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   LR20                CHECK NEXT                                   
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         USING PRGEL10,R6                                                       
         MVC   LSBKNM1,PRGNAM1                                                  
         MVC   LSBKNM2,PRGNAM2                                                  
*                                                                               
         L     R6,AIO                                                           
         USING PRGRECD,R6                                                       
         MVC   LSPGID,PRGKID                                                    
*                                                                               
* GET DEF REC HERE                                                              
         CLC   KEY(6),SAVEKEY2          IS GROUP REC PART OF SAME ID?           
         BE    LR50                     YES, DON'T READ DEF REC AGAIN           
*                                                                               
         XC    BYTE2,BYTE2              NEW GROUP ID, NEW BREAK LEN             
         MVC   SAVEKEY2,KEY             SAVE KEY OF CURRENT PRINTED REC         
         MVC   DISKADD,KEY+14          SAVE ADDRESS OF RECORD                   
         BAS   RE,GETGPNM                                                       
         MVC   LSPGNUM,SAVGNUM                                                  
         MVC   DMDSKADD,DISKADD         RESTORE ADDRESS TO TABLE                
*                                                                               
         MVC   KEY(L'SAVEKEY2),SAVEKEY2     RESTORE KEY                         
         GOTO1 HIGH                                                             
         B     LR55                                                             
*                                                                               
LR50     DS    0H                                                               
         MVC   SAVEKEY2,KEY             SAVE KEY OF CURRENT PRINTED REC         
         UNPK  DUB,PRGKGRP(3)                                                   
         LLC   RE,BYTE2            GET TOTAL DIGITS                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LSPGNUM(0),DUB+3 ** EXECUTED **                                  
         DROP  R2                                                               
*                                                                               
LR55     CLI   MODE,LISTRECS                                                    
         BNE   LR60                                                             
         GOTO1 LISTMON                                                          
         B     LR70                                                             
*                                                                               
LR60     GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR70     B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
* XRECPUT AND XRECADD - ADD/DELETE PASSIVE KEYS                                 
*                                                                               
XR       DS    0H                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN                                     
         BNE   XR2                                                              
         CLI   MEDIA,C'T'          FOR TV                                       
         BNE   XR2                                                              
         BAS   RE,COPYMEDT         NEED TO COPY TO MEDIA N AND C                
*                                                                               
XR2      OC    ADPRDS(ADPRDLQ),ADPRDS       WHOLE TABLE EMPTY?                  
         BZ    XRX                          YES, EXIT                           
*                                                                               
         LA    R3,ADPRDS                                                        
XR5      OC    0(L'ADPRDS,R3),0(R3)     IS THIS PRD FIELD BLANK?                
         BZ    XRX                      YES, DONE                               
         XC    DELGRP,DELGRP                                                    
         MVC   TEMPMED,SVBAGYMD                                                 
*                                                                               
XR7      XC    KEY,KEY                  READ X'00' PRODUCT RECORDS              
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         MVC   PKEYAM,TEMPMED                                                   
         MVC   PKEYCLT,SVBCLT                                                   
         MVC   PKEYPRD,0(R3)            LOOK FOR SAVED PRODUCT                  
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'Y'            READ FOR UPDATE                         
         GOTO1 READ                     SHOULD BE RIGHT REC CAUSE OF VR         
*        GOTO1 HIGH                     SHOULD BE RIGHT REC CAUSE OF VR         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   DISKADD,KEY+14           SAVE D/A FOR ADDING P-KEYS              
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDR,R6                                                        
         XC    SVPGRPS(15),SVPGRPS      SAVE OLD PRD GRP ASSIGNS                
         MVC   SVPGRPS(9),PGRP1                                                 
         MVC   SVPGRPS+9(6),PGRP4                                               
         MVC   SVPGRPS+15(15),PGRP6                                             
*                                                                               
         LA    R4,SAVEKEY                                                       
         USING PRGRECD,R4                                                       
*                                                                               
         LA    R0,10                                                            
         LA    R1,SVPGRPS                                                       
XR10     CLC   0(1,R1),PRGKID           COMPARE TO ID OF RECORD                 
         BE    XR20                                                             
         LA    R1,3(R1)                                                         
         BCT   R0,XR10                                                          
*                                                                               
         LA    R0,10                                                            
         LA    R1,SVPGRPS                                                       
XR15     OC    0(3,R1),0(R1)            IS PRDGRP ASSIGN BLANK                  
         BZ    XR20                     YES, ADD NEW GRP ASSIGN                 
         LA    R1,3(R1)                                                         
         BCT   R0,XR15                                                          
         B     ERROVFL                  GRP ASSIGNS FULL, REPORT ERROR          
*                                                                               
XR20     MVC   DELGRP,0(R1)             SAVE GROUP FOR DELETION                 
*                                                                               
XR25     MVC   0(3,R1),PRGKID           SAVE NEW ASSIGN                         
         MVC   PGRP1(9),SVPGRPS         RESTORE TABLE TO PRD REC                
         MVC   PGRP4(6),SVPGRPS+9                                               
         MVC   PGRP6(15),SVPGRPS+15                                             
         DROP  R4,R6                                                            
*                                                                               
         MVI   ACTELOPT,C'N'           DON'T ADD ACTIV ELEM                     
*                                                                               
*** USE DMWRITE CAUSE IT'S AN OLD RECORD WITH NO ELEMENTS                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE',DISKADD,AIO                   
         MVI   ACTELOPT,C'Y'                                                    
*                                                                               
*** DELETE PASSIVE KEY                                                          
*                                                                               
         OC    DELGRP,DELGRP                                                    
         BZ    XR30                     NO PASSIVE TO DELETE                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD  P-KEY, PTS TO PROD REC                
         USING PRGRECD,R4                                                       
         MVC   PRGPTYP,=X'0D81'         PASSIVE KEY                             
         MVC   PRGPAGMD,TEMPMED                                                 
         MVC   PRGPCLT,SVBCLT                                                   
         MVC   PRGPID(3),DELGRP                                                 
         MVC   PRGPPRD,0(R3)                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XR30                     NO P-KEY FOUND, ADD P-KEY               
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                                                            
*** ADD PASSIVE KEY                                                             
*                                                                               
XR30     XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD PRODUCT GROUP DEF. KEY                 
         USING PRGRECD,R4                                                       
         MVC   PRGPTYP,=X'0D81'         PASSIVE KEY                             
         MVC   PRGPAGMD,TEMPMED                                                 
         MVC   PRGPCLT,SVBCLT                                                   
         MVC   PRGPID,PGRPID                                                    
         MVC   PRGPGRP,PGRPNM                                                   
         MVC   PRGPPRD,0(R3)                                                    
         MVC   KEY+14(4),DISKADD                                                
         DROP  R4                                                               
*                                                                               
         OI    DMINBTS,X'08'            READ DELETED REC'S                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    XR40                     UNDELETE OLD PASSIVE PTR                
         MVC   KEY(18),KEYSAVE          13 KEY, 1 STATUS, 4 D/A                 
         GOTO1 ADD                                                              
         B     XR45                                                             
*                                                                               
XR40     NI    KEY+13,X'FF'-X'80'       TURN OFF DELETE BIT                     
         GOTO1 WRITE                                                            
*                                                                               
XR45     CLI   SVAPROF+7,C'C'      CANADIAN                                     
         BNE   XR50                                                             
         CLI   MEDIA,C'T'          FOR TV                                       
         BNE   XR50                                                             
*                                                                               
         MVC   BYTE,TEMPMED        SEE WHICH MEDIA I JUST FINISHED              
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'08'          LAST WAS COMBINED - SO DONE                  
         BE    XR50                                                             
         CLI   BYTE,X'03'                                                       
         BE    DOCOMB              LAST WAS NETWORK - DO COMBINED               
*                                                                               
         NI    TEMPMED,X'F0'                                                    
         OI    TEMPMED,X'03'         LAST WAS TV - DO NETWORK                   
         B     XR7                                                              
*                                                                               
DOCOMB   NI    TEMPMED,X'F0'                                                    
         OI    TEMPMED,X'08'         LAST WAS NETW - DO COMBINED                
         B     XR7                                                              
*                                                                               
XR50     LA    R3,3(R3)                 POINT TO NEXT PRODUCT                   
         B     XR5                                                              
*                                                                               
XRX      MVI   RDUPDATE,C'N'                                                    
         MVI   XRECFLG,C'Y'        HAVE VISITED XREC                            
         NI    DMINBTS,X'FF'-X'08'                                              
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),SAVEKEY                                                  
         B     DR                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
DPGRPERR MVC   ERRNUM,=AL2(DUPGRP)                                              
         B     SPERREX                                                          
ERROVFL  MVC   ERRNUM,=AL2(GPOVFL)                                              
         B     SPERREX                                                          
ERRPID   MVC   ERRNUM,=AL2(BADPID)                                              
         B     SPERREX                                                          
BADPID   EQU   1104                                                             
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
* INFORMATION MESSAGE EXIT                                                      
*                                                                               
INFMSGX  DS    0H                                                               
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVI   GTMSGNO1,7          MY MESSAGE                                   
         MVI   GTMTYP,GTMINF       INFORMATION MESSAGE                          
         MVI   GTMSYS,2            SPOT SYSTEM                                  
         LA    R2,CONHEADH                                                      
         GOTO1 ERREX                                                            
*                                                                               
IMX      B     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
*                                                                               
*POVFL   EQU   443                                                              
GPOVFL   EQU   1275                                                             
DUPGRP   EQU   448                                                              
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
***********************************************************************         
*        CLEARS ADD PRODUCTS, PRODUCT LIST, AND ADDRESS FIELDS                  
***********************************************************************         
*                                                                               
CLRSCR   NTR1                                                                   
         LA    R2,PGMPRD1H                                                      
         LA    R3,PGMENLSH                                                      
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
         BNH   CLRSCR10            NO, CONTINUE                                 
*                                                                               
         MVC   PGMUSER,SPACES           CLEAR MASTER PRODUCT FIELD              
         OI    PGMUSERH+6,X'80'                                                 
*                                                                               
         MVC   PGMADD1,SPACES           CLEAR ADDRESS FIELDS                    
         OI    PGMADD1H+6,X'80'                                                 
         MVC   PGMADD2,SPACES                                                   
         OI    PGMADD2H+6,X'80'                                                 
         MVC   PGMADD3,SPACES                                                   
         OI    PGMADD3H+6,X'80'                                                 
         MVC   PGMADD4,SPACES                                                   
         OI    PGMADD4H+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        COPY MEDIA T FOR CANADIAN TO C AND N                                   
***********************************************************************         
*                                                                               
COPYMEDT NTR1                                                                   
         L     R4,AIO1                                                          
         L     RE,AIO2                  COPY NEW REC TO AIO2                    
         L     R0,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(R4)              LENGTH OF REC                           
         LR    R1,RF                                                            
         MVCL  RE,R0               COPY AIO1 TO AIO2                            
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   CP50                                                             
*                                                                               
CP10     DS    0H                                                               
         L     R4,AIO2                  NEW MEDIA REC                           
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'03'              FOR NETWORK                             
         MVC   AIO,AIO2                                                         
         GOTO1 ADDREC                                                           
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'08'              FOR COMBINED                            
         GOTO1 ADDREC                                                           
         B     CPX                                                              
*                                                                               
CP50     MVC   KEY(13),SAVEKEY                                                  
         LA    R5,KEY                                                           
*                                                                               
         NI    2(R5),X'F0'                                                      
         OI    2(R5),X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE          FOUND MEDIA N RECORD?                   
         BNE   ERRINV                                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
CP60     MVC   AIO,AIO3                                                         
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
         CLC   KEY(13),KEYSAVE          FOUND MEDIA C RECORD?                   
         BNE   ERRINV                                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO2                  NEW MEDIA REC                           
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'08'              FOR NETWORK                             
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC                                                           
*                                                                               
CPX      MVC   KEY,SAVEKEY         RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE GROUP NUMBER                                                  
***********************************************************************         
*                                                                               
VALGRPNM NTR1                                                                   
         MVI   BKFLG,0                                                          
         LA    R1,9(R2)                 POINT TO FIRST BYTE                     
*                                                                               
         LLC   RE,5(R2)                 GET LENGTH OF GROUP # INPUT             
         BCTR  RE,0                     DECREMENT ONCE FOR GROUP ID             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),=C'000'          CAN'T BE ALL ZERO'S                     
         BE    ERRINV                                                           
*                                                                               
         CLC   0(3,R1),=C'999'                                                  
         BE    ERRINV                                                           
*                                                                               
*        CHECK EACH BYTE FOR NUMERIC                                            
         LA    RE,3                                                             
*                                                                               
VGP10    DS    0H                                                               
         CLI   0(R1),0                  WHEN NO INPUT FOR NEXT BYTE             
         BE    VGP20                    DON'T LOOK AT OTHER BYTES               
         CLI   0(R1),C'0'                                                       
         BL    ERRINV                                                           
         CLI   0(R1),C'9'                                                       
         BH    ERRINV                                                           
         LA    R1,1(R1)                 POINT TO NEXT BYTE                      
         BCT   RE,VGP10                                                         
*                                                                               
VGP20    XC    DUB,DUB                                                          
         LLC   R1,5(R2)                 GET LENGTH OF GROUP # INPUT             
         BCTR  R1,0                     DECREMENT ONCE FOR GROUP ID             
         STC   R1,HALF+1                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),9(R2)                                                     
         PACK  PGRPNM(2),DUB(3)                                                 
         NI    PGRPNM+1,X'FF'-X'0F'     TURN OFF LOW NIBBLE = SIGN              
*                                                                               
* READ X'0000' REC'S                                                            
         LA    R4,KEY              BUILD PRODUCT GROUP DEF. KEY                 
         USING PRGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,BAGYMD                                                  
         MVC   PRGKCLT,BCLT             BINARY CLIENT CODE                      
         MVC   PRGKID,PGRPID                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE          FOUND DEF REC?                          
         BE    VGP30                    YES                                     
         CLI   ACTEQU,ACTLIST           NO, LIST?                               
         BE    VGPX                     YES, ANY INPUT OK                       
         B     ERRINV                   NOT LIST, DEF REC NOT FOUND             
*                                                                               
VGP30    MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         USING PRGEL01,R6                                                       
         CLI   PRGBK2LN,0                IS BREAK LEN 2 PRESENT?                
         BE    *+8                      NO, DON'T SET FLAG                      
         OI    BKFLG,X'08'              FLAG MEANS SECOND BREAK PRESENT         
*                                                                               
         LLC   R0,PRGBK1LN                                                      
         LLC   R1,PRGBK2LN                                                      
         AR    R0,R1                                                            
         STC   R0,HALF                                                          
         CLC   HALF(1),HALF+1                                                   
         BNE   ERRINV                                                           
*                                                                               
         DROP  R4                                                               
VGPX     B     XIT                      YES, DONE                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY GROUP NUMBERS                                                         
***********************************************************************         
*                                                                               
GETGPNM  NTR1                                                                   
         MVI   BKFLG,0                                                          
*                                                                               
         XC    KEY,KEY                                                          
         XC    SAVGNUM,SAVGNUM                                                  
         MVC   KEY(6),SAVEKEY2          GET DEF REC                             
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),SAVEKEY2          FOUND DEF REC?                          
         BE    GGN10                    YES, SKIP                               
         CLI   ACTEQU,ACTLIST           LIST?                                   
         BNE   *+14                     NO                                      
         MVC   SAVGNUM,=C'???'          LIST, INDICATE MISSING DEF REC          
         B     GGNX                                                             
         B     ERRINV                   NOT LIST, NO DEF'N REC, ERROR           
*                                                                               
GGN10    MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
*        BE    *+6                 REQUIRED ELEMENT                             
*        DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING PRGEL01,R6                                                       
*                                                                               
         LLC   R0,PRGBK1LN                                                      
         LLC   RE,PRGBK2LN                                                      
         AR    R0,RE                                                            
         STC   R0,BYTE2            SAVE TOTAL DIGITS                            
*                                                                               
         CLI   PRGBK2LN,0                IS BREAK LEN 2 PRESENT?                
         BE    *+8                      NO, DON'T SET FLAG                      
         OI    BKFLG,X'08'              FLAG MEANS SECOND BREAK PRESENT         
*                                                                               
         USING PRGRECD,R6                                                       
         LA    R6,SAVEKEY2              POINT TO PRINTED RECORD                 
*                                                                               
         UNPK  DUB,PRGKGRP(3)                                                   
         LLC   RE,BYTE2            GET TOTAL DIGITS                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVGNUM(0),DUB+3 ** EXECUTED **                                  
         MVC   AIO,AIO1                                                         
         DROP  R6                                                               
GGNX     B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MASTER PRODUCT                                                
***********************************************************************         
*                                                                               
VALMPRD  NTR1                                                                   
         XC    INPRD,INPRD                                                      
         MVC   INPRD,8(R2)                                                      
         OC    INPRD,SPACES                                                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         MVC   PKEYAM,SVBAGYMD                                                  
         MVC   PKEYCLT,SVBCLT                                                   
         MVC   PKEYPRD,INPRD                                                    
*                                                                               
         GOTO1 HIGH                     READ??                                  
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRINV                                                           
*                                                                               
VMPX     B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SINGLE PRODUCT                                                
***********************************************************************         
*                                                                               
VALPRD   NTR1                                                                   
         XC    DELGRP,DELGRP                                                    
         XC    INPRD,INPRD                                                      
         MVC   INPRD,8(R2)                                                      
         OC    INPRD,SPACES                                                     
*                                                                               
         XC    KEY,KEY                  READ X'00' PRODUCT RECORDS              
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         MVC   PKEYAM,SVBAGYMD                                                  
         MVC   PKEYCLT,SVBCLT                                                   
         MVC   PKEYPRD,INPRD                                                    
         DROP  R4                                                               
*                                                                               
         GOTO1 READ                     PRODUCT EXISTS FOR AGYMD/CLT?           
*                                                                               
* TEST FOR DUPLICATE ASSIGN                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD PRODUCT GROUP DEF. KEY                 
         USING PRGRECD,R4                                                       
         MVC   PRGPTYP,=X'0D81'         PASSIVE KEY                             
         MVC   PRGPAGMD,SVBAGYMD                                                
         MVC   PRGPCLT,SVBCLT                                                   
         MVC   PRGPID,PGRPID                                                    
         MVC   PRGPGRP,PGRPNM                                                   
         MVC   PRGPPRD,INPRD                                                    
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
VLPX     B     XIT                      PRODUCT IS VALID INPUT                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*  DELETE PRODUCT IF DDS TERMINAL ONLY                                          
***********************************************************************         
*                                                                               
DELTPRD  NTR1                                                                   
         MVC   TEMPMED,SVBAGYMD                                                 
*                                                                               
DP5      XC    KEY,KEY                  READ X'00' PRODUCT RECORDS              
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         MVC   PKEYAM,TEMPMED                                                   
         MVC   PKEYCLT,SVBCLT                                                   
         MVC   PKEYPRD,INPRD       LOOK FOR PRD FOR DELETION                    
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'Y'            READ FOR UPDATE                         
         GOTO1 READ                     SHOULD BE RIGHT REC CAUSE OF VR         
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   DISKADD,KEY+14           SAVE D/A FOR ADDING P-KEYS              
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDR,R6                                                        
         XC    SVPGRPS(15),SVPGRPS      SAVE OLD PRD GRP ASSIGNS                
         MVC   SVPGRPS(9),PGRP1                                                 
         MVC   SVPGRPS+9(6),PGRP4                                               
         MVC   SVPGRPS+15(15),PGRP6                                             
*                                                                               
         LA    R4,SAVEKEY                                                       
         USING PRGRECD,R4                                                       
*                                                                               
         LA    R0,10                                                            
         LA    R1,SVPGRPS                                                       
DP10     CLC   0(3,R1),PRGKID      CHECK FOR SAME ID AND GROUP                  
         BE    DP25                                                             
         LA    R1,3(R1)                                                         
         BCT   R0,DP10                                                          
         B     ERRINV              PRD GRP NOT FOUND, ERROR!                    
*                                                                               
DP25     MVC   DELGRP,0(R1)        SAVE GROUP FOR DELETION                      
*                                                                               
         XC    0(3,R1),0(R1)       CLEAR GROUP FROM PRD REC                     
         MVC   PGRP1(9),SVPGRPS         RESTORE TABLE TO PRD REC                
         MVC   PGRP4(6),SVPGRPS+9                                               
         MVC   PGRP6(15),SVPGRPS+15                                             
         DROP  R4,R6                                                            
*                                                                               
         MVI   ACTELOPT,C'N'           DON'T ADD ACTIV ELEM                     
*                                                                               
*** USE DMWRITE CAUSE IT'S AN OLD RECORD WITH NO ELEMENTS                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE',DISKADD,AIO                   
         MVI   ACTELOPT,C'Y'                                                    
*                                                                               
*** DELETE PASSIVE KEY                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD  P-KEY, PTS TO PROD REC                
         USING PRGRECD,R4                                                       
         MVC   PRGPTYP,=X'0D81'         PASSIVE KEY                             
         MVC   PRGPAGMD,TEMPMED                                                 
         MVC   PRGPCLT,SVBCLT                                                   
         MVC   PRGPID(3),DELGRP                                                 
         MVC   PRGPPRD,INPRD                                                    
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DP30                     NO P-KEY FOUND                          
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                                                            
*                                                                               
* IF CANADIAN AGY AND MEDIA T, DO MEDIA C AND N TOO                             
*                                                                               
DP30     CLI   SVAPROF+7,C'C'      CANADIAN                                     
         BNE   DP50                                                             
         CLI   MEDIA,C'T'          FOR TV                                       
         BNE   DP50                                                             
*                                                                               
         MVC   BYTE,TEMPMED        SEE WHICH MEDIA I JUST FINISHED              
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'08'          LAST WAS COMBINED - SO DONE                  
         BE    DP50                                                             
         CLI   BYTE,X'03'                                                       
         BE    DOCOMB2             LAST WAS NETWORK - DO COMBINED               
*                                                                               
         NI    TEMPMED,X'F0'                                                    
         OI    TEMPMED,X'03'         LAST WAS TV - DO NETWORK                   
         B     DP5                                                              
*                                                                               
DOCOMB2  NI    TEMPMED,X'F0'                                                    
         OI    TEMPMED,X'08'         LAST WAS NETW - DO COMBINED                
         B     DP5                                                              
*                                                                               
DP50     MVI   RDUPDATE,C'N'                                                    
         NI    DMINBTS,X'FF'-X'08'                                              
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),SAVEKEY                                                  
DPX      B     XIT                                                              
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
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        USERCHK SUBROUTINE                                           *         
***********************************************************************         
USERCHK  NTR1                                                                   
*                                                                               
         OC    ASECBLK,ASECBLK                                                  
         BZ    UCHKNO              NO SECURITY NO CHANGE                        
**       DC    H'0'                                                             
         L     R1,ASECBLK                                                       
         USING SECD,R1                                                          
         MVC   PIDNAME,SECPID                                                   
         DROP  R1                                                               
*                                                                               
         BRAS  RE,CHECKPID         NEED THE 2 BYTE PID NOW                      
         BNE   UCHKNO                                                           
*                                                                               
         XC    KEY2,KEY2                                                        
         MVC   KEY2(L'KEY),KEY                                                  
         XC    KEY,KEY                                                          
         CLC   SAVEKEY(2),=X'0D01'                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(6),SAVEKEY      CLIENT GROUP DEFINITION RECORD KEY           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      SHOULD BE THERE                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING PRGRECD,R6                                                       
         L     R6,AIO2             WE NEED THE GROUP DEFINITION RECORD          
         MVI   ELCODE,PRGSCMCQ     X'12' SECURITY MANAGER ID ELEMENT            
         BRAS  RE,GETEL                                                         
         BNE   UCHKYES             NO RESTRICTION, WE'RE GOOD TO GO             
         USING PRGSCMD,R6                                                       
         OC    PRGSCM(12),PRGSCM   ANYTHING HERE?                               
         BZ    UCHKYES              - NOPE, WE OK                               
*                                                                               
         LA    RF,6                BCT LOOP 6 TIMES                             
         LA    R2,PRGSCM1          START WITH 1ST MANAGER                       
*                                                                               
UCHK50   CLC   PIDNUM,0(R2)        SAME PERSON?                                 
         BE    UCHKYES              - YUP, WE GOOD                              
         LA    R2,L'PRGSCM(R2)     BUMP NOW                                     
         BCT   RF,UCHK50                                                        
*                                                                               
UCHKNO   LTR   RE,RE                                                            
         B     *+6                                                              
*                                                                               
UCHKYES  CR    RE,RE                                                            
         J     XIT                                                              
         DROP  R6                                                               
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
         SSPEC H1,30,C'PRG LIST'                                                
         SSPEC H2,30,C'--------'                                                
         SPACE 1                                                                
         SSPEC H4,1,C'MD'                                                       
         SSPEC H4,5,C'CLI'                                                      
         SSPEC H4,10,C'ID'                                                      
         SSPEC H4,14,C'GRP'                                                     
         SSPEC H4,19,C'BREAK NAME1'                                             
         SSPEC H4,46,C'BREAK NAME2'                                             
         SPACE 1                                                                
         SSPEC H5,1,C'--'                                                       
         SSPEC H5,5,C'---'                                                      
         SSPEC H5,10,C'--'                                                      
         SSPEC H5,14,C'---'                                                     
         SSPEC H5,19,C'-----------'                                             
         SSPEC H5,46,C'-----------'                                             
         SPACE 1                                                                
         DC    X'00'                                                            
**** TABLE FOR MEDIA AND ELEMENT CODES ****                                     
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
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMFED               MAINT SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMFDD               LIST SCREEN                             
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
CLIENT   DS    CL3                                                              
MEDIA    DS    CL1                                                              
MEDX     DS    XL1                                                              
AGYX     DS    XL1                                                              
AGYHI    DS    XL1                                                              
PGRPID   DS    CL1                      PRODUCT GROUP ID                        
PGRPNM   DS    H                        PRODUCT GROUP NUMBER                    
PIDNAME  DS    CL8                 USER'S PIDNAME                               
PIDNUM   DS    XL2                 SAVE OFF USER'S PID                          
SECALPHA DS    CL2                 SECURITY AGENCY ALPHA                        
KEY2     DS    CL50                                                             
SAVEKEY  DS    CL13                                                             
SAVEKEY2 DS    CL13                                                             
SAVEKEY3 DS    CL13                                                             
SVBAGYMD DS    XL1                      SAVED AGENCY/MEDIA CODE                 
SVBCLT   DS    XL2                      SAVED CLIENT CODE                       
BYTE2    DS    XL1                      STORE TOTAL BK LN                       
BKFLG    DS    XL1                      BREAK FLAG                              
INPRD    DS    CL3                      INPUT PRODUCT                           
TEMPMED  DS    XL1                      TEMPORARY MEDIA                         
*                                                                               
         DS    0H                                                               
ADPRDS   DS    10CL3                                                            
ADPRDEND DS    0H                                                               
ADPRDLQ  EQU   ADPRDEND-ADPRDS                                                  
*                                                                               
*VPGRPS  DS    5CL3                     SAVED PRODUCT GROUP ASSIGNS             
SVPGRPS  DS    10CL3               INCREASED FROM 5 TO 10                       
DELGRP   DS    CL3                      SAVED PRD GRP ASSIGN FOR DEL            
DPRDFLG  DS    CL1                      DELETE PRODUCT FLAG                     
XRECFLG  DS    CL1                      VISITED XREC FLAG                       
SAVGNUM  DS    CL3                      SAVED DISPLAYABLE GROUP #               
SAVNAM   DS    CL24                SAVED BREAK NAME FOR MESSSAGES               
DISKADD  DS    F                        SAVED DISK ADDRESS                      
TEMPFLD  DS    XL11                                                             
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
MISCFLG1 DS    CL1                 MISCELLANEOUS FLAG 1                         
MF1UPD   EQU   X'80'                - RECORD IS UPDATED                         
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
LSPGNUM  DS    CL3                                                              
         DS    CL2                                                              
LSBKNM1  DS    CL24                                                             
         DS    CL3                                                              
LSBKNM2  DS    CL24                                                             
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107SPSFM4E   11/06/18'                                      
         END                                                                    
