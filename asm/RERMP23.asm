*          DATA SET RERMP23    AT LEVEL 098 AS OF 02/19/10                      
*PHASE T81023C                                                                  
*INCLUDE DAYUNPK                                                                
*INCLUDE MILEDIT                                                                
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - HISTORY'               
*                                                                               
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* OCT26/90 (MRR) --- MODIFY DBLOCK AND PRINT ROUTINE FOR 1 DECIMAL*             
*                                                                 *             
* APR16/09 (KUI) --- SUPPORT NEW INVENTORY KEY                    *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T81023   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 00,RERMP23*,RR=RE                                                
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKING STORAGE             
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             ESTABLISH TWA                                
         USING T810FFD,RA                                                       
*                                                                               
         L     R8,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
*                                                                               
         ST    RE,RELO23                                                        
*                                                                               
         L     R7,=A(WORK23)       ESTABLISH PROGRAM WORKAREA                   
         A     R7,RELO23           RE-LOCATE ADDRESS                            
         USING WORK23,R7           INCLUDES COMMON ROUTINES                     
*                                                                               
         L     R5,ATWA                                                          
         USING FA$TWAD,R5                                                       
         MVC   USERID,FA$TWAUSRID                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
*                                                                               
*        INIT PRINT LINES                                                       
*                                                                               
         LA    R0,9                MAX 9 PRINT LINES                            
         LA    R3,PL1              FIRST PRINT LINE                             
*                                                                               
         MVC   0(132,R3),SPACES    INIT PRINT LINES                             
         LA    R3,132(R3)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - PRMODE'                
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              DETERMINE MODES HANDLED BY PROGRAM                 *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
PRMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PRNTREP                                                          
*                                                                               
         XIT1                      IGNORE ALL OTHERS                            
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - PRNTREP'               
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              PRINT THE REPORT                                   *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
PRNTREP  DS    0H                                                               
*                                                                               
         LA    R2,HOOK             SET PRINT HEADHOOK                           
         ST    R2,HEADHOOK                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,CMPTODAY)  COMPRESS DATE               
*                                                                               
         MVI   RCSUBPRG,0                                                       
*                                                                               
         MVC   CPARREP,AGENCY      SAVE REP ID                                  
*                                                                               
*        FIND PARENT REP ID IN REP RECORD                                       
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH REPREC KEY                         
         USING RREPKEY,R4                                                       
*                                                                               
         MVI   RREPKTYP,X'01'      SET RECORD ID                                
         MVC   RREPKREP,CPARREP    SET REP ID                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 READ                READ RECORD POINTER                          
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         MVI   ELCODE,X'01'        SET TO FIND REP ELEMENT                      
         LA    R6,IO                                                            
*                                                                               
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST FIND ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         USING RREPELEM,R6         ESTABLISH REP ELEMENT                        
         MVC   CPARREP,RREPPAR     SAVE PARENT REP ID FOR INV RECORD            
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - GTPROF'                
********************************************************************            
*                                                                  *            
*        GTPROF --- GET AND SET SFM/REP PROFILES FROM THE REP REC  *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
GTPROF   DS    0H                                                               
*                                                                               
         XC    RMPPROF,RMPPROF     INIT RMP PROGRAM PROFILE                     
*                                                                               
         MVI   ELCODE,X'04'        SET TO FIND PROGRAM PROFILE ELM              
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         BNE   GTPROFX             NO PROFILE FOUND                             
*                                                                               
*- FIND RMP PROGRAM PROFILE WITHIN PROGRAM PROFILE ELEMENT                      
*                                                                               
         USING RREPPGMP,R6         ESTABLISH PROGRAM PROFILE ELEMENT            
*                                                                               
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
*                                                                               
         CLI   0(RE),RREPQRMP      LOOKING FOR RMP PROGRAM PROFILE              
         BE    *+16                                                             
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,*-12                                                          
         B     GTPROFX             NO MATCH                                     
*                                                                               
         MVC   RMPPROF,2(RE)       SAVE PROFILE                                 
*                                                                               
GTPROFX DS     0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         L     R2,=A(STACK)        SET INTERNAL ADDRESSES                       
         A     R2,RELO23                                                        
         ST    R2,ASTACK                                                        
*                                                                               
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2                                                      
*                                                                               
         MVC   DEMOMATH,CDEMOMTH                                                
         MVC   DEMAINT,CDEMAINT                                                 
*                                                                               
         DROP  R2                                                               
*                                                                               
         L     R2,=V(DAYUNPK)                                                   
         A     R2,RELO23                                                        
         ST    R2,DAYUNPK                                                       
*                                                                               
         L     R2,=V(MILEDIT)                                                   
         A     R2,RELO23                                                        
         ST    R2,MILEDIT                                                       
*                                                                               
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - MAIN'                  
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              MAIN PRINT LOOP                                    *             
*              REPEAT FOR EACH DAYPART                            *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
PRTREP   DS    0H                                                               
*                                                                               
         LA    R3,STLIST           POINT TO LIST OF STATIONS                    
*                                                                               
PRPSTALP DS    0H                                                               
*                                                                               
         USING STLISTD,R3          ESTABLISH ENTRY IN STATION LIST              
*                                                                               
         ST    R3,ASTA             SET A(STATION)                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    PRPSTADN                                                         
*                                                                               
         LA    R2,DPLIST           POINT TO DAYPARTS TO BE LISTED               
         LA    R4,L'DPLIST         MAX NUMBER OF DAYPARTS                       
*                                                                               
PRDPTLP  DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE IF END OF DPTS REACHED                  
         BE    PRDPTDN                                                          
*                                                                               
         ST    R2,ADPT             SAVE LIST POINTER                            
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE AND HEADLINES                 
         MVC   PAGE,=H'1'                                                       
*                                                                               
         GOTO1 =A(PRPRNT),RR=RELO23                                             
*                                                                               
PRDPTCN  DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            POINT TO NEXT DAYPART IN LIST                
         BCT   R4,PRDPTLP                                                       
*                                                                               
PRDPTDN  DS    0H                  DONE IF NO MORE DAYPARTS                     
*                                                                               
         OC    ABOX(4),ABOX                                                     
         BZ    PRBOXX              ON-LINE, NO BOX TO SET                       
*                                                                               
         L     R4,ABOX             ESTABLISH BOX AREA                           
         USING BOXD,R4                                                          
*                                                                               
         CLI   BOXYORN,C'Y'        SKIP UNLESS USING BOXES                      
         BNE   PRBOXX                                                           
*                                                                               
         CLI   BOXSTAT,C'I'        SKIP UNLESS INSIDE A BOX                     
         BNE   PRBOXX                                                           
*                                                                               
         MVI   BOXREQ,C'C'         CLOSE ANY BOXES                              
         MVI   FORCEHED,C'N'       NO NEW PAGE AND HEADLINES                    
*                                                                               
         BAS   RE,SPLAT                                                         
*                                                                               
PRBOXX   DS    0H                                                               
*                                                                               
*        CHECK IF A LOCK NEEDS TO BE RELEASED                                   
*                                                                               
         TM    WHEN,X'20'          SKIP IF NOT SOON                             
         BNO   PRLCKX                                                           
*                                                                               
         MVC   CSTAT,STLSSTAC      PASS STATION CALL LETTERS                    
*                                                                               
         GOTO1 CHKLOCK,DMCB,('LKUNLKQ',0)  UNLOCK STATION                       
*                                                                               
         BE    *+6                 TEST FOR ERRORS                              
         DC    H'0'                                                             
*                                                                               
PRLCKX   DS    0H                                                               
*                                                                               
PRPSTACN DS    0H                                                               
*                                                                               
         LA    R3,STLISTL(R3)      BUMP TO NEXT STATION IN LIST                 
         B     PRPSTALP                                                         
*                                                                               
PRPSTADN DS    0H                                                               
*                                                                               
PRTREPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - PRPRNT'                
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              BUILD A TABLE OF DISK ADDRESSES OF RECORDS TO      *             
*                 BE INCLUDED IN REPORT.                          *             
*              RECORDS ARE FOUND USING DAYPART PASSIVE POINTER    *             
*                 FOR INVENTORY RECORD                            *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
PRPRNT   NTR1                      BUILD A STACK OF D/A                         
*                                                                               
*        BUILD A STACK OF D/A FOR INVENTORY RECORDS                             
*                                                                               
         L     R5,ASTACK           INIT NEXT SLOT IN STACK POINTER              
         SR    R6,R6               INIT NUMBER IN STACK                         
*                                                                               
*        READ INVENTORY RECORDS BY DAYPART POINTERS                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH INV REC DPT PASSIVE KEY            
         USING RIDPKEY,R4                                                       
*                                                                               
         MVI   RIDPKTYP,RIDPKTYQ   SET RECORD ID                                
         MVC   RIDPKREP,CPARREP    SET REP                                      
*                                                                               
         L     R3,ASTA             POINT TO CURRENT STATION                     
         USING STLISTD,R3          ESTABLISH LIST ENTRY                         
*                                                                               
         MVC   RIDPKSTA,STLSSTAC   SET STATION                                  
         CLI   RIDPKSTA+4,C' '     BLANK MEANS TV                               
         BH    *+8                                                              
         MVI   RIDPKSTA+4,C'T'                                                  
*                                                                               
         L     R2,ADPT             POINT TO CURRENT DAYPART                     
         MVC   RIDPKDPT,0(R2)      SET DAYPART                                  
*                                                                               
         GOTO1 HIGH                READ FIRST POINTER                           
*                                                                               
PRSTCKLP DS    0H                                                               
*                                                                               
         CLC   KEYSAVE(RIDPKDAY-RIDPKEY),KEY   IF REP/STA/DPT CHANGES           
         BNE   PRSTCKDN                       DONE                              
*                                                                               
*        FILTER ON INVENTORY NUMBERS                                            
*                                                                               
         LA    RF,INVLIST          POINT TO INVENTORY NO. FILTERS               
*                                                                               
         CLC   0(L'RIDPKINV,RF),SPACES  SKIP FILTER IF NONE                     
         BNH   PRSINVOK                                                         
*                                                                               
PRSINVLP DS    0H                                                               
*                                                                               
         CLC   0(L'RIDPKINV,RF),SPACES  CHECK FOR EOL                           
         BNH   PRSINVDN                                                         
*                                                                               
         CLC   RIDPKINV,0(RF)      INV NO MUST LIE IN RANGE                     
         BL    PRSINVCN                                                         
         CLC   RIDPKINV,L'RIDPKINV(RF)                                          
         BH    PRSINVCN                                                         
*                                                                               
         B     PRSINVOK                                                         
*                                                                               
PRSINVCN DS    0H                                                               
*                                                                               
         LA    RF,2*L'RIDPKINV(RF)  BUMP TO NEXT INV NO IN FILTER               
         B     PRSINVLP                                                         
*                                                                               
PRSINVDN DS    0H                                                               
*                                                                               
         B     PRSTCKCN            FAILS INVENTORY NUMBER FILTER                
*                                                                               
PRSINVOK DS    0H                                                               
*                                                                               
*        INVENTORY NUMBER PASSED FILTER                                         
*                                                                               
         CLC   RIDPKSTD,STRTOPT    TIME MUST FIT REQUESTED TIME PERIOD          
         BL    PRSTCKCN                                                         
         CLC   RIDPKSTD,ENDOPT                                                  
         BH    PRSTCKCN                                                         
*                                                                               
         CLI   RIDPKINV,C' '       KEEP ONLY NEW FORMAT RECORDS                 
         BL    PRSTCKCN                                                         
*                                                                               
         MVC   0(4,R5),KEY+28      SAVE DISK ADDRESS                            
*                                                                               
         LA    R5,4(R5)            BUMP STACK POINTER                           
         LA    R6,1(R6)            BUMP DISK ADDRESS COUNTER                    
*                                                                               
PRSTCKCN DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT POINTER                            
*                                                                               
         B     PRSTCKLP                                                         
*                                                                               
PRSTCKDN DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
         LTR   R6,R6               IF STACK IS EMPTY                            
         BZ    PRPRNTX                ALL DONE                                  
*                                                                               
         STM   R5,R6,SAVESTAK      SAVE STACK POINTERS                          
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - PRINV'                 
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              READ INVENTORY RECORDS USING DISK ADDRS IN STACK   *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
PRINV    DS    0H                                                               
*                                                                               
         L     R5,ASTACK           POINT TO FIRST DISK ADDR IN STACK            
         L     R6,SAVESTAK+4       GET NUMBER OF ITEMS IN STACK                 
*                                                                               
PRINVLP  DS    0H                                                               
*                                                                               
         STM   R5,R6,SAVESTAK      SAVE CURRENT STACK POINTERS                  
*                                                                               
         MVC   KEY+28(4),0(R5)     SET NEXT DISK ADDR IN KEY                    
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         MVC   KEY(27),IO          SAVE KEY OF FOUND RECORD                     
*****    LA    R4,IO                                                            
         USING RINVKEY,R4          ESTABLISH AS INVENTORY REC KEY               
*                                                                               
         LA    R6,IO               SEARCH INVENTORY HEADER RECORD               
         MVI   ELCODE,X'01'        FIND PROGRAM ELEMENT (HEADER)                
         BAS   RE,GETEL                                                         
*                                                                               
         USING RINVPEL,R6          ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         MVC   SVTIMCHG,RINVTCHG   SAVE FOR DEMO LOOKUPS                        
         TM    RINVGPRO,X'80'      SKIP IF PROTECTED FROM TRANSFER              
         BO    PRINVCN                                                          
*                                                                               
*        SUPPRESS PRINT IN MULTIPLE DPTS                                        
*                                                                               
         CLI   DPLIST+1,0         NO CHANCE OF DUPLICATE IF ONLY 1 DPT          
         BE    PRINVDPD              REQUESTED                                  
*                                                                               
         GOTO1 =A(CHKDPT),RR=RELO23  CHECK DAYPART LISTS FOR DUPES              
         BNE   PRINVCN               DROP INVENTORY RECORD                      
*                                                                               
PRINVDPD DS    0H                                                               
*                                                                               
*        TEST IF PROGRAM FILTER COMPATABLE WITH ASKED FOR FILTER                
*                                                                               
         LA    R1,RINVPFLT         POINT TO PROGRAM FILTER                      
         LA    R5,TITFILT          POINT TO ASKED FOR FILTER                    
         LA    R0,6                MAX NUMBER OF FILTER POSTIONS                
*                                                                               
PRFLTLP  DS    0H                                                               
*                                                                               
         CLI   0(R5),C'A'          IF NOTHING IS SPECIFIED FOR PROGRAM          
         BL    PRFLTCN                IGNORE                                    
*                                                                               
         CLC   0(1,R5),0(R1)       ELSE MUST MATCH ENTERED FILTER               
         BNE   PRINVCN                SKIP RECORD                               
*                                                                               
PRFLTCN  DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP TO NEXT SPOT IN PROGRAM FILTER          
         LA    R5,1(R5)            BUMP TO NEXT SPOT IN ENTERED FILTER          
         BCT   R0,PRFLTLP                                                       
*                                                                               
PRFLTDN  DS    0H                                                               
*                                                                               
*        IF PROGRAM HAS AN EFFECTIVE DATE RANGE THEN END DATE                   
*              MUST BE AFTER TODAY                                              
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2   IF NO EFFECTIVE END DATE              
         BZ    *+14                          SKIP TEST                          
         CLC   RINVPEFF+2(2),CMPTODAY IF EFFECTIVE END DATE AFTER TODAY         
         BL    PRINVCN                 REJECT RECORD                            
*                                                                               
*****SMTEST   DC    H'0'                                                        
*                                                                               
         GOTO1 =A(PRHDR),RR=RELO23    FORMAT INVENTORY DETAILS                  
*                                                                               
         GOTO1 =A(PURE),RR=RELO23     LOOK FOR PURE DETAILS                     
*                                                                               
         MVI   SPACING,2           DOUBLE SPACE BEFORE PRINTING                 
         BAS   RE,SPLAT            PRINT LINE                                   
*                                                                               
         CLI   TRACOPT,C'Y'        IF TRACE OPTION ON                           
         BNE   PRINVCN                                                          
         GOTO1 =A(TRACE),RR=RELO23    PRINT TRACE DATA                          
*                                                                               
PRINVCN  DS    0H                                                               
*                                                                               
         LM    R5,R6,SAVESTAK      GET STACK POINTERS                           
         LA    R5,4(R5)            BUMP TO NEXT IN STACK                        
         BCT   R6,PRINVLP                                                       
*                                                                               
PRINVDN  DS    0H                                                               
*                                                                               
PRPRNTX  DS    0H                                                               
         XIT1                                                                   
         SPACE 1                                                                
TRACOPT  DC    C'N'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - PRHDR'                 
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              FORMAT HEADER DETAILS                              *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
PRHDR    NTR1                                                                   
*                                                                               
         LA    R4,IO               ESTABLISH INVENTORY RECORD                   
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   PL1+1(4),RINVKINV   PRINT INVENTORY NUMBER                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,PL1+6) EFFECTIVE START DATE          
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2   IF NO EFFECTIVE END DATE              
         BZ    PRHDR2                                                           
         CLC   RINVPEFF(2),RINVPEFF+2     OR IT IS SAME AS START DATE           
         BE    PRHDR2                        SKIP PRINTING END DATE             
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(8,PL2+6) PRINT EFFECTIVE END         
*                                                                               
PRHDR2   DS    0H                                                               
*                                                                               
*        PROGRAM DAY/TIMES                                                      
*                                                                               
         NI    FLAG1,X'FF'-MHONQ   INIT FLAG FOR MULTI-HUT                      
*                                                                               
*   SINGLE (OR MULTI, IF FOUND) X'02' ELEMENTS MUST GENERATE AN                 
*        X'CE' ELEMENT.  THEN, THE MULTIPLE X'09' ELEMENTS                      
*        MUST BE PROCESSED SEPARATELY.                                          
*                                                                               
***      LA    R6,IO                                                            
***      MVI   ELCODE,X'09'        LOOK FOR A MULTI HUT ELEMENT                 
***      BAS   RE,GETEL                                                         
***      BE    PRHDR4             IF FOUND, USE ALTERNATE "LOGIC"               
*                                 ELSE, CONTINUE                                
*                                                                               
*    ORIGINAL SINGLE DAY TIME LOGIC (SHOULD ONLY BE 1 X'02')                    
*     ADDITIONAL DAYS WILL BE DENOTED BY ONE OR MORE X'09'S                     
*                                                                               
         LA    R3,PL1              STARTING PRINT POSITION                      
         LA    R5,CEELS            X'CE' ELMS AREA                              
         XC    CEELS(100),CEELS    INIT AREA                                    
         XC    X09ELS,X09ELS       ENSURE EMPTY                                 
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'        SET TO FIND A DAY/TIME ELEMENTS              
*                                                                               
         BAS   RE,GETEL            FIND FIRST DAY/TIME ELEMENT                  
         BNE   PRHWDTDN            NONE FOUND - SKIP PRINTING - IF NO           
*                                     X'02'S, THERE WILL BE NO X'09'S           
*                                                                               
PRHWDTLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         GOTO1 UNDAY,PARAS,RIDTDAY,16(R3)      DAY                              
*                                                                               
         GOTO1 UNTIME,PARAS,RIDTTIME,26(R3)    TIME                             
*                                                                               
*        BUILD A DAY/TIME ELEMENT                                               
*                                                                               
         USING RINVZEL,R5          ESTABLISH ELEMENT                            
*                                                                               
         MVI   RINVZCOD,X'CE'      SET ELEMENT CODE TO 'CE'                     
         MVI   RINVZLEN,10         SET ELEMENT LENGTH TO 10                     
         MVC   RINVZDAY,RIDTDAY    SET DAYS                                     
         MVC   RINVZTIM,RIDTTIME   SET START AND END TIMES                      
         MVC   RINVZBK,CBOOKS      BOOK                                         
         NI    RINVZBK,X'FF'-X'08' TURN OFF TIME PERIOD BIT                     
         DROP  R5,R6                                                            
*                                                                               
PRHWDTCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP TO NEXT LINE                            
         LA    R5,10(R5)           BUMP TO NEXT ELMENT BUILD AREA               
*                                                                               
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
         BE    PRHWDTLP            ANOTHER X'02' FOUND                          
***      B     PRHWDTDN                                                         
*                                                                               
*   DAY TIME LOGIC FOR HANDLING INV WITH MULTI HUT DAY/TIMS                     
*                                                                               
PRHDR4   DS    0H                                                               
*                                                                               
***>>>   LA    R3,PL1              STARTING PRINT POSITION                      
         LA    R5,X09ELS           X'09' ELMS AREA                              
         XC    X09ELS,X09ELS   INIT ELEMENT LIST AREA                           
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'09'        SET FOR MULTI HUT ELEMENTS                   
*                                                                               
         BAS   RE,GETEL            FIND FIRST MH DAY/TIME ELEMENT               
         BNE   PRHWDTDN            NO X'09' ELEMENT FOUND                       
                                                                                
PRHDR4LP EQU   *                                                                
         MVC   0(7,R5),0(R6)       COPY ELEMENT INTO LIST                       
         LA    R5,7(R5)            BUMP TO NEXT LIST AREA                       
         XC    0(7,R5),0(R5)       CLEAR NEXT LIST AREA                         
         BAS   RE,NEXTEL                                                        
         BE    PRHDR4LP            GO BACK FOR NEXT                             
*                                                                               
PRHWDTDN DS    0H                  END OF DAY-TIME ELEMENTS                     
*                                                                               
*        PRINT PROGRAM NAMES                                                    
*                                                                               
         LA    R3,PL1              RESET LINE POINTER TO FIRST                  
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        FIND A PROGRAM ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   PRHWPGX                                                          
*                                                                               
PRHWPGLP DS    0H                                                               
*                                                                               
         USING RIPGELEM,R6         ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RIPGLEN          ELEMENT LENGTH                               
         SH    RF,=Y(RIPGNAME-RIPGELEM)  PROGRAM NAME LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   38(0,R3),RIPGNAME   PRINT PROGRAM NAME                           
*                                                                               
PRHWPGCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP POINTER                                 
         BAS   RE,NEXTEL           NEXT PROGRAM ELEMENT                         
         BE    PRHWPGLP            ONE FOUND                                    
*                                                                               
PRHWPGDN DS    0H                                                               
*                                                                               
PRHWPGX  DS    0H                                                               
*                                                                               
PRHDRX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - PURE'                  
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              ROUTINES TO FIND PURE DETAILS                      *             
*                                                                 *             
*NTRY    R4 ==> BASE INVENTORY RECORD                             *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
PURE     NTR1                                                                   
*                                                                               
         USING RINVREC,R4          ESTABLISH 1ST REC AS INVENTORY               
*                                                                               
         LA    R4,IO               POINT TO INVENTORY HEADER                    
         USING RINVREC,R4                                                       
*                                                                               
         CLI   RINVPAUT,C'N'       OPTION NOT TO TRANSFER AUTO.                 
         BE    PUREX                                                            
*                                                                               
         MVC   SVATD,RINVATD       SAVE AUTO TRANSFER DEFAULT                   
*                                                                               
         L     R4,=A(DATA)         BUILD NEW DATA RECORD                        
         A     R4,RELO23                                                        
         ST    R4,ADATAREC         SET POINTER TO DATA RECORD                   
*                                                                               
         LA    R1,2000(R4)         THEN SET ONE TO INTERIM RECORD AREA          
         ST    R1,AINTEREC                                                      
*                                                                               
         XCEF  (R4),4000           CLEAR RECORD AREAS                           
*                                                                               
         USING RINVREC,R4          ESTABLISH 1ST REC AS INVENTORY               
*                                                                               
         MVC   0(24,R4),IO         KEY AS FOR HEADER                            
*                                                                               
*        FIND KSRC                                                              
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIRSVC,TITSRCE     SET RATING SERVICE                           
         MVC   GSIBITS,CBOOKS      BOOKVAL BITS                                 
         MVC   GSIBKTYP,CBOOKS+3   BOOK TYPE                                    
*                                                                               
         CLI   GSIBKTYP,C'D'       IF D BOOK - MAKE 'S' TYPE BOOK               
         BNE   *+12                                                             
         OI    GSIBITS,X'02'          SET AS SPECIAL BOOK                       
         MVI   GSIBKTYP,0             NO LONGER D TYPE                          
*                                                                               
         MVC   RINVKRSR,TITSRCE    SET RTG IN KEY                               
         MVC   RINVKQLF,GSIBITS    SET QLF IN KEY                               
         MVC   RINVKBTP,GSIBKTYP   SET BTP IN KEY                               
*                                                                               
PURESRCX DS    0H                                                               
*                                                                               
         MVC   RINVKBK,CBOOKS+1    ADD BOOK                                     
         MVC   RINVLEN,=H'35'      RECORD LENGTH                                
*                                                                               
         MVI   TPCNT,0             SET COUNTERS                                 
         MVI   HITCNT,0                                                         
         MVI   PCNT,0                                                           
         MVI   VARSW,C'N'          SET MORE THAN 2 PROGRAM SWITCH               
         XC    PROGEL,PROGEL                                                    
         XC    DEMODUB,DEMODUB     CLEAR EXTRA STORAGE FOR DEMUP                
         XC    TOTSHR(12),TOTSHR   CLEAR SHARE ACCUMULATORS                     
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
*                                                                               
         XC    0(128,R1),0(R1)                                                  
*                                                                               
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'      RTG = 1 DECIMAL                               
         MVI   DBXTTSP,X'01'      SHR = 1 DECIMAL                               
         MVI   DBXTTIP,X'02'      IMP = 00'S                                    
*                                                                               
         LA    RF,DBXTTIDX         SET NEXT EXTENSION ADDRESS                   
         STCM  RF,15,DBXTNEXT                                                   
*                                                                               
         DROP  R1                                                               
*                                                                               
*   INSERT USERID INTO EXTENDED AREA                                            
*                                                                               
         L     R1,DBXTNEXT-DBXTTID+DBEXTRA1  A(NEXT EXTENSION AREA)             
         USING DBXTUIDD,R1                                                      
*                                                                               
         XC    0(128,R1),0(R1)                                                  
*                                                                               
         MVC   DBXID,=C'UID '              SPECIAL USER ID USED                 
         MVC   DBXBID,USERID               STORE BINARY USER ID                 
*                                                                               
*   TEST: FORCE USERID WHEN REQUESTOR = '*X*'                                   
         CLC   =C'*X*',CONWHEN+5                                                
         BNE   TEST0100                                                         
         MVC   DBXBID,=X'1B11'     FORCE USER ID                                
TEST0100 EQU   *                                                                
*   TEST END:                                                                   
*                                                                               
         LA    RF,DBXTTIDX-DBXTTID                                              
*                                  SET L(EXTENSION AREA)                        
         L     RE,DBXTNEXT-DBXTTID+DBEXTRA1                                     
*                                  SET A(THIS EXTENSION)                        
         AR    RF,RE               BUMP TO NEXT EXTENSION                       
         STCM  RF,15,DBXTNEXT-DBXTTID+DBEXTRA1                                  
*                                  SAVE A(NEXT EXTENSION)                       
*                                                                               
         DROP  R1                                                               
*                                                                               
*                                                                               
         MVC   DBSELAGY,CPARREP                                                 
*                                                                               
*        DETERMINE DEMO FILE TO USE                                             
*                                                                               
         TM    CBOOKS,X'08'        IF TP FILE NEEDED                            
         BNO   *+14                                                             
         MVC   DBFILE,=C'TP '         SET FILE                                  
         B     PUREFILX               AND ALL DONE                              
*                                                                               
         MVC   DBFILE,=C'PAV'      DEFAULT IS PAV FILE                          
*                                                                               
         TM    SVATD,X'80'         IF DEFAULT IS PAV                            
         BNO   *+10                                                             
         MVC   DBFILE,=C'PAV'         SET FILE TO PAV                           
*                                                                               
         TM    SVATD,X'60'         IF DEFAULT IS TP OR TT                       
         BZ    *+10                                                             
         MVC   DBFILE,=C'TP '         SET FILE TO TP                            
*                                                                               
PUREFILX DS    0H                                                               
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         BNO   *+8                                                              
         MVI   DBTAPEP,C'Y'           TURN ON FLAG                              
*                                                                               
         TM    RMPPROF+RMP_WKWB,RMP_WKWA                                        
         BNO   *+8                                                              
         MVI   DBPRGDUR,C'Y'          TURN ON FLAG                              
*                                                                               
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
*                                                                               
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELSRC,TITSRCE    SELECT OPTIONS                               
         MVC   DBSELBK,CBOOKS+1                                                 
         MVC   DBSELSTA,RINVKSTA                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBTIMCHG,SVTIMCHG   FROM INV HEADER                              
*                                                                               
         MVC   DBBTYPE,CBOOKS+3    SET BOOK TYPE                                
*                                                                               
***>>>   NI    DBBTYPE,X'FF'-X'40' MAKE 'BOOK TYPE' LOWER CASE                  
*                                                                               
         TM    CBOOKS,X'08'        IF TP FILE ASKED FOR                         
         BNO   *+12                                                             
         MVI   DBTPTT,C'P'            SET FOR 4WK AVERAGE OPTION                
         B     PURE1                                                            
*                                                                               
         CLI   DBFILE,C'T'         TEST FOR TIME PERIOD                         
         BNE   PURE2                                                            
*                                                                               
         MVI   DBTPTT,C'P'         SET 4 WK AVERAGE OPTION                      
*                                                                               
         TM    SVATD,X'20'         IF TT TRANSFER DEFAULT                       
         BNO   *+8                                                              
         MVI   DBTPTT,C'T'            SET FOR TT OPTION                         
*                                                                               
PURE1    DS    0H                                                               
*                                                                               
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'2'     FOR A PS/1 STATION                           
         BE    *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
PURE2    LR    R0,R4                                                            
         LA    R4,IO                                                            
*                                                                               
         L     R3,DBXTNEXT-DBXTTID+DBEXTRA1  A(NEXT EXTENSION AREA)             
         USING DBXTLD,R3           ESTABLISH DAY/TIME EXTENSION                 
*                                                                               
         XC    0(128,R3),0(R3)                                                  
*                                                                               
         MVC   DBDQD(2),=AL2(DBDQUXTD)  USE EXTENSION FOR DYTIMS                
*                                                                               
         XC    DBXTLD(DBXTLIST-DBXTLD),DBXTLD   INIT FIRST PART                 
*                                                                               
         MVC   DBXTLID,=C'DYTM'    SET EXTENSION ID                             
*                                                                               
         LA    R3,DBXTLIST         POINT TO DAYS/TIMES LIST                     
         XC    DBXTLIST,DBXTLIST   INIT FIRST ENTRY                             
*                                                                               
         DROP  R3                                                               
*                                                                               
*        MULTI HUT DAY TIME ELMS HAVE SAME STRUCTURE AS                         
*        DAY/TIME ELEMENTS. USE MULTI HUTS IF PRESENT                           
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'09'        FIND FIRST MULTI HUT ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    PUREDTLP            AT LEAST ONE FOUND                           
*                                                                               
*        REVERT TO DAY/TIME ELMS IF NO MULTI HUT                                
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'02'        FIND FIRST DAY/TIME  ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   PUREDTDN            END OF ELMENTS                               
*                                                                               
PUREDTLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         MVC   0(1,R3),RIDTDAY     PASS FIRST DAY/TIME                          
         MVC   1(4,R3),RIDTTIME                                                 
*                                                                               
PUREDTCN DS    0H                                                               
*                                                                               
         LA    R3,L'DBXTLIST(R3)   NEXT PLACE IN LIST                           
         BAS   RE,NEXTEL                                                        
         BE    PUREDTLP                                                         
*                                                                               
PUREDTDN DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVI   DBBEST,C'B'                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,DBSELTIM                                                    
         AH    R1,=H'200'          ADD 2 HOURS TO START                         
         CH    R1,=H'2400'         TEST IF PAST MIDNIGHT                        
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
*                                                                               
         CLC   RINVPTIM+2(2),=C'CC' TEST FOR TO CONCLUSION                      
         BNE   *+8                                                              
         STCM  R1,3,DBSELTIM+2     SET END TIME                                 
*                                                                               
         LR    R4,R0               A(DEMO TRACK BUILD AREA)                     
         LA    R1,DBLOCK           INITIALIZE BLOCK FOR DEMOMATH                
         ST    R1,MTHCFACS                                                      
         XC    MTHFCTR,MTHFCTR                                                  
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         BNO   *+8                                                              
         MVI   DBTAPEP,C'Y'           TURN ON FLAG                              
*                                                                               
         TM    RMPPROF+RMP_WKWB,RMP_WKWA                                        
         BNO   *+8                                                              
         MVI   DBPRGDUR,C'Y'          TURN ON FLAG                              
*                                                                               
PURE4    DS    0H                                                               
*                                                                               
         L     RF,=A(DHOOK)                                                     
         A     RF,RELO23           RELOCATE ADDRESS                             
*                                                                               
         XC    WTTOT,WTTOT         INIT WEIGHT FACTOR ACCUMULATOR               
*&&DO                                                                           
***                                                                             
*   TEST                                                                        
         LA    RF,1                                                             
         LNR   RF,RF                                                            
         LA    RE,DBLOCK                                                        
         LA    R1,DBEXTEND                                                      
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
                                                                                
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,(RF)                                          
*                                                                               
         CLI   HITCNT,0            ANY LUCK                                     
         BNE   PURE5               YES - NEED TO AVERAGE RECORD                 
*                                                                               
         CLC   =C'PAV',DBFILE      DONE IF NOT PAV FILE                         
         BNE   PUREX                                                            
*                                                                               
         MVC   DBFILE,=C'TP '      RESET FOR TIME PERIOD FILES                  
         MVI   DBTPTT,C'T'         RESET FOR TYPICAL TIME                       
*                                                                               
         B     PURE4                                                            
*                                                                               
PURE5    DS    0H                                                               
*                                                                               
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BE    *+6                                                              
         DC    H'0'                DEMO MODULE ERROR                            
*                                                                               
*        UNWEIGHT THE INTERIM RECORD AND SHARES BEFORE 'FF' CALL                
*                                                                               
         MVC   MTHFCTR,WTTOT                                                    
*                                                                               
         GOTO1 DEMOMATH,DMCB,=C'DIVIDE',AINTEREC,AINTEREC,MATHFAC               
*                                                                               
         BAS   RE,DIVSHR                                                        
*                                                                               
*        BUILD DUMMY UPGRADE ELEMENT FOR INDEX 'FF' DEMUP CALL                  
*        TO GET OLD HPT VALUES                                                  
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
*                                                                               
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNBKS,DBFILE     SET FILE NAME FROM DBLOCK                    
         MVI   RAVLNCAT,C'D'       DAY/TIME TRANSFER                            
         MVC   RAVLNOP1,=X'FFFF'   GET OLD HPT'S                                
         MVC   RAVLNOP2,=H'1'      UNWEIGHTED RECORD                            
*                                                                               
         LA    R1,DBLOCK           PASS A(MY DBLOCK)                            
         STCM  R1,15,RAVLNOP3                                                   
*                                                                               
         MVC   DEMODUB(4),=C'RDB=' PASS DBLOCK ADDRESS                          
         STCM  R1,15,DEMODUB+4                                                  
*****    MVC   DEMODUB(4),=C'RID=' SET REP ID                                   
*****    MVC   DEMODUB+4(2),CPARREP                                             
*                                                                               
         DROP  RE                                                               
*                                                                               
         L     R5,AINTEREC         UPGRADE INTERIM RECORD                       
*                                                                               
         SR    R0,R0                                                            
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA   IF DEMOS TO BE BASED ON IMPS         
         BNO   *+8                                                              
         ICM   R0,1,=C'I'               TURN ON FLAG                            
*                                                                               
         GOTO1 DEMUP,DMCB,23(R5),((R0),WORK),ACOMFACS,DEMODUB,HOMSHR            
*                                                                               
         MVC   MTHIFIL,=C'PAV'     FORCE BOOK ELEMENT LOOKUP                    
         MVC   MTHOFIL,=C'INV'     CONVERT TO INVENTORY FORMAT                  
         MVC   MTHOSRC,=C'NSI'     AND MAD INTERIM TO FINAL RECORD              
*                                                                               
         GOTO1 DEMOMATH,DMCB,=C'MAD',AINTEREC,ADATAREC,MATHFAC                  
*                                                                               
         L     RE,AINTEREC                                                      
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         LR    R0,RE                                                            
         MVCL  RE,R0               CLEAR INTERIM RECORD AREA                    
*                                                                               
         MVC   MTHIFIL,=C'INV'                                                  
*                                                                               
         GOTO1 DEMOMATH,DMCB,=C'DIVIDE',(R4),(R4),MTHCFACS                      
*                                                                               
PURE6    OC    PROGEL,PROGEL       ANY PROGRAMS TO ADD                          
         BZ    PURE8                                                            
*                                                                               
         LA    R5,PROGEL                                                        
         USING PROGELD,R5                                                       
*                                                                               
         MVI   PCODE,X'01'                                                      
         MVI   PELLEN,PNAME2-PROGELD    FIND ELEMENT LENGTH                     
*                                                                               
         MVI   PLIN,1                                                           
         MVC   WORK(2),CBOOKS+1    DISPLAY FROM BOOK (MMMYY)                    
         MVI   WORK+2,X'01'                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(6,DUB)                                     
*                                                                               
         MVC   PMON,DUB                                                         
         MVC   PYR,DUB+4                                                        
*                                                                               
         CLI   CBOOKS+3,C' '       SKIP IF NO BOOK TYPE                         
         BE    PURE62                                                           
         CLI   CBOOKS+3,0          SKIP IF NO BOOK TYPE                         
         BE    PURE62                                                           
*                                                                               
         GOTO1 GETBTYPE,DMCB,(CBOOKS+3,0)                                       
         CLI   DMCB,0                                                           
         BE    PURE62                                                           
                                                                                
         LA    RF,PBKTYPE                                                       
         MVI   0(RF),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),DMCB+2                                                   
                                                                                
         LA    RF,2(R1,RF)                                                      
         MVI   0(RF),C')'                                                       
*                                                                               
PURE62   DS    0H                                                               
*                                                                               
         MVC   PINVCODE,=C'TP '    DEFAULT                                      
*                                                                               
         CLC   DBFILE,=C'PAV'      UPDATE IF PAV FILE                           
         BNE   *+14                                                             
         MVC   PINVCODE,=C'PA'                                                  
         B     PURE63                                                           
*                                                                               
         CLI   DBTPTT,C'T'         UPDATE IF TYPICAL TIME PERIOD                
         BNE   *+10                                                             
         MVC   PINVCODE,=C'TT'                                                  
*                                                                               
PURE63   DS    0H                                                               
*                                                                               
         MVI   PEQS,C'='                                                        
         OC    PFBK(9),SPACES                                                   
*                                                                               
         CLC   PNAME1,PNAME2       SKIP IF ONLY ONE PROGRAM NAME                
         BE    PURE7                                                            
*                                                                               
*        PROGRAM NAME IS 1ST 7CH'S FROM FIRST SHOW AND                          
*        1ST 7CH'S FROM LAST SHOW IN PNAME2                                     
*                                                                               
         MVC   PNAME1+7(L'PNAME1-7),SPACES                                      
         MVI   PNAME1+7,C'/'                                                    
         MVC   PNAME1+8(7),PNAME2                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
PURE7    DS    0H                                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R4),PROGEL,0                     
*                                                                               
PURE8    LA    R5,CDEL             BUILD AND ADD A CD ELEMENT                   
         XC    CDEL,CDEL                                                        
         USING RINVCEL,R5                                                       
*                                                                               
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,=C'TP'                                                  
*                                                                               
         CLI   DBTPTT,C'T'         SKIP IF NOT TT                               
         BNE   *+10                                                             
         MVC   RINVCODE,=C'TT'                                                  
*                                                                               
         CLI   DBFILE,C'T'         TEST FOR TIME PERIOD                         
         BE    *+10                                                             
         MVC   RINVCODE,SPACES                                                  
*                                                                               
         LR    R6,R4               POSITION ELEMENT INSERTION                   
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),CDEL,(R6)                                   
*                                                                               
*        ADD DAY/TIMES ELEMENTS                                                 
*                                                                               
PURE10   DS    0H                                                               
*                                                                               
*        FIRST DELETE ALL CURRENT '09'S                                         
*                                                                               
         LR    R6,R4               SET A(INVENTORY RECORD)                      
         LA    R6,34(R6)           SET A(FIRST ELEMENT IN RECORD)               
*                                                                               
PUREX020 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    PUREX100            YES - FINISHED SCAN                          
         CLI   0(R6),X'09'         X'09' ELEMENT?                               
         BNE   PUREX060            NO  - SKIP IT                                
PUREX040 EQU   *                                                                
         MVI   0(R6),X'FF'         SET ELEMENT TO BE DELETED                    
PUREX060 EQU   *                                                                
         ZIC   RF,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RF                                                            
         B     PUREX020            GO BACK FOR NEXT                             
PUREX100 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'FF',0(R4)),0,0                  
*                                                                               
PURE11A  DS    0H                                                               
         TM    FLAG1,MHONQ         IS THIS A MULTI HUT RECORD?                  
         BO    PURE09DD            YES,  DO X'09' ADDS                          
*                                                                               
*                                                                               
PURECEDD DS    0H                  ELSE, DO X'CE' ADDS                          
*                                                                               
*        ADD NEW X'CE' ELEMENTS                                                 
*                                                                               
         LA    R5,CEELS            POINT TO DAY/TIME ELEMENTS                   
         LA    R0,8                MAX NUMBER OF ELEMENTS                       
PURECEAL DS    0H                                                               
         USING RINVZEL,R5          ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         CLI   RINVZCOD,0          END OF TABLE?                                
         BE    PURECEAD            YES                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R4),0(R5),0                      
*                                                                               
PURECEAC DS    0H                                                               
*                                                                               
         LA    R5,10(R5)           BUMP TO NEXT ELEMENT                         
         BCT   R0,PURECEAL                                                      
*                                                                               
         DROP  R5                                                               
*                                                                               
PURECEAD DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK             BUILD TRANSFER FROM ELEMENT                  
         USING RINVFREL,RE                                                      
*                                                                               
         MVI   RINVFRCD,X'03'                                                   
         MVI   RINVFRLN,16                                                      
         MVC   RINVFRST,RINVKSTA   SET STATION FROM KEY                         
         LA    RF,CEEL             GET FROM SOURCE/BOOK FROM 1ST CE EL          
         MVC   RINVFRBK,RINVZBK-RINVZEL(RF)                                     
         MVI   RINVFRTY,C'P'                                                    
         MVI   RINVFRPR,C'O'       OVERNIGHT TRANSFERS                          
         MVC   RINVFRBT,CBOOKS+3   BOOK TYPE                                    
*                                                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R4),WORK,0                       
***      B     PURE12                                                           
*                                                                               
PURE09DD DS    0H                                                               
*                                                                               
*        ADD NEW X'09' ELEMENTS                                                 
*                                                                               
         LA    R5,X09ELS           POINT TO MH DAY/TIME ELEMENTS                
         LA    R0,8                MAX NUMBER OF ELEMENTS                       
*                                                                               
PURE09AL DS    0H                                                               
         USING RIMHELEM,R5          ESTABLISH MH DAY/TIME ELEMENT               
*                                                                               
         CLI   RIMHCODE,0          DONE IF END OF ELEMENTS FOUND                
         BE    PURE12                                                           
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R4),0(R5),0                      
*                                                                               
         LA    R5,7(R5)           BUMP TO NEXT ELEMENT                          
         BCT   R0,PURE09AL                                                      
*                                                                               
         DROP  R5                                                               
*                                                                               
PURE12   XC    WORK,WORK           BUILD DUMMY UPGRADE AS PRELUDE TO            
         LA    RE,WORK             INDEX 100 DEMUP CALL FOR NEW HPTS            
         USING RAVLNEL,RE                                                       
*                                                                               
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNOP1,=H'100'                                                 
*                                                                               
         DROP  RE                                                               
*                                                                               
**                                 ADD UPGRADE EL AND CALL DEMUP                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R4),WORK,0                       
*                                                                               
*                                                                               
         NI    11(R4),X'FF'-X'40'  BECAUSE OF UT/TP PROBLEM                     
         SR    R0,R0                                                            
*                                                                               
         LA    R1,DBLOCK           PASS A(MY DBLOCK)                            
         MVC   DEMODUB(4),=C'RDB=' PASS DBLOCK ADDRESS                          
         STCM  R1,15,DEMODUB+4                                                  
*****    MVC   DEMODUB(4),=C'RID=' SET REP ID                                   
*****    MVC   DEMODUB+4(2),CPARREP                                             
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA   IF DEMOS TO BE BASED ON IMPS         
         BNO   *+8                                                              
         ICM   R0,1,=C'I'               TURN ON FLAG                            
*                                                                               
         GOTO1 DEMUP,DMCB,34(R4),((R0),WORK),ACOMFACS,DEMODUB,HOMSHR            
         OI    11(R4),X'40'                                                     
*                                                                               
         ST    R4,DBAREC                                                        
         LA    R1,34(R4)                                                        
         ST    R1,DBAQUART                                                      
*                                                                               
         MVC   DBFILE,=C'INV'                                                   
*                                                                               
         ZIC   R3,PCNT                                                          
         MH    R3,=H'132'                                                       
         LA    R3,PL1(R3)          POSITION TO P1-P4                            
*                                                                               
         MVC   90(9,R3),=C'(AVERAGE)'                                           
*                                                                               
         BAS   RE,PERF                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,CMPTODAY),(3,DUB)                                 
*****                                                                           
*****    TM    CBOOKS,X'08'        IF NOT A TRANSFER TO TP BOOK                 
*****    BO    *+8                                                              
*****    NI    RINVKSRC-RINVKEY(R4),X'FF'-X'02'   CLEAR TP BIT                  
*                                                                               
         MVC   KEY,0(R4)           SEE IF RECORD EXISTS                         
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    PURE16                                                           
*                                                                               
         MOVE  (IO,2000),0(R4)                                                  
         MVC   KEY,IO              NO - SO ADD A NEW ONE                        
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK             BUILD ACTIVITY ELEMENT                       
         USING RINVAEL,RE                                                       
*                                                                               
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,DUB                                                     
         MVC   RINVALST,DUB                                                     
         MVI   RINVAWHY,C'A'                                                    
*                                                                               
         DROP  RE                                                               
*                                                                               
         LA    R4,IO                                                            
         LR    R6,R4                                                            
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),WORK,(R6)                                   
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         B     PUREX                                                            
*                                                                               
PURE16   DS    0H                  OVERWRITE WITH NEW RECORD                    
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 GETREC              OVERWRITE WITH NEW RECORD                    
*                                                                               
         LA    R6,IO                                                            
*                                                                               
         NI    27(R6),X'FF'-X'80'  REMOVE ANY DELETE BIT                        
*                                                                               
         MVI   ELCODE,X'EF'        SEARCH FOR OLD ACTIVITY ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   PURE18                                                           
*                                                                               
         USING RINVAEL,R6                                                       
*                                                                               
         MVC   RINVALST,DUB                                                     
         MVI   RINVAWHY,C'C'                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
         LR    R3,R6               SAVE ELEMENT ADDRESS                         
         LR    R6,R4                                                            
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL                                                         
         GOTO1 VRECUP,DMCB,(2,(R4)),(R3),(R6)                                   
*                                                                               
PURE18   MOVE  (IO,2000),0(R4)                                                  
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         TM    KEY+27,X'80'        IF RECORD WAS DELETED                        
         BNO   PUREDELX                                                         
*                                                                               
         NI    KEY+27,X'FF'-X'80'     TURN OFF DELETE BIT                       
*                                                                               
         GOTO1 WRITE                  RE-WRITE POINTER                          
*                                                                               
PUREDELX DS    0H                                                               
*                                                                               
         B     PUREX                                                            
*                                                                               
PUREX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - DHOOK'                 
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              HOOK TO HANDLE DEMO RECORDS                        *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
DHOOK    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,AINTEREC                                                      
*                                                                               
         CLI   0(RE),0             TEST FOR INITIALIZED INTERIM REC             
         BNE   DH1                                                              
*                                                                               
         MVC   0(1,RE),DBFILE                                                   
         MVC   20(2,RE),=H'24'     DUMMY RECORD LENGTH                          
*                                                                               
DH1      DS    0H                                                               
*                                                                               
         TM    RMPPROF+RMP_WKWB,RMP_WKWA    IF NOT WEEKLY WEIGHTING             
         BO    *+14                                                             
         MVC   MTHFCTR+2(2),DBFACTOR   USE DBLOCK WEIGHT FACTOR                 
         B     DHWTX                                                            
*                                                                               
         CLC   DBFILE,=C'PAV'      IF PAV FILE                                  
         BNE   DHWTPAVN                                                         
*                                                                               
         GOTO1 DEFINE,DMCB,=C'TOTD',DBLOCK,BLOCK                                
*                                                                               
         MVC   MTHFCTR+3(1),BLOCK  WEIGHTING FACTOR                             
*                                                                               
         B     DHWTX                                                            
*                                                                               
DHWTPAVN DS    0H                  TP FILE BY DEFAULT                           
*                                                                               
         MVC   MTHFCTR+2(2),DBFACTOR   USE DBLOCK WEIGHT FACTOR                 
         B     DHWTX                                                            
*                                                                               
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,BLOCK   QUARTER HRS                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,BLOCK+6        NUMBER OF QUARTER HOURS                      
         ST    RF,FULL             SAVE                                         
*                                                                               
         GOTO1 DEFINE,DMCB,=C'WEEK',DBLOCK,BLOCK   WEEKS                        
*                                                                               
         SR    R0,R0               WEEK COUNTER                                 
         SR    RF,RF                                                            
         ICM   RF,8,BLOCK          WEEKS INDICATOR                              
         SLL   RF,4                WEEKS TO HIGH NYBBLE                         
         LA    R1,4                MAX NUMBER OF WEEKS                          
*                                                                               
         SR    RE,RE                                                            
         SLDL  RE,1                TOP BIT IN RF TO RE                          
         AR    R0,RE               ADD WEEK TO CTR IF BIT WAS ON                
         BCT   R1,*-8              LOOP COUNTER                                 
*                                                                               
         LR    RF,R0               COPY WEEKS                                   
         M     RE,FULL             TOTAL QUARTER HOURS                          
*                                                                               
         STCM  RF,15,MTHFCTR       WEIGHTING FACTOR                             
*                                                                               
DHWTX    DS    0H                                                               
*                                                                               
         ICM   RF,15,MTHFCTR       BUMP WEIGHT ACCUMULATOR                      
         A     RF,WTTOT                                                         
         ST    RF,WTTOT                                                         
*                                                                               
         AI    PCNT,1                                                           
         AI    HITCNT,1                                                         
         ZIC   R3,PCNT             1-4                                          
         BCTR  R3,0                                                             
         MH    R3,=H'132'                                                       
         LA    R3,PL1(R3)          POSITION TO P1-P4                            
*                                                                               
         TM    CBOOKS,X'08'                                                     
         BO    DH2                                                              
*                                                                               
         MVC   BLOCK(4),SPACES                                                  
*                                                                               
         GOTO1 DEFINE,DMCB,=C'PURE',DBLOCK,BLOCK                                
*                                                                               
         MVC   69(4,R3),BLOCK+3                                                 
*                                                                               
DH2      GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,BLOCK                                 
*                                                                               
         MVC   75(3,R3),BLOCK+2                                                 
*                                                                               
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,BLOCK                                
*                                                                               
         GOTO1 MILEDIT,DMCB,BLOCK+2,79(R3)                                      
*                                                                               
DH4      MVC   BLOCK(16),SPACES                                                 
*                                                                               
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,BLOCK                             
*                                                                               
         MVC   87(16,R3),BLOCK                                                  
*                                                                               
         BAS   RE,PERF                                                          
*                                                                               
         BAS   RE,GETSHR           GET SHARES FROM INPUT REC AND WT             
*                                                                               
*              MAD BY QUARTER HOUR WEIGHT INTO INTERIM RECORD                   
*                                                                               
         MVC   MTHIFIL,DBFILE      KEEP FILE FORMAT OF INPUT RECORD             
         MVC   MTHOFIL,DBFILE                                                   
         MVC   MTHOSRC(1),DBSELSRC                                              
*====================================================================           
         XCEFL IUNWORK,4032                                                     
         MVC   IUNWORK(1),DBFILE                                                
         MVC   IUNWORK+20,=H'24'   DUMMY RECORD LENGTH                          
*                                                                               
         GOTO1 REGETIUN,DMCB,(9,DBLOCK),IUNWORK+500                             
*                                                                               
         MVC   DBNUMVLS,=H'320'                                                 
*                                                                               
         LA    R4,IUNWORK+500                                                   
         USING IUNREC,R4                                                        
*                                                                               
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',SHARES),DBLOCK,HOMESHR,0                       
*                                                                               
         L     R3,DBAQUART                                                      
         L     R4,DBAREC                                                        
         LA    RE,IUNWORK                                                       
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
*                                                                               
         LA    RF,OFORMAT          DEFAULT FORMAT FIELDS                        
*                                                                               
         CLI   DBTAPEP,C'Y'        IF DEMOS BASED ON IMPRESSIONS                
         BNE   *+8                                                              
         LA    RF,OFORMATI            SWITCH FORMAT FIELDS                      
*                                                                               
         GOTO1 DEMAINT,DMCB,=C'PUT',DBLOCK,IUNWORK+500,(RF)                     
         DROP  R4                                                               
*                                                                               
         MVC   MTHOSRC,=C'NSI'                                                  
*                                                                               
         GOTO1 DEMOMATH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
         ST    R4,DBAREC           RESTORE DBLOCK STUFF                         
         ST    R3,DBAQUART                                                      
         B     HOOK2T                                                           
*                                                                               
HOOK1T   GOTO1 DEMOMATH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
*                                                                               
HOOK2T   EQU   *                                                                
*====================================================================           
*--->    GOTO1 DEMOMATH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
*                                                                               
         CLI   PCNT,4                                                           
         BNE   DH6                                                              
*                                                                               
         BAS   RE,SPLAT            PRINT LINE                                   
         MVI   PCNT,0                                                           
*                                                                               
DH6      DS    0H                  CAPTURE PROG NAMES                           
*                                                                               
*****    CLC   BLOCK(4),=C'AVG.'   FILTER OUT THIS NAME                         
*****    BE    DHOOKX                                                           
*                                                                               
         LA    R2,PROGEL                                                        
         USING PROGELD,R2                                                       
*                                                                               
         MVC   PNAME2,BLOCK        CAPTURE LAST PROGRAM NAME                    
*                                                                               
         OC    PNAME1,PNAME1       CAPTURE FIRST PROGRAM NAME                   
         BNZ   *+10                                                             
         MVC   PNAME1,BLOCK                                                     
*                                                                               
DH7      DS    0H                                                               
*                                                                               
         B     DHOOKX              ALL DONE                                     
*                                                                               
DHOOKX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - CHKDPT'                
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              COMPARE REQUESTED DAYPARTS TO INVENTORY RECORDS    *             
*              DROP INVENTORY IF PRIMARY DPT IS ONE OF THOSE      *             
*                REQUESTED BUT NOT THIS ONE                       *             
*                (IT WILL BE PROCESSED WHEN ITS PRIMARY DPT COMES *             
*                UP                                               *             
*              PROCESS INVENTORY THE FIRST TIME A NON-PRIMARY DPT *             
*                IS ENCOUNTERED                                   *             
*                                                                 *             
*                                                                 *             
*NTRY    R6==> RINVPEL                                            *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
         DS    0H                                                               
CHKDPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RINVPEL,R6          ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         L     R2,ADPT             POINT TO CURRENT DAYPART                     
         CLC   0(1,R2),RINVDP      OKAY IF CURRENT MATCHES PRIMARY              
         BE    CHKDPTOK                                                         
*                                                                               
*        DROP RECORD IF PRIMARY DAYPART IS AMONG THOSE REQUESTED                
*                                                                               
         LA    RE,DPLIST           REQUESTED DAYPARTS                           
         LA    RF,L'DPLIST                                                      
*                                                                               
         CLI   0(RE),C' '          DONE AT END OF LIST                          
         BNH   *+22                                                             
         CLC   0(1,RE),RINVDP      MATCH LIST TO PRIMARY DPT                    
         BE    CHKDPTEX            DROP IF MATCH FOUND                          
         LA    RE,1(RE)            BUMP LIST POINTER                            
         BCT   RF,*-22                                                          
*                                                                               
*        OKAY IF REQUESTED DPTS MATCH ONLY ONE IN RECORD                        
*                                                                               
         USING RINVPEL,R6          ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         LA    RE,DPLIST           REQUESTED DAYPARTS                           
         LA    RF,L'DPLIST                                                      
*                                                                               
CHKDPT1L DS    0H                                                               
*                                                                               
         CLI   0(RE),C' '          DONE AT END OF LIST                          
         BNH   CHKDPT1D                                                         
*                                                                               
         LA    R1,RINVDP+1         INVENTORY DAYPARTS (FIRST ALREADY            
         LA    R0,L'RINVDP-1          CHECKED)                                  
*                                                                               
CHKDPT2L DS    0H                                                               
*                                                                               
         CLI   0(R1),C' '          CHECK FOR END OF LIST                        
         BNH   CHKDPT2D                                                         
*                                                                               
         CLC   0(1,RE),0(R1)       SKIP IF DAYPARTS DON'T MATCH                 
         BNE   CHKDPT2C                                                         
*                                                                               
         CR    RE,R2               IF MATCH BEFORE CURRENT DPT                  
         BL    CHKDPTEX               DROP INVENTORY AS DUPLICATE               
*                                                                               
         B     CHKDPTOK            ELSE PROCESS INVENTORY RECORD                
*                                                                               
CHKDPT2C DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP LIST POINTER                            
         BCT   R0,CHKDPT2L                                                      
*                                                                               
CHKDPT2D DS    0H                                                               
*                                                                               
CHKDPT1C DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            BUMP LIST POINTER                            
         BCT   RF,CHKDPT1L                                                      
*                                                                               
CHKDPT1D DS    0H                                                               
*                                                                               
CHKDPTOK DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     CHKDPTX                                                          
*                                                                               
CHKDPTEX DS    0H                                                               
         LTR   RB,RB               SET NE CC                                    
*                                                                               
CHKDPTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - TRACE'                 
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              TRACE ROUTINE                                      *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
TRACE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,=A(DATA)                                                      
         A     R4,RELO23                                                        
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   DUB,RINVLEN                                                      
         LH    R3,DUB                                                           
*                                                                               
TRACE2   LTR   R3,R3                                                            
         BZ    TRACEX                                                           
*                                                                               
         CH    R3,=H'40'                                                        
         BH    TRACE4                                                           
*                                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R4)                                                       
*                                                                               
         LA    R3,1(R3)                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,(R4),P+44,(R3),=C'TOG'                               
*                                                                               
         MVI   SPACING,2                                                        
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
         B     TRACEX                                                           
*                                                                               
TRACE4   MVC   P(40),0(R4)                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,(R4),P+44,40,=C'TOG'                                 
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
         LA    R4,40(R4)                                                        
         SH    R3,=H'40'                                                        
         B     TRACE2                                                           
*                                                                               
TRACEX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - WORK23D'               
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
WORK23   DS    0D                                                               
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - GETSHR'                
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              ROUTINE TO GET SHARES AND UPDATE ACCUMULATORS      *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
GETSHR   NTR1                                                                   
*                                                                               
         MVC   DUB(2),DBACTBK                                                   
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'P',DEMOSHR),DBLOCK,HOMSHR                         
*                                                                               
         MVC   DBACTBK,DUB         RESTORE BOOK VALUE                           
*                                                                               
         LA    R1,HOMSHR           POINT TO OUTPUT AREA                         
         LA    RE,TOTSHR           POINT TO ACCUMS                              
         LA    R0,3                                                             
*                                                                               
GETSHRLP L     RF,0(R1)                                                         
         MH    RF,MTHFCTR+2        MULTIPLY SHARES BY WEIGHTING                 
         A     RF,0(RE)            UPDATE SHARE ACCUMS                          
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)            NEXT OUTPUT VALUE                            
         LA    RE,4(RE)            NEXT ACCUM                                   
         BCT   R0,GETSHRLP                                                      
GETSHRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
DEMOSHR  DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - HOOK'                  
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              HEADLINE ROUTINES                                  *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RE,4(RD)                                                         
         CLC   0(4,RE),=C'SPUL'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     *-14                                                             
         LM    RE,RC,12(RE)                                                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R3,ASTA             POINT TO CURRENT STATION                     
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
         MVC   H4+10(4),STLSSTAC     STATION CALL LETTERS                       
*                                                                               
         CLI   STLSSTAC+4,C'T'       PRINT BAND IF NOT TV                       
         BE    HOOKBNDX                                                         
*                                                                               
         LA    RF,H4+10+3          POINT TO LAST CALL LETTER                    
*                                                                               
         CLI   0(RF),C' '          BACK UP IF BLANK                             
         BH    *+6                                                              
         BCTR  RF,0                                                             
*                                                                               
         MVI   1(RF),C'-'          PRINT BAND                                   
*                                                                               
         MVC   2(1,RF),STLSSTAC+4                                               
*                                                                               
         CLI   2(RF),C'A'          IF AM/FM PRINT MODULATION                    
         BE    *+8                                                              
         CLI   2(RF),C'F'                                                       
         BNE   *+8                                                              
         MVI   3(RF),C'M'                                                       
*                                                                               
HOOKBNDX DS    0H                                                               
*                                                                               
         MVC   H4+19(24),TITMKT                                                 
*                                                                               
         LA    R3,DPTBL            LOOK UP DAYPART                              
         USING DPTBLD,R3                                                        
*                                                                               
         L     R2,ADPT                                                          
*                                                                               
         MVC   DPBYTE,0(R2)                                                     
*                                                                               
HOOK2    CLC   DPBYTE,DPTBCODE     MATCH TO DAYPART CODE                        
         BE    HOOK4                                                            
*                                                                               
         LA    R3,DPTBLL(R3)                                                    
*                                                                               
         CLI   0(R3),X'00'         TEST EOT                                     
         BNE   HOOK2                                                            
*                                                                               
HOOK4    MVC   H4+60(15),DPTBLNAM  DAYPART                                      
*                                                                               
         MVC   H4+101(3),TITSRCE                                                
*                                                                               
         ZIC   R1,CBOOKS+2         BOOK                                         
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
*                                                                               
         LA    R2,H4+113           POINT TO NEXT PRINT POSITION                 
*                                                                               
         MVC   0(3,R2),0(R1)                                                    
         MVI   3(R2),C'/'                                                       
         EDIT  (1,CBOOKS+1),(2,4(R2))                                           
*                                                                               
         LA    R2,6(R2)                                                         
*                                                                               
         CLI   CBOOKS+3,C' '       PRINT BOOK TYPE IF THERE                     
         BE    HOOK50                                                           
         CLI   CBOOKS+3,0          PRINT BOOK TYPE IF THERE                     
         BE    HOOK50                                                           
*                                                                               
         GOTO1 GETBTYPE,DMCB,(CBOOKS+3,0)                                       
         CLI   DMCB,0                                                           
         BE    HOOK50                                                           
                                                                                
         MVI   0(R2),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),DMCB+2                                                   
                                                                                
         LA    R2,2(R1,R2)                                                      
         MVI   0(R2),C')'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
HOOK50   DS    0H                                                               
         MVC   1(2,R2),=C'TP'                                                   
*                                                                               
         CLI   TITBOOK,C'T'                                                     
         BE    *+10                                                             
         MVC   1(3,R2),=C'PAV'                                                  
*                                                                               
         OC    ABOX(4),ABOX                                                     
         BZ    HOOK6               ON-LINE, NO BOX TO SET                       
*                                                                               
         L     R4,ABOX             INITIALIZE BOXES                             
         USING BOXD,R4                                                          
*                                                                               
         MVI   BOXOFF,0                                                         
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+05,C'C'     NUM                                          
         MVI   BOXCOLS+15,C'C'     EFF DATE                                     
         MVI   BOXCOLS+25,C'C'     DAY(S)                                       
         MVI   BOXCOLS+37,C'C'     TIME(S)                                      
         MVI   BOXCOLS+61,C'C'     PROGRAMMING                                  
         MVI   BOXCOLS+65,C'R'     TR                                           
         MVI   BOXCOLS+68,C'L'                                                  
         MVI   BOXCOLS+74,C'C'     NUM                                          
         MVI   BOXCOLS+78,C'C'     DAY                                          
         MVI   BOXCOLS+86,C'C'     TIME                                         
         MVI   BOXCOLS+103,C'C'    PROGRAMMING                                  
         MVI   BOXCOLS+110,C'C'    WT                                           
         MVI   BOXCOLS+117,C'C'    HUT                                          
         MVI   BOXCOLS+124,C'C'    SHR                                          
         MVI   BOXCOLS+131,C'R'    HUT                                          
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
*                                                                               
HOOK6    EQU   *                                                                
*                                                                               
HOOKX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL (R6),34,ELCODE                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - DIVSHR'                
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              ROUTINE TO DIVIDE SHARES BY TOTAL WEIGHTING        *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
DIVSHR   NTR1                                                                   
*                                                                               
         LA    R0,3                COUNTER                                      
         LA    R1,TOTSHR           POINT TO SHARE ACCUMS                        
         LA    R2,HOMSHR           OUTPUT AREA                                  
*                                                                               
DIVSHR2  L     RF,0(R1)                                                         
         SR    RE,RE                                                            
         SLDL  RE,1                ROUNDED DIVIDE                               
         D     RE,MTHFCTR                                                       
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
         ST    RF,0(R2)            UNWEIGHTED VALUE TO OUTPUT                   
         LA    R1,4(R1)            POINT TO NEXT SHARE                          
         LA    R2,4(R2)                                                         
         BCT   R0,DIVSHR2                                                       
*                                                                               
DIVSHRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - PERF'                  
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              SHOW PERFORMANCE FOR EACH LINE                     *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
PERF     NTR1                                                                   
*                                                                               
         EDIT  (4,MTHFCTR),(6,104(R3)),0                                        
*                                                                               
         LA    R2,DEMLIST                                                       
         LA    R3,111(R3)                                                       
         LA    R4,3                                                             
         XC    CDEMOS,CDEMOS       CLEAR AN OUTPUT AREA FOR DEMOUT              
         LA    R5,CDEMOS                                                        
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'P',(R2)),DBLOCK,CDEMOS                            
*                                                                               
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PERFDEM2 EQU   *                                                                
         L     R1,0(R5)                                                         
         EDIT  (R1),(6,(R3)),1                                                  
         LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R5,4(R5)            POINT TO NEXT OUTPUT VALUE                   
         BCT   R4,PERFDEM2                                                      
*                                                                               
PERFX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
DEMLIST  DS    0H                                                               
         DC    X'81',C'R',AL1(01)                                               
         DC    X'81',C'S',AL1(01)                                               
         DC    X'81',C'P',AL1(01)                                               
         DC    X'81',C'R',AL1(45)                                               
         DC    X'81',C'R',AL1(95)                                               
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - SPLAT'                 
*******************************************************************             
*                                                                 *             
*        RERMP23 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*              ROUTINE TO OUTPUT CHUNKS                           *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
SPLAT    NTR1                                                                   
*                                                                               
*        PRINT DETAIL LINES - THERE COULD BE 8                                  
*                                                                               
         LA    R3,PL1              START OF PRINT LINES                         
*                                                                               
SPLTPR1L DS    0H                                                               
*                                                                               
         LA    R2,P                START OF PRINT AREA (4 LINES WORTH)          
         LA    R0,4                MAX 4 LINES AT A TIME                        
*                                                                               
SPLTPR2L DS    0H                                                               
*                                                                               
         MVC   0(132,R2),0(R3)                                                  
         MVC   0(132,R3),SPACES                                                 
*                                                                               
SPLTPR2C DS    0H                                                               
*                                                                               
         LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         BCT   R0,SPLTPR2L                                                      
*                                                                               
SPLTPR2D DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)   PRINT THE DATA                                
*                                                                               
SPLTPR1C DS    0H                                                               
*                                                                               
         CLC   0(132,R3),SPACES    DONE IF ONLY SPACES TO PRINT                 
         BH    SPLTPR1L            THERE IS EXTRA LINE TO STOP LOOP             
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)   SPACING LINE                                  
*                                                                               
SPLATX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
         DS    CL4                    DBLOCK 4 BYTE OVERFLOW                    
DBEXTRA1 DS    CL400                                                            
         EJECT                                                                  
*              CONSTANTS TABLES LTORG ETC                                       
         SPACE 3                                                                
TPCNT    DC    AL1(0)                                                           
HITCNT   DC    AL1(0)                                                           
PCNT     DC    AL1(0)                                                           
VARSW    DS    C                                                                
PROGEL   DS    CL255                                                            
CEELS    DS    9CL10               DAY/TIMES ELEMENT BUILD AREA                 
         ORG   CEELS                                                            
CEEL     DS    CL10                                                             
         ORG                                                                    
CDEL     DS    CL10                                                             
         SPACE 1                                                                
X09ELS   DS    9CL7                MULTI HUT DAY/TIMES ELEMENT BLD AREA         
         ORG   X09ELS                                                           
X09EL    DS    CL7                                                              
         ORG   X09ELS                                                           
X09DEL   DS    CL7                                                              
         ORG                                                                    
*                                                                               
FLAG1    DS    XL1                 FLAGS HOLDER                                 
MHONQ    EQU   X'80'               MULTI HUT OPTION ON                          
         SPACE 1                                                                
         DS    0F                                                               
MATHFAC  DS    0CL17                                                            
MTHCFACS DS    F                                                                
MTHFCTR  DS    F                                                                
MTHIFIL  DS    CL3                                                              
MTHOFIL  DS    CL3                                                              
MTHOSRC  DS    CL3                                                              
         SPACE 1                                                                
DEMOMATH DS    V                                                                
DEMAINT  DS    V                                                                
DAYUNPK  DS    V                                                                
MILEDIT  DS    V                                                                
ADATAREC DS    A                                                                
AINTEREC DS    A                                                                
HOMSHR   DS    3F                                                               
TOTSHR   DS    3F                                                               
DEMODUB  DS    D                                                                
*                                                                               
PL1      DS    CL132               PRINT WORKAREA                               
PL2      DS    CL132               PRINT WORKAREA                               
PL3      DS    CL132               PRINT WORKAREA                               
PL4      DS    CL132               PRINT WORKAREA                               
PL5      DS    CL132               PRINT WORKAREA                               
PL6      DS    CL132               PRINT WORKAREA                               
PL7      DS    CL132               PRINT WORKAREA                               
PL8      DS    CL132               PRINT WORKAREA                               
PL9      DS    CL132               PRINT WORKAREA                               
*                                                                               
TYPTAB   DC    C'TTSA.'                                                         
OFORMAT  DC    C'IUNUIUN',X'530B00'                                             
OFORMATI DC    C'IUNUIUN',X'5A0B00'                                             
INDEX    DC    C'&&',X'FF'            INDEX UPGRADE MARKER                      
SHARES   DC    X'00',C'S',AL1(1)                                                
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
IUNWORK  DS    4032C                                                            
         SPACE 2                                                                
* PROGRAM NAME TEXT ELEMENT DSECT                                               
*                                                                               
PROGELD  DSECT                                                                  
PCODE    DS    X                                                                
PELLEN   DS    X                                                                
PLIN     DS    X                                                                
         DS    CL3                 SPARE                                        
PFBK     DS    0CL5                FROM BOOK                                    
PMON     DS    CL3                                                              
PYR      DS    CL2                                                              
PBKTYPE  DS    CL4                 (BOOKTYPE)                                   
PINVCODE DS    CL2                                                              
PEQS     DS    CL1                                                              
PNAME1   DS    CL16                FIRST PROGRAM NAME                           
PNAME2   DS    CL16                SECOND PROGRAM NAME                          
PROGELX  EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
* RERMPWORKD                                                                    
       ++INCLUDE RERMPWORKD                                                     
*RERMPPROF                                                                      
       ++INCLUDE RERMPPROF                                                      
* REGENREP                                                                      
       ++INCLUDE REGENREPA                                                      
* REGENINV                                                                      
       ++INCLUDE REGENINVA                                                      
* REGENAVL                                                                      
       ++INCLUDE REGENAVL                                                       
* REGENRDP                                                                      
       ++INCLUDE REGENRDP                                                       
* REGENSET                                                                      
       ++INCLUDE REGENSET                                                       
* DDSPLWORKD                                                                    
       ++INCLUDE DDSPLWORKD                                                     
* DDSPOOLD                                                                      
       ++INCLUDE DDSPOOLD                                                       
* DDBIGBOX                                                                      
       ++INCLUDE DDBIGBOX                                                       
* FALOCKETD                                                                     
       ++INCLUDE FALOCKETD                                                      
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* RERMPFFD                                                                      
       ++INCLUDE RERMPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE1D                                                       
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
*        MUST BE IN SYNC WITH RERMP21                                           
*                                                                               
STRTOPT  DS    CL3                *REQUEST START DATE                           
ENDOPT   DS    CL3                *REQUEST END   DATE                           
DPMENU   DS    CL4                *DAYPART MENU ID                              
DPLIST   DS    CL20               *DAY PART LIST                                
RELO21   DS    A                  *                                             
RELO23   DS    A                  *                                             
FADDR    DS    A                  *A(FIELD IN ERROR)                            
*                                                                               
DPTBL    DS    XL(24*DPTBLL)      *DAYPART TABLE                                
*                                                                               
STMENU   DS    CL4                *STATION MENU CODE                            
STMENUNM DS    CL60               *STATION MENU NAME                            
*                                                                               
STLIST   DS    XL(24*STLISTL)     *STATIONS LIST                                
*                                                                               
INVLIST  DS    XL((INVMAX+1)*2*L'RINVKINV)  INVENTORY NUMBERS LIST              
*                                                                               
INVMAX   EQU   30                  MAXIMUM NUMBER OF INVENTORY NUMBERS          
*                                                                               
ADPT     DS    F                                                                
ASTA     DS    A                   A(CURRENT STATION)                           
SAVESTAK DS    2F                                                               
ASTACK   DS    A                                                                
DPBYTE   DS    CL1                                                              
CMPTODAY DS    XL2                 TODAY - COMPRESSED                           
SVATD    DS    XL1                 SAVEAREA FOR AUTO TRANSFER DEFAULT           
RMPPROF  DS    XL8                 RMP PROGRAM PROFILE                          
SVTIMCHG DS    C                   TIME CHANGE FLAG FROM HEADER                 
*                                                                               
*        INPUT CONTROL BLOCK FOR GETKSRC                                        
*                                                                               
GSRCIN   DS    0C                  GETKSRC INPUT BLOCK                          
GSIRSVC  DS    CL1                 RATING SERVICE                               
GSIQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSIKSRC  DS    CL1                 RINVKSRC FOR KEY                             
GSIBITS  DS    XL1                 BOOKVAL BITS                                 
GSIBKTYP DS    CL1                 BOOKTYPE                                     
GSRCINL  EQU   *-GSRCIN            INPUT BLOCK LENGTH                           
*                                                                               
*        OUTPUT CONTROL BLOCK FOR GETKSRC                                       
*                                                                               
GSRCOUT  DS    0C                  GETKSRC OUTPUT BLOCK                         
GSORSVC  DS    CL1                 RATING SERVICE                               
GSOQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSOKSRC  DS    CL1                 RINVKSRC FOR KEY                             
GSOBITS  DS    XL1                 BOOKVAL BITS                                 
GSOBKTYP DS    CL1                 BOOKTYPE                                     
GSRCOUTL EQU   *-GSRCOUT           OUTPUT BLOCK LENGTH                          
*                                                                               
WTTOT    DS    F                   WEIGHTING FACTOR ACCUMULATOR                 
*                                                                               
         EJECT                                                                  
*                                                                               
*        DEMO RECORD IUN DSECT FOR USE BY FIXPAV                                
*                                                                               
IUNREC   DSECT                                                                  
UPREC    DS    0F                                                               
***********************************************************************         
*                                  ORIGINAL BOOK VALUES               *         
***********************************************************************         
OLDUNV   DS    (NUMVALS)F          UNIVERSES                          *         
OLDUNVX  EQU   *                                                      *         
***********************************************************************         
OLDRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                      *         
         ORG                                                                    
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
OLDRTGX  EQU   *                                                      *         
***********************************************************************         
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                      *         
         ORG                                                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                      *         
         ORG                                                                    
OLDHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  NEW VALUES                         *         
NEWUNV   EQU   OLDTOT              DEFINE ORIGIN FOR SPGETIUN CALL    *         
*                                                                     *         
***********************************************************************         
NEWRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                      *         
         ORG                                                                    
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
NEWRTGX  EQU   *                                                      *         
***********************************************************************         
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                      *         
         ORG                                                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
NEWHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  OTHER VALUES                       *         
***********************************************************************         
HOMESHR  DS    3F                  ORIGINAL HOMES SHARES              *         
HOMSHRX  EQU   *                                                      *         
HOMSHRLN EQU   *-HOMSHR                                               *         
***********************************************************************         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                       *         
LUNVX    EQU   *                                                      *         
***********************************************************************         
UPRECX   DS    0F                                                               
*                                                                               
NUMVALS  EQU   32                                                               
DISPHOM  EQU   20                                                               
LENVALS  EQU   NUMVALS*4                                                        
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - DPTBLD'                
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T81023 --- RERMP23 --- OVERNIGHT TRANSFERS - STLISTD'           
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST OF STATIONS                                   *         
*                                                                     *         
***********************************************************************         
STLISTD  DSECT                                                                  
STLSSTCD DS    CL1                 STATION SORT CODE                            
STLSSTAC DS    CL5                 STATION CALL LETTERS                         
STLISTL  EQU   *-STLISTD           LENGTH OF TABLE ENTRY                        
*                                                                               
         EJECT                                                                  
DBXTUIDD DSECT                                                                  
DBXID    DS    CL4                                                              
DBXNXT   DS    A                                                                
DBXBID   DS    H                                                                
DBXTUIDQ EQU   *-DBXID                                                          
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         ORG   GTBLOCK+L'GTBLOCK                                                
*PREFIX=FA$                                                                     
       ++INCLUDE FATWA                                                          
*PREFIX=                                                                        
*                                                                               
DATA     CSECT                                                                  
         DC    4000X'00'                                                        
         SPACE 1                                                                
STACK    CSECT                                                                  
         DC    10000X'00'                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098RERMP23   02/19/10'                                      
         END                                                                    
