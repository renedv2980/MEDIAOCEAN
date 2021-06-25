*          DATA SET MPRDR0E    AT LEVEL 015 AS OF 05/01/02                      
*PHASE T5100E,*                                                                 
         TITLE 'T5100E - QSPEC WEIGHTS'                                         
T5100E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T5100E                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         GOTO1 SETADDR             SET 'FLOATING' ADDRESSES                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,DISPKEY        DISPLAY RECORD KEY                           
         BE    DKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,RECDEL         DELETE HOOK (SELECT)                         
         BE    VREC                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       HARD COPY                                    
         BE    PLIST                                                            
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
         SPACE 3                                                                
         USING QSDKEY,R4                                                        
VKEY     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         CLI   ACTNUM,ACTREST      RESTORE IS INVALID                           
         BNE   VK2                                                              
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
VK2      DS    0H                                                               
         LA    R2,QSWSURH                                                       
         CLI   5(R2),0             IF NO KEY                                    
         BE    VK8                 JUST RETURN                                  
*                                                                               
         GOTO1 VALSURV                                                          
*                                                                               
         LA    R2,QSWQSPH                                                       
         GOTO1 ANY                                                              
         GOTO1 VALQSP                                                           
*                                                                               
         LA    R2,QSWCODH          WEIGHT CODE                                  
         SR    RF,RF               DEFAULT TO 0                                 
         MVI   ERROPT,C'Y'                                                      
         BAS   RE,VRNUM                                                         
         CH    R0,=H'255'                                                       
         BNH   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         STC   R0,BYTE                                                          
*                                                                               
         MVC   KQSWCOD,BYTE                                                     
         OC    KQSWCOD,KQSWCOD     TEST ANY CODE                                
         BNZ   VK4B                YES- OK                                      
         CLI   ACTNUM,ACTLIST      NO - OK ONLY FOR LIST                        
         BE    VK8                                                              
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK4B     DS    0H                  SEE IF WEIGHT ALREADY IN RECORD              
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VK4D     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   VK7                                                              
         USING QSPWGTEL,R6                                                      
         CLC   QSPWCOD,KQSWCOD     TEST RIGHT WEIGHT CODE                       
         BNE   VK4D                                                             
*                                                                               
VK6      DS    0H                  WEIGHT IS THERE                              
         CLI   ACTNUM,ACTADD       ERROR IF ADDING                              
         BNE   VK8                                                              
         MVI   ERROR,DUPLICAT                                                   
         B     TRAPERR                                                          
*                                                                               
VK7      DS    0H                  WEIGHT NOT THERE                             
         CLI   ACTNUM,ACTADD       OK IF ADD                                    
         BE    VK8                                                              
         CLI   ACTNUM,ACTLIST      OR LIST                                      
         BE    VK8                                                              
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
VK8      DS    0H                                                               
         CLI   ACTNUM,ACTADD       ON ADD                                       
         BE    VK8B                                                             
         CLI   ACTNUM,ACTDEL       AND DELETE                                   
         BNE   VK9                                                              
*                                                                               
VK8B     DS    0H                                                               
         OI    WHENOK,X'01'        TELL GENCON NOT TO BOTHER                    
*                                                                               
VK9      DS    0H                                                               
         MVI   USEIO,C'Y'          DONT LET GENCON READ QSPEC RECS              
*                                  BECAUSE WILL LOSE LIB PARAM SUBS             
         B     XIT                                                              
         SPACE 3                                                                
*        DISPLAY KEY                                                            
         SPACE 3                                                                
DKEY     DS    0H                                                               
         ZIC   RF,SELLISTN         GET WEIGHT CODE FROM MY LIST                 
         MH    RF,=Y(WLISTDL)                                                   
         LA    RF,WLIST(RF)                                                     
         MVC   KQSWCOD,0(RF)                                                    
         XC    FULL,FULL                                                        
         MVC   FULL+3(4),0(RF)       WEIGHT CODE                                
         LA    R2,QWLCODH                                                       
         BAS   RE,NUMDISP                                                       
         XIT                                                                    
*                                                                               
         EJECT                                                                  
*        VALIDATE RECORD                                                        
*        ---------------                                                        
         SPACE 1                                                                
VREC     DS    0H                                                               
         L     R5,VNODBLK                                                       
         USING NODBLKD,R5                                                       
         CLI   NDLIBLEV,0          NO CHANGES WITHIN LIB CALL                   
         BE    VR2                                                              
         CLC   NDLIBLEV,NDLEV                                                   
         BNL   VR2                                                              
*                                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,LIBUSERR                                                   
         B     TRAPERR                                                          
*                                                                               
VR2      DS    0H                                                               
         LA    R2,QSWREDH          BUILD REDEFINITION DATE LIST                 
         BAS   RE,BLDRED                                                        
*                                                                               
         LA    R2,QSWOPTH                                                       
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
         GOTO1 GETREC              GETREC HERE TO SET DMWORK                    
*                                  (NOT OBTAINED FROM NODIO)                    
         CLI   ACTNUM,ACTDEL       UNLESS DELETE                                
         BE    VR5                                                              
         CLI   MODE,RECDEL                                                      
         BE    VR5                                                              
*                                  TEST ANY INPUT                               
         L     R2,AFRSTREC                                                      
*                                                                               
VR3      DS    0H                                                               
         CLI   5(R2),0                                                          
         BH    VR5                 HAVE INPUT                                   
         BAS   RE,BUMPU            NEXT UNPROTECTED FIELD                       
         BNE   VR3                                                              
*                                  END OF SCREEN- NO DATA                       
         L     R2,AFRSTREC         EXIT WITH ENTER MSG                          
         MVI   MSGNUM,1                                                         
         GOTO1 MSGSET                                                           
         B     XIT                                                              
*                                  WEIGHT FIELDS EDIT                           
*                                  ------------------                           
VR5      DS    0H                                                               
         L     R6,AIO              FIRST FIND DEFINITION ELEM                   
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R6                                                            
         USING QSPELEM,R3                                                       
*                                                                               
         CLI   ACTNUM,ACTADD       FOR ADD                                      
         BE    VR6                 SKIP ELEM FIND                               
*                                                                               
         MVI   ELCODE,X'25'        FIND WEIGHT ELEM                             
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR5D     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING QSPWGTEL,R6                                                      
         CLC   QSPWCOD,KQSWCOD     GET RIGHT ELEM                               
         BNE   VR5D                                                             
*                                                                               
         CLI   ACTNUM,ACTDEL       FOR DELETE- REMOVE ELEM                      
         BE    VR5F                                                             
         CLI   MODE,RECDEL                                                      
         BNE   VR6D                                                             
*                                                                               
VR5F     DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,AIO),(1,KQSWCOD)                
         MVI   ELCODE,X'51'        AND REMOVE SPEC ELEMS                        
         MVC   WORK(2),CWAVDAT     SET 'KEY' OF SPEC ELEMS                      
         MVC   WORK+2(1),KQSWCOD                                                
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,AIO),(3,WORK)                   
         B     VDR10                                                            
*                                                                               
VR6      DS    0H                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'25'                                                       
         MVI   ELEM+1,30                                                        
*                                                                               
         MVC   QSPWCOD,KQSWCOD    SET CODE                                      
         MVC   QSPWSAM,NOVAL      SET COUNT TO NO VALUE                         
*                                                                               
VR6D     DS    0H                                                               
         CLI   QSPCOMPW,C' '       ARE WEIGHTS ALLOWED                          
         BH    *+16                                                             
         LA    R2,QSWCODH                                                       
         MVI   ERROR,COMPWERR                                                   
         B     TRAPERR                                                          
*                                                                               
         LA    R2,QSWDSCH          DESCRIPTION                                  
         CLI   5(R2),0             ANY INPUT                                    
         BE    VR7                                                              
         GOTO1 ANY                                                              
         MVC   QSPWDSC,WORK                                                     
*                                                                               
VR7      DS    0H                                                               
         BAS   RE,BUMPU            NEXT FIELD- WEIGHT                           
         GOTO1 ANY                                                              
         MVC   BYTE,QSPCWPRE       PRECISION                                    
         NI    BYTE,X'7F'          STRIP HOB                                    
         ZIC   R0,BYTE                                                          
         ZIC   R3,5(R2)            LENGTH                                       
         GOTO1 CASHVAL,DMCB,((R0),8(R2)),(R3)                                   
         CLI   DMCB,X'FF'                                                       
         BE    VR7B                                                             
         ICM   R0,15,DMCB+4        CANNOT BE ZERO                               
         BZ    VR7B                                                             
         LPR   R1,R0                                                            
         C     R1,=F'32767'        MAXIMUM WEIGHT                               
         BH    VR7B                                                             
         CLI   QSPCOMPW,C'M'       IF MULTIPLICATIVE                            
         BNE   VR7D                                                             
         LTR   R0,R0               CANNOT BE NEGATIVE                           
         BP    VR7D                                                             
*                                                                               
VR7B     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VR7D     DS    0H                                                               
         STCM  R0,15,QSPWGT                                                     
         GOTO1 ADDELEM                                                          
         B     VDREC                                                            
*                                                                               
         SPACE 3                                                                
VRNUM    DS    0H                  HANDLE NUMERIC VALIDATION                    
         ST    RE,SAVRE                                                         
         ST    RF,FULL             SAVE DEFAULT                                 
         MVI   ERROR,0                                                          
         GOTO1 ANY                                                              
         MVI   ERROPT,0                                                         
         CLI   ERROR,0             ERROPT MAY HAVE BEEN SET                     
         BNE   VRNUM4              NO DATA MEANS USE DEFAULT                    
         MVC   WK,WORK             SET FOR VALNUM                               
         MVC   LWK,5(R2)                                                        
         GOTO1 VALNUM                                                           
*                                                                               
VRNUM4   DS    0H                                                               
         MVI   ERROR,0                                                          
         L     R0,FULL             RETURN VALUE IN FULL AND R0                  
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
*        DISPLAY RECORD                                                         
*        --------------                                                         
         SPACE 1                                                                
DREC     DS    0H                                                               
         LA    R2,QSWREDH          BUILD REDEFINITION DATE LIST                 
         BAS   RE,BLDRED                                                        
*                                                                               
         LA    R2,QSWOPTH                                                       
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
*                                  DISPLAY WEIGHT FIELDS                        
*                                  ---------------------                        
DR8      DS    0H                                                               
         L     R6,AIO              GET WEIGHT ELEM                              
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DR8B     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                ELEM MUST BE THERE                           
*                                                                               
         USING QSPWGTEL,R6                                                      
         CLC   QSPWCOD,KQSWCOD     FIND RIGHT ONE                               
         BNE   DR8B                                                             
*                                                                               
         LA    R2,QSWDSCH          DESCRIPTION                                  
         MVC   WORK(10),QSPWDSC                                                 
         BAS   RE,GENDISP                                                       
*                                                                               
         BAS   RE,BUMPU            WEIGHT                                       
         BAS   RE,FMTWGT                                                        
         MVC   WORK,WK                                                          
         BAS   RE,GENDISP                                                       
*                                                                               
         B     VDREC                                                            
*                                                                               
DR10     DS    0H                                                               
         B     XIT                                                              
         SPACE 3                                                                
*        VALIDATE/DISPLAY 'FREE' FIELDS                                         
         SPACE 2                                                                
VDREC    DS    0H                                                               
         MVI   RWSW,C'N'           CLEAR REWRITE SWITCH                         
         LA    R2,QSWSAMH          CLEAR COUNT DISPLAY                          
         MVC   WORK,SPACES                                                      
         BAS   RE,GENDISP                                                       
         LA    R2,QSWPOPH                                                       
         BAS   RE,GENDISP                                                       
*                                                                               
         LA    R2,QSWSPC1H         SPECIFICATION                                
         MVI   ELCODE,X'51'                                                     
         MVI   FREEMAX,7           7 LINES                                      
         MVC   FREEKEY(2),CWAVDAT     'KEY' IS WAVE DATE                        
         MVC   FREEKEY+2(1),KQSWCOD   AND WGT CODE                              
         MVI   FREEKLN,3           'KEY' LENGTH                                 
         MVI   FREECTL,X'80'       REQUIRED                                     
         GOTO1 VALIFREE                                                         
*                                                                               
         CLC   =C'DELETE',QSWSPC1  TEST DELETING THIS WAVE                      
         BNE   VDR4                NO                                           
         MVI   FREECTL,0           SET NOT REQUIRED                             
         MVI   QSWSPC1H+5,0        BACK TO VALIFREE                             
         GOTO1 VALIFREE            TO REMOVE ELEMENT                            
         B     VDR9                                                             
*                                                                               
VDR4     DS    0H                  FIND COMP WEIGHT ELEM                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VDR8     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         USING QSPWGTEL,R6                                                      
         CLC   QSPWCOD,KQSWCOD     MUST BE RIGHT ONE                            
         BNE   VDR8                                                             
*                                                                               
         L     R3,VMPQBLK                                                       
         USING MPQBLKD,R3                                                       
*                                                                               
         CLI   SQSPEC,C'$'         SKIP SPEC EDIT FOR LIB MEMBERS               
         BE    VDR9                                                             
         CLI   SURWMRGE,C'Y'       COUNTS ONLY IF WAVES MERGED                  
         BNE   VDR8D                                                            
         CLC   QSPWSAM,NOVAL       TEST COUNTS HAVE BEEN CLEARED                
         BNE   VDR8D                                                            
         MVI   FORCESPC,C'Y'       YES- FORCE SPEC VALIDATE                     
         MVI   RWSW,C'Y'           AND SET TO REWRITE RECORD                    
*                                                                               
VDR8D    DS    0H                                                               
         MVC   SAVOPT,VSPOPT                                                    
         OI    VSPOPT,X'40'        SUPPRESS COUNTS (UNTIL FILTER                
*                                  ON BASE)                                     
         MVI   ELCODE,X'51'        VALIDATE SPEC                                
         MVI   NUMSPOK,C'N'        DONT ALLOW NUMERIC SPEC                      
         GOTO1 VALSPEC,DMCB,(R2)                                                
         TM    NEWSPEC,X'80'       TEST SPEC PROC'D THIS TIME                   
         BZ    VDR8M               NO- DONE                                     
         CLI   SURWMRGE,C'N'       DONE-IF WAVES NOT MERGED                     
         BE    VDR8M               (NO COUNTS)                                  
*                                  'AND' WITH QSPEC BASE SPEC                   
         L     RF,ATIA             HOLD THIS VECTOR IN TIA                      
         LH    R1,MPQBBVLN                                                      
         LA    RE,SYSD                                                          
         AH    RE,=Y(MQSTACK-SYSD)                                              
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         LA    RF,DUB              SET FILTER LIST                              
         ST    RF,MPQBFLST                                                      
         MVC   0(4,RF),ATIA        A(FILTER VECTOR)                             
         MVI   0(RF),X'C0'         SET ACTIVE AND EOL                           
*                                  NOW DO BASE SPEC                             
         NI    VSPOPT,X'BF'        LIFT COUNT SUPRESSING                        
         MVI   FREEKEY+2,0         USE QSP BASE SPEC ELEMS                      
         GOTO1 VALSPEC,DMCB,0                                                   
         XC    MPQBFLST,MPQBFLST   CLEAR FILTER LIST                            
         CLI   SURWMRGE,C'N'       DONT SAVE COUNTS IF                          
         BE    *+10                WAVES NOT MERGED                             
         MVC   QSPWSAM,VSPSAM                                                   
*                                                                               
VDR8M    DS    0H                                                               
         MVC   VSPSAM,QSPWSAM      DISPLAY COUNTS                               
         GOTO1 DSPCOUNT,DMCB,QSWSAMH,QSWPOPH                                    
         DROP  R3                                                               
*                                                                               
VDR9     DS    0H                                                               
         CLI   MODE,VALREC         IF NOT VALIDATING                            
         BE    VDR10               RETURN TO DISPLAY LOGIC                      
*                                                                               
VDR9B    DS    0H                                                               
         CLI   RWSW,C'Y'           TEST TO REWRITE REC EVEN ON DISPLAY          
         BNE   DR10                                                             
*                                                                               
VDR10    DS    0H                                                               
*                                                                               
         SPACE 2                                                                
NODPUT   DS    0H                                                               
         MVI   IOOPT,C'Y'          MY OWN IO                                    
         L     R5,VNODBLK                                                       
         USING NODBLKD,R5                                                       
         XC    NDHOOK,NDHOOK       NO HOOK                                      
         GOTO1 NODIO,DMCB,VNODBLK,=C'PUT',SQSPEC,0                              
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         LA    R2,QSWQSPH                                                       
         GOTO1 NODERRP             HANDLE NODIO ERRORS                          
         DROP  R5                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       FOR ADDS                                     
         BNE   VDR26                                                            
         MVI   MSGNUM,ADDMSG       SET ADD MESSAGE                              
         LA    R2,QSWCODH                                                       
         GOTO1 MSGSET                                                           
*                                                                               
VDR26    DS    0H                                                               
         CLI   ACTNUM,ACTDEL       FOR DELETES                                  
         BE    VDR27                                                            
         CLI   MODE,RECDEL                                                      
         BNE   VDR28                                                            
*                                                                               
VDR27    DS    0H                                                               
         MVI   MSGNUM,DELMSG      SET DELETE MESSAGE                            
         LA    R2,QSWCODH                                                       
         GOTO1 MSGSET                                                           
         GOTO1 ERREX2              DONT LET NODIO DELETE RECORD                 
*                                                                               
VDR28    DS    0H                                                               
         MVI   IOOPT,C'Y'          TELL GENCON NOT TO BOTHER                    
         B     XIT                                                              
         SPACE 2                                                                
FMTWGT   DS    0H                  **NEED SOFT DECIMAL POINT                    
         EDIT  (B4,QSPWGT),(13,WK),3,FLOAT=-,ALIGN=LEFT                         
         BR    RE                                                               
         EJECT                                                                  
*        VALIDATE OPTIONS                                                       
         SPACE 2                                                                
VALOPT   NTR1                                                                   
         L     R3,VMPQBLK                                                       
         USING MPQBLKD,R3                                                       
*                                                                               
         XC    SCANWRK,SCANWRK                                                  
         GOTO1 SCANNER,DMCB,(R2),SCANWRK                                        
*                                                                               
         LA    R5,SCANWRK                                                       
         XC    WAVDATE,WAVDATE     DATE DEFAULTS TO 1ST WAVE                    
         XC    SAVWAVDT,SAVWAVDT                                                
*                                                                               
         NI    VSPOPT,X'BF'        DONT SUPRESS COUNTS                          
         CLI   SURWMRGE,C'Y'       IF WAVES MERGED                              
         BE    *+8                                                              
         OI    VSPOPT,X'40'        ELSE SUPRESS COUNTS                          
*                                                                               
VOPT2    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    VOPT40                                                           
*                                                                               
         CLC   =C'WAVE',12(R5)     WAVE SET                                     
         BNE   VOPT4                                                            
*                                  USE VALWAVE                                  
         MVI   OPTION2,C'W'        DATA WILL BE BE IN WORK                      
         MVI   OPTION,C'Y'         DO FULL VALIDATION                           
         MVC   WORK,22(R5)                                                      
         GOTO1 VALWAVE                                                          
         CLI   ERROR,0                                                          
         BNE   TRAPERR                                                          
*                                                                               
         CLI   WAVDATE,X'FF'       IF FIRST WAVE                                
         BNE   *+10                                                             
         XC    WAVDATE,WAVDATE     CLEAR                                        
         CLI   MODE,VALREC         UNLESS VALIDATING                            
         BE    VOPT20                                                           
         CLI   ACTNUM,ACTLIST      OR LISTING                                   
         BE    VOPT20                                                           
         MVI   BYTE,C'E'           MUST FIND EXACT MATCH                        
         BAS   RE,FNDWVE           IN QSPEC WAVE LIST                           
         BE    VOPT20                                                           
         MVI   ERROR,WAVNFERQ      NOT FOUND FOR QSPEC                          
         B     TRAPERR                                                          
*                                                                               
VOPT4    DS    0H                                                               
         CLI   TWAOFFC,C'*'        WGTS FOR DDS ONLY                            
         BNE   VOPT6                                                            
         CLC   =C'WEIGHT',12(R5)   WEIGHT                                       
         BE    VOPT5                                                            
         CLC   =C'WGT',12(R5)                                                   
         BNE   VOPT6                                                            
*                                                                               
VOPT5    DS    0H                                                               
         ST    R5,WGTOPTA          SAVE OPTION POSITION                         
         MVC   WORK(4),22(R5)      WEIGHT CODE (BLANK MEANS DEFAULT)            
         GOTO1 VALWGT                                                           
         BNE   VOPT91                                                           
         MVI   MPQBNOWT,C'N'       WGTS ACTIVE                                  
         B     VOPT20                                                           
*                                                                               
VOPT6    DS    0H                                                               
         B     VOPT91              ERROR-UNDEFINED ENTRY                        
*                                                                               
VOPT20   DS    0H                                                               
         LA    R5,32(R5)                                                        
         B     VOPT2                                                            
*                                                                               
VOPT40   DS    0H                  END OF SCAN LIST                             
         CLI   MPQBNOWT,C'N'       IF WGTS ACTIVE                               
         BNE   VOPT41                                                           
         TM    VSPOPT,X'40'        COUNTS MUST BE ALSO                          
         BZ    VOPT41                                                           
         L     R5,WGTOPTA          POINT TO WGT OPTION                          
         B     VOPT91                                                           
*                                                                               
VOPT41   DS    0H                                                               
         CLI   WAVDATE,0           TEST HAVE WAVE                               
         BNE   VOPT42              YES                                          
*                                  NO- SET TO FIRST WAVE                        
         MVI   OPTION2,C'V'        SET ALREADY HAVE DATE                        
         GOTO1 VALWAVE             WILL SET MPQBLK                              
*                                                                               
VOPT42   DS    0H                                                               
         MVC   CWAVDAT,WAVDATE     SET COMPLEMENTED WAVE DATE                   
         XC    CWAVDAT,=2X'FF'                                                  
*                                                                               
VOPTX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
VOPT91   DS    0H                                                               
         XC    MSGTXT,MSGTXT                                                    
         MVC   MSGTXT(41),=C'** ERROR** INVALID OPTION - FIELD NO. NN*'         
         LA    R0,SCANWRK-32                                                    
         SR    R5,R0                                                            
         SRL   R5,5                /32                                          
         LA    R3,MSGTXT+38                                                     
         EDIT  (R5),(2,0(R3)),FILL=0                                            
         MVI   MSGNUM,0                                                         
         GOTO1 MSGSET                                                           
         GOTO1 ERREX2                                                           
         SPACE 3                                                                
*        FNDWVE- FIND ACTIVE WAVE FOR QSPEC                                     
*                                                                               
FNDWVE   DS    0H                                                               
         LA    RF,WVLIST                                                        
         MVC   DUB(2),WAVDATE                                                   
         XC    DUB(2),=2X'FF'      COMPLEMENT                                   
*                                                                               
FNDW4    DS    0H                                                               
         CLI   0(RF),0             EOL                                          
         BE    FNDWERR                                                          
         CLC   DUB(2),0(RF)                                                     
         BL    FNDW6              NEXT HIGHER                                   
         BE    FNDW8              EXACT HIT                                     
         LA    RF,3(RF)                                                         
         B     FNDW4                                                            
*                                                                               
FNDW6    DS    0H                                                               
         CLI   BYTE,C'E'           IF NEEDED EXACT HIT                          
         BE    FNDWERR             WE DIDNT FIND IT                             
*                                                                               
FNDW8    DS    0H                                                               
         MVC   WAVDATE,DUB                                                      
         XC    WAVDATE,=2X'FF'     UN-COMPLEMENT                                
         OC    SAVWAVDT,SAVWAVDT   IF HAVE OTHER WAVE DATE                      
         BZ    *+14                                                             
         CLC   SAVWAVDT,WAVDATE    THIS MUST MATCH                              
         BER   RE                                                               
*                                                                               
FNDWERR  DS    0H                                                               
         LTR   RE,RE               CC NOT = ON ERROR                            
         BR    RE                                                               
         DROP   R3                                                              
         EJECT                                                                  
*        BLDRED- BUILD REDEFINITION DATE LIST                                   
         SPACE 2                                                                
BLDRED   NTR1                                                                   
         MVC   WORK,SPACES                                                      
         BAS   RE,GENDISP                                                       
*                                                                               
         SR    R3,R3               FOR COUNT OF REDEFINITONS                    
         MVI   WVLIST,0            SET EOL                                      
         L     R6,AIO                                                           
         AH    R6,DATADISP                                                      
*                                                                               
BRED4    DS    0H                                                               
         CLI   0(R6),X'51'         LOOK AT 51 ELEMS- REGULAR SPECS              
         BE    BRED6                                                            
         CLI   0(R6),X'52'         AND 52'S- BASES                              
         BE    BRED6                                                            
         CLI   0(R6),0             EOR                                          
         BE    BRED7                                                            
*                                                                               
BRED5    DS    0H                  NEXT ELEM                                    
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     BRED4                                                            
*                                                                               
BRED6    DS    0H                                                               
         CLC   4(1,R6),KQSWCOD     MUST BE FOR RIGHT WGT COD                    
         BNE   BRED5                                                            
         LA    R4,WVLIST           SET SPEC DATE IN WVLIST                      
*                                                                               
BRED6B   DS    0H                                                               
         CLI   0(R4),0             EOL                                          
         BE    BRED6D                                                           
         CLC   2(2,R6),0(R4)       TEST DATES =                                 
         BE    BRED6F              YES - NEXT ELEM                              
         LA    R4,3(R4)                                                         
         B     BRED6B                                                           
*                                                                               
BRED6D   DS    0H                                                               
         MVC   0(2,R4),2(R6)       SET IN LIST                                  
         LA    R3,1(R3)            BUMP COUNT                                   
         MVI   3(R4),0             SET NEW EOL                                  
*                                                                               
BRED6F   DS    0H                                                               
         CLC   =C'NOT DEF',6(R6)   IF NOT DEFINED                               
         BNE   *+8                                                              
         OI    2(R4),X'80'         SET AS SUCH                                  
         B     BRED5                                                            
*                                                                               
BRED7    DS    0H                  END OF RECORD                                
         CH    R3,=H'1'            UNLESS MORE THAN ONE DEFINITION              
         BNH   BREDX               DONT SHOW LIST                               
*                                                                               
         MVC   WORK(16),=C'**REDEFINED FOR*'                                    
         LA    R6,WORK+18                                                       
         LA    R5,8                MAX ON LINE                                  
         LA    R4,WVLIST                                                        
*                                                                               
BRED9    DS    0H                                                               
         CLI   0(R4),0             EOL                                          
         BE    BRED10                                                           
         CLI   0(R4),X'FF'         SKIP 'FIRST' WAVE                            
         BE    BRED10                                                           
         MVC   DUB(2),0(R4)                                                     
         XC    DUB(2),=2X'FF'     COMPLEMENT                                    
         GOTO1 DATCON,DMCB,(2,DUB),(6,0(R6))                                    
         TM    2(R4),X'80'         TEST NOT DEFINED                             
         BZ    *+12                                                             
         MVI   6(R6),C'*'                                                       
         LA    R6,1(R6)                                                         
         MVI   6(R6),C','                                                       
         LA    R6,7(R6)                                                         
         LA    R4,3(R4)                                                         
         BCT   R5,BRED9                                                         
*                                                                               
BRED10   DS    0H                                                               
         BCTR  R6,R0                                                            
         MVI   0(R6),C' '          CLEAR LAST COMMA                             
         BAS   RE,GENDISP                                                       
*                                                                               
BREDX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        LIST ROUTINE                                                           
         SPACE 2                                                                
PLIST    DS    0H                  HARD COPY ENTRY POINT                        
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LIST     DS    0H                  SCREEN LIST ENTRY POINT                      
         LA    R2,QWLOPTH                                                       
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
         OC    KEY,KEY             FIRST TIME TEST                              
         BNZ   *+10                NO                                           
         MVC   STRTCOD,KQSWCOD     ON FIRT TIME SET START CODE                  
*                                                                               
         MVC   KEY,SVQSPKEY                                                     
         GOTO1 GETREC                                                           
*                                                                               
         LA    R4,WLIST            FOR MY SAVED WEIGHT LIST                     
         ST    R4,NXTQL                                                         
*                                                                               
         MVI   STRTSW,0            LIST START CONTROL                           
         MVI   ELCODE,X'25'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+12                                                             
*                                                                               
LIST4    DS    0H                                                               
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   LIST20                                                           
         USING QSPWGTEL,R6                                                      
*                                                                               
         CLI   STRTSW,0            TEST HAVE ALREADY FOUND START                
         BNE   LIST5               YES                                          
         CLC   STRTCOD,QSPWCOD     TEST START                                   
         BH    LIST4                                                            
         MVI   STRTSW,1                                                         
*                                                                               
LIST5    DS    0H                                                               
         MVC   STRTCOD,QSPWCOD          SET NEW START CODE                      
         ZIC   RF,STRTCOD+L'STRTCOD-1                                           
         LA    RF,1(RF)                                                         
         STC   RF,STRTCOD+L'STRTCOD-1                                           
*                                                                               
         L     R4,NXTQL                                                         
         MVC   0(1,R4),QSPWCOD     SAVED CODE                                   
         LA    R4,WLISTDL(R4)      NEXT SLOT                                    
         ST    R4,NXTQL                                                         
*                                                                               
         LA    R5,P1               USE P                                        
         CLI   MODE,PRINTREP       IF HARD COPY                                 
         BE    LIST6                                                            
*                                                                               
         MVC   QWLHED(L'LISTHD),LISTHD                                          
         OI    QWLHEDH+6,X'80'                                                  
         LA    R5,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
*                                                                               
LIST6    DS    0H                                                               
         USING LSTLIND,R5                                                       
         EDIT  (B1,QSPWCOD),(3,LCODE)                                           
*                                                                               
         MVC   LDESC,QSPWDSC       DESCRITPION                                  
         BAS   RE,FMTWGT           WEIGHT VALUE                                 
         MVC   LWGT,WK                                                          
*                                                                               
         MVI   ELCODE,X'51'        SPECS                                        
         MVC   FREEKEY(2),CWAVDAT                                               
         MVC   FREEKEY+2(1),QSPWCOD                                             
         MVI   FREEKLN,3                                                        
         MVI   FREEFRST,C'Y'                                                    
         MVI   FREECTL,X'40'       SET TO RETURN IN WORK                        
*                                                                               
         GOTO1 DISPFREE                                                         
         MVC   LSPEC,WORK                                                       
         B     LIST7                                                            
*                                                                               
LIST6D   DS    0H                                                               
         GOTO1 DISPFREE                                                         
         MVC   LSPEC,WORK                                                       
         CLI   LSPEC,C' '          TEST END                                     
         BE    LIST4               YES-NEXT WEIGHT                              
*                                                                               
LIST7    DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LIST8                                                            
*                                                                               
         GOTO1 LISTMON                                                          
         B     LIST4                                                            
*                                                                               
LIST8    DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LIST6D              TRY NEXT SPEC LINE                           
*                                                                               
LIST20   DS    0H                                                               
         B     XIT                                                              
*        SPACE 3                                                                
LISTHD   DC    C'CODE  DESCRIPTION WEIGHT        SPEC'                          
LISTHU   DC    C'----  ----------- ------        ----'                          
         SPACE 2                                                                
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H5(L'LISTHD),LISTHD                                              
         MVC   H6(L'LISTHU),LISTHU                                              
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,2,C'MEDIA PLANNING SYSTEM'                                    
         SSPEC H2,2,C'---------------------'                                    
         SSPEC H1,37,C'COMPONENT WEIGHTS FOR A QSPEC'                           
         SSPEC H2,37,C'-----------------------------'                           
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,96,REQUESTOR                                                  
         SSPEC H5,77,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'               END MARKER FOR SPECS                         
         EJECT                                                                  
*        VARIOUS SHORT ROUTINES                                                 
*        ----------------------                                                 
*                                                                               
GENDISP  ZIC   R1,0(R2)            GENERAL DISPLAY                              
         SH    R1,=H'9'                                                         
         EX    R1,GDCLC            TEST ALREADY ON SCREEN                       
         BER   RE                  YES- RETURN (CC=)                            
         EX    R1,GDMVC            NO- PUT IT THERE                             
         OI    6(R2),X'80'         TRANSMIT                                     
         BR    RE                  RETURN (CC NOT=)                             
*                                                                               
GDCLC    CLC   8(0,R2),WORK                                                     
GDMVC    MVC   8(0,R2),WORK                                                     
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
NUMDISP  DS    0H                  DISPLAY A SIMPLE NUMERIC FIELD               
         EDIT  (B4,FULL),(10,WORK),ALIGN=LEFT                                   
         B     GENDISP                                                          
*                                                                               
BADX     LTR   RB,RB                                                            
         B     XIT                 CC OF NEQ = ERROR EXIT                       
*                                                                               
GOODX    CR    RB,RB                                                            
         B     XIT                 CC OF EQ = GOOD EXIT                         
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE MPGENQS                                                        
       ++INCLUDE MPQBLKD                                                        
       ++INCLUDE DDNODBLKD                                                      
       ++INCLUDE MPRDRFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDRFED                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDREED                                                       
         EJECT                                                                  
       ++INCLUDE MPRDRWORKD                                                     
         SPACE 3                                                                
         ORG   SYSUNON             NON-SAVED AREA                               
WGTOPTA  DS    A                                                                
SAVOPT   DS    C                                                                
RWSW     DS    C                                                                
WVLIST   DS    XL100               CODE ONLY                                    
         SPACE 3                                                                
         ORG   SYSUSAV             OVERLAY SAVED AREA                           
WLISTDL  EQU   1                   CODE ONLY                                    
WLIST    DS    CL(15*WLISTDL)      15 WEIGHTS                                   
*                                                                               
KQSWCOD  DS    X                                                                
STRTCOD  DS    X                                                                
STRTSW   DS    X                                                                
CWAVDAT  DS    XL2                 COMPLEMENTED WAVE DATE                       
SAVWAVDT DS    XL2                                                              
         SPACE 3                                                                
LSTLIND  DSECT                     DSECT FOR LIST DISPLAY LINE                  
         DS    CL1                                                              
LCODE    DS    CL3                                                              
         DS    CL2                                                              
LDESC    DS    CL10                                                             
         DS    CL2                                                              
LWGT     DS    CL13                                                             
         DS    CL1                                                              
LSPEC    DS    CL36                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015MPRDR0E   05/01/02'                                      
         END                                                                    
