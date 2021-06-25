*          DATA SET MPRDR0A    AT LEVEL 045 AS OF 05/01/02                      
*PHASE T5100A,*                                                                 
         TITLE 'T5100A - QSPEC RECORDS'                                         
T5100A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T5100A                                                         
*                                                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T5100A+4096,R7        **2ND BASE REG                             
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
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PLIST                                                            
         B     XIT                                                              
         EJECT                                                                  
*        VALIDATE KEY                                                           
         SPACE 2                                                                
         USING QSPKEY,R4                                                        
*              NOTE ALL QSPEC SCREENS HAVE IDENTICAL KEY FIELDS                 
         SPACE 2                                                                
VKEY     DS    0H                                                               
         MVI   SVERROR,0           CLEAR SAVED ERROR                            
         CLI   ACTNUM,ACTREN       RENAME                                       
         BE    VKREN                                                            
         CLI   ACTNUM,ACTCOPY      COPY                                         
         BE    VKCPY                                                            
*                                                                               
         CLI   RECNUM,RECQSP       FOR QSPEC REC                                
         BNE   VK3                                                              
         CLI   ACTNUM,ACTADD       ADD NOT ALLOWED                              
         BNE   VK3                                                              
         MVI   ERROR,INVRCACT                                                   
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
VK3      DS    0H                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         LA    R2,TOPSURH                                                       
         GOTO1 VALSURV                                                          
*                                  VALIDATE QSPEC                               
         LA    R2,TOPQSPH                                                       
         CLI   ACTNUM,ACTLIST      FOR LIST CODE IS OPTIONAL                    
         BE    VK3M                                                             
         TM    4(R2),X'20'         IF QSPEC CHANGED                             
         BNZ   *+8                                                              
         NI    TOPOPTH+4,X'DF'     MUST RE-EDIT OPTIONS                         
         B     VK4                                                              
*                                                                               
VK3M     DS    0H                                                               
         MVC   WORK,SPACES                                                      
         CLI   5(R2),0             KEY IS OPTIONAL                              
         BE    VK4D                                                             
*                                                                               
VK4      DS    0H                                                               
         GOTO1 ANY                                                              
*                                                                               
VK4D     DS    0H                                                               
         CLI   WORK,C'$'           ALL LIBRARY REFERENCES                       
         BNE   VK4F                                                             
         CLI   TWAOFFC,C'*'        ARE FOR DDS ONLY                             
         BE    VK4F                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
VK4F     DS    0H                                                               
         GOTO1 VALQSP                                                           
         CLI   ACTNUM,ACTADD       FOR ADDS                                     
         BE    VK6                                                              
         CLI   ACTNUM,ACTDEL       AND DELETES                                  
         BE    VK6                                                              
         CLI   ACTNUM,ACTREST      AND RESTORES                                 
         BNE   VK8                                                              
*                                                                               
VK6      DS    0H                                                               
         OI    WHENOK,X'01'        TELL GENCON WE CAN HANDLE                    
*                                                                               
VK8      DS    0H                                                               
         CLI   ACTNUM,ACTLIST      FOR LISTS                                    
         BNE   VK10                                                             
         MVI   SECTLST,C'N'                                                     
         MVI   LIBLST,C'N'                                                      
         CLI   TOPQSP,C'*'         TEST DOING 'SECTION' LIST                    
         BNE   VK8D                                                             
         CLI   TOPQSP+1,C' '                                                    
         BH    VK8D                                                             
         MVI   SECTLST,C'Y'        SET DOING 'SECTION' LIST                     
*                                                                               
VK8D     DS    0H                                                               
         MVI   LEVLIM,C' '         NO DEPTH LIMIT                               
         CLI   RECNUM,RECQSP       FOR QSPEC LIST                               
         BE    *+10                                                             
         MVC   LEVLIM,CONREC       ELSE T,Q,A FROM RECORD TYPE                  
*                                                                               
         CLI   SQSPEC,C'$'         BUT FOR LIBRARY GRPS                         
         BNE   VK8F                                                             
         CLI   SQSPEC+1,C' '       IF $ ALONE                                   
         BNH   VK8E                                                             
         CLI   RECNUM,RECANS       OR IF REC NOT ANSWER                         
         BE    VK8F                                                             
*                                                                               
VK8E     DS    0H                                                               
         MVI   LIBLST,C'Y'         SET DOING LIBLST                             
         MVI   LEVLIM,C'T'         AND LIMIT TO LIBRARY HEADERS                 
*                                                                               
VK8F     DS    0H                                                               
         LA    R2,QSLOPTH                                                       
         CLC   RECNUM,TWALREC      ON CHANGE OF RECORD                          
         BE    *+8                                                              
         NI    4(R2),X'DF'         UNVALIDATE OPTION FIELD                      
         BAS   RE,VALOPT                                                        
         B     VK20                                                             
*                                                                               
VK10     DS    0H                                                               
         CLI   ACTNUM,ACTADD      FOR ADDS                                      
         BNE   *+14                                                             
         MVC   SQTYP,CONREC       SET TYPE FROM RECORD FIELD                    
         B     VK20                                                             
*                                                                               
         BAS   RE,CHKSCR          FOR OTHER MAINTS MAY NEED NEW SCREEN          
         MVC   AIO,AIO3           GENCON MAY READ RECORD- DONT LET IT           
*                                 READ OVER MY VERSION ($ CALLS)                
VK20     DS    0H                                                               
         B     XIT                                                              
         SPACE 2                                                                
*        DISPLAY KEY                                                            
         SPACE 1                                                                
DKEY     DS    0H                                                               
         MVI   SVERROR,0           CLEAR SAVED ERROR                            
         ZIC   R4,SELLISTN         INDEX INTO LISTDIR                           
         MH    R4,=H'6'                                                         
         LA    R4,LISTDIR(R4)                                                   
         MVC   BYTE,0(R4)                                                       
         CLI   0(R4),C'D'          SELECT DELETES NOT ALLOWED                   
         BE    DKEY4                                                            
         CLI   0(R4),C'R'          AND RESTORES                                 
         BNE   DKEY6                                                            
*                                                                               
DKEY4    DS    0H                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,ACTNSERR      ACTION NOT SUPPORTED                         
         MVI   SVERROR,ACTNSERR    SAVE ERROR                                   
         B     TRAPERR                                                          
*                                                                               
DKEY6    DS    0H                                                               
         ZIC   R4,SELLISTN         INDEX INTO QLIST                             
         MH    R4,=Y(QLISTDL)                                                   
         LA    R4,QLIST(R4)                                                     
         USING QLISTD,R4                                                        
*                                                                               
         CLI   QLQSPEC,C' '        MUST BE A RECORD THERE                       
         BH    *+16                                                             
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
         CLI   BYTE,C'A'           SELECT/CHANGES ALLOWED ONLY IF               
         BNE   DKEY7               RECORD SELECTED = CONREC                     
*                                  (ELSE GENCON GETS LOST IF THERE              
*                                  ARE ERRORS)                                  
         LA    RF,RECTOP           TEST CONREC ENTRY                            
         CLI   QLQTYP,C'T'                                                      
         BE    DKEY6H                                                           
         LA    RF,RECQUE                                                        
         CLI   QLQTYP,C'Q'                                                      
         BE    DKEY6H                                                           
         LA    RF,RECANS                                                        
*                                                                               
DKEY6H   DS    0H                                                               
         CLM   RF,1,RECNUM                                                      
         BNE   DKEY4                                                            
*                                                                               
DKEY7    DS    0H                                                               
         MVC   SQTYP,QLQTYP        SET QTYP                                     
         BAS   RE,CHKSCR           AND SEE IF NEED NEW SCREEN                   
*                                                                               
         MVC   WORK(30),QLQSPEC                                                 
         LA    R2,TOPQSPH                                                       
         BAS   RE,GENDISP                                                       
*                                                                               
         GOTO1 VALQSP              MUST LET NODIO READ RECORD                   
*                                  (THERE MAY BE A $ CALL)                      
         MVC   LCQSP,QLQSPEC       SET TO CONTINUE LIST AT THIS REC             
         B     XIT                                                              
         SPACE 3                                                                
*        CHECK SCREEN- LOAD IF NECESSARY                                        
         SPACE 1                                                                
CHKSCR   NTR1                                                                   
*                                  SEE IF HAVE RIGHT SCREEN                     
         MVI   BYTE,X'FA'          TOPIC SCREEN                                 
         CLI   SQTYP,C'T'                                                       
         BE    CKS4                                                             
*                                                                               
         MVI   BYTE,X'FB'          QUEST SCREEN                                 
         CLI   SQTYP,C'Q'                                                       
         BE    CKS4                                                             
*                                                                               
         MVI   BYTE,X'FC'          ANSWER SCREEN                                
*                                                                               
CKS4     DS    0H                                                               
         CLC   BYTE,TWASCR         DO I HAVE THE SCREEN                         
         BE    CKS5                YES-OK                                       
         MVC   TWASCR,BYTE         NO- SET NEW SCREEN                           
         MVC   DMCB+4(4),SYSPHASE                                               
         MVC   DMCB+7(1),BYTE                                                   
         GOTO1 CALLOV,DMCB,CONTAGH,,0                                           
         OC    DMCB+9(3),DMCB+9                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  NEED TO RE-DISPLAY KEY ON NEW SCREEN         
         LA    R2,TOPSURH                                                       
         OI    4(R2),X'20'         SET VALIDATED                                
         MVC   WORK(8),SVQSDKEY+QSPKFCD-QSPKEY                                  
         BAS   RE,GENDISP                                                       
*                                                                               
         LA    R2,TOPQSPH                                                       
         OI    4(R2),X'20'         SET VALIDATED                                
         MVC   WORK(L'SQSAV),SQSAV          QSPEC AS INPUT                      
         BAS   RE,GENDISP                                                       
*                                                                               
         CLI   ACTNUM,ACTCHA       FOR CHANGES                                  
         BNE   *+8                 TELL GENCON KEY IS NEW                       
         OI    TOPSURH+4,X'80'     SO WE WILL GET A DISPLAY                     
*                                                                               
CKS5     DS    0H                                                               
         LA    R4,=CL8'TOPIC'                                                   
         MVI   RECNUM,RECTOP                                                    
         CLI   SQTYP,C'T'                                                       
         BE    CKS5B                                                            
         LA    R4,=CL8'QUESTION'                                                
         MVI   RECNUM,RECQUE                                                    
         CLI   SQTYP,C'Q'                                                       
         BE    CKS5B                                                            
         LA    R4,=CL8'ANSWER'                                                  
         MVI   RECNUM,RECANS                                                    
*                                                                               
CKS5B    DS    0H                                                               
         LA    R2,CONRECH                                                       
         MVC   WORK(8),0(R4)                                                    
         BAS   RE,GENDISP                                                       
*                                                                               
CKS5D    DS    0H                                                               
CHKSCRX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RENAME ROUTINE- ALL PROCESSING AT VALKEY                               
*                                                                               
VKREN    DS    0H                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         LA    R2,QRNSURH                                                       
         GOTO1 VALSURV                                                          
*                                                                               
         LA    R2,QRNQSOH          VALIDATE OLD QSPEC                           
         GOTO1 ANY                                                              
         GOTO1 VALQSP                                                           
*                                                                               
         LA    R2,QRNQSNH          NEW CODE                                     
         GOTO1 ANY                                                              
         LA    R3,WORK             CHECK FOR INVALID CHARACTERS                 
         ZIC   R4,5(R2)            LENGTH FOR BCT                               
*                                                                               
VKREN4   DS    0H                                                               
         CLI   0(R3),C' '          EMBEDDED BLANKS                              
         BNH   VKREN4E                                                          
         CLI   0(R3),C'.'          MUST BE ONE LEVEL                            
         BE    VKREN4E                                                          
         CLI   0(R3),C','          COMMA                                        
         BE    VKREN4E                                                          
         CLI   0(R3),C'/'                                                       
         BE    VKREN4E                                                          
*                                                                               
         LA    R3,1(R3)                                                         
         BCT   R4,VKREN4                                                        
         B     VKREN5                                                           
*                                                                               
VKREN4E  DS    0H                                                               
         MVI   ERROR,QSFMTERR                                                   
         B     TRAPERR                                                          
*                                                                               
VKREN5   DS    0H                                                               
         ZIC   R0,5(R2)                                                         
         L     R5,VNODBLK                                                       
         USING NODBLKD,R5                                                       
         XC    NDHOOK,NDHOOK       NO HOOK                                      
         GOTO1 NODIO,DMCB,VNODBLK,=C'REN',SQSPEC,((R0),WORK)                    
*                                                                               
         CLI   NDERR,0                                                          
         BE    VKREN10                                                          
         GOTO1 NODERRP             HANDLE NODIO ERROR                           
*                                                                               
VKREN10  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*        COPY ROUTINE- ALL PROCESSING AT VALKEY                                 
*                                                                               
VKCPY    DS    0H                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         LA    R2,QCPSURH                                                       
         GOTO1 VALSURV                                                          
*                                                                               
         LA    R2,QCPQSOH          VALIDATE OLD QSPEC                           
         GOTO1 ANY                                                              
         GOTO1 VALQSP                                                           
*                                                                               
         L     R5,VNODBLK                                                       
         USING NODBLKD,R5                                                       
         MVC   CPYOLD,SQSPEC       SAVE OLD QSPEC                               
         MVC   OLDTYP,SQTYP        AND TYPE                                     
*                                                                               
         LA    R2,QCPQSNH          NEW CODE                                     
         GOTO1 ANY                                                              
         IC    R0,ACTNUM           SAVE ACTNUM                                  
         MVI   ACTNUM,ACTADD       TELL VALQSP WE'RE ADDING                     
         GOTO1 VALQSP                                                           
         STC   R0,ACTNUM           RESET ACTNUM                                 
*                                  NB- VALQSP WILL RETURN SQTYP                 
*                                  OF LAST LEVEL READ                           
         CLI   SQTYP,C' '          NO TYPE FOR NEW MEANS ADDING                 
         BNH   VKCPY8              AT LEVEL 1- ANY TYPE OK                      
         CLC   SQTYP,OLDTYP        ELSE MUST BE HIGHER                          
         BNH   VKCPY91             (T-Q,T-A,Q-A)                                
*                                                                               
VKCPY8   DS    0H                                                               
         XC    NDHOOK,NDHOOK       NO HOOK                                      
         GOTO1 NODIO,DMCB,VNODBLK,=C'COPY',CPYOLD,0,WORK                        
*                                                                               
         CLI   NDERR,0                                                          
         BE    VKCPY10                                                          
         GOTO1 NODERRP             HANDLE NODIO ERROR                           
*                                                                               
VKCPY10  DS    0H                                                               
         XC    NDSETACD,NDSETACD   CLEAR ATTACH CODE                            
         XC    NDSETAND,NDSETAND   AND NODE                                     
         B     XIT                                                              
*                                                                               
VKCPY91  DS    0H                                                               
         MVI   ERROR,COPYERR                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*        VALIDATE RECORD                                                        
         SPACE 3                                                                
VREC     DS    0H                                                               
         CLI   SVERROR,ACTNSERR    TEST HAD SELECT/CHANGE ERROR                 
         BE    VR04                                                             
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         CLI   TWAOFFC,C'*'        MUST BE DDS TERM                             
         BE    *+16                                                             
*                                                                               
VR04     DS    0H                                                               
         MVI   ERROR,ACTNSERR                                                   
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
         CLI   ACTNUM,ACTREN       FOR RENAMES                                  
         BE    XIT                                                              
         CLI   ACTNUM,ACTCOPY      AND COPIES                                   
         BE    XIT                 IGNORE                                       
*                                                                               
         CLI   ACTNUM,ACTREST      FOR RESTORES                                 
         BE    VR2                                                              
         CLI   ACTNUM,ACTDEL       AND DELETES                                  
         BNE   VR2B                                                             
*                                                                               
VR2      DS    0H                                                               
         MVI   MODE,DISPREC        DO A DISPLAY                                 
         B     DREC                                                             
*                                                                               
VR2B     DS    0H                                                               
         CLI   ACTNUM,ACTADD       FOR ADDS                                     
         BNE   *+8                                                              
         MVI   FORCESPC,C'Y'       FORCE RE-READ OF SPEC                        
*                                                                               
         LA    R2,QUEREDH          DO REDEFINITION LINE NOW                     
         CLI   SQTYP,C'Q'          TO GET WVLIST BUILT                          
         BE    VR2D                                                             
         LA    R2,ANSREDH                                                       
         CLI   SQTYP,C'A'                                                       
         BNE   *+8                                                              
*                                                                               
VR2D     DS    0H                                                               
         BAS   RE,BLDRED                                                        
         LA    R2,TOPOPTH                                                       
         BAS   RE,VALOPT           VALIDATE OPTIONS FIELD                       
*                                                                               
         CLI   ACTNUM,ACTADD       FOR ADDS                                     
         BNE   VR6                                                              
         L     RE,AIO              CLEAR IOA                                    
         L     RF,SIZEIO                                                        
         XCEF                                                                   
*                                  ALSO TEST ANY INPUT                          
         L     R2,AFRSTREC                                                      
*                                                                               
VR3      DS    0H                                                               
         CLI   5(R2),0                                                          
         BH    VR6                 HAVE INPUT                                   
         BAS   RE,BUMPU            NEXT UNPROTECTED FIELD                       
         BNE   VR3                                                              
*                                  END OF SCREEN- NO DATA                       
         L     R2,AFRSTREC                                                      
         MVI   MSGNUM,ENTMSG       ENTER DATA MESSAGE                           
         GOTO1 MSGSET                                                           
         B     XIT                                                              
*                                                                               
VR5      DS    0H                                                               
VR6      DS    0H                                                               
         BAS   RE,GETDFEL          TRY FOR QSPEC DEFINITION ELEM                
         BE    *+8                 FOUND- OK                                    
         BAS   RE,SETDFEL          ELSE SET NEW ONE                             
*                                                                               
         CLI   SQTYP,C'T'          TOPIC                                        
         BE    VTOPIC                                                           
         CLI   SQTYP,C'Q'          QUESTION                                     
         BE    VQUEST                                                           
         CLI   SQTYP,C'A'          ANSWER                                       
         BE    VANSWR                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
*        VTOPIC- VALIDATE TOPIC FIELDS                                          
         SPACE 2                                                                
VTOPIC   DS    0H                                                               
         LA    R2,TOPCPGH          POINT TO CODE BOOK PAGE FIELD                
         BAS   RE,COMEDTA          EDIT OF CODEBOOK AND QUEST. PGS              
         B     VDTOPIC             EDIT/DISPLAY CODE                            
         SPACE 2                                                                
*        VQUEST- VALIDATE QUEST FIELDS                                          
         SPACE 2                                                                
VQUEST   DS    0H                                                               
         LA    R2,QUECPGH          POINT TO CODE BOOK PAGE FIELD                
         BAS   RE,COMEDTA          EDIT OF CODEBOOK AND QUEST. PGS              
         B     VDQUEST             EDIT/DISPLAY CODE                            
         SPACE 2                                                                
*        VANSWR- VALIDATE ANSWER FIELDS                                         
         SPACE 2                                                                
VANSWR   DS    0H                                                               
         LA    R2,ANSCPGH          POINT TO CODE BOOK PAGE FIELD                
         BAS   RE,COMEDTA          EDIT OF CODEBOOK AND QUEST. PGS              
*                                                                               
         LA    R2,ANSARVH          ARITH VALUE                                  
         USING QSPELEM,R6                                                       
         MVC   QSPAVAL,NOVAL       SET NO VALUE                                 
         CLI   5(R2),0                                                          
         BE    VANS4                                                            
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R0)                                      
         CLI   DMCB,X'FF'                                                       
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVC   QSPAVAL,DMCB+4                                                   
*                                                                               
VANS4    DS    0H                                                               
         LA    R2,ANSANCH          ANSWER CLASS                                 
         XC    QSPACLAS,QSPACLAS                                                
         CLI   5(R2),0                                                          
         BE    VANS5                                                            
         GOTO1 ANY                                                              
         MVC   QSPACLAS,WORK                                                    
         CLI   WORK,C'U'           USAGE LEVEL                                  
         BE    VANS5                                                            
         CLI   WORK,C'S'           SOLE USER                                    
         BE    VANS5                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VANS5    DS    0H                  WEIGHT CONTROL                               
         LA    R2,ANSCWTH                                                       
         MVI   QSPCOMPW,0                                                       
         CLI   5(R2),0                                                          
         BE    VANS6                                                            
         GOTO1 ANY                                                              
         MVC   QSPCOMPW,WORK                                                    
         CLI   WORK,C'A'           A=ADDTIVE                                    
         BE    VANS6                                                            
         CLI   WORK,C'M'           M=MULTIPLICATIVE                             
         BE    VANS6                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VANS6    DS    0H                                                               
*                          **NB- PRESET PRECISION AND BINARY DATA LEN           
*                                (MAY WISH TO SOFTEN THESE LATER)               
         MVI   QSPCWPRE,X'83'      SET TO .001                                  
         MVI   QSPCWDLN,2          DATA LENGTH = 2 BYTES                        
*                                                                               
         B     VDANSWR             EDIT/DISPLAY CODE                            
*                                                                               
         SPACE 2                                                                
*        DISPLAY RECORD                                                         
         SPACE 1                                                                
DREC     DS    0H                                                               
         MVI   SVERROR,0           CLEAR SAVED ERROR                            
         MVC   AIO,AIO1            RESTORE AIO                                  
         LA    R2,QUEREDH          DO REDEFINITION LINE NOW                     
         CLI   SQTYP,C'Q'          TO GET WVLIST BUILT                          
         BE    DR2D                                                             
         LA    R2,ANSREDH                                                       
         CLI   SQTYP,C'A'                                                       
         BNE   *+8                                                              
*                                                                               
DR2D     DS    0H                                                               
         BAS   RE,BLDRED                                                        
         LA    R2,TOPOPTH                                                       
         BAS   RE,VALOPT           LOOK AT OPTIONS EVEN ON DISPLAY              
         BAS   RE,GETDFEL          GET QSPEC DEFINITION ELEM                    
*                                                                               
         CLI   TWAOFFC,C'*'        TEST DDS TERM                                
         BE    DR4                                                              
         TM    QSPSTAT,X'80'       TEST DDS ONLY SPEC                           
         BZ    DR4                                                              
         MVI   ERROR,SECLOCK                                                    
         B     TRAPERR                                                          
*                                                                               
DR4      DS    0H                                                               
         CLI   SQTYP,C'T'          TOPIC                                        
         BE    DTOPIC                                                           
         CLI   SQTYP,C'Q'          QUESTION                                     
         BE    DQUEST                                                           
         CLI   SQTYP,C'A'          ANSWER                                       
         BE    DANSWR                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
*        DTOPIC- DISPLAY TOPIC FIELDS                                           
         SPACE 2                                                                
DTOPIC   DS    0H                                                               
         LA    R2,TOPCPGH          POINT TO CODE BOOK PAGE FIELD                
         BAS   RE,COMDSPA          DISP OF CODEBOOK AND QUEST. PGS              
         LA    R2,TOPSTATH                                                      
         BAS   RE,SETLIB           SHOW LIBRARY CALL DATA                       
*                                                                               
         B     VDTOPIC             EDIT/DISPLAY CODE                            
         SPACE 2                                                                
*        DQUEST- DISPLAY QUEST FIELDS                                           
         SPACE 2                                                                
DQUEST   DS    0H                                                               
         LA    R2,QUECPGH          POINT TO CODE BOOK PAGE FIELD                
         BAS   RE,COMDSPA          DISP OF CODEBOOK AND QUEST. PGS              
         LA    R2,QUESTATH                                                      
         BAS   RE,SETLIB           SHOW LIBRARY CALL DATA                       
*                                                                               
         B     VDQUEST             EDIT/DISPLAY CODE                            
         SPACE 2                                                                
*        DANSWR- DISPLAY ANSWER FIELDS                                          
         SPACE 2                                                                
DANSWR   DS    0H                                                               
         LA    R2,ANSCPGH          POINT TO CODE BOOK PAGE FIELD                
         BAS   RE,COMDSPA          DISP OF CODEBOOK AND QUEST. PGS              
         BAS   RE,GETDFEL          GET DEFINITION ELEM                          
         USING QSPELEM,R6                                                       
         CLC   QSPAVAL,NOVAL       ARITH VALUE                                  
         BE    DANS4               NOT THERE                                    
         LA    R2,ANSARVH                                                       
         EDIT  (B4,QSPAVAL),(10,WK),2,FLOAT=-,ALIGN=LEFT                        
         MVC   WORK(10),WK                                                      
         BAS   RE,GENDISP                                                       
*                                                                               
DANS4    DS    0H                                                               
         LA    R2,ANSANCH          ANSWER CLASS                                 
         MVC   WORK(L'ANSANC),SPACES                                            
         MVC   WORK(L'QSPACLAS),QSPACLAS                                        
         BAS   RE,GENDISP                                                       
*                                                                               
         LA    R2,ANSCWTH          COMPONENT WEIGHT TYPE                        
         MVC   WORK(L'ANSCWT),SPACES                                            
         MVC   WORK(L'QSPCOMPW),QSPCOMPW                                        
         BAS   RE,GENDISP                                                       
*                                                                               
         LA    R2,ANSSTATH                                                      
         BAS   RE,SETLIB           SHOW LIBRARY CALL DATA                       
         B     VDANSWR             EDIT/DISPLAY CODE                            
         SPACE 3                                                                
*        SETLIB- SHOW LIBRARY CALL DATA                                         
         SPACE 2                                                                
SETLIB   DS    0H                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNER  RE                                                               
         L     R5,VNODBLK                                                       
         USING NODBLKD,R5                                                       
*                                                                               
         MVC   WORK,SPACES                                                      
         CLI   NDLIBLEV,0          ONLY IF LIBRARY INVOLVED                     
         BE    SETLIB4                                                          
*                                                                               
         MVC   WORK(2),=C'$='                                                   
         LA    RF,WORK+2                                                        
         CLC   NDLEV,NDLIBLEV      TEST CALL IS AT THIS LEVEL                   
         BNE   *+14                                                             
         MVC   WORK(6),=C'$CALL='                                               
         LA    RF,WORK+6                                                        
         MVC   0(7,RF),NDATTCOD+1                                               
*                                                                               
SETLIB4  DS    0H                                                               
         LR    R0,RE                                                            
         BAS   RE,GENDISP                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*        VDTOPIC- VALIDATE/DISPLAY TOPIC FIELDS                                 
         SPACE 2                                                                
VDTOPIC  DS    0H                                                               
         LA    R2,TOPTTLH          TITLE                                        
         MVI   ELCODE,X'C1'                                                     
         MVI   FREEMAX,2           2 LINES                                      
         MVI   FREEKEY,0           NO 'KEY'                                     
         MVI   FREEKLN,0           KEY LENGTH                                   
         MVI   FREECTL,X'80'       REQUIRED                                     
         GOTO1 VALIFREE                                                         
*                                                                               
VDEND    DS    0H                                                               
         CLI   MODE,VALREC                                                      
         BNE   VDENDX                                                           
         MVI   IOOPT,C'Y'                                                       
         CLI   ACTNUM,ACTADD       LET NODIO DO ADD                             
         BE    NODADD                                                           
         CLI   ACTNUM,ACTCHA       OR CHANGE                                    
         BE    NODPUT                                                           
         CLI   ACTNUM,ACTSEL       OR SELECT (FOR CHANGE)                       
         BE    NODPUT                                                           
*                                                                               
VDENDX   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*        VDQUEST- VALIDATE/DISPLAY QUESTION FIELDS                              
         SPACE 2                                                                
VDQUEST  DS    0H                                                               
         LA    R2,QUETTLH          TITLE                                        
         MVI   ELCODE,X'C1'                                                     
         MVI   FREEMAX,2           2 LINES                                      
         MVI   FREEKEY,0           NO 'KEY'                                     
         MVI   FREEKLN,0           KEY LEN                                      
         MVI   FREECTL,X'80'       REQUIRED                                     
         GOTO1 VALIFREE                                                         
*                                                                               
         LA    R2,QUEFTXH          FULL TEXT                                    
         MVI   ELCODE,X'27'                                                     
         MVI   FREEMAX,4           4 LINES                                      
         MVI   FREECTL,0           NOT REQUIRED                                 
         GOTO1 VALIFREE                                                         
*                                                                               
         LA    R2,QUESAMH          CLEAR COUNT DISPLAY                          
         MVC   WORK,SPACES                                                      
         BAS   RE,GENDISP                                                       
         LA    R2,QUEPOPH                                                       
         BAS   RE,GENDISP                                                       
*                                                                               
         LA    R2,QUEBSDH          BASE DESCRIPTION                             
         CLI   MODE,VALREC         IF VALIDATING                                
         BNE   VDQUE2                                                           
         MVI   MAX,2               TREAT DESC AND SPEC AS A PAIR                
         MVI   OPTION,2            EVERY OTHER (2ND DESC IS OPT)                
         GOTO1 MULTIF                                                           
*                                  EVEN IF BOTH EMPTY- DO VALIDATE              
VDQUE2   DS    0H                  TO CLEAR ELEMS                               
         MVI   ELCODE,X'28'                                                     
         MVI   FREEMAX,1           1 LINE                                       
         GOTO1 VALIFREE                                                         
*                                                                               
         LA    R2,QUEBSSH          BASE SPECIFICATION                           
         MVI   ELCODE,X'52'                                                     
         MVI   FREEKLN,3           KEY=DATE(2) + SPARE                          
         MVC   FREEKEY(2),CWAVDAT                                               
         MVI   FREEKEY+2,0                                                      
         GOTO1 VALIFREE                                                         
*                                                                               
         CLC   =C'DELETE',QUEBSS   TEST DELETING THIS WAVE                      
         BNE   VDQUE3              NO                                           
         MVI   QUEBSSH+5,0         YES-BACK TO VALIFREE                         
         GOTO1 VALIFREE            TO REMOVE ELEMENT                            
         B     VDQUE4                                                           
*                                                                               
VDQUE3   DS    0H                  VALIDATE SPEC                                
         CLI   SQSPEC,C'$'         BUT SKIP IF WITHIN LIB                       
         BE    VDQUE4                                                           
         BAS   RE,GETDFEL          GET DEFINITION ELEM                          
         USING QSPELEM,R6                                                       
*                                                                               
         MVI   ELCODE,X'52'        VALIDATE SPECS                               
         MVI   NUMSPOK,C'N'        DONT ALLOW NUMERIC SPEC                      
         GOTO1 VALSPEC,DMCB,(R2)                                                
         CLI   SURWMRGE,C'N'       TEST WAVES MERGED                            
         BE    VDQUE3D             NO - DONT SET COUNTS                         
         TM    NEWSPEC,X'40'       TEST COUNTS SET THIS TIME                    
         BZ    VDQUE3C             NO- DONT SET COUNTS                          
         MVC   QSPSAM,VSPSAM       SET SAMPLE COUNT                             
*                                                                               
VDQUE3C  DS    0H                                                               
         MVC   VSPSAM,QSPSAM       DISPLAY COUNTS                               
VDQUE3D  DS    0H                                                               
         GOTO1 DSPCOUNT,DMCB,QUESAMH,QUEPOPH                                    
*                                                                               
VDQUE4   DS    0H                                                               
         LA    R2,QUENUMFH         NUMERIC FIELD DEFINITION                     
         MVI   ELCODE,X'51'                                                     
         MVI   FREEMAX,1                                                        
         MVI   FREEKLN,3           KEY=DATE(2) + SPARE                          
         MVC   FREEKEY(2),CWAVDAT                                               
         MVI   FREEKEY+2,0                                                      
         GOTO1 VALIFREE                                                         
*                                                                               
         CLC   =C'DELETE',QUENUMF  TEST DELETING THIS WAVE                      
         BNE   VDQUE5                                                           
         MVI   5(R2),0              YES- BACK TO VALIFREE                       
         GOTO1 VALIFREE             TO REMOVE ELEMENT                           
*                                                                               
VDQUE5   DS    0H                  VALIDATE SPEC                                
         CLI   5(R2),0             IF NO INPUT- DONE                            
         BE    VDQUE6                                                           
         CLI   SQSPEC,C'$'         AND SKIP IF WITHIN LIB                       
         BE    VDQUE6                                                           
*                                                                               
         MVI   NUMSPOK,C'R'        MUST BE NUMERIC SPEC                         
         GOTO1 VALSPEC,DMCB,(R2)     NO COUNTS                                  
*                                                                               
VDQUE6   DS    0H                                                               
         BAS   RE,GETDFEL                                                       
         MVC   QSPQTYP,SPECTYP     SET AS NUMERIC                               
*                                                                               
         B     VDEND                                                            
         EJECT                                                                  
*        VDANSWR- VALIDATE/DISPLAY ANSWER FIELDS                                
         SPACE 2                                                                
VDANSWR  DS    0H                                                               
         LA    R2,ANSTXTH          TEXT                                         
         MVI   ELCODE,X'C1'                                                     
         MVI   FREEMAX,3           3 LINES                                      
         MVI   FREEKEY,0           NO 'KEY'                                     
         MVI   FREEKLN,0           KEY LENGTH                                   
         MVI   FREECTL,X'80'       REQUIRED                                     
         GOTO1 VALIFREE                                                         
*                                                                               
         LA    R2,ANSCHDH          COLUMN HEADS                                 
         MVI   ELCODE,X'C2'                                                     
         MVI   FREEMAX,5           5 LINES                                      
         MVI   FREECTL,0           NOT REQUIRED                                 
         GOTO1 VALIFREE                                                         
*                                                                               
         LA    R2,ANSSAMH          CLEAR COUNT DISPLAY                          
         MVC   WORK,SPACES                                                      
         BAS   RE,GENDISP                                                       
         LA    R2,ANSPOPH                                                       
         BAS   RE,GENDISP                                                       
*                                                                               
         LA    R2,ANSSPCH          SPECIFICATION                                
         MVI   ELCODE,X'51'                                                     
         MVI   FREEMAX,3           3 LINES                                      
         MVI   FREECTL,X'80'       SET INPUT REQUIRED                           
         MVI   FREEKLN,3           KEY=DATE(2) + SPARE                          
         MVC   FREEKEY(2),CWAVDAT                                               
         MVI   FREEKEY+2,0                                                      
         GOTO1 VALIFREE                                                         
*                                                                               
         CLC   =C'DELETE',ANSSPC   TEST DELETING THIS WAVE                      
         BNE   VDANS3              NO                                           
         MVI   FREECTL,0           SET NOT REQUIRED                             
         MVI   ANSSPCH+5,0         BACK TO VALIFREE                             
         GOTO1 VALIFREE            TO REMOVE ELEMENT                            
         B     VDANS4                                                           
*                                                                               
VDANS3   DS    0H                  VALIDATE SPEC                                
         CLI   SQSPEC,C'$'         BUT SKIP IF WITHIN LIB                       
         BE    VDANS4                                                           
         BAS   RE,GETDFEL          GET DEFINITION ELEM                          
         USING QSPELEM,R6                                                       
*                                                                               
         MVI   ELCODE,X'51'        VALIDATE SPECS                               
         MVI   NUMSPOK,C'N'        DONT ALLOW NUMERIC SPEC                      
         GOTO1 VALSPEC,DMCB,(R2)                                                
         TM    NEWSPEC,X'20'       TEST SPEC ACTUALLY AMMENDED                  
         BZ    *+8                                                              
         BAS   RE,WGTCLR           YES- CLEAR ANY COMP WGT COUNTS               
         CLI   SURWMRGE,C'N'       TEST WAVES MERGED                            
         BE    VDANS3D             NO DONT SET COUNTS                           
         TM    NEWSPEC,X'40'       TEST COUNTS SET THIS TIME                    
         BZ    VDANS3C             NO- DONT SET COUNT                           
         MVC   QSPSAM,VSPSAM       SET SAMPLE COUNT                             
*                                                                               
VDANS3C  DS    0H                                                               
         MVC   VSPSAM,QSPSAM       DISPLAY COUNTS                               
VDANS3D  DS    0H                                                               
         GOTO1 DSPCOUNT,DMCB,ANSSAMH,ANSPOPH                                    
*                                                                               
VDANS4   DS    0H                                                               
         B     VDEND                                                            
         SPACE 2                                                                
NODADD   DS    0H                                                               
         L     R5,VNODBLK                                                       
         USING NODBLKD,R5                                                       
         XC    NDHOOK,NDHOOK       NO HOOK                                      
         XC    DMCB+12(4),DMCB+12  CLEAR PARAM4                                 
         CLI   NODBA,C' '          IF NO POSITIONING                            
         BNH   NODADD2                                                          
*                                                                               
         LA    RF,NODPOS                                                        
         ST    RF,DMCB+12                                                       
         MVI   DMCB+12,0           NO LENGTH                                    
         OC    NODPOS,NODPOS       IF ADD AT START                              
         BZ    *+8                                                              
         MVI   DMCB+12,8           LENGTH OF POSITIONER                         
*                                                                               
NODADD2  DS    0H                                                               
         L     RF,NDIOA            **NB- TEMP TEST FOR BAD CTL BYTES            
         AH    RF,NDCNTL                                                        
         OC    0(4,RF),0(RF)                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 NODIO,DMCB,VNODBLK,(NODBA,=C'ADD'),SQSPEC                        
         CLI   NDERR,0                                                          
         BNE   NODADD4                                                          
         MVI   MSGNUM,ADDMSG       RETURN REC ADDED MESSAGE                     
         LA    R2,TOPSURH                                                       
         GOTO1 MSGSET                                                           
         B     XIT                                                              
*                                                                               
NODADD4  DS    0H                                                               
         LA    R2,TOPSURH          CURSOR TO KEY                                
         CLI   NDERR,NDPMDERR      UNLESS POSITION ERROR                        
         BNE   *+8                                                              
         LA    R2,TOPOPTH          THEN TO OPTIONS                              
         GOTO1 NODERRP             HANDLE NODIO ERRORS                          
         SPACE 2                                                                
NODPUT   DS    0H                                                               
         L     R5,VNODBLK                                                       
         USING NODBLKD,R5                                                       
         XC    NDHOOK,NDHOOK       NO HOOK                                      
*                                                                               
         L     RF,NDIOA            **NB- TEMP TEST FOR BAD CTL BYTES            
         AH    RF,NDCNTL                                                        
         OC    0(4,RF),0(RF)                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 NODIO,DMCB,VNODBLK,=C'PUT',SQSPEC,0                              
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         LA    R2,TOPSURH                                                       
         GOTO1 NODERRP             HANDLE NODIO ERRORS                          
         DROP  R5                                                               
*                                                                               
         SPACE 3                                                                
*        SETDFEL- SET QSPEC DEFINITION ELEM                                     
         SPACE 2                                                                
SETDFEL  DS    0H                                                               
         ST    RE,SAVRE                                                         
         LA    R6,ELEM                                                          
         USING QSPELEM,R6                                                       
*                                                                               
         XC    ELEM(50),ELEM                                                    
         MVI   0(R6),X'10'                                                      
         MVI   1(R6),60            LENGTH                                       
*                                                                               
         MVC   QSPTYP,SQTYP        SET QSPEC TYPE                               
         MVC   QSPSAM,NOVAL        AND SET COUNTS TO NO VALUE                   
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R6,AIO                                                           
         BAS   RE,GETEL            POINT R6 TO ELEM                             
*                                                                               
         L     RE,SAVRE                                                         
         BR    RE                                                               
         SPACE 3                                                                
*        GETDFEL- POINT R6 TO QSPEC DEFINITION ELEM                             
         SPACE 2                                                                
GETDFEL  DS    0H                                                               
         ST    RE,SAVRE                                                         
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         L     RE,SAVRE            RETRUN WITH CC SET                           
         BR    RE                                                               
*                                                                               
         SPACE 3                                                                
*        COMEDTA- COMMON EDIT 'A' (CODEBOOK PAGE + QUESTIONNAIRE PG)            
*                 (R6 POINTS TO QSPELEM, R2 TO 1ST SCREEN FIELD)                
         SPACE 2                                                                
COMEDTA  NTR1                                                                   
         ST    R2,SVFLDH           SAVE FIRST FIELD                             
         USING QSPELEM,R6                                                       
*                                  CODEBOOK PAGE                                
         MVC   QSPCBPG,SPACES                                                   
         CLI   5(R2),0                                                          
         BE    CEDA4                                                            
         GOTO1 ANY                                                              
         MVC   QSPCBPG,WORK                                                     
*                                                                               
CEDA4    DS    0H                                                               
         CLI   QSPCBPG,C' '        TEST ANY                                     
         BH    CEDA4D                                                           
         CLI   SQTYP,C'Q'          IF QUESTION                                  
         BNE   CEDA4B                                                           
         MVC   QSPCBPG,SQTCBPG     TAKE FROM TOPIC                              
         B     CEDA4D                                                           
*                                                                               
CEDA4B   DS    0H                                                               
         CLI   SQTYP,C'A'          IF ANSWER                                    
         BNE   CEDA4D                                                           
         MVC   QSPCBPG,SQQCBPG     TAKE FROM QUESTION                           
         CLI   QSPCBPG,C' '                                                     
         BH    CEDA4D                                                           
         MVC   QSPCBPG,SQTCBPG     OR TOPIC                                     
*                                                                               
CEDA4D   DS    0H                  QUESTIONNARE PAGE                            
         BAS   RE,BUMPU                                                         
         MVC   QSPQUPG,SPACES                                                   
         CLI   5(R2),0                                                          
         BE    CEDA5                                                            
         GOTO1 ANY                                                              
         MVC   QSPQUPG,WORK                                                     
*                                                                               
CEDA5    DS    0H                                                               
         CLI   QSPQUPG,C' '        TEST ANY                                     
         BH    CEDA5D                                                           
         CLI   SQTYP,C'Q'          IF QUESTION                                  
         BNE   CEDA5B                                                           
         MVC   QSPQUPG,SQTQPG      TAKE FROM TOPIC                              
         B     CEDA5D                                                           
*                                                                               
CEDA5B   DS    0H                                                               
         CLI   SQTYP,C'A'          IF ANSWER                                    
         BNE   CEDA5D                                                           
         MVC   QSPQUPG,SQQQPG      TAKE FROM QUESTION                           
         CLI   QSPQUPG,C' '                                                     
         BH    CEDA5D                                                           
         MVC   QSPQUPG,SQTQPG      OR TOPIC                                     
*                                                                               
CEDA5D   DS    0H                                                               
         L     R2,SVFLDH           RESTORE FIRST FIELD                          
         BAS   RE,COMDSPA          DISPLAY FIELDS (MAY HAVE BEEN                
*                                  LOOKED UP)                                   
         NI    QSPSTAT,X'7F'       SET OF DDS ONLY SWITCH                       
         CLC   QSPCBPG,=CL6'*DDS  '  TEST *DDS IN CODE-BOOK PAGE                
         BNE   *+8                                                              
         OI    QSPSTAT,X'80'       SET DDS ONLY                                 
         B     XIT                                                              
         SPACE 3                                                                
*        COMDSPA- COMMON DISP 'A' (CODEBOOK PAGE + QUEST. PAGE)                 
*                 (R6 POINTS TO QSPELEM, R2 TO 1ST SCREEN FIELD)                
         SPACE 2                                                                
COMDSPA  NTR1                                                                   
         USING QSPELEM,R6                                                       
*                                                                               
         TM    QSPSTAT,X'80'       TEST DDS ONLY                                
         BZ    *+10                (USE STATUS AS REAL TEST)                    
         MVC   QSPCBPG,=CL6'*DDS  '                                             
*                                                                               
         MVC   WORK(L'QSPCBPG),QSPCBPG   CODE BOOK PAGE                         
         BAS   RE,GENDISP                                                       
*                                                                               
         BAS   RE,BUMPU                                                         
         MVC   WORK(L'QSPQUPG),QSPQUPG   QUESTIONNAIRE PAGE                     
         BAS   RE,GENDISP                                                       
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
*        WGTCLR- CLEAR ANY COMP WEIGHT COUNTS                                   
*                                                                               
WGTCLR   DS    0H                                                               
         USING QSPELEM,R6                                                       
         CLI   QSPCOMPW,C' '       TEST ANY COMP WGTS                           
         BNHR  RE                  NO- RETURN                                   
*                                                                               
         NTR1                                                                   
         MVI   ELCODE,X'25'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
WCL4     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   WCL8                                                             
         USING QSPWGTEL,R6                                                      
         XC    QSPWSAM,NOVAL       SET TO 'NO VALUE'                            
         B     WCL4                                                             
*                                                                               
WCL8     DS    0H                                                               
         B     XIT                                                              
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
         CLI   4(R6),0             DONT LOOK AT COMP WGT ELEMS                  
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
*        VALIDATE OPTIONS                                                       
         SPACE 2                                                                
VALOPT   NTR1                                                                   
         L     R3,VMPQBLK                                                       
         USING MPQBLKD,R3                                                       
         MVI   NODBA,0                                                          
         MVI   NODPOS,X'FF'        SET TO ADD AT END                            
         MVI   SAVSEQ,0            CLEAR SEQ OPT                                
         MVI   FORMOPT,C'S'        AND SPEC LIST                                
         XC    WAVDATE,WAVDATE     DEFAULT TO FIRST WAVE                        
         XC    WCTDATE,WCTDATE     ALSO FOR COUNTS                              
         XC    SAVWAVDT,SAVWAVDT                                                
         MVI   CTSW,C'N'                                                        
         MVI   WVSW,C'N'                                                        
         NI    VSPOPT,X'BF'        DONT SUPPRESS COUNTS                         
         CLI   SURWMRGE,C'Y'       UNLESS WAVES MERGED                          
         BE    *+8                                                              
         MVI   VSPOPT,X'40'        ELSE SUPRESS COUNTS                          
         LA    R5,SCANWRK                                                       
*                                                                               
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    VOPT40                                                           
         CLI   ACTNUM,ACTADD       OPTIONS VALID ONLY FOR ADD                   
         BE    VOPT1M                                                           
         CLI   ACTNUM,ACTCHA       CHANGE                                       
         BE    VOPT1M                                                           
         CLI   ACTNUM,ACTSEL       SELECT (FOR CHANGE)                          
         BE    VOPT1M                                                           
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VOPT1M                                                           
         CLI   ACTNUM,ACTDIS       AND DISPLAY                                  
         BE    VOPT1M                                                           
         LA    R5,SCANWRK                                                       
         B     VOPT91              ERROR                                        
*                                                                               
VOPT1M   DS    0H                                                               
         XC    SCANWRK,SCANWRK                                                  
         GOTO1 SCANNER,DMCB,(R2),SCANWRK                                        
*                                                                               
VOPT2    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    VOPT40                                                           
*                                                                               
         CLC   =C'WAVE',12(R5)     WAVE SET                                     
         BNE   VOPT2B                                                           
         MVI   WVSW,C'Y'           SET HAVE WAVE INPUT                          
         MVC   SAVWAVDT,WAVDATE                                                 
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
*                                                                               
         CLI   MODE,VALREC         UNLESS VALIDATING RECORD                     
         BE    VOPT20                                                           
         CLI   ACTNUM,ACTLIST      OR LISTING                                   
         BE    VOPT20                                                           
         MVI   BYTE,C'E'           FIND EXACT MATCH                             
         BAS   RE,FNDWVE           IN WAVES FOR QSPEC                           
         BE    *+12                                                             
         MVI   ERROR,WAVNFERQ      WAVE NOT DEFINED FOR QSPEC                   
         B     TRAPERR                                                          
*                                                                               
         B     VOPT20                                                           
*                                                                               
VOPT2B   DS    0H                                                               
         CLC   =C'COUNT',12(R5)    COUNT OPTION                                 
         BNE   VOPT2B8                                                          
         CLI   SURWMRGE,C'Y'       WAVES MUST BE MERGED                         
         BE    VOPT2B5                                                          
         CLI   ACTNUM,ACTLIST      IF LIST                                      
         BE    VOPT91                                                           
*                                                                               
VOPT2B5  DS    0H                                                               
         NI    VSPOPT,X'BF'        CANCEL SUPPRESS                              
         MVI   FORMOPT,C'C'        SET FORMAT TO COUNT                          
         MVC   SAVWAVDT,WAVDATE    SAVE REAL WAVE DATE                          
*                                                                               
         CLI   22(R5),C' '         IF NONE GIVEN                                
         BE    VOPT2B6                                                          
         CLI   22(R5),C'Y'         OR 'YES'                                     
         BE    VOPT2B6             COUNT DATE WILL BE SET FROM WAVDATE          
*                                  ELSE VALIDATE DATE                           
         MVI   CTSW,C'Y'           SET HAVE COUNT DATE                          
         XC    WAVDATE,WAVDATE                                                  
         MVI   OPTION2,C'W'        DATA IN WORK                                 
         MVI   OPTION,C'Y'         FULL VALIDATION                              
         MVC   WORK(8),22(R5)                                                   
         GOTO1 VALWAVE                                                          
         CLI   ERROR,0                                                          
         BNE   TRAPERR                                                          
         CLI   WAVDATE,X'FF'      IF AT FIRST WAVE                              
         BNE   *+10                                                             
         XC    WAVDATE,WAVDATE     CLEAR                                        
         MVI   BYTE,C'N'           DONT NEED EXACT MATCH                        
         BAS   RE,FNDWVE           FIND ACTIVE WAVE                             
         BE    *+12                                                             
         MVI   ERROR,COUNTERR      COUNT DATE ERROR                             
         B     TRAPERR                                                          
         B     VOPT2B7                                                          
*                                                                               
VOPT2B6  DS    0H                  NO COUNT DATE- USE FIRST WAVE                
         MVI   OPTION2,C'V'        USE VALWAVE TO SET MPQBWVDT                  
         GOTO1 VALWAVE                                                          
*                                                                               
VOPT2B7  DS    0H                                                               
         CLI   WAVDATE,X'FF'       IF AT FIRST WAVE                             
         BNE   *+10                                                             
         XC    WAVDATE,WAVDATE     CLEAR                                        
         MVC   WCTDATE,WAVDATE                                                  
         MVC   WAVDATE,SAVWAVDT                                                 
         B     VOPT20                                                           
*                                                                               
VOPT2B8  DS    0H                                                               
         CLI   TWAOFFC,C'*'        WGTS ONLY FOR DDS                            
         BNE   VOP6                                                             
         CLC   =C'WEIGHT',12(R5)   WEIGHT                                       
         BE    VOP5B                                                            
         CLC   =C'WGT',12(R5)                                                   
         BNE   VOP6                                                             
*                                                                               
VOP5B    DS    0H                                                               
         ST    R5,WGTOPTA          WGT OPTION POSITION                          
         MVC   WORK(4),22(R5)      WEIGHT CODE (BLANK MEANS DEFAULT)            
         GOTO1 VALWGT                                                           
         BNE   VOPT91                                                           
         MVI   MPQBNOWT,C'N'       ACTIVATE WGTS                                
         B     VOPT20                                                           
*                                                                               
VOP6     DS    0H                                                               
         CLI   MODE,VALKEY         TEST DOING LIST OPTIONS                      
         BE    VOPT10                                                           
*                                                                               
*                                  MAINT OPTIONS                                
*                                  -------------                                
         CLC   =C'AFT',12(R5)      AFTER                                        
         BNE   VOPT4                                                            
         MVI   NODBA,C'A'                                                       
*                                                                               
VOPT2C   DS    0H                  SET IN NODPOS                                
         CLI   ACTNUM,ACTADD       ONLY FOR ADDS                                
         BNE   VOPT91                                                           
         CLI   TOPQSP,C'$'         NOT FOR LIBRARY GROUPS                       
         BE    VOPT91                                                           
*                                                                               
         XC    NODPOS,NODPOS                                                    
         CLC   =C'START',22(R5)    FOR START LEAVE NULLS                        
         BNE   VOPT2D                                                           
         CLI   NODBA,C'B'          START ONLY WITH BEFORE                       
         BE    VOPT20                                                           
         B     VOPT91                                                           
*                                                                               
VOPT2D   DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R5)          LENGTH OF 2ND FIELD                          
         BZ    VOPT91                                                           
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NODPOS(0),22(R5)                                                 
         B     VOPT20                                                           
*                                                                               
VOPT4    DS    0H                                                               
         CLC   =C'BEF',12(R5)      BEFORE                                       
         BNE   VOPT6                                                            
         MVI   NODBA,C'B'                                                       
         B     VOPT2C                                                           
*                                                                               
VOPT6    DS    0H                                                               
         CLC   =C'TEST',12(R5)     TEST OPTION                                  
         BNE   VOPT7                                                            
         OI    VSPOPT,X'80'        SET ON                                       
         CLI   22(R5),C'N'         UNLESS N                                     
         BNE   *+8                                                              
         NI    VSPOPT,X'7F'                                                     
         B     VOPT20                                                           
*                                                                               
VOPT7    DS    0H                                                               
         B     VOPT91              ERROR- UNDEFINED ENTRY                       
*                                                                               
*                                  LIST OPTIONS                                 
*                                  -----------                                  
VOPT10   DS    0H                                                               
         CLC   =C'SEQ',12(R5)      SEQUENCE                                     
         BNE   VOPT11                                                           
         CLI   TWAOFFC,C'*'        **DDS ONLY**                                 
         BNE   VOPT91                                                           
         MVC   SAVSEQ,22(R5)                                                    
         B     VOPT20                                                           
*                                                                               
VOPT11   DS    0H                                                               
         CLC   =C'FOR',12(R5)                                                   
         BNE   VOPT12                                                           
         CLI   22(R5),C'S'         SPEC FORMAT                                  
         BE    VOPT11D                                                          
         CLI   22(R5),C'T'         TRACE FORMAT                                 
         BNE   VOPT91                                                           
         CLI   TWAOFFC,C'*'        DDS ONLY                                     
         BNE   VOPT91                                                           
*                                                                               
VOPT11D  DS    0H                                                               
         MVC   FORMOPT,22(R5)                                                   
         B     VOPT20                                                           
*                                                                               
VOPT12   DS    0H                                                               
VOPT19   DS    0H                                                               
         B     VOPT91              ERROR-UNDEFINED ENTRY                        
*                                                                               
VOPT20   DS    0H                                                               
         LA    R5,32(R5)                                                        
         B     VOPT2                                                            
*                                                                               
VOPT40   DS    0H                  END OF SCAN LIST                             
         CLI   MPQBNOWT,C'N'       IF WEIGHTS ACTIVE                            
         BNE   VOPT43                                                           
         TM    VSPOPT,X'40'        THEN COUNTS MUST BE ALSO                     
         BZ    VOPT43                                                           
         L     R5,WGTOPTA          POINT TO WGT OPTION                          
         B     VOPT91                                                           
*                                                                               
VOPT43   DS    0H                                                               
         CLI   CTSW,C'Y'           IF  COUNT DATE SET                           
         BE    VOPT49              OK                                           
         MVC   WCTDATE,WAVDATE     ELSE SET TO WAVE DATE                        
         MVI   OPTION2,C'V'        USE VALWAVE TO SET MPQBLK                    
         GOTO1 VALWAVE                                                          
         B     VOPT49D                                                          
*                                                                               
VOPT49   DS    0H                                                               
         CLI   WVSW,C'Y'           IF WAVE DATE NOT ENTERED                     
         BNE   VOPT49D             USE COUNT DATE                               
         CLC   WCTDATE,WAVDATE     ELSE THEY MUST AGREE                         
         BE    VOPT49D                                                          
*                                                                               
         MVI   ERROR,COUNTERR                                                   
         B     TRAPERR                                                          
*                                                                               
VOPT49D  DS    0H                                                               
         MVC   CWAVDAT,WCTDATE     SET COMPLEMENTED DATE                        
         XC    CWAVDAT,=2X'FF'                                                  
         TM    VSPOPT,X'40'        IF COUNTS ON                                 
         BNZ   VOPT49F                                                          
         CLI   SURWMRGE,C'N'       AND WAVES NOT MERGED                         
         BNE   VOPT49F                                                          
         MVI   FORCESPC,C'Y'       MUST RE-VALIDATE SPECS                       
*                                                                               
VOPT49F  DS    0H                                                               
         MVC   SEQOPT,SAVSEQ       NO- RESTORE LAST                             
         CLI   SEQOPT,0            IS IT SET NOW                                
         BNE   *+8                 YES-OK                                       
         MVI   SEQOPT,C'U'         NO- DEFAULT TO USER SEQ                      
         CLI   SECTLST,C'Y'        IF DOING A SECTION LIST                      
         BNE   *+8                                                              
         MVI   SEQOPT,C'K'         MUST DO KEY SEQUENTIAL                       
         CLI   LIBLST,C'Y'         OR IF DOING A LIBRARY LIST                   
         BNE   *+8                                                              
         MVI   SEQOPT,C'K'         MUST DO KEY SEQUENTIAL                       
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
         LA    R6,MSGTXT+38                                                     
         EDIT  (R5),(2,0(R6)),ALIGN=LEFT                                        
         MVI   MSGNUM,0                                                         
         GOTO1 MSGSET                                                           
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
*        FNDWVE- FIND ACTIVE WAVE FOR QSPEC                                     
*                                                                               
FNDWVE   DS    0H                                                               
         LA    RF,WVLIST                                                        
         MVC   DUB(2),WAVDATE                                                   
         XC    DUB(2),=2X'FF'       COMPLEMENT                                  
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
         MVC   WAVDATE,0(RF)                                                    
         XC    WAVDATE,=2X'FF'     COMPLEMENT                                   
         OC    SAVWAVDT,SAVWAVDT   IF HAVE OTHER WAVE DATE                      
         BZR   RE                                                               
         CLC   SAVWAVDT,WAVDATE    THIS MUST MATCH                              
         BER   RE                                                               
*                                                                               
FNDWERR  DS    0H                                                               
         LTR   RE,RE               CC NOT = ON ERROR                            
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
*        LIST RECORDS                                                           
         SPACE 3                                                                
PLIST    DS    0H                  HARD COPY                                    
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LIST     DS    0H                  ENTRY POINT FOR SCREEN LIST                  
         LA    R4,QLIST                                                         
         ST    R4,NXTQL                                                         
         USING QLISTD,R4                                                        
         LA    RF,QSLSELH          FIRST SELECT FIELD                           
         ST    RF,SELFLD                                                        
*                                                                               
         CLI   LISTSW,C'L'         FOR 'LAST'                                   
         BNE   LR3                                                              
         CLI   SEQOPT,C'U'         MUST BE USER SEQ                             
         BE    *+16                                                             
         MVI   ERROR,ACTNSERR                                                   
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
         MVC   LCQSP,QLQSPEC       SET TO FIRST DISPLAYED ON PREV SCR           
         LA    R4,QLIST            POINT TO LAST QLIST ENTRY                    
         AH    R4,=Y(QLISTDL*(MAXLIST-1))                                       
         ST    R4,NXTQL                                                         
         LA    RF,QSLLINXH         AND POSITION TO LAST SCREEN FIELD            
         ST    RF,ATHISLST                                                      
         LA    RF,QSLSELXH         AND LAST SEL FIELD                           
         ST    RF,SELFLD                                                        
*                                                                               
LR3      DS    0H                                                               
         LA    RE,QLIST            CLEAR QLIST                                  
         LA    RF,L'QLIST                                                       
         XCEF                                                                   
*                                                                               
         CLI   LISTSW,C'L'         IF 'LAST', LCQSP IS SET ABOVE                
         BE    LR3D                FROM QLIST                                   
*                                                                               
         CLI   LISTSW,C'F'         IF 'FIRST'                                   
         BNE   *+10                                                             
         XC    KEY,KEY             START OVER                                   
         OC    KEY,KEY             IF FIRST TIME                                
         BNZ   *+10                                                             
         MVC   LCQSP,SQSPEC        SET LCQSP                                    
*                                  SET HEADLINES                                
LR3D     DS    0H                                                               
         MVC   LISTHD,SPACES                                                    
         MVC   LISTHU,SPACES                                                    
         CLI   FORMOPT,C'T'        TRACE LISTING                                
         BNE   LR4D                                                             
         MVC   LISTHD(L'LHDT),LHDT                                              
         MVC   LISTHU(L'LHUT),LHUT                                              
         B     LR4M                                                             
*                                                                               
LR4D     DS    0H                                                               
         CLI   FORMOPT,C'S'        SPECS                                        
         BNE   LR4F                                                             
         MVC   LISTHD(L'LHDS),LHDS                                              
         MVC   LISTHU(L'LHUS),LHUS                                              
         B     LR4M                                                             
*                                                                               
LR4F     DS    0H                                                               
         CLI   FORMOPT,C'C'        COUNTS                                       
         BNE   LR4M                                                             
         MVC   LISTHD(L'LHDC),LHDC                                              
         MVC   LISTHU(L'LHUC),LHUC                                              
*                                                                               
*                                  NODIO CALL TO POSITION FOR SEQS              
*                                  -------------------------------              
LR4M     DS    0H                                                               
         L     R2,VNODBLK                                                       
         USING NODBLKD,R2                                                       
         MVI   NDUPDTSW,C'N'       DO NOT READ FOR UPDATE                       
         SR    RF,RF               NO HOOK ON POSITIONING READ                  
         CLI   LISTSW,C'L'         IF 'LAST'                                    
         BE    LR4P                                                             
         CLI   SEQOPT,C'B'         OR BACKWARDS                                 
         BE    LR4P                                                             
         OC    KEY,KEY             OR IF NOT FIRST TIME                         
         BNZ   LR4P                                                             
         LA    RF,LRHOOK                                                        
*                                                                               
LR4P     DS    0H                                                               
         ST    RF,NDHOOK                                                        
*                                                                               
         GOTO1 NODIO,DMCB,VNODBLK,=C'READ',LCQSP,0                              
         CLI   NDERR,0                                                          
         BNE   LRNDERR                                                          
*                                                                               
         OC    KEY,KEY             IS IT  FIRST TIME                            
         BZ    LR5                 YES                                          
         MVC   NDSQBACK,LCBACK     NO- RESTORE SAVED BACK UP CONTROL            
         MVI   NDSKIP,C'Y'         SET TO SKIP FIRST (RESET LATER IF            
         B     LR6                 NOT BACKWARDS READ)                          
*                                                                               
LR5      DS    0H                                                               
         MVI   NDSKIP,C'N'         NO SKIP ON FIRST TIME                        
         MVI   KEY,X'FF'                                                        
*                                                                               
         CLI   SQSPEC,C'$'         UNLESS LIST OF ONE LIBRARY GROUP             
         BE    *+8                                                              
         MVI   NDSQBACK,0          SET TO READ BACK THRU HIGHER LEVS            
         MVC   LCBACK,NDSQBACK     SAVE BACK UP CONTROL                         
*                                                                               
LR6      DS    0H                                                               
         LA    R3,=C'BSEQ'                                                      
         CLI   SEQOPT,C'B'         BACKWARDS                                    
         BE    LR6B                                                             
         CLI   LISTSW,C'L'         BACKWARDS IF DOING 'LAST'                    
         BE    LR6B                                                             
         MVI   NDSKIP,C'N'         NO SKIP HERE UNLESS BACKWARDS                
         LA    R3,=C'SEQ'                                                       
         CLI   SEQOPT,C'K'         'KEY' SEQUENCE                               
         BE    LR6B                                                             
         LA    R3,=C'LSEQ'         ELSE LOGICAL SEQ                             
*                                                                               
LR6B     DS    0H                                                               
         LA    RF,LRHOOK                                                        
         ST    RF,NDHOOK                                                        
         GOTO1 NODIO,DMCB,VNODBLK,(R3),LCQSP,0                                  
         CLI   NDERR,0                                                          
         BNE   LRNDERR                                                          
*                                                                               
         CLI   LISTSW,C'L'         IF DOING LAST AND REACH END                  
         BNE   LRX                                                              
         CLI   MODE,PRINTREP       UNLESS HARD COPY                             
         BE    LRX                                                              
         BAS   RE,SCRUP            ADJUST SCREEN                                
*                                                                               
LRX      DS    0H                                                               
         B     XIT                                                              
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   NDLEV,0                                                          
         BE    LRHX                                                             
*                                                                               
         CLI   SECTLST,C'Y'        IF DOING SECTION LIST                        
         BNE   LRH3                                                             
         CLI   LCQSP,C'*'          QSPEC MUST START WITH *                      
         BNH   LRH3                                                             
         MVI   NDMODE,NDEND        IF HIGH TELL NODIO WE'RE DONE                
         B     LRHX                                                             
*                                                                               
LRH3     DS    0H                                                               
         CLI   NDMODE,NDFRST       AT FIRST FOR A LEVEL                         
         BNE   LRH4                                                             
*                                                                               
         MVI   SQTYP,C' '          SET QTYPE                                    
         L     R6,NDIOA                                                         
         MVI   ELCODE,X'10'        FIND DEFINITION ELEM                         
         BAS   RE,GETEL                                                         
         BNE   LRHX                                                             
         USING QSPELEM,R6                                                       
         MVC   SQTYP,QSPTYP                                                     
*                                                                               
         CLI   TWAOFFC,C'*'        TEST DDS TERM                                
         BE    *+12                YES- OK                                      
         TM    QSPSTAT,X'80'       ELSE SKIP DDS ONLY QSPECS                    
         BNZ   LRH3D                                                            
*                                                                               
         CLC   SQTYP,LEVLIM        IF AT LEVEL LIMIT                            
         BH    LRHX                                                             
*                                                                               
LRH3D    DS    0H                                                               
         MVI   NDSKIP,C'Y'         SET TO SKIP LOWER                            
         B     LRHX                RETURN TO NODIO                              
*                                                                               
LRH4     DS    0H                                                               
         CLI   NDMODE,NDPROC                                                    
         BNE   LRH30                                                            
*                                                                               
         MVI   SQTYP,C' '          SET QTYPE                                    
         L     R6,NDIOA                                                         
         MVI   ELCODE,X'10'        FIND DEFINITION ELEM                         
         BAS   RE,GETEL                                                         
         USING QSPELEM,R6                                                       
         BNE   *+10                                                             
         MVC   SQTYP,QSPTYP                                                     
         L     R4,NXTQL            NEXT QLIST ENTRY                             
*                                                                               
         CLI   TWAOFFC,C'*'        TEST DDS TERM                                
         BE    *+12                YES- OK                                      
         TM    QSPSTAT,X'80'       ELSE SKIP DDS ONLY QSPECS                    
         BNZ   LRH30                                                            
*                                                                               
         LA    R5,P                USE P FOR HARD COPY                          
         CLI   MODE,PRINTREP                                                    
         BE    LRH6                                                             
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         MVC   QSLHED,LISTHD                                                    
         OI    QSLHEDH+6,X'80'                                                  
*                                                                               
LRH6     DS    0H                                                               
         USING LSTLIND,R5                                                       
         MVC   LQTYP,SQTYP                                                      
         MVC   QLQTYP,SQTYP        SET ALSO IN QLIST                            
*                                                                               
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   DMDSKADD,NDLVDA     SAVE CURRENT DISK ADRESS                     
         ZIC   RF,NDCKEYL                                                       
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QLQSPEC(0),LCQSP                                                 
         MVC   QLNODE,NDLVNOD      SAVE NODE                                    
*                                                                               
*              **NOTE LOCAL USE OF R6 FOR END OF TWA                            
*                                                                               
         LA    R6,2048(RA)                                                      
         LA    R6,2048(R6)                                                      
         USING CONHEADH-64+4096,R6                                              
*                                                                               
         LA    RF,QLIST            SET ADDRESS OF QLIST                         
         LA    R0,SYSUSAV                                                       
         SR    RF,R0               MUST BE RELATIVE ADDRESS                     
         STCM  RF,15,AQLIST                                                     
         CLI   RECNUM,RECQSP       IF QSPEC LIST (NOT TOPIC,                    
         BNE   *+8                 QUEST, OR ANS)                               
         OI    AQLIST,X'80'        SET TO RESET RECORD TYPE                     
*                                                                               
         CLI   NOPEND,C'Y'         TEST ANY PENDINGS                            
         BE    LRH7T                                                            
*                                                                               
         ST    R2,SAVR2            NEED TO USE R2 FOR SELECT FIELDS             
         L     R2,SELFLD           SELECT FIELD                                 
*                                                                               
         CLC   MVMNOD,QLNODE                                                    
         BNE   LRH7D                                                            
         CLC   MVMCOD,QLQSPEC                                                   
         BNE   LRH7D                                                            
         MVC   WORK(3),=C'*M '                                                  
         BAS   RE,GENDISP                                                       
         B     LRH7G                                                            
*                                                                               
LRH7D    DS    0H                                                               
         CLC   MVLNOD,QLNODE       LOCATION NODE                                
         BNE   LRH7G                                                            
         CLC   MVLPOS,NDLVCOD                                                   
         BNE   LRH7G                                                            
         MVI   WORK,C'*'                                                        
         MVC   WORK+1(1),MVBA                                                   
         MVI   WORK+2,C' '                                                      
         BAS   RE,GENDISP                                                       
*                                                                               
LRH7G    DS    0H                                                               
         L     R2,SAVR2            RESTORE R2                                   
         DROP  R6                                                               
*                                                                               
LRH7T    DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,1,NDLEV            INDENT ACCORDING TO LEVEL                  
         BZ    *+6                                                              
         BCTR  RF,R0                                                            
         MVI   LQSPEC,C'.'                                                      
         MVI   LQSPEC+1,C'.'                                                    
         LA    RF,LQSPEC(RF)                                                    
         MVC   0(8,RF),NDLVCOD                                                  
*                                                                               
         CLI   FORMOPT,C'T'        TRACE LISTING                                
         BNE   LRH8                                                             
*                                **NODES AND DISK ADDRESS**                     
         GOTO1 HEXOUT,DMCB,NDLVNOD+1,WORK,3,=C'N'                               
         MVC   LNOD1,WORK+1      5 CHARS                                        
         GOTO1 HEXOUT,DMCB,NDLVNOD2+1,WORK,3,=C'N'                              
         MVC   LNOD2,WORK+1      5 CHARS                                        
         GOTO1 HEXOUT,DMCB,NDLVDA,LDISK,4,=C'N'                                 
*                                                                               
         CLI   NDLIBLEV,0          TST WITHIN LIBRARY CALL                      
         BE    LRH7U                                                            
         CLC   NDLEV,NDLIBLEV                                                   
         BNH   LRH7U                                                            
         MVI   LNOD1-1,C'$'                                                     
*                                                                               
LRH7U    DS    0H                                                               
         MVC   LBACK,NDLVBACK                                                   
         MVC   LFWRD,NDLVFWRD                                                   
         MVC   LFIRST,NDLVFRST                                                  
         MVC   LLAST,NDLVLAST                                                   
*                                                                               
         CLC   NDLEV,NDLIBLEV      TEST LIBRARY CALLER                          
         BNE   LRH20                                                            
         MVC   LLAST,SPACES                                                     
         MVC   LFIRST(2),=C'$='                                                 
         MVC   LFIRST+2(8),NDATTCOD                                             
         B     LRH20                                                            
         DROP  R3                                                               
*                                                                               
LRH8     DS    0H                                                               
         MVI   ELCODE,X'C1'        TITLE                                        
         MVI   FREEFRST,C'Y'                                                    
         MVI   FREECTL,X'40'       SET TO RETURN IN WORK                        
         MVI   FREEKEY,0                                                        
         MVI   FREEKLN,0                                                        
         GOTO1 DISPFREE                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,NDLEV            INDENT ACCORDING TO LEVEL                  
         BZ    *+6                                                              
         BCTR  RF,R0                                                            
         MVI   LTITLE,C'.'                                                      
         MVI   LTITLE+1,C'.'                                                    
         LA    RF,LTITLE(RF)                                                    
         MVC   0(L'LTITLE-2,RF),WORK                                            
*                                                                               
         BAS   RE,GETDFEL                                                       
         USING QSPELEM,R6                                                       
         MVC   LTYPE(1),QSPTYP     SET TYPE                                     
         CLI   QSPQTYP,C'N'        ALSO IDENTIFY NUMERIC Q'S                    
         BNE   *+8                                                              
         MVI   LTYPE+1,C'N'                                                     
*                                                                               
         CLI   FORMOPT,C'C'        COUNT FORMAT                                 
         BNE   LRH10               ------------                                 
*                                                                               
         LA    R2,QSPSAM                                                        
         LA    R3,LSAM                                                          
         BAS   RE,EDTCNT                                                        
         B     LRH20                                                            
*                                                                               
LRH10    DS    0H                                                               
         CLI   FORMOPT,C'S'        SPEC LISTING                                 
         BNE   LRH20                                                            
*                                                                               
         MVI   ELCODE,X'51'        USE 51'S                                     
         CLI   SQTYP,C'A'          FOR ANSWERS                                  
         BE    LRH11                                                            
         CLI   QSPQTYP,C'N'        AND NUMERIC Q'S                              
         BE    LRH11                                                            
         MVI   ELCODE,X'52'        ELSE USE 52'S (BASES)                        
*                                                                               
LRH11    DS    0H                                                               
         MVI   FREEKLN,3           KEY=DATE(2) + SPARE                          
         MVC   FREEKEY(2),CWAVDAT                                               
         MVI   FREEKEY+2,0                                                      
         MVI   FREECTL,X'40'       RETURN IN WORK                               
         MVI   FREEFRST,C'Y'                                                    
         GOTO1 DISPFREE                                                         
         MVC   LSPEC,WORK                                                       
*                                                                               
LRH20    DS    0H                                                               
         MVC   QLNDLEV,NDLEV       SAVE NODIO LEVEL                             
         CLI   LISTSW,C'L'         IF DOING 'LAST'                              
         BE    LRH21               SPECIAL                                      
         CLI   MODE,PRINTREP       IF HARD COPY QLIST NOT NEEDED                
         BE    LRH23               (SKIP BUMP TO AVOID OVERFLOW)                
         LA    R4,QLISTDL(R4)      ELSE NEXT QLIST ENTRY                        
         ST    R4,NXTQL                                                         
         L     R2,SELFLD           BUMP SEL FIELD POINTER                       
         LA    R2,LSTSCRL(R2)                                                   
         ST    R2,SELFLD                                                        
         B     LRH23                                                            
*                                                                               
LRH21    DS    0H                  FOR 'LAST' LIST                              
         MVC   SAVTHISL,ATHISLST                                                
         SH    R4,=Y(QLISTDL)      BACK UP IN QLIST                             
         ST    R4,NXTQL                                                         
         L     R2,SELFLD           BACK UP SEL FIELD POINTER                    
         SH    R2,=Y(LSTSCRL)                                                   
         ST    R2,SELFLD                                                        
*                                                                               
LRH23    DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LRH24                                                            
*                                                                               
         MVI   NDREREAD,C'Y'      FORCE RE-READ (IRRELEVANT DURING LIST         
         GOTO1 LISTMON            BUT NECESSARY AFTER)                          
         CLI   LISTSW,C'L'         IF DOING 'LAST'                              
         BNE   LRH30                                                            
*                                                                               
         L     RF,SAVTHISL                       BACK UP ON SCREEN              
         SH    RF,=Y(LSTSCRL)                                                   
         ST    RF,ATHISLST                                                      
         B     LRH30                                                            
*                                                                               
LRH24    DS    0H                                                               
         LA    R0,10                                                            
LRH24B   DS    0H                                                               
         MVC   LISTHU,P                                                         
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         MVC   P,LISTHU                                                         
         BCT   R0,LRH24B                                                        
         B     LRH30                                                            
*                                                                               
         DROP  R4                                                               
         DROP  R5                                                               
         DROP  R2                                                               
*                                                                               
LRH30    DS    0H                                                               
LRHX     DS    0H                                                               
         B     XIT                 RETURN TO NODIO                              
         SPACE 2                                                                
LRNDERR  DS    0H                                                               
         LA    R2,QSLQSPH                                                       
         GOTO1 NODERRP                                                          
         SPACE 2                                                                
*        SCRUP- MOVE SCREEN LINES UP AT END OF LASTS                            
         SPACE 1                                                                
SCRUP    DS    0H                                                               
         CLI   LISTNUM,0                                                        
         BER   RE                                                               
         LA    R2,QSLSELH          FIRST SELECT                                 
         L     R4,SAVTHISL         LAST LINE ACTUALLY USED                      
         SH    R4,=Y(L'QSLSEL+8)   BACK UP TO SELECT FIELD                      
         CR    R4,R2               TEST HAD A FULL SCREEN                       
         BNHR  RE                  YES- NO ADJUST NECESSARY                     
*                                                                               
         LA    R3,QLIST            TOP OF QLIST                                 
         L     R5,NXTQL                                                         
         LA    R5,QLISTDL(R5)       LAST QLIST SLOT USED                        
*                                                                               
         ZIC   R6,LISTNUM          COUNT OF ITEMS                               
*                                                                               
SCRUP4   DS    0H                                                               
         MVC   8(L'QSLSEL,R2),8(R4)      MOVE SELECT FIELD                      
         OI    6(R2),X'80'                                                      
         MVC   L'QSLSEL+16(L'QSLLIN,R2),L'QSLSEL+16(R4)  LINE                   
         OI    L'QSLSEL+8+6(R2),X'80'                                           
         MVC   0(QLISTDL,R3),0(R5)       QLIST ENTRY                            
*                                                                               
         LA    R2,LSTSCRL(R2)                                                   
         LA    R4,LSTSCRL(R4)                                                   
         LA    R3,QLISTDL(R3)                                                   
         LA    R5,QLISTDL(R5)                                                   
         BCT   R6,SCRUP4                                                        
*                                  CLEAR REST OF SCREEN                         
         LR    R0,RE                                                            
         GOTO1 CLRSCR,DMCB,(C'A',(R2)),0                                        
         LR    RE,R0                                                            
*                                                                               
SCRUPX   DS    0H                                                               
         BR    RE                                                               
         SPACE 2                                                                
LSTSCRL  EQU   L'QSLSEL+8+L'QSLLIN+8   LENGTH OF LINE ON LIST SCREEN            
         SPACE 2                                                                
LHDT     DC    C'QSPEC      PREV     NEXT     FIRST    LAST     NODE1 NX        
               ODE2 DISKADDR'                                                   
LHUT     DC    C'-----      ----     ----     -----    ----     ----- -X        
               ---- --------'                                                   
LHDC     DC    C'QSPEC      TITLE                 TYPE  SAMPLE SIZE'            
LHUC     DC    C'-----      -----                 ----  -----------'            
LHDS     DC    C'QSPEC      TITLE                 TYPE  SPECIFICATION'          
LHUS     DC    C'-----      -----                 ----  -------------'          
         SPACE 2                                                                
EDTCNT   DS    0H                  EDIT COUNTS                                  
         CLC   0(4,R2),NOVAL       TEST NO VALUE                                
         BER   RE                                                               
         EDIT  (B4,0(R2)),(11,0(R3)),COMMAS=YES,ZERO=NOBLANK                    
         BR    RE                                                               
*                                                                               
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H5,LISTHD                                                        
         MVC   H6,LISTHU                                                        
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,2,C'MEDIA PLANNING SYSTEM'                                    
         SSPEC H2,2,C'---------------------'                                    
         SSPEC H1,40,C'QSPEC LISTING'                                           
         SSPEC H2,45,C'-------------'                                           
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,96,REQUESTOR                                                  
         SSPEC H5,77,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'               END MARKER FOR SPECS                         
         EJECT                                                                  
*              VARIOUS SHORT ROUTINES                                           
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
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
BUMPU    ZIC   R0,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS- RETURN =                                
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
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE MPGENQS                                                        
       ++INCLUDE MPQBLKD                                                        
       ++INCLUDE MPRDRFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDRFAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDRFBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDRFCD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDREAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDREBD                                                       
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDRECD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDNODBLKD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE MPRDRWORKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
SYSD     DSECT                     RETURN TO SYSD DSECT                         
         ORG   SYSUSAV             USER SAVE AREA                               
LEVLIM   DS    CL1                                                              
BACKCTL  DS    CL1                                                              
LCBACK   DS    CL1                                                              
LCQSP    DS    CL30                                                             
SEQOPT   DS    CL1                                                              
SAVSEQ   DS    CL1                                                              
SECTLST  DS    C                                                                
LIBLST   DS    C                                                                
FORMOPT  DS    CL1                                                              
NODBA    DS    C                                                                
NODPOS   DS    CL8                                                              
*                                                                               
QLIST    DS    CL(MAXLIST*QLISTDL)                                              
*                                                                               
*                                                                               
         SPACE 3                                                                
         ORG   SYSUNON             NON-SAVED AREA                               
SVDA     DS    F                                                                
SVDA2    DS    F                                                                
WVLIST   DS    XL100                                                            
SAVR2    DS    F                                                                
SAVTHISL DS    F                                                                
SELFLD   DS    F                                                                
WGTOPTA  DS    A                                                                
CWAVDAT  DS    XL2                 COMPLEMENTED WAVE DATE                       
SAVWAVDT DS    XL2                                                              
CTSW     DS    CL1                 COUNT SW                                     
WVSW     DS    CL1                 WAVE SW                                      
OLDTYP   DS    CL1                                                              
CPYOLD   DS    CL30                                                             
LISTHD   DS    CL132                                                            
LISTHU   DS    CL132                                                            
*                                                                               
         SPACE 3                                                                
LSTLIND  DSECT                     DSECT FOR LIST DISPLAY LINE                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LQSPEC   DS    CL10                                                             
         DS    CL1                                                              
LBACK    DS    CL8                 TRACE LINE                                   
         DS    CL1                                                              
LFWRD    DS    CL8                                                              
         DS    CL1                                                              
LFIRST   DS    CL8                                                              
         DS    CL1                                                              
LLAST    DS    CL8                                                              
         DS    CL1                                                              
LNOD1    DS    CL5                                                              
         DS    CL1                                                              
LNOD2    DS    CL5                                                              
         DS    CL1                                                              
LDISK    DS    CL8                                                              
LQTYP    DS    CL1                                                              
*                                                                               
         ORG   LBACK               COUNT LINE                                   
LTITLE   DS    CL22                                                             
         DS    CL1                                                              
LTYPE    DS    CL3                                                              
         DS    CL1                                                              
LSAM     DS    CL11                                                             
*                                                                               
         ORG   LSAM                SPEC LINE                                    
         DS    CL1                                                              
LSPEC    DS    CL29                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045MPRDR0A   05/01/02'                                      
         END                                                                    
